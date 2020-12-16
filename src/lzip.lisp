;;; This file is part of cl-lzlib
;;; Copyright 2019-2020 Guillaume LE VAILLANT
;;; Distributed under the GNU GPL v3 or later.
;;; See the file LICENSE for terms of use and distribution.

(in-package :lzlib)


(eval-when (:compile-toplevel :load-toplevel :execute)
  (defconstant +buffer-size+ 65536))

(deftype u8 () '(unsigned-byte 8))


;;;
;;; Errors
;;;

(define-condition lzlib-error (simple-error)
  ())

(defmacro lz-error (message &rest args)
  `(error 'lzlib-error
          :format-control ,message
          :format-arguments (list ,@args)))


;;;
;;; Compression functions
;;;

(defun compress (encoder input output member-size)
  "Read the data from the INPUT octet stream, compress it with the ENCODER, and
write the result to the OUTPUT octet stream."
  (declare (type (unsigned-byte 63) member-size)
           (optimize (speed 3) (space 0) (debug 0) (safety 1)))
  (let ((buffer (cffi:make-shareable-byte-vector #.+buffer-size+)))
    (declare (dynamic-extent buffer))
    (cffi:with-pointer-to-vector-data (ffi-buffer buffer)
      (labels ((write-to-compressor (processed)
                 (declare (type i32 processed))
                 (let ((size (min (lz-compress-write-size encoder)
                                  +buffer-size+)))
                   (declare (type (integer 0 #.+buffer-size+) size))
                   (if (plusp size)
                       (let ((n (read-sequence buffer input :end size)))
                         (declare (type (integer 0 #.+buffer-size+) n))
                         (cond
                           ((and (plusp n)
                                 (/= (lz-compress-write encoder ffi-buffer n) n))
                            (lz-error "Library error (LZ-COMPRESS-WRITE)."))
                           ((< n size)
                            (lz-compress-finish encoder)
                            (+ processed n))
                           (t
                            (write-to-compressor (+ processed n)))))
                       processed)))
               (read-from-compressor (processed)
                 (declare (type i32 processed))
                 (let ((n (lz-compress-read encoder ffi-buffer +buffer-size+)))
                   (declare (type i32 n))
                   (cond
                     ((minusp n)
                      (lz-error "LZ-COMPRESS-READ error: ~a."
                                (lz-strerror (lz-compress-errno encoder))))
                     ((plusp n)
                      (write-sequence buffer output :end n)
                      (read-from-compressor (+ processed n)))
                     (t
                      processed))))
               (compress-data ()
                 (let* ((in-size (write-to-compressor 0))
                        (out-size (read-from-compressor 0)))
                   (declare (type i32 in-size out-size))
                   (cond
                     ((and (zerop in-size) (zerop out-size))
                      (lz-error "Library error (LZ-COMPRESS-READ)."))
                     ((zerop (lz-compress-member-finished encoder))
                      (compress-data))
                     ((= (lz-compress-finished encoder) 1)
                      t)
                     ((minusp (lz-compress-restart-member encoder member-size))
                      (lz-error "LZ-COMPRESS-RESTART-MEMBER error: ~a."
                                (lz-strerror (lz-compress-errno encoder))))
                     (t
                      (compress-data))))))
        (compress-data)))))

(defun lzma-options (level dictionary-size match-len-limit)
  "Get the LZMA parameters matching the given arguments."
  (cond
    ((and dictionary-size (not match-len-limit))
     (lz-error "MATCH-LEN-LIMIT is not set."))
    ((and match-len-limit (not dictionary-size))
     (lz-error "DICTIONARY-SIZE is not set."))
    ((and dictionary-size match-len-limit)
     (let ((min-dictionary-size (lz-min-dictionary-size))
           (max-dictionary-size (lz-max-dictionary-size))
           (min-match-len-limit (lz-min-match-len-limit))
           (max-match-len-limit (lz-max-match-len-limit)))
       (cond
         ((not (and (integerp dictionary-size)
                    (<= min-dictionary-size
                        dictionary-size
                        max-dictionary-size)))
          (lz-error "DICTIONARY-SIZE must be between ~d and ~d."
                    min-dictionary-size
                    max-dictionary-size))
         ((not (and (integerp match-len-limit)
                    (<= min-match-len-limit
                        match-len-limit
                        max-match-len-limit)))
          (lz-error "MATCH-LEN-LIMIT must be between ~d and ~d."
                    min-match-len-limit
                    max-match-len-limit))
         (t
          (list dictionary-size match-len-limit)))))
    (level
     (case level
       ((0) '(65535 16))
       ((1) '(1048576 5))
       ((2) '(1572864 6))
       ((3) '(2097152 8))
       ((4) '(3145728 12))
       ((5) '(4194304 20))
       ((6) '(8388608 36))
       ((7) '(16777216 68))
       ((8) '(25165824 132))
       ((9) '(33554432 273))
       (t (lz-error "LEVEL must be between 0 and 9."))))
    (t
     (lz-error "Either LEVEL or DICTIONARY-SIZE and MATCH-LEN-LIMIT must be set."))))

(defun compress-stream-1 (input output
                          &key
                            (level 6) (member-size 2251799813685248)
                            dictionary-size match-len-limit)
  "Read the data from the INPUT octet stream, compress it, and write the result
to the OUTPUT octet stream."
  (cond
    ((not (and (integerp member-size)
               (<= 100000 member-size 2251799813685248)))
     (lz-error "MEMBER-SIZE must be bewteen 100000 and 2251799813685248."))
    (t
     (destructuring-bind (dictionary-size match-len-limit)
         (lzma-options level dictionary-size match-len-limit)
       (let* ((encoder (lz-compress-open dictionary-size
                                         match-len-limit
                                         member-size))
              (errno (if (cffi:null-pointer-p encoder)
                         +lz-mem-error+
                         (lz-compress-errno encoder))))
         (unwind-protect
              (case errno
                ((#.+lz-ok+)
                 (compress encoder input output member-size))
                ((#.+lz-mem-error+)
                 (lz-error "Not enough memory. Try a smaller dictionary size."))
                (t
                 (lz-error "Invalid argument to encoder.")))
           (lz-compress-close encoder)))))))

(defun compress-stream-n (input output
                          &key
                            (threads 2) (level 6) member-size
                            dictionary-size match-len-limit)
  "Read the data from the INPUT octet stream, compress it using multiple
threads, and write the result to the OUTPUT octet stream."
  (destructuring-bind (dictionary-size match-len-limit)
      (lzma-options level dictionary-size match-len-limit)
    (let ((lparallel:*kernel* (lparallel:make-kernel threads))
          (queue (lparallel.queue:make-queue))
          (buffer (make-array +buffer-size+ :element-type 'u8))
          (member-size (or member-size (* 2 dictionary-size))))
      (labels ((read-block (size pipe first-read-p)
                 (let ((n (read-sequence buffer input
                                         :end (min size +buffer-size+))))
                   (cond
                     ((zerop n)
                      (not first-read-p))
                     (t
                      (write-sequence buffer pipe :end n)
                      (read-block (- size n) pipe nil)))))
               (compress-block (pipe)
                 (unwind-protect
                      (octet-streams:with-octet-output-stream (output)
                        (compress-stream-1 pipe output
                                           :level level
                                           :member-size member-size
                                           :dictionary-size dictionary-size
                                           :match-len-limit match-len-limit))
                   (close pipe)))
               (add-task ()
                 (let ((pipe (octet-streams:make-octet-pipe)))
                   (if (read-block member-size pipe t)
                       (let ((task (lparallel:future (compress-block pipe))))
                         (lparallel.queue:push-queue task queue))
                       (close pipe))))
               (process-queue ()
                 (unless (lparallel.queue:queue-empty-p queue)
                   (let* ((task (lparallel.queue:pop-queue queue))
                          (compressed-data (lparallel:force task)))
                     (write-sequence compressed-data output))
                   (add-task)
                   (process-queue))))
        (unwind-protect
             (lparallel:task-handler-bind ((error (lambda (e)
                                                    (error e))))
               (dotimes (i threads)
                 (add-task))
               (process-queue)
               t)
          (lparallel:end-kernel))))))

(defun compress-stream (input output
                        &key
                          (threads 1) (level 6) (member-size 2251799813685248)
                          dictionary-size match-len-limit)
  "Read the data from the INPUT octet stream, compress it, and write the result
to the OUTPUT octet stream."
  (if (< threads 2)
      (compress-stream-1 input output
                         :level level
                         :member-size member-size
                         :dictionary-size dictionary-size
                         :match-len-limit match-len-limit)
      (compress-stream-n input output
                         :threads threads
                         :level level
                         ;; If a member size different from the default one is
                         ;; specified, use it. Otherwise the default of twice
                         ;; the dictionary size will be used.
                         :member-size (if (/= member-size 2251799813685248)
                                          member-size
                                          nil)
                         :dictionary-size dictionary-size
                         :match-len-limit match-len-limit)))

(defun compress-file (input output
                      &key
                        (threads 1) (level 6) (member-size 2251799813685248)
                        dictionary-size match-len-limit)
  "Read the data from the INPUT file, compress it, and write the result to the
OUTPUT file."
  (with-open-file (input-stream input :element-type 'u8)
    (with-open-file (output-stream output :direction :output :element-type 'u8)
      (compress-stream input-stream output-stream
                       :threads threads
                       :level level
                       :member-size member-size
                       :dictionary-size dictionary-size
                       :match-len-limit match-len-limit))))

(defun compress-buffer (buffer
                        &key
                          (start 0) end (threads 1) (level 6)
                          (member-size 2251799813685248)
                          dictionary-size match-len-limit)
  "Read the data between the START and END offsets in the BUFFER, compress it,
and return the resulting octet vector."
  (let ((end (or end (length buffer))))
    (octet-streams:with-octet-output-stream (output)
      (octet-streams:with-octet-input-stream (input buffer start end)
        (compress-stream input output
                         :threads threads
                         :level level
                         :member-size member-size
                         :dictionary-size dictionary-size
                         :match-len-limit match-len-limit)))))


;;;
;;; Decompression functions
;;;

(defun decompress (decoder input output ignore-trailing loose-trailing)
  "Read the data from the INPUT octet stream, decompress it with the DECODER,
and write the result to the OUTPUT octet stream."
  (declare (optimize (speed 3) (space 0) (debug 0) (safety 1)))
  (let ((buffer (cffi:make-shareable-byte-vector #.+buffer-size+)))
    (declare (dynamic-extent buffer))
    (cffi:with-pointer-to-vector-data (ffi-buffer buffer)
      (labels ((write-to-decompressor (processed)
                 (declare (type i32 processed))
                 (let ((size (min (lz-decompress-write-size decoder)
                                  +buffer-size+)))
                   (declare (type i32 size))
                   (if (plusp size)
                       (let ((n (read-sequence buffer input :end size)))
                         (declare (type (integer 0 #.+buffer-size+) n))
                         (cond
                           ((and (plusp n)
                                 (/= (lz-decompress-write decoder ffi-buffer n) n))
                            (lz-error "Library error (LZ-DECOMPRESS-WRITE)."))
                           ((< n size)
                            (lz-decompress-finish decoder)
                            (+ processed n))
                           (t
                            (write-to-decompressor (+ processed n)))))
                       processed)))
               (read-from-decompressor (processed first-member-p)
                 (declare (type i32 processed)
                          (type boolean first-member-p))
                 (let ((n (lz-decompress-read decoder ffi-buffer +buffer-size+)))
                   (declare (type i32 n))
                   (if (minusp n)
                       (values n first-member-p)
                       (let ((first-member-p
                               (when first-member-p
                                 (zerop (lz-decompress-member-finished decoder)))))
                         (declare (type boolean first-member-p))
                         (cond
                           ((plusp n)
                            (write-sequence buffer output :end n)
                            (read-from-decompressor (+ processed n)
                                                    first-member-p))
                           (t
                            (values processed first-member-p)))))))
               (process-error (first-member-p)
                 (declare (type boolean first-member-p))
                 (let ((member-pos (lz-decompress-member-position decoder))
                       (pos (lz-decompress-total-in-size decoder)))
                   (declare (type u64 member-pos pos))
                   (case (lz-decompress-errno decoder)
                     ((#.+lz-library-error+)
                      (lz-error "Library error (LZ-DECOMPRESS-READ)."))
                     ((#.+lz-header-error+)
                      (cond
                        (first-member-p
                         (lz-error "Bad magic number (file not in lzip format)."))
                        ((not ignore-trailing)
                         (lz-error "Trailing data not allowed."))
                        (t
                         t)))
                     ((#.+lz-mem-error+)
                      (lz-error "Not enough memory."))
                     ((#.+lz-unexpected-eof+)
                      (cond
                        ((> member-pos 6)
                         (lz-error "File ends unexpectedly at position ~d." pos))
                        (first-member-p
                         (lz-error "File ends unexpectedly at member header."))
                        (t
                         (lz-error "Truncated header in multimember file."))))
                     ((#.+lz-data-error+)
                      (cond
                        ((> member-pos 6)
                         (lz-error "Decoder error at position ~d." pos))
                        ((= member-pos 4)
                         (lz-error "Version ~d member format not supported."
                                   (lz-decompress-member-version decoder)))
                        ((= member-pos 5)
                         (lz-error "Invalid dictionary size in member header."))
                        (first-member-p
                         (lz-error "Bad version or dictionary size in member header."))
                        ((not loose-trailing)
                         (lz-error "Corrupt header in multimember file."))
                        ((not ignore-trailing)
                         (lz-error "Trailing data not allowed."))
                        (t
                         t)))
                     (t
                      (lz-error "Decoder error at position ~d." pos)))))
               (decompress-data (first-member-p)
                 (declare (type boolean first-member-p))
                 (let ((in-size (write-to-decompressor 0)))
                   (declare (type i32 in-size))
                   (multiple-value-bind (out-size first-member-p)
                       (read-from-decompressor 0 first-member-p)
                     (declare (type i32 out-size)
                              (type boolean first-member-p))
                     (cond
                       ((or (minusp out-size)
                            (and (zerop out-size) first-member-p))
                        (process-error first-member-p))
                       ((= (lz-decompress-finished decoder) 1)
                        t)
                       ((and (zerop in-size) (zerop out-size))
                        (lz-error "Library error (stalled)."))
                       (t
                        (decompress-data first-member-p)))))))
        (decompress-data t)))))

(defun decompress-stream-1 (input output
                            &key
                              (ignore-trailing t) loose-trailing)
  "Read the data from the INPUT octet stream, decompress it, and write the
result to the OUTPUT octet stream."
  (let* ((decoder (lz-decompress-open))
         (errno (if (cffi:null-pointer-p decoder)
                    +lz-mem-error+
                    (lz-decompress-errno decoder))))
    (unwind-protect
         (case errno
           ((#.+lz-ok+)
            (decompress decoder input output ignore-trailing loose-trailing))
           (t
            (lz-error "Not enough memory.")))
      (lz-decompress-close decoder))))

(defun decompress-stream-n (input output
                            &key
                              (threads 2) (ignore-trailing t) loose-trailing)
  "Read the data from the INPUT octet stream, decompress it using multiple
threads, and write the result to the OUTPUT octet stream."
  (let ((lparallel:*kernel* (lparallel:make-kernel threads))
        (queue (lparallel.queue:make-queue))
        (buffer (make-array 1048576 :element-type 'u8))
        (end 0))
    (labels ((find-magic (buffer start end)
               (declare (type (simple-array u8 (*)) buffer)
                        (type fixnum start end)
                        (optimize (speed 3)))
               (flet ((jump-size (x)
                        (declare (type u8 x))
                        (case x
                          ((73) 1)
                          ((76) 3)
                          ((90) 2)
                          (t 4))))
                 (do ((index start
                             (+ index (jump-size (aref buffer (+ index 3))))))
                     ((> index (- end 4)) nil)
                   (when (and (= (aref buffer (+ index 3)) 80)
                              (= (aref buffer (+ index 2)) 73)
                              (= (aref buffer (+ index 1)) 90)
                              (= (aref buffer index) 76))
                     (return index)))))
             (read-member-size (buffer start)
               (logior (aref buffer start)
                       (ash (aref buffer (+ start 1)) 8)
                       (ash (aref buffer (+ start 2)) 16)
                       (ash (aref buffer (+ start 3)) 24)
                       (ash (aref buffer (+ start 4)) 32)
                       (ash (aref buffer (+ start 5)) 48)
                       (ash (aref buffer (+ start 6)) 56)
                       (ash (aref buffer (+ start 7)) 64)))
             (read-data (start)
               (read-sequence buffer input :start start))
             (decompress-member (pipe)
               (unwind-protect
                    (octet-streams:with-octet-output-stream (output)
                      (decompress-stream-1 pipe output
                                           :ignore-trailing ignore-trailing
                                           :loose-trailing loose-trailing))
                 (close pipe)))
             (read-member (member-size pipe)
               (setf end (read-data end))
               (let ((index (find-magic buffer 0 end)))
                 (cond
                   ((and (zerop end) (zerop member-size))
                    nil)
                   ((null index)
                    ;; Continue reading the current member.
                    (let ((n (- end 8)))
                      (cond
                        ((plusp n)
                         (write-sequence buffer pipe :end n)
                         (replace buffer buffer :start2 n :end2 end)
                         (decf end n)
                         (read-member (+ member-size n) pipe))
                        (t
                         ;; End of stream.
                         (write-sequence buffer pipe :end end)
                         (setf end 0)
                         t))))
                   ((zerop index)
                    ;; New member.
                    (write-sequence buffer pipe :end 4)
                    (replace buffer buffer :start2 4 :end2 end)
                    (decf end 4)
                    (read-member 4 pipe))
                   ((and (>= index 8)
                         (= (read-member-size buffer (- index 8))
                            (+ member-size index)))
                    ;; Finish reading the current member.
                    (write-sequence buffer pipe :end index)
                    (replace buffer buffer :start2 index :end2 end)
                    (decf end index)
                    t)
                   (t
                    (lz-error "Bad member in archive.")))))
             (add-task ()
               (let ((pipe (octet-streams:make-octet-pipe)))
                 (if (read-member 0 pipe)
                     (let ((task (lparallel:future (decompress-member pipe))))
                       (lparallel.queue:push-queue task queue))
                     (close pipe))))
             (process-queue ()
               (unless (lparallel.queue:queue-empty-p queue)
                 (let* ((task (lparallel.queue:pop-queue queue))
                        (data (lparallel:force task)))
                   (write-sequence data output))
                 (add-task)
                 (process-queue))))
      (unwind-protect
           (lparallel:task-handler-bind ((error (lambda (e)
                                                  (error e))))
             (dotimes (i threads)
               (add-task))
             (process-queue)
             t)
        (lparallel:end-kernel)))))

(defun decompress-stream (input output
                          &key
                            (threads 1) (ignore-trailing t) loose-trailing)
  "Read the data from the INPUT octet stream, decompress it, and write the
result to the OUTPUT octet stream."
  (if (< threads 2)
      (decompress-stream-1 input output
                           :ignore-trailing ignore-trailing
                           :loose-trailing loose-trailing)
      (decompress-stream-n input output
                           :threads threads
                           :ignore-trailing ignore-trailing
                           :loose-trailing loose-trailing)))

(defun decompress-file (input output
                        &key
                          (threads 1) (ignore-trailing t) loose-trailing)
  "Read the data from the INPUT file, decompress it, and write the result to the
OUTPUT file."
  (with-open-file (input-stream input :element-type 'u8)
    (with-open-file (output-stream output :direction :output :element-type 'u8)
      (decompress-stream input-stream output-stream
                         :threads threads
                         :ignore-trailing ignore-trailing
                         :loose-trailing loose-trailing))))

(defun decompress-buffer (buffer
                          &key
                            (start 0) end (threads 1)
                            (ignore-trailing t) loose-trailing)
  "Read the data between the START and END offsets in the BUFFER, decompress it,
and return the resulting octet vector."
  (let ((end (or end (length buffer))))
    (octet-streams:with-octet-output-stream (output)
      (octet-streams:with-octet-input-stream (input buffer start end)
        (decompress-stream input output
                           :threads threads
                           :ignore-trailing ignore-trailing
                           :loose-trailing loose-trailing)))))
