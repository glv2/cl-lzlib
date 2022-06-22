;;; This file is part of cl-lzlib
;;; Copyright 2019-2022 Guillaume LE VAILLANT
;;; Distributed under the GNU GPL v3 or later.
;;; See the file LICENSE for terms of use and distribution.

(in-package :lzlib)


(deftype u8 () '(unsigned-byte 8))
(defconstant +buffer-size+ 65536)


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
;;; Compression Gray streams
;;;

(defclass compressing-stream (fundamental-binary-output-stream)
  ((output-stream :accessor output-stream)
   (encoder :accessor encoder)
   (member-size :accessor member-size)
   (buffer :accessor buffer)))

(defmethod stream-element-type ((stream compressing-stream))
  '(unsigned-byte 8))

(defun compress-and-write (stream)
  (with-slots (output-stream encoder buffer buffer-end) stream
    (cffi:with-pointer-to-vector-data (ffi-buffer buffer)
      (do ((n (lz-compress-read encoder ffi-buffer +buffer-size+)
              (lz-compress-read encoder ffi-buffer +buffer-size+)))
          ((zerop n))
        (if (minusp n)
            (lz-error "LZ-COMPRESS-READ error: ~a."
                      (lz-strerror (lz-compress-errno encoder)))
            (write-sequence buffer output-stream :end n))))))

(defmethod stream-write-byte ((stream compressing-stream) byte)
  (with-slots (encoder member-size buffer) stream
    (cffi:with-pointer-to-vector-data (ffi-buffer buffer)
      (setf (aref buffer 0) byte)
      (when (= (lz-compress-member-finished encoder) 1)
        (when (minusp (lz-compress-restart-member encoder member-size))
          (lz-error "LZ-COMPRESS-RESTART-MEMBER error: ~a."
                    (lz-strerror (lz-compress-errno encoder)))))
      (unless (= (lz-compress-write encoder ffi-buffer 1) 1)
        (lz-error "Library error (LZ-COMPRESS-WRITE)."))))
  (compress-and-write stream)
  byte)

(defmethod stream-write-sequence ((stream compressing-stream) seq start end
                                  &key &allow-other-keys)
  (with-slots (encoder member-size buffer) stream
    (cffi:with-pointer-to-vector-data (ffi-buffer buffer)
      (do ((n (min (lz-compress-write-size encoder)
                   (- end start)
                   +buffer-size+)
              (min (lz-compress-write-size encoder)
                   (- end start)
                   +buffer-size+)))
          ((zerop n))
        (replace buffer seq :end1 n :start2 start)
        (when (= (lz-compress-member-finished encoder) 1)
          (when (minusp (lz-compress-restart-member encoder member-size))
            (lz-error "LZ-COMPRESS-RESTART-MEMBER error: ~a."
                      (lz-strerror (lz-compress-errno encoder)))))
        (unless (= (lz-compress-write encoder ffi-buffer n) n)
          (lz-error "Library error (LZ-COMPRESS-WRITE)."))
        (incf start n)
        (compress-and-write stream))))
  start)

(defmethod stream-finish-output ((stream compressing-stream))
  (with-slots (output-stream encoder buffer) stream
    (lz-compress-finish encoder)
    (compress-and-write stream)
    (finish-output output-stream))
  nil)

(defmethod close ((stream compressing-stream) &key &allow-other-keys)
  (when (open-stream-p stream)
    (finish-output stream)
    (with-slots (encoder buffer) stream
      (lz-compress-close encoder)
      (setf encoder nil)
      (setf buffer nil)))
  t)

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

(defun make-compressing-stream (output-stream
                                &key
                                  (level 6) (member-size 2251799813685248)
                                  dictionary-size match-len-limit)
  "Return a stream that will compress the bytes written to it at the given
compression LEVEL and write them to the OUTPUT-STREAM."
  (let ((stream (make-instance 'compressing-stream)))
    (setf (output-stream stream) output-stream)
    (unless (and (integerp member-size)
                 (<= 100000 member-size 2251799813685248))
      (lz-error "MEMBER-SIZE must be bewteen 100000 and 2251799813685248."))
    (setf (member-size stream) member-size)
    (with-slots (encoder member-size buffer) stream
      (destructuring-bind (dictionary-size match-len-limit)
          (lzma-options level dictionary-size match-len-limit)
        (setf encoder (lz-compress-open dictionary-size
                                        match-len-limit
                                        member-size))
        (case (if (cffi:null-pointer-p encoder)
                  +lz-mem-error+
                  (lz-compress-errno encoder))
          ((#.+lz-ok+)
           t)
          ((#.+lz-mem-error+)
           (lz-compress-close encoder)
           (lz-error "Not enough memory. Try a smaller dictionary size."))
          (t
           (lz-compress-close encoder)
           (lz-error "Invalid argument to encoder."))))
      (setf buffer (cffi:make-shareable-byte-vector +buffer-size+)))
    stream))

(defmacro with-compressing-stream ((stream output-stream
                                    &key
                                      (level 6) (member-size 2251799813685248)
                                      dictionary-size match-len-limit)
                                   &body body)
  "Within BODY, STREAM is bound to a compressing stream for the given
compression LEVEL and OUTPUT-STREAM. The result of the last form of BODY is
returned."
  `(with-open-stream (,stream (make-compressing-stream
                               ,output-stream
                               :level ,level
                               :member-size ,member-size
                               :dictionary-size ,dictionary-size
                               :match-len-limit ,match-len-limit))
     ,@body))


;;;
;;; Decompression Gray streams
;;;

(defclass decompressing-stream (fundamental-binary-input-stream)
  ((input-stream :accessor input-stream)
   (decoder :accessor decoder)
   (ignore-trailing :accessor ignore-trailing)
   (loose-trailing :accessor loose-trailing)
   (first-member :accessor first-member)
   (buffer :accessor buffer)
   (output :accessor output)
   (output-index :accessor output-index)))

(defmethod stream-element-type ((stream decompressing-stream))
  '(unsigned-byte 8))

(defun process-decompression-error (stream)
  (with-slots (decoder first-member ignore-trailing loose-trailing) stream
    (let ((member-pos (lz-decompress-member-position decoder))
          (pos (lz-decompress-total-in-size decoder)))
      (case (lz-decompress-errno decoder)
        ((#.+lz-library-error+)
         (lz-error "Library error (LZ-DECOMPRESS-READ)."))
        ((#.+lz-header-error+)
         (cond
           (first-member
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
           (first-member
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
           (first-member
            (lz-error "Bad version or dictionary size in member header."))
           ((not loose-trailing)
            (lz-error "Corrupt header in multimember file."))
           ((not ignore-trailing)
            (lz-error "Trailing data not allowed."))
           (t
            t)))
        (t
         (lz-error "Decoder error at position ~d." pos))))))

(defun read-and-decompress (stream)
  (with-slots (input-stream decoder first-member buffer output output-index)
      stream
    (cffi:with-pointer-to-vector-data (ffi-buffer buffer)
      (let ((size (min (lz-decompress-write-size decoder) +buffer-size+)))
        (when (plusp size)
          (let ((n (read-sequence buffer input-stream :end size)))
            (when (and (plusp n)
                       (/= (lz-decompress-write decoder ffi-buffer n) n))
              (lz-error "Library error (LZ-DECOMPRESS-WRITE)."))
            (when (< n size)
              (lz-decompress-finish decoder)))))

      (let* ((size (- +buffer-size+ output-index))
             (n (lz-decompress-read decoder ffi-buffer size)))
        (unless (minusp n)
          (setf first-member
                (when first-member
                  (zerop (lz-decompress-member-finished decoder))))
          (replace output buffer :start1 output-index :end2 n)
          (incf output-index n))
        (when (or (minusp n) (and (zerop n) first-member))
          (process-decompression-error stream))))
    output-index))

(defmethod stream-listen ((stream decompressing-stream))
  (with-slots (input-stream output-index) stream
    (or (plusp output-index)
        (listen input-stream))))

(defmethod stream-read-byte ((stream decompressing-stream))
  (let ((available (read-and-decompress stream)))
    (if (plusp available)
        (with-slots (output output-index) stream
          (let ((byte (aref output 0)))
            (replace output output :start2 1 :end2 output-index)
            (decf output-index)
            byte))
        :eof)))

(defmethod stream-read-sequence ((stream decompressing-stream) seq start end
                                 &key &allow-other-keys)
  (do ((available (read-and-decompress stream) (read-and-decompress stream)))
      ((or (= start end) (zerop available)))
    (with-slots (output output-index) stream
      (let ((n (min (- end start) output-index)))
        (replace seq output :start1 start :end2 n)
        (incf start n)
        (replace output output :start2 n :end2 output-index)
        (decf output-index n))))
  start)

(defmethod close ((stream decompressing-stream) &key &allow-other-keys)
  (when (open-stream-p stream)
    (with-slots (decoder buffer output output-index) stream
      (lz-decompress-close decoder)
      (setf decoder nil)
      (setf buffer nil)
      (setf output nil)
      (setf output-index nil)))
  t)

(defun make-decompressing-stream (input-stream
                                  &key (ignore-trailing t) loose-trailing)
  "Return a stream that will supply the bytes resulting from the decompression
of the data read from the INPUT-STREAM."
  (let ((stream (make-instance 'decompressing-stream)))
    (setf (input-stream stream) input-stream)
    (setf (ignore-trailing stream) ignore-trailing)
    (setf (loose-trailing stream) loose-trailing)
    (with-slots (decoder first-member buffer output output-index)
        stream
      (setf decoder (lz-decompress-open))
      (case (if (cffi:null-pointer-p decoder)
                +lz-mem-error+
                (lz-decompress-errno decoder))
        ((#.+lz-ok+)
         t)
        (t
         (lz-decompress-close decoder)
         (lz-error "Not enough memory.")))
      (setf first-member t)
      (setf buffer (cffi:make-shareable-byte-vector +buffer-size+))
      (setf output (make-array +buffer-size+ :element-type 'u8))
      (setf output-index 0))
    stream))

(defmacro with-decompressing-stream ((stream input-stream
                                      &key (ignore-trailing t) loose-trailing)
                                     &body body)
  "Within BODY, STREAM is bound to a decompressing stream for the given
INPUT-STREAM. The result of the last form of BODY is returned."
  `(with-open-stream (,stream (make-decompressing-stream
                               ,input-stream
                               :ignore-trailing ,ignore-trailing
                               :loose-trailing ,loose-trailing))
     ,@body))


;;;
;;; Compression functions
;;;

(defun compress-stream-1 (input output
                          &key
                            (level 6) (member-size 2251799813685248)
                            dictionary-size match-len-limit)
  "Read the data from the INPUT octet stream, compress it, and write the result
to the OUTPUT octet stream."
  (with-compressing-stream (stream output
                                   :level level
                                   :member-size member-size
                                   :dictionary-size dictionary-size
                                   :match-len-limit match-len-limit)
    (let ((buffer (make-array +buffer-size+ :element-type 'u8)))
      (do ((n (read-sequence buffer input) (read-sequence buffer input)))
          ((zerop n) t)
        (write-sequence buffer stream :end n)))))

(defun compress-stream-n (input output
                          &key
                            (threads 2) (level 6) (member-size 2251799813685248)
                            block-size dictionary-size match-len-limit)
  "Read the data from the INPUT octet stream, compress it using multiple
threads, and write the result to the OUTPUT octet stream."
  (destructuring-bind (dictionary-size match-len-limit)
      (lzma-options level dictionary-size match-len-limit)
    (let* ((lparallel:*kernel* (lparallel:make-kernel threads))
           (queue (lparallel.queue:make-queue))
           (buffer-size 1048576)
           (buffer (make-array buffer-size :element-type 'u8))
           (block-size (or block-size (* 2 dictionary-size))))
      (labels ((read-block (size pipe first-read-p)
                 (let ((n (read-sequence buffer input
                                         :end (min size buffer-size))))
                   (cond
                     ((zerop n)
                      (not first-read-p))
                     (t
                      (write-sequence buffer pipe :end n)
                      (read-block (- size n) pipe nil)))))
               (compress-block (pipe out)
                 (compress-stream-1 pipe out
                                    :level level
                                    :member-size member-size
                                    :dictionary-size dictionary-size
                                    :match-len-limit match-len-limit)
                 (list (octet-streams:get-output-stream-octets out)
                       pipe
                       out))
               (add-task (pipe out)
                 (let ((pipe (or pipe (octet-streams:make-octet-pipe)))
                       (out (or out (octet-streams:make-octet-output-stream))))
                   (cond
                     ((read-block block-size pipe t)
                      (let ((task (lparallel:future (compress-block pipe out))))
                        (lparallel.queue:push-queue task queue)))
                     (t
                      (close pipe)
                      (close out)))))
               (process-queue ()
                 (unless (lparallel.queue:queue-empty-p queue)
                   (destructuring-bind (compressed-data pipe out)
                       (lparallel:force (lparallel.queue:pop-queue queue))
                     (write-sequence compressed-data output)
                     (add-task pipe out)
                     (process-queue)))))
        (unwind-protect
             (lparallel:task-handler-bind ((error (lambda (e)
                                                    (error e))))
               (dotimes (i threads)
                 (add-task nil nil))
               (process-queue)
               t)
          (lparallel:end-kernel))))))

(defun compress-stream (input output
                        &key
                          (threads 1) (level 6) (member-size 2251799813685248)
                          block-size dictionary-size match-len-limit)
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
                         :member-size member-size
                         :block-size block-size
                         :dictionary-size dictionary-size
                         :match-len-limit match-len-limit)))

(defun compress-file (input output
                      &key
                        (threads 1) (level 6) (member-size 2251799813685248)
                        block-size dictionary-size match-len-limit)
  "Read the data from the INPUT file, compress it, and write the result to the
OUTPUT file."
  (with-open-file (input-stream input :element-type 'u8)
    (with-open-file (output-stream output :direction :output :element-type 'u8)
      (compress-stream input-stream output-stream
                       :threads threads
                       :level level
                       :member-size member-size
                       :block-size block-size
                       :dictionary-size dictionary-size
                       :match-len-limit match-len-limit))))

(defun compress-buffer (buffer
                        &key
                          (start 0) end (threads 1) (level 6)
                          (member-size 2251799813685248)
                          block-size dictionary-size match-len-limit)
  "Read the data between the START and END offsets in the BUFFER, compress it,
and return the resulting octet vector."
  (let ((end (or end (length buffer))))
    (octet-streams:with-octet-output-stream (output)
      (octet-streams:with-octet-input-stream (input buffer start end)
        (compress-stream input output
                         :threads threads
                         :level level
                         :member-size member-size
                         :block-size block-size
                         :dictionary-size dictionary-size
                         :match-len-limit match-len-limit)))))


;;;
;;; Decompression functions
;;;

(defun decompress-stream-1 (input output
                            &key
                             (ignore-trailing t) loose-trailing)
  "Read the data from the INPUT octet stream, decompress it, and write the
result to the OUTPUT octet stream."
  (with-decompressing-stream (stream input
                                     :ignore-trailing ignore-trailing
                                     :loose-trailing loose-trailing)
    (let ((buffer (make-array +buffer-size+ :element-type 'u8)))
      (do ((n (read-sequence buffer stream) (read-sequence buffer stream)))
          ((zerop n) t)
        (write-sequence buffer output :end n)))))

(defun decompress-stream-n (input output
                            &key
                              (threads 2) (ignore-trailing t) loose-trailing)
  "Read the data from the INPUT octet stream, decompress it using multiple
threads, and write the result to the OUTPUT octet stream."
  (let* ((lparallel:*kernel* (lparallel:make-kernel threads))
         (queue (lparallel.queue:make-queue))
         (buffer-size 1048576)
         (buffer (make-array buffer-size :element-type 'u8))
         (input-pipe (octet-streams:make-octet-pipe))
         (magic (map '(simple-array u8 (4)) #'char-code "LZIP"))
         (jump-table (octet-streams:make-jump-table magic)))
    (labels ((find-magic (pipe)
               (octet-streams:octet-stream-search pipe magic jump-table))
             (read-member-size (pipe start)
               (logior
                (octet-streams:octet-stream-ref pipe start)
                (ash (octet-streams:octet-stream-ref pipe (+ start 1)) 8)
                (ash (octet-streams:octet-stream-ref pipe (+ start 2)) 16)
                (ash (octet-streams:octet-stream-ref pipe (+ start 3)) 24)
                (ash (octet-streams:octet-stream-ref pipe (+ start 4)) 32)
                (ash (octet-streams:octet-stream-ref pipe (+ start 5)) 40)
                (ash (octet-streams:octet-stream-ref pipe (+ start 6)) 48)
                (ash (octet-streams:octet-stream-ref pipe (+ start 7)) 56)))
             (decompress-member (pipe out)
               (decompress-stream-1 pipe out
                                    :ignore-trailing ignore-trailing
                                    :loose-trailing loose-trailing)
               (list (octet-streams:get-output-stream-octets out)
                     pipe
                     out))
             (move-bytes (pipe n)
               (when (plusp n)
                 (let ((length (read-sequence buffer input-pipe
                                              :end (min n buffer-size))))
                   (write-sequence buffer pipe :end length)
                   (move-bytes pipe (- n length)))))
             (read-member (member-size pipe)
               (let ((n (read-sequence buffer input)))
                 (write-sequence buffer input-pipe :end n))
               (let ((length (octet-streams:octet-stream-length input-pipe))
                     (index (find-magic input-pipe)))
                 (cond
                   ((and (zerop length) (zerop member-size))
                    nil)
                   ((null index)
                    ;; Continue reading the current member.
                    (let ((n (- length 8)))
                      (cond
                        ((plusp n)
                         (move-bytes pipe n)
                         (read-member (+ member-size n) pipe))
                        (t
                         ;; End of stream.
                         (move-bytes pipe length)
                         t))))
                   ((zerop index)
                    ;; New member.
                    (move-bytes pipe 4)
                    (read-member 4 pipe))
                   ((and (>= index 8)
                         (= (read-member-size input-pipe (- index 8))
                            (+ member-size index)))
                    ;; Finish reading the current member.
                    (move-bytes pipe index)
                    t)
                   (t
                    (lz-error "Bad member in archive.")))))
             (add-task (pipe out)
               (let ((pipe (or pipe (octet-streams:make-octet-pipe)))
                     (out (or out (octet-streams:make-octet-output-stream))))
                 (cond
                   ((read-member 0 pipe)
                    (let ((task (lparallel:future (decompress-member pipe out))))
                      (lparallel.queue:push-queue task queue)))
                   (t
                    (close pipe)
                    (close out)))))
             (process-queue ()
               (unless (lparallel.queue:queue-empty-p queue)
                 (destructuring-bind (data pipe out)
                     (lparallel:force (lparallel.queue:pop-queue queue))
                   (write-sequence data output)
                   (add-task pipe out)
                   (process-queue)))))
      (unwind-protect
           (lparallel:task-handler-bind ((error (lambda (e)
                                                  (error e))))
             (dotimes (i threads)
               (add-task nil nil))
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
