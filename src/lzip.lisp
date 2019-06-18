;;; This file is part of cl-lzlib
;;; Copyright 2019 Guillaume LE VAILLANT
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
    (cffi:with-pointer-to-vector-data (ffi-buffer buffer)
      (loop do
        (let ((in-size 0)
              (out-size 0))
          (declare (type i32 in-size out-size))
          (loop for max-in-size = (lz-compress-write-size encoder)
                while (plusp max-in-size)
                do (let* ((size (min max-in-size #.+buffer-size+))
                          (rd (read-sequence buffer input :end size)))
                     (declare (type (integer 0 #.+buffer-size+) size rd))
                     (when (plusp rd)
                       (unless (= rd (lz-compress-write encoder ffi-buffer rd))
                         (lz-error "Library error (LZ-COMPRESS-WRITE).")))
                     (when (< rd size)
                       (lz-compress-finish encoder))
                     (incf in-size rd)))

          (setf out-size (lz-compress-read encoder ffi-buffer #.+buffer-size+))
          (cond
            ((minusp out-size)
             (let ((msg (lz-strerror (lz-compress-errno encoder))))
               (lz-error "LZ-COMPRESS-READ error: ~a." msg)))
            ((plusp out-size)
             (write-sequence buffer output :end out-size))
            ((zerop in-size)
             (lz-error "Library error (LZ-COMPRESS-READ).")))

          (unless (zerop (lz-compress-member-finished encoder))
            (when (= 1 (lz-compress-finished encoder))
              (return))
            (when (minusp (lz-compress-restart-member encoder member-size))
              (let ((msg (lz-strerror (lz-compress-errno encoder))))
                (lz-error "LZ-COMPRESS-RESTART-MEMBER error: ~a." msg))))))))
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
       (unless (and (integerp dictionary-size)
                    (<= min-dictionary-size
                        dictionary-size
                        max-dictionary-size))
         (lz-error "DICTIONARY-SIZE must be between ~d and ~d."
                   min-dictionary-size
                   max-dictionary-size))
       (unless (and (integerp match-len-limit)
                    (<= min-match-len-limit
                        match-len-limit
                        max-match-len-limit))
         (lz-error "MATCH-LEN-LIMIT must be between ~d and ~d."
                   min-match-len-limit
                   max-match-len-limit))
       (list dictionary-size match-len-limit)))
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

(defun compress-stream (input output &key (level 6) (member-size 2251799813685248) dictionary-size match-len-limit)
  "Read the data from the INPUT octet stream, compress it, and write the result
to the OUTPUT octet stream."
  (unless (and (integerp member-size)
               (<= 100000 member-size 2251799813685248))
    (lz-error "MEMBER-SIZE must be bewteen 100000 and 2251799813685248."))
  (destructuring-bind (dictionary-size match-len-limit)
      (lzma-options level dictionary-size match-len-limit)
    (let* ((encoder (lz-compress-open dictionary-size match-len-limit member-size))
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
        (lz-compress-close encoder)))))

(defun compress-file (input output &key (level 6) (member-size 2251799813685248) dictionary-size match-len-limit)
  "Read the data from the INPUT file, compress it, and write the result to the
OUTPUT file."
  (with-open-file (input-stream input :element-type 'u8)
    (with-open-file (output-stream output :direction :output :element-type 'u8)
      (compress-stream input-stream output-stream
                       :level level
                       :member-size member-size
                       :dictionary-size dictionary-size
                       :match-len-limit match-len-limit))))

(defun compress-buffer (buffer &key (start 0) end (level 6) (member-size 2251799813685248) dictionary-size match-len-limit)
  "Read the data between the START and END offsets in the BUFFER, compress it,
and return the resulting octet vector."
  (let ((end (or end (length buffer))))
    (octet-streams:with-octet-output-stream (output)
      (octet-streams:with-octet-input-stream (input buffer start end)
        (compress-stream input output
                         :level level
                         :member-size member-size
                         :dictionary-size dictionary-size
                         :match-len-limit match-len-limit)))))


;;;
;;; Decompression functions
;;;

(defun decompress (decoder input output ignore-trailing loose-trailing)
  "Read the data from the INPUT octet stream, decompress itwith the DECODER, and
write the result to the OUTPUT octet stream."
  (declare (optimize (speed 3) (space 0) (debug 0) (safety 1)))
  (let ((first-member t)
        (buffer (cffi:make-shareable-byte-vector #.+buffer-size+)))
    (cffi:with-pointer-to-vector-data (ffi-buffer buffer)
      (loop do
        (let ((max-in-size (min (lz-decompress-write-size decoder)
                                #.+buffer-size+))
              (in-size 0)
              (out-size 0))
          (declare (type i32 max-in-size in-size out-size))
          (when (plusp max-in-size)
            (setf in-size (read-sequence buffer input :end max-in-size))
            (when (plusp in-size)
              (unless (= in-size
                         (lz-decompress-write decoder ffi-buffer in-size))
                (lz-error "Library error (LZ-DECOMPRESS-WRITE).")))
            (when (< in-size max-in-size)
              (lz-decompress-finish decoder)))

          (loop do
            (let ((rd (lz-decompress-read decoder ffi-buffer #.+buffer-size+)))
              (declare (type i32 rd))
              (cond
                ((plusp rd)
                 (write-sequence buffer output :end rd)
                 (incf out-size rd))
                ((minusp rd)
                 (setf out-size rd)
                 (return)))
              (when (= 1 (lz-decompress-member-finished decoder))
                (setf first-member nil))
              (unless (plusp rd)
                (return))))

          (when (or (minusp out-size) (and first-member (zerop out-size)))
            (let ((member-pos (lz-decompress-member-position decoder))
                  (lz-errno (lz-decompress-errno decoder)))
              (declare (type u64 member-pos)
                       (type i32 lz-errno))
              (when (= lz-errno +lz-library-error+)
                (lz-error "Library error (LZ-DECOMPRESS-READ)."))
              (when (<= member-pos 6)
                (cond
                  ((= lz-errno +lz-unexpected-eof+)
                   (if first-member
                       (lz-error "File ends unexpectedly at member header.")
                       (lz-error "Truncated header in multimember file.")))
                  ((= lz-errno +lz-data-error+)
                   (cond
                     ((= member-pos 4)
                      (let ((version (lz-decompress-member-version decoder)))
                        (lz-error "Version ~d member format not supported." version)))
                     ((= member-pos 5)
                      (lz-error "Invalid dictionary size in member header."))
                     (first-member
                      (lz-error "Bad version or dictionary size in member header."))
                     ((not loose-trailing)
                      (lz-error "Corrupt header in multimember file."))
                     ((not ignore-trailing)
                      (lz-error "Trailing data not allowed."))
                     (t
                      (return))))))
              (when (= lz-errno +lz-header-error+)
                (cond
                  (first-member
                   (lz-error "Bad magic number (file not in lzip format)."))
                  ((not ignore-trailing)
                   (lz-error "Trailing data not allowed."))
                  (t
                   (return))))
              (when (= lz-errno +lz-mem-error+)
                (lz-error "Not enough memory."))
              (let ((pos (lz-decompress-total-in-size decoder)))
                (declare (type u64 pos))
                (if (= lz-errno +lz-unexpected-eof+)
                    (lz-error "File ends unexpectedly at pos ~d." pos)
                    (lz-error "Decoder error ar pos ~d." pos)))))

          (when (= 1 (lz-decompress-finished decoder))
            (return))
          (when (and (zerop in-size) (zerop out-size))
            (lz-error "Library error (stalled)."))))))
  t)

(defun decompress-stream (input output &key (ignore-trailing t) loose-trailing)
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

(defun decompress-file (input output &key (ignore-trailing t) loose-trailing)
  "Read the data from the INPUT file, decompress it, and write the result to the
OUTPUT file."
  (with-open-file (input-stream input :element-type 'u8)
    (with-open-file (output-stream output :direction :output :element-type 'u8)
      (decompress-stream input-stream output-stream
                         :ignore-trailing ignore-trailing
                         :loose-trailing loose-trailing))))

(defun decompress-buffer (buffer &key (start 0) end (ignore-trailing t) loose-trailing)
  "Read the data between the START and END offsets in the BUFFER, decompress it,
and return the resulting octet vector."
  (let ((end (or end (length buffer))))
    (octet-streams:with-octet-output-stream (output)
      (octet-streams:with-octet-input-stream (input buffer start end)
        (decompress-stream input output
                           :ignore-trailing ignore-trailing
                           :loose-trailing loose-trailing)))))
