;;; This file is part of cl-lzlib
;;; Copyright 2019 Guillaume LE VAILLANT
;;; Distributed under the GNU GPL v3 or later.
;;; See the file LICENSE for terms of use and distribution.

(in-package :lzlib)


(defconstant +buffer-size+ 65536)


(declaim (inline copy-to-ffi-buffer copy-from-ffi-buffer))

(defun copy-to-ffi-buffer (buffer ffi-buffer size)
  (declare (type (simple-array (unsigned-byte 8) (*)) buffer)
           (type fixnum size)
           (optimize (speed 3) (space 0) (debug 0) (safety 1)))
  (dotimes (i size ffi-buffer)
    (setf (cffi:mem-aref ffi-buffer :unsigned-char i) (aref buffer i))))

(defun copy-from-ffi-buffer (ffi-buffer buffer size)
  (declare (type (simple-array (unsigned-byte 8) (*)) buffer)
           (type fixnum size)
           (optimize (speed 3) (space 0) (debug 0) (safety 1)))
  (dotimes (i size buffer)
    (setf (aref buffer i) (cffi:mem-aref ffi-buffer :unsigned-char i))))


(defun compress (encoder input output member-size)
  (declare (optimize (speed 3) (space 0) (debug 0) (safety 1)))
  (let ((buffer (make-array +buffer-size+ :element-type '(unsigned-byte 8))))
    (declare (type (simple-array (unsigned-byte 8) (*)) buffer)
             (dynamic-extent buffer))
    (cffi:with-foreign-object (ffi-buffer :unsigned-char +buffer-size+)
      (loop do
        (let ((in-size 0)
              (out-size 0))
          (loop while (plusp (lz-compress-write-size encoder)) do
            (let* ((size (min (lz-compress-write-size encoder) +buffer-size+))
                   (rd (read-sequence buffer input :end size)))
              (when (plusp rd)
                (copy-to-ffi-buffer buffer ffi-buffer rd)
                (when (/= (lz-compress-write encoder ffi-buffer rd) rd)
                  (error "Library error (LZ-COMPRESS-WRITE).")))
              (when (< rd size)
                (lz-compress-finish encoder))
              (incf in-size rd)))

          (setf out-size (lz-compress-read encoder ffi-buffer +buffer-size+))
          (cond
            ((minusp out-size)
             (let ((msg (lz-strerror (lz-compress-errno encoder))))
               (error "LZ-COMPRESS-READ error: ~a." msg)))
            ((plusp out-size)
             (copy-from-ffi-buffer ffi-buffer buffer out-size)
             (write-sequence buffer output :end out-size))
            ((zerop in-size)
             (error "Library error (LZ-COMPRESS-READ).")))

          (unless (zerop (lz-compress-member-finished encoder))
            (when (= (lz-compress-finished encoder) 1)
              (return))
            (when (minusp (lz-compress-restart-member encoder member-size))
              (let ((msg (lz-strerror (lz-compress-errno encoder))))
                (error "LZ-COMPRESS-RESTART-MEMBER error: ~a." msg))))))))
  t)

(defun lzma-options (level dictionary-size match-len-limit)
  (cond
    ((and dictionary-size (not match-len-limit))
     (error "MATCH-LEN-LIMIT is not set."))
    ((and match-len-limit (not dictionary-size))
     (error "DICTIONARY-SIZE is not set."))
    ((and dictionary-size match-len-limit)
     (assert (<= (lz-min-dictionary-size)
                 dictionary-size
                 (lz-max-dictionary-size)))
     (assert (<= (lz-min-match-len-limit)
                 match-len-limit
                 (lz-max-match-len-limit)))
     (list dictionary-size match-len-limit))
    (level
     (ecase level
       ((0) '(65535 16))
       ((1) '(1048576 5))
       ((2) '(1572864 6))
       ((3) '(2097152 8))
       ((4) '(3145728 12))
       ((5) '(4194304 20))
       ((6) '(8388608 36))
       ((7) '(16777216 68))
       ((8) '(25165824 132))
       ((9) '(33554432 273))))
    (t
     (error "Either LEVEL or DICTIONARY-SIZE and MATCH-LEN-LIMIT must be set."))))

(defun compress-stream (input output &key (level 6) (member-size 2251799813685248) dictionary-size match-len-limit)
  "Read the data from the INPUT octet stream, compress it, and write the result
to the OUTPUT octet stream."
  (assert (<= 0 member-size #x7fffffffffffffff))
  (destructuring-bind (dictionary-size match-len-limit)
      (lzma-options level dictionary-size match-len-limit)
    (let ((encoder (lz-compress-open dictionary-size match-len-limit member-size)))
      (unwind-protect
           (if (or (cffi:null-pointer-p encoder)
                   (/= (lz-compress-errno encoder) +lz-ok+))
               (if (or (cffi:null-pointer-p encoder)
                       (= (lz-compress-errno encoder) +lz-mem-error+))
                   (error "Not enough memory. Try a smaller dictionary size.")
                   (error "Invalid argument to encoder."))
               (compress encoder input output member-size))
        (lz-compress-close encoder)))))

(defun compress-file (input output &key (level 6) (member-size 2251799813685248) dictionary-size match-len-limit)
  "Read the data from the INPUT file, compress it, and write the result to the
OUTPUT file."
  (with-open-file (input-stream input :element-type '(unsigned-byte 8))
    (with-open-file (output-stream output
                                   :direction :output
                                   :element-type '(unsigned-byte 8))
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


(defun decompress (decoder input output ignore-trailing loose-trailing)
  (declare (optimize (speed 3) (space 0) (debug 0) (safety 1)))
  (let ((first-member t)
        (buffer (make-array +buffer-size+ :element-type '(unsigned-byte 8))))
    (declare (type (simple-array (unsigned-byte 8) (*)) buffer)
             (dynamic-extent buffer))
    (cffi:with-foreign-object (ffi-buffer :unsigned-char +buffer-size+)
      (loop do
        (let ((max-in-size (min (lz-decompress-write-size decoder) +buffer-size+))
              (in-size 0)
              (out-size 0))
          (when (plusp max-in-size)
            (setf in-size (read-sequence buffer input :end max-in-size))
            (when (plusp in-size)
              (copy-to-ffi-buffer buffer ffi-buffer in-size)
              (when (/= (lz-decompress-write decoder ffi-buffer in-size) in-size)
                (error "Library error (LZ-DECOMPRESS-WRITE).")))
            (when (< in-size max-in-size)
              (lz-decompress-finish decoder)))

          (loop do
            (let ((rd (lz-decompress-read decoder ffi-buffer +buffer-size+)))
              (cond
                ((plusp rd)
                 (copy-from-ffi-buffer ffi-buffer buffer rd)
                 (write-sequence buffer output :end rd)
                 (incf out-size rd))
                ((minusp rd)
                 (setf out-size rd)
                 (return)))
              (when (= (lz-decompress-member-finished decoder) 1)
                (setf first-member nil))
              (unless (plusp rd)
                (return))))

          (when (or (minusp out-size) (and first-member (zerop out-size)))
            (let ((member-pos (lz-decompress-member-position decoder))
                  (lz-errno (lz-decompress-errno decoder)))
              (when (= lz-errno +lz-library-error+)
                (error "Library error (LZ-DECOMPRESS-READ)."))
              (when (<= member-pos 6)
                (cond
                  ((= lz-errno +lz-unexpected-eof+)
                   (if first-member
                       (error "File ends unexpectedly at member header.")
                       (error "Truncated header in multimember file.")))
                  ((= lz-errno +lz-data-error+)
                   (cond
                     ((= member-pos 4)
                      (let ((version (lz-decompress-member-version decoder)))
                        (error "Version ~d member format not supported." version)))
                     ((= member-pos 5)
                      (error "Invalid dictionary size in member header."))
                     (first-member
                      (error "Bad version or dictionary size in member header."))
                     ((not loose-trailing)
                      (error "Corrupt header in multimember file."))
                     ((not ignore-trailing)
                      (error "Trailing data not allowed."))
                     (t
                      (return))))))
              (when (= lz-errno +lz-header-error+)
                (cond
                  (first-member
                   (error "Bad magic number (file not in lzip format)."))
                  ((not ignore-trailing)
                   (error "Trailing data not allowed."))
                  (t
                   (return))))
              (when (= lz-errno +lz-mem-error+)
                (error "Not enough memory."))))

          (when (= (lz-decompress-finished decoder) 1)
            (return))
          (when (and (zerop in-size) (zerop out-size))
            (error "Library error (stalled)."))))))
  t)

(defun decompress-stream (input output &key (ignore-trailing t) loose-trailing)
  "Read the data from the INPUT octet stream, decompress it, and write the
result to the OUTPUT octet stream."
  (let ((decoder (lz-decompress-open)))
    (unwind-protect
         (if (or (cffi:null-pointer-p decoder)
                 (/= (lz-decompress-errno decoder) +lz-ok+))
             (error "Not enough memory.")
             (decompress decoder input output ignore-trailing loose-trailing))
      (lz-decompress-close decoder))))

(defun decompress-file (input output &key (ignore-trailing t) loose-trailing)
  "Read the data from the INPUT file, decompress it, and write the result to the
OUTPUT file."
  (with-open-file (input-stream input :element-type '(unsigned-byte 8))
    (with-open-file (output-stream output
                                   :direction :output
                                   :element-type '(unsigned-byte 8))
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
