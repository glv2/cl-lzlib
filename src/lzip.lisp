;;; This file is part of cl-lzlib
;;; Copyright 2019 Guillaume LE VAILLANT
;;; Distributed under the GNU GPL v3 or later.
;;; See the file LICENSE for terms of use and distribution.

(in-package :lzlib)


(defconstant +max-member-size+ 2251799813685248)
(defconstant +buffer-size+ 65536)

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

(defun copy-to-ffi-buffer (buffer ffi-buffer size)
  (dotimes (i size ffi-buffer)
    (setf (cffi:mem-aref ffi-buffer :unsigned-char i) (aref buffer i))))

(defun copy-from-ffi-buffer (ffi-buffer buffer size)
  (dotimes (i size buffer)
    (setf (aref buffer i) (cffi:mem-aref ffi-buffer :unsigned-char i))))

(defun compress (encoder input output member-size)
  (let ((buffer (make-array +buffer-size+ :element-type '(unsigned-byte 8))))
    (cffi:with-foreign-object (ffi-buffer :unsigned-char +buffer-size+)
      (loop do
        (let ((in-size 0)
              (out-size 0))
          (loop while (plusp (lz-compress-write-size encoder)) do
            (let ((size (min (lz-compress-write-size encoder) +buffer-size+))
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

(defun compress-stream (input output &key (level 6) (member-size +max-member-size+) dictionary-size match-len-limit)
  (assert (<= 0 member-size +max-member-size+))
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

(defun compress-file (input output &key (level 6) (member-size +max-member-size+) dictionary-size match-len-limit)
  (with-open-file (input-stream input :element-type '(unsigned-byte 8))
    (with-open-file (output-stream output :element-type '(unsignde-byte 8))
      (compress-stream input-stream output-stream
                       :level level
                       :dictionary-size dictionary-size
                       :match-len-limit match-len-limit))))

(defun compress-buffer (buffer &key (level 6) (member-size +max-member-size+) dictionary-size match-len-limit)
  ;; TODO
  )

(defun decompress-stream (input output &key (ignore-trailing t) loose-trailing)
  )

(defun decompress-file (input output &key (ignore-trailing t) loose-trailing)
  (with-open-file (input-stream input :element-type '(unsigned-byte 8))
    (with-open-file (output-stream output :element-type '(unsignde-byte 8))
      (decompress-stream input-stream output-stream
                         :ignore-trailing ignore-trailing
                         :loose-trailing loose-trailing))))

(defun decompress-buffer (buffer &key (ignore-trailing t) loose-trailing)
  ;; TODO
  )
