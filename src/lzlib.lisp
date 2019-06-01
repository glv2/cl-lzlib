;;; This file is part of cl-lzlib
;;; Copyright 2019 Guillaume LE VAILLANT
;;; Distributed under the GNU GPL v3 or later.
;;; See the file LICENSE for terms of use and distribution.

(in-package :lzlib)


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

(defun compress-stream (in out &key (level 6) dictionary-size match-len-limit)
  )

(defun compress-file (in out &key (level 6) dictionary-size match-len-limit)
  (with-open-file (input-stream in :element-type '(unsigned-byte 8))
    (with-open-file (output-stream out :element-type '(unsignde-byte 8))
      (compress-stream input-stream output-stream
                       :level level
                       :dictionary-size dictionary-size
                       :match-len-limit match-len-limit))))

(defun compress-buffer (buffer &key (level 6) dictionary-size match-len-limit)
  )

(defun decompress-stream (in out &key (ignore-trailing t) loose-trailing)
  )

(defun decompress-file (in out &key (ignore-trailing t) loose-trailing)
  (with-open-file (input-stream in :element-type '(unsigned-byte 8))
    (with-open-file (output-stream out :element-type '(unsignde-byte 8))
      (decompress-stream input-stream output-stream
                         :ignore-trailing ignore-trailing
                         :loose-trailing loose-trailing))))

(defun decompress-buffer (in out &key (ignore-trailing t) loose-trailing)
  )
