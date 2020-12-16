;;; This file is part of cl-lzlib
;;; Copyright 2019-2020 Guillaume LE VAILLANT
;;; Distributed under the GNU GPL v3 or later.
;;; See the file LICENSE for terms of use and distribution.

(defpackage :lzlib-tests
  (:use :cl :cl-octet-streams :fiveam :lzlib))

(in-package :lzlib-tests)


(defun data-file-path (filename)
  (let ((path (concatenate 'string "tests/" filename)))
    (asdf:system-relative-pathname "lzlib-tests" path)))

(defun load-data-file (filename)
  (with-open-file (file (data-file-path filename)
                        :element-type '(unsigned-byte 8))
    (let* ((size (file-length file))
           (buffer (make-array size :element-type '(unsigned-byte 8))))
      (read-sequence buffer file)
      buffer)))

(defun same-files-p (path-1 path-2)
  (with-open-file (file-1 path-1 :element-type '(unsigned-byte 8))
    (with-open-file (file-2 path-2 :element-type '(unsigned-byte 8))
      (let ((buffer-1 (make-array 16384 :element-type '(unsigned-byte 8)))
            (buffer-2 (make-array 16384 :element-type '(unsigned-byte 8))))
        (loop for read-1 = (read-sequence buffer-1 file-1)
              for read-2 = (read-sequence buffer-2 file-2)
              never (or (/= read-1 read-2)
                        (mismatch buffer-1 buffer-2 :end1 read-1 :end2 read-1))
              until (zerop read-1))))))


(def-suite lzlib-unit-tests
  :description "Unit tests for lzip (de)compression.")

(in-suite lzlib-unit-tests)


(test decompress-stream
  (is (equalp #()
              (with-octet-output-stream (output)
                (with-octet-input-stream (input #(76 90 73 80 1 12 0 131
                                                  255 251 255 255 192 0 0 0
                                                  0 0 0 0 0 0 0 0
                                                  0 0 0 0 36 0 0 0
                                                  0 0 0 0))
                  (decompress-stream input output)))))
  (is (equalp #(1 2 3 4 5)
              (with-octet-output-stream (output)
                (with-octet-input-stream (input #(76 90 73 80 1 12 0 0
                                                  128 157 97 211 29 7 5 127
                                                  255 248 129 32 0 244 153 11
                                                  71 5 0 0 0 0 0 0
                                                  0 41 0 0 0 0 0 0
                                                  0))
                  (decompress-stream input output)))))
  (let ((tmp (with-octet-output-stream (output)
               (with-octet-input-stream (input #(76 90 73 80 1 113 0 57
                                                 239 251 191 254 163 177 94 229
                                                 248 63 178 170 38 85 248 104
                                                 112 65 112 21 15 141 253 30
                                                 76 27 138 66 183 25 244 105
                                                 24 113 174 102 35 138 138 77
                                                 47 163 13 217 127 166 227 140
                                                 35 17 83 224 89 24 197 117
                                                 138 226 119 248 182 148 127 12
                                                 106 192 222 116 73 100 226 233
                                                 92 83 178 4 214 177 246 68
                                                 181 91 255 255 185 170 0 0
                                                 122 78 48 21 160 134 1 0
                                                 0 0 0 0 116 0 0 0
                                                 0 0 0 0))
                 (decompress-stream input output)))))
    (is (= 100000 (length tmp)))
    (is-true (every (lambda (x) (= x 115)) tmp))))

(test decompress-file
  (let ((decompressed (data-file-path "test.txt"))
        (compressed (data-file-path "test.txt.lz"))
        (tmp "/tmp/lzlib-test.txt"))
    (unwind-protect
         (progn
           (is-true (decompress-file compressed tmp))
           (is (same-files-p decompressed tmp)))
      (uiop:delete-file-if-exists tmp)))
  (let ((decompressed (data-file-path "test-multimember.dat"))
        (compressed (data-file-path "test-multimember.dat.lz"))
        (tmp "/tmp/lzlib-multimember.dat"))
    (unwind-protect
         (progn
           (is-true (decompress-file compressed tmp))
           (is (same-files-p decompressed tmp)))
      (uiop:delete-file-if-exists tmp)))
  (let ((decompressed (data-file-path "test-multimember.dat"))
        (compressed (data-file-path "test-multimember.dat.lz"))
        (tmp "/tmp/lzlib-multimember.dat"))
    (unwind-protect
         (progn
           (is-true (decompress-file compressed tmp :threads 2))
           (is (same-files-p decompressed tmp)))
      (uiop:delete-file-if-exists tmp))))

(test decompress-buffer
  (let ((decompressed (load-data-file "test.txt"))
        (compressed (load-data-file "test.txt.lz")))
    (is (equalp decompressed (decompress-buffer compressed))))
  (is (equalp #(1 2 3 4 5) (decompress-buffer #(76 90 73 80 1 12 0 0
                                                128 157 97 211 29 7 5 127
                                                255 248 129 32 0 244 153 11
                                                71 5 0 0 0 0 0 0
                                                0 41 0 0 0 0 0 0
                                                0)))))

(test decompress-corrupt-header
  ;; No header
  (signals lzlib-error (decompress-buffer #()))
  ;; Bad header
  (signals lzlib-error (decompress-buffer #(1 2 3 4 5 6 7 8 9)))
  ;; Incomplete header
  (signals lzlib-error (decompress-buffer #(76 90 73 80)))
  ;; Bad version number
  (signals lzlib-error (decompress-buffer #(76 90 73 80 23 12 0 0
                                            128 157 97 211 29 7 5 127
                                            255 248 129 32 0 244 153 11
                                            71 5 0 0 0 0 0 0
                                            0 41 0 0 0 0 0 0
                                            0)))
  ;; Bad dictionary size
  (signals lzlib-error (decompress-buffer #(76 90 73 80 1 255 0 0
                                            128 157 97 211 29 7 5 127
                                            255 248 129 32 0 244 153 11
                                            71 5 0 0 0 0 0 0
                                            0 41 0 0 0 0 0 0
                                            0))))

(test decompress-corrupt-data
  ;; Bad byte in LZMA stream
  (signals lzlib-error (decompress-buffer #(76 90 73 80 1 12 0 0
                                            128 157 14 211 29 7 5 127
                                            255 248 129 32 0 244 153 11
                                            71 5 0 0 0 0 0 0
                                            0 41 0 0 0 0 0 0
                                            0)))
  ;; Bad byte in CRC
  (signals lzlib-error (decompress-buffer #(76 90 73 80 1 12 0 0
                                            128 157 97 211 29 7 5 127
                                            255 248 129 32 0 244 13 11
                                            71 5 0 0 0 0 0 0
                                            0 41 0 0 0 0 0 0
                                            0)))
  ;; Bad byte in data size
  (signals lzlib-error (decompress-buffer #(76 90 73 80 1 12 0 0
                                            128 157 97 211 29 7 5 127
                                            255 248 129 32 0 244 153 11
                                            71 5 2 0 0 0 0 0
                                            0 41 0 0 0 0 0 0
                                            0)))
  ;; Bad byte in member size
  (signals lzlib-error (decompress-buffer #(76 90 73 80 1 12 0 0
                                            128 157 97 211 29 7 5 127
                                            255 248 129 32 0 244 153 11
                                            71 5 0 0 0 0 0 0
                                            0 32 0 0 0 0 0 0
                                            0)))
  ;; Incomplete stream
  (signals lzlib-error (decompress-buffer #(76 90 73 80 1 12 0 0
                                            128 157 97 211 29 7 5 127
                                            255 248))))

(test decompress-trailing-data
  ;; Trailing data
  (is (equalp #(1 2 3 4 5) (decompress-buffer #(76 90 73 80 1 12 0 0
                                                128 157 97 211 29 7 5 127
                                                255 248 129 32 0 244 153 11
                                                71 5 0 0 0 0 0 0
                                                0 41 0 0 0 0 0 0
                                                0 9 8 7 6 5 4 3
                                                2 1 0))))
  (signals lzlib-error (decompress-buffer #(76 90 73 80 1 12 0 0
                                            128 157 97 211 29 7 5 127
                                            255 248 129 32 0 244 153 11
                                            71 5 0 0 0 0 0 0
                                            0 41 0 0 0 0 0 0
                                            0 9 8 7 6 5 4 3
                                            2 1 0)
                                          :ignore-trailing nil))
  ;; Trailing bad header
  (signals lzlib-error (decompress-buffer #(76 90 73 80 1 12 0 0
                                            128 157 97 211 29 7 5 127
                                            255 248 129 32 0 244 153 11
                                            71 5 0 0 0 0 0 0
                                            0 41 0 0 0 0 0 0
                                            0 76 90 0 80 1 12 0
                                            0 0 0 0 0 0 0)))
  (is (equalp #(1 2 3 4 5) (decompress-buffer #(76 90 73 80 1 12 0 0
                                                128 157 97 211 29 7 5 127
                                                255 248 129 32 0 244 153 11
                                                71 5 0 0 0 0 0 0
                                                0 41 0 0 0 0 0 0
                                                0 76 90 0 80 1 12 0
                                                0 0 0 0 0 0 0)
                                              :loose-trailing t))))

(test compress-stream
  (is (equalp #()
              (with-octet-output-stream (output)
                (with-octet-pipe (pipe)
                  (with-octet-input-stream (input #())
                    (compress-stream input pipe)
                    (decompress-stream pipe output))))))
  (is (equalp #(1 2 3 4 5)
              (with-octet-output-stream (output)
                (with-octet-pipe (pipe)
                  (with-octet-input-stream (input #(1 2 3 4 5))
                    (compress-stream input pipe)
                    (decompress-stream pipe output))))))
  (let* ((data (make-array 100000
                           :element-type '(unsigned-byte 8)
                           :initial-element 115))
         (tmp (with-octet-output-stream (output)
                (with-octet-pipe (pipe)
                  (with-octet-input-stream (input data)
                    (compress-stream input pipe)
                    (decompress-stream pipe output))))))
    (is (equalp data tmp))))

(test compress-file
  (let ((decompressed (data-file-path "test.txt"))
        (tmp-1 "/tmp/lzlib-test.txt.lz")
        (tmp-2 "/tmp/lzlib-test.txt"))
    (unwind-protect
         (progn
           (is-true (compress-file decompressed tmp-1))
           (is-true (decompress-file tmp-1 tmp-2))
           (is (same-files-p decompressed tmp-2)))
      (uiop:delete-file-if-exists tmp-1)
      (uiop:delete-file-if-exists tmp-2)))
  (let ((decompressed (data-file-path "test-multimember.dat"))
        (tmp-1 "/tmp/test-multimember.dat.lz")
        (tmp-2 "/tmp/test-multimember.dat"))
    (unwind-protect
         (progn
           (is-true (compress-file decompressed tmp-1 :member-size 100000))
           (is-true (decompress-file tmp-1 tmp-2))
           (is (same-files-p decompressed tmp-2)))
      (uiop:delete-file-if-exists tmp-1)
      (uiop:delete-file-if-exists tmp-2)))
  (let ((decompressed (data-file-path "test-multimember.dat"))
        (tmp-1 "/tmp/test-multimember.dat.lz")
        (tmp-2 "/tmp/test-multimember.dat"))
    (unwind-protect
         (progn
           (is-true (compress-file decompressed tmp-1
                                   :threads 2
                                   :block-size 100000))
           (is-true (decompress-file tmp-1 tmp-2))
           (is (same-files-p decompressed tmp-2)))
      (uiop:delete-file-if-exists tmp-1)
      (uiop:delete-file-if-exists tmp-2))))

(test compress-buffer
  (let* ((decompressed (load-data-file "test.txt"))
         (tmp-1 (compress-buffer decompressed))
         (tmp-2 (decompress-buffer tmp-1)))
    (is (< (length tmp-1) (length decompressed)))
    (is-false (mismatch decompressed tmp-2)))
  (let* ((decompressed (make-array 123456
                                   :element-type '(unsigned-byte 8)
                                   :initial-element 55))
         (tmp-1 (compress-buffer decompressed))
         (tmp-2 (decompress-buffer tmp-1)))
    (is (< (length tmp-1) (length decompressed)))
    (is-false (mismatch decompressed tmp-2))))

(test compress-bad-options
  ;; Missing compression options
  (signals lzlib-error (compress-buffer #(7 6 6 7 6 6)
                                        :level nil))
  ;; Invalid compression level
  (signals lzlib-error (compress-buffer #(7 6 6 7 6 6)
                                        :level 15))
  ;; Dictionary size too small
  (signals lzlib-error (compress-buffer #(7 6 6 7 6 6)
                                        :dictionary-size 3
                                        :match-len-limit 10))
  ;; Dictionary size too big
  (signals lzlib-error (compress-buffer #(7 6 6 7 6 6)
                                        :dictionary-size 1073741824
                                        :match-len-limit 10))
  ;; Match length limit too small
  (signals lzlib-error (compress-buffer #(7 6 6 7 6 6)
                                        :dictionary-size 2097152
                                        :match-len-limit 1))
  ;; Match length limit too big
  (signals lzlib-error (compress-buffer #(7 6 6 7 6 6)
                                        :dictionary-size 2097152
                                        :match-len-limit 1000))
  ;; Missing match length limit
  (signals lzlib-error (compress-buffer #(7 6 6 7 6 6)
                                        :dictionary-size 2097152))
  ;; Missing dictionary size
  (signals lzlib-error (compress-buffer #(7 6 6 7 6 6)
                                        :match-len-limit 10))
  ;; Member size to small
  (signals lzlib-error (compress-buffer #(7 6 6 7 6 6)
                                        :member-size 10))
  ;; Member size to big
  (signals lzlib-error (compress-buffer #(7 6 6 7 6 6)
                                        :member-size 18446744073709551616)))
