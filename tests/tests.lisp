;;; This file is part of cl-lzlib
;;; Copyright 2019 Guillaume LE VAILLANT
;;; Distributed under the GNU GPL v3 or later.
;;; See the file LICENSE for terms of use and distribution.

(defpackage :lzlib-tests
  (:use :cl :fiveam :lzlib))

(in-package :lzlib-tests)


(defun data-file-path (filename)
  (let ((path (concatenate 'string "tests/" filename)))
    (asdf:system-relative-pathname "lzlib-tests"path)))

(defun load-data-file (filename)
  (with-open-file (file (data-file-path filename) :element-type '(unsigned-byte 8))
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


(test decompress-file
  (let ((decompressed (data-file-path "test.txt"))
        (compressed (data-file-path "test.txt.lz"))
        (tmp "/tmp/lzlib-test.txt"))
    (is-true (decompress-file compressed tmp))
    (is (same-files-p decompressed tmp))
    (uiop:delete-file-if-exists tmp)))

(test decompress-buffer
  (let ((decompressed (load-data-file "test.txt"))
        (compressed (load-data-file "test.txt.lz")))
    (is (equalp decompressed (decompress-buffer compressed))))
  (is (equalp #(1 2 3 4 5) (decompress-buffer #(76 90 73 80 1 12 0 0 128 157 97 211 29 7 5 127 255 248 129 32 0 244 153 11 71 5 0 0 0 0 0 0 0 41 0 0 0 0 0 0 0))))
  ;; No header
  (signals lzlib-error (decompress-buffer #()))
  ;; Bad header
  (signals lzlib-error (decompress-buffer #(1 2 3 4 5 6 7 8 9)))
  ;; Incomplete header
  (signals lzlib-error (decompress-buffer #(76 90 73 80)))
  ;; Bad version number
  (signals lzlib-error (decompress-buffer #(76 90 73 80 23 12 0 0 128 157 97 211 29 7 5 127 255 248 129 32 0 244 153 11 71 5 0 0 0 0 0 0 0 41 0 0 0 0 0 0 0)))
  ;; Bad dictionary size
  (signals lzlib-error (decompress-buffer #(76 90 73 80 1 255 0 0 128 157 97 211 29 7 5 127 255 248 129 32 0 244 153 11 71 5 0 0 0 0 0 0 0 41 0 0 0 0 0 0 0)))
  ;; Bad byte in LZMA stream
  (signals lzlib-error (decompress-buffer #(76 90 73 80 1 12 0 0 128 157 14 211 29 7 5 127 255 248 129 32 0 244 153 11 71 5 0 0 0 0 0 0 0 41 0 0 0 0 0 0 0)))
  ;; Bad byte in CRC
  (signals lzlib-error (decompress-buffer #(76 90 73 80 1 12 0 0 128 157 97 211 29 7 5 127 255 248 129 32 0 244 13 11 71 5 0 0 0 0 0 0 0 41 0 0 0 0 0 0 0)))
  ;; Bad byte in data size
  (signals lzlib-error (decompress-buffer #(76 90 73 80 1 12 0 0 128 157 97 211 29 7 5 127 255 248 129 32 0 244 153 11 71 5 2 0 0 0 0 0 0 41 0 0 0 0 0 0 0)))
  ;; Bad byte in member size
  (signals lzlib-error (decompress-buffer #(76 90 73 80 1 12 0 0 128 157 97 211 29 7 5 127 255 248 129 32 0 244 153 11 71 5 0 0 0 0 0 0 0 32 0 0 0 0 0 0 0)))
  ;; Trailing data
  (is (equalp #(1 2 3 4 5) (decompress-buffer #(76 90 73 80 1 12 0 0 128 157 97 211 29 7 5 127 255 248 129 32 0 244 153 11 71 5 0 0 0 0 0 0 0 41 0 0 0 0 0 0 0 9 8 7 6 5 4 3 2 1 0))))
  (signals lzlib-error (decompress-buffer #(76 90 73 80 1 12 0 0 128 157 97 211 29 7 5 127 255 248 129 32 0 244 153 11 71 5 0 0 0 0 0 0 0 41 0 0 0 0 0 0 0 9 8 7 6 5 4 3 2 1 0) :ignore-trailing nil))
  ;; Incomplete stream
  (signals lzlib-error (decompress-buffer #(76 90 73 80 1 12 0 0 128 157 97 211 29 7 5 127 255 248))))

(test compress-file
  (let ((decompressed (data-file-path "test.txt"))
        (tmp-1 "/tmp/lzlib-test.txt.lz")
        (tmp-2 "/tmp/lzlib-test.txt"))
    (is-true (compress-file decompressed tmp-1))
    (is-true (decompress-file tmp-1 tmp-2))
    (is (same-files-p decompressed tmp-2))
    (uiop:delete-file-if-exists tmp-1)
    (uiop:delete-file-if-exists tmp-2)))

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
