;;; This file is part of cl-lzlib
;;; Copyright 2019-2022 Guillaume LE VAILLANT
;;; Distributed under the GNU GPL v3 or later.
;;; See the file LICENSE for terms of use and distribution.

(defpackage :lzlib
  (:use :cl :trivial-gray-streams)
  (:export #:compress-stream
           #:compress-file
           #:compress-buffer
           #:decompress-stream
           #:decompress-file
           #:decompress-buffer
           #:lzlib-error
           #:make-compressing-stream
           #:make-decompressing-stream
           #:with-compressing-stream
           #:with-decompressing-stream))
