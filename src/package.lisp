;;; This file is part of cl-lzlib
;;; Copyright 2019 Guillaume LE VAILLANT
;;; Distributed under the GNU GPL v3 or later.
;;; See the file LICENSE for terms of use and distribution.

(defpackage :lzlib
  (:use :cl)
  (:export #:compress-stream
           #:compress-file
           #:compress-buffer
           #:decompress-stream
           #:decompress-file
           #:decompress-buffer
           #:lzlib-error))
