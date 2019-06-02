;;; This file is part of cl-lzlib
;;; Copyright 2019 Guillaume LE VAILLANT
;;; Distributed under the GNU GPL v3 or later.
;;; See the file LICENSE for terms of use and distribution.

(defsystem "lzlib"
  :name "lzlib"
  :description "FFI binding the lzlib LZMA (de)compressor"
  :version "0.1"
  :author "Guillaume LE VAILLANT"
  :license "GPL-3"
  :depends-on ("cffi")
  :components ((:module "src"
                :serial t
                :components ((:file "package")
                             (:file "lzlib")
                             (:file "lzip")))))
