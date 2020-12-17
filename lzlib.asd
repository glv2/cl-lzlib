;;; This file is part of cl-lzlib
;;; Copyright 2019-2020 Guillaume LE VAILLANT
;;; Distributed under the GNU GPL v3 or later.
;;; See the file LICENSE for terms of use and distribution.

(defsystem "lzlib"
  :name "lzlib"
  :description "lzip (LZMA) (de)compression using bindings to lzlib"
  :version "1.1"
  :author "Guillaume LE VAILLANT"
  :license "GPL-3"
  :depends-on ("cffi" "cl-octet-streams" "lparallel")
  :in-order-to ((test-op (test-op "lzlib-tests")))
  :components ((:module "src"
                :serial t
                :components ((:file "package")
                             (:file "lzlib")
                             (:file "lzip")))))
