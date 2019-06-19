;;; This file is part of cl-lzlib
;;; Copyright 2019 Guillaume LE VAILLANT
;;; Distributed under the GNU GPL v3 or later.
;;; See the file LICENSE for terms of use and distribution.

(defsystem "lzlib-tests"
  :name "lzlib-tests"
  :description "Tests for lzlib"
  :version "1.0"
  :author "Guillaume LE VAILLANT"
  :license "GPL-3"
  :depends-on ("cl-octet-streams" "fiveam" "lzlib" "uiop")
  :in-order-to ((test-op (load-op "lzlib-tests")))
  :perform (test-op (op s)
             (let ((tests (uiop:find-symbol* 'lzlib-unit-tests :lzlib-tests)))
               (uiop:symbol-call :fiveam 'run! tests)))
  :components ((:module "tests"
                :serial t
                :components ((:static-file "test.txt")
                             (:static-file "test.txt.lz")
                             (:static-file "test-multimember.dat")
                             (:static-file "test-multimember.dat.lz")
                             (:file "tests")))))
