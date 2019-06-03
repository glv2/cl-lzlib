;;; This file is part of cl-lzlib
;;; Copyright 2019 Guillaume LE VAILLANT
;;; Distributed under the GNU GPL v3 or later.
;;; See the file LICENSE for terms of use and distribution.

(in-package :lzlib)


(cffi:define-foreign-library lzlib
  (:unix (:or "liblz.so"
              "liblz.so.1"))
  (t (:default "liblz")))

(cffi:use-foreign-library lzlib)


(defconstant +lz-ok+ 0)
(defconstant +lz-bad-argument+ 1)
(defconstant +lz-mem-error+ 2)
(defconstant +lz-sequence-error+ 3)
(defconstant +lz-header-error+ 4)
(defconstant +lz-unexpected-eof+ 5)
(defconstant +lz-data-error+ 6)
(defconstant +lz-library-error+ 7)

(cffi:defcfun ("LZ_version" lz-version) :string)
(cffi:defcfun ("LZ_strerror" lz-strerror) :string
  (lz-errno :int))
(cffi:defcfun ("LZ_min_dictionary_bits" lz-min-dictionary-bits) :int)
(cffi:defcfun ("LZ_min_dictionary_size" lz-min-dictionary-size) :int)
(cffi:defcfun ("LZ_max_dictionary_bits" lz-max-dictionary-bits) :int)
(cffi:defcfun ("LZ_max_dictionary_size" lz-max-dictionary-size) :int)
(cffi:defcfun ("LZ_min_match_len_limit" lz-min-match-len-limit) :int)
(cffi:defcfun ("LZ_max_match_len_limit" lz-max-match-len-limit) :int)

(cffi:defcfun ("LZ_compress_open" lz-compress-open) :pointer
  (dictionary-size :int)
  (match-len-limit :int)
  (member-size :unsigned-long-long))
(cffi:defcfun ("LZ_compress_close" lz-compress-close) :int
  (encoder :pointer))
(cffi:defcfun ("LZ_compress_finish" lz-compress-finish) :int
  (encoder :pointer))
(cffi:defcfun ("LZ_compress_restart_member" lz-compress-restart-member) :int
  (encoder :pointer)
  (member-size :unsigned-long-long))
(cffi:defcfun ("LZ_compress_sync_flush" lz-compress-sync-flush) :int
  (encoder :pointer))
(cffi:defcfun ("LZ_compress_read" lz-compress-read) :int
  (encoder :pointer)
  (buffer :pointer)
  (size :int))
(cffi:defcfun ("LZ_compress_write" lz-compress-write) :int
  (encoder :pointer)
  (buffer :pointer)
  (size :int))
(cffi:defcfun ("LZ_compress_write_size" lz-compress-write-size) :int
  (encoder :pointer))
(cffi:defcfun ("LZ_compress_errno" lz-compress-errno) :int
  (encoder :pointer))
(cffi:defcfun ("LZ_compress_finished" lz-compress-finished) :int
  (encoder :pointer))
(cffi:defcfun ("LZ_compress_member_finished" lz-compress-member-finished) :int
  (encoder :pointer))
(cffi:defcfun ("LZ_compress_data_position" lz-compress-data-position) :unsigned-long-long
  (encoder :pointer))
(cffi:defcfun ("LZ_compress_member_position" lz-compress-member-position) :unsigned-long-long
  (encoder :pointer))
(cffi:defcfun ("LZ_compress_total_in_size" lz-compress-total-in-size) :unsigned-long-long
  (encoder :pointer))
(cffi:defcfun ("LZ_compress_total_out_size" lz-compress-total-out-size) :unsigned-long-long
  (encoder :pointer))

(cffi:defcfun ("LZ_decompress_open" lz-decompress-open) :pointer)
(cffi:defcfun ("LZ_decompress_close" lz-decompress-close) :int
  (decoder :pointer))
(cffi:defcfun ("LZ_decompress_finish" lz-decompress-finish) :int
  (decoder :pointer))
(cffi:defcfun ("LZ_decompress_reset" lz-decompress-reset) :int
  (decoder :pointer))
(cffi:defcfun ("LZ_decompress_sync_to_member" lz-decompress-sync-to-member) :int
  (decoder :pointer))
(cffi:defcfun ("LZ_decompress_read" lz-decompress-read) :int
  (decoder :pointer)
  (buffer :pointer)
  (size :int))
(cffi:defcfun ("LZ_decompress_write" lz-decompress-write) :int
  (decoder :pointer)
  (buffer :pointer)
  (size :int))
(cffi:defcfun ("LZ_decompress_write_size" lz-decompress-write-size) :int
  (decoder :pointer))
(cffi:defcfun ("LZ_decompress_errno" lz-decompress-errno) :int
  (decoder :pointer))
(cffi:defcfun ("LZ_decompress_finished" lz-decompress-finished) :int
  (decoder :pointer))
(cffi:defcfun ("LZ_decompress_member_finished" lz-decompress-member-finished) :int
  (decoder :pointer))
(cffi:defcfun ("LZ_decompress_member_version" lz-decompress-member-version) :int
  (decoder :pointer))
(cffi:defcfun ("LZ_decompress_dictionary_size" lz-decompress-dictionary-size) :int
  (decoder :pointer))
(cffi:defcfun ("LZ_decompress_data_crc" lz-decompress-data-crc) :unsigned-int
  (decoder :pointer))
(cffi:defcfun ("LZ_decompress_data_position" lz-decompress-data-position) :unsigned-long-long
  (decoder :pointer))
(cffi:defcfun ("LZ_decompress_member_position" lz-decompress-member-position) :unsigned-long-long
  (decoder :pointer))
(cffi:defcfun ("LZ_decompress_total_in_size" lz-decompress-total-in-size) :unsigned-long-long
  (decoder :pointer))
(cffi:defcfun ("LZ_decompress_total_out_size" lz-decompress-total-out-size) :unsigned-long-long
  (decoder :pointer))
