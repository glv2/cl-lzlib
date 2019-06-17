;;; This file is part of cl-lzlib
;;; Copyright 2019 Guillaume LE VAILLANT
;;; Distributed under the GNU GPL v3 or later.
;;; See the file LICENSE for terms of use and distribution.

(in-package :lzlib)


(deftype i32 () '(signed-byte 32))
(deftype u32 () '(unsigned-byte 32))
(deftype u64 () '(unsigned-byte 64))

(cffi:define-foreign-library lzlib
  (:unix (:or "liblz.so"
              "liblz.so.1"))
  (t (:default "liblz")))

(cffi:use-foreign-library lzlib)


;;;
;;; Library version
;;;

(declaim (ftype (function () t) lz-version))
(cffi:defcfun ("LZ_version" lz-version) :string
  "Returns the library version as a string.")


;;;
;;; Errors
;;;

(defconstant +lz-ok+ 0
  "The value of this constant is 0 and is used to indicate that there is no
error.")

(defconstant +lz-bad-argument+ 1
  "At least one of the arguments passed to the library function was invalid.")

(defconstant +lz-mem-error+ 2
  "No memory available. The system cannot allocate more virtual memory because
its capacity is full.")

(defconstant +lz-sequence-error+ 3
  "A library function was called in the wrong order. For example
'LZ_compress_restart_member' was called before 'LZ_compress_member_finished'
indicates that the current member is finished.")

(defconstant +lz-header-error+ 4
  "An invalid member header (one with the wrong magic bytes) was read. If this
happens at the end of the data stream it may indicate trailing data.")

(defconstant +lz-unexpected-eof+ 5
  "The end of the data stream was reached in the middle of a member.")

(defconstant +lz-data-error+ 6
  "The data stream is corrupt. If 'LZ_decompress_member_position' is 6 or less,
it indicates either a format version not supported, an invalid dictionary size,
a corrupt header in a multimember data stream, or trailing data too similar to
a valid lzip header. Lziprecover can be used to remove conflicting trailing data
from a file.")

(defconstant +lz-library-error+ 7
  "A bug was detected in the library.")

(declaim (ftype (function (i32) t) lz-strerror))
(cffi:defcfun ("LZ_strerror" lz-strerror) :string
  "Returns the standard error message for a given error code. The messages are
fairly short; there are no multi-line messages or embedded newlines. This
function makes it easy for your program to report informative error messages
about the failure of a library call. The value of lz_errno normally comes from
a call to 'LZ_(de)compress_errno'."
  (lz-errno :int))


;;;
;;; Parameter limits
;;;

(declaim (ftype (function () i32) lz-min-dictionary-bits))
(cffi:defcfun ("LZ_min_dictionary_bits" lz-min-dictionary-bits) :int
  "Returns the base 2 logarithm of the smallest valid dictionary size.")

(declaim (ftype (function () i32) lz-min-dictionary-size))
(cffi:defcfun ("LZ_min_dictionary_size" lz-min-dictionary-size) :int
  "Returns the smallest valid dictionary size.")

(declaim (ftype (function () i32) lz-max-dictionary-bits))
(cffi:defcfun ("LZ_max_dictionary_bits" lz-max-dictionary-bits) :int
  "Returns the base 2 logarithm of the largest valid dictionary size.")

(declaim (ftype (function () i32) lz-max-dictionary-size))
(cffi:defcfun ("LZ_max_dictionary_size" lz-max-dictionary-size) :int
  "Returns the largest valid dictionary size.")

(declaim (ftype (function () i32) lz-min-match-len-limit))
(cffi:defcfun ("LZ_min_match_len_limit" lz-min-match-len-limit) :int
  "Returns the smallest valid match length limit.")

(declaim (ftype (function () i32) lz-max-match-len-limit))
(cffi:defcfun ("LZ_max_match_len_limit" lz-max-match-len-limit) :int
  "Returns the largest valid match length limit.")


;;;
;;; Compression functions
;;;
;;; These are the functions used to compress data. In case of error, all of them
;;; return -1 or 0, for signed and unsigned return values respectively, except
;;; 'LZ_compress_open' whose return value must be verified by calling
;;; 'LZ_compress_errno' before using it.
;;;

(declaim (ftype (function (i32 i32 u64) t) lz-compress-open))
(cffi:defcfun ("LZ_compress_open" lz-compress-open) :pointer
  "Initializes the internal stream state for compression and returns a pointer
that can only be used as the encoder argument for the other LZ_compress
functions, or a null pointer if the encoder could not be allocated. The returned
pointer must be verified by calling 'LZ_compress_errno' before using it. If
'LZ_compress_errno' does not return 'LZ_ok', the returned pointer must not be
used and should be freed with 'LZ_compress_close' to avoid memory leaks.
dictionary_size sets the dictionary size to be used, in bytes. Valid values
range from 4 KiB to 512 MiB. Note that dictionary sizes are quantized. If the
specified size does not match one of the valid sizes, it will be rounded upwards
by adding up to (dictionary_size / 8) to it. match_len_limit sets the match
length limit in bytes. Valid values range from 5 to 273. Larger values usually
give better compression ratios but longer compression times. If dictionary_size
is 65535 and match_len_limit is 16, the fast variant of LZMA is chosen, which
produces identical compressed output as lzip -0. (The dictionary size used will
be rounded upwards to 64 KiB). member_size sets the member size limit in bytes.
Valid values range from 100 kB to 2 PiB. Small member size may degrade
compression ratio, so use it only when needed. To produce a single-member data
stream, give member_size a value larger than the amount of data to be produced.
Values larger than 2 PiB will be reduced to 2 PiB to prevent the uncompressed
size of the member from overflowing."
  (dictionary-size :int)
  (match-len-limit :int)
  (member-size :unsigned-long-long))

(declaim (ftype (function (t) i32) lz-compress-close))
(cffi:defcfun ("LZ_compress_close" lz-compress-close) :int
  "Frees all dynamically allocated data structures for this stream. This
function discards any unprocessed input and does not flush any pending output.
After a call to 'LZ_compress_close', encoder can no longer be used as an
argument to any LZ_compress function."
  (encoder :pointer))

(declaim (ftype (function (t) i32) lz-compress-finish))
(cffi:defcfun ("LZ_compress_finish" lz-compress-finish) :int
  "Use this function to tell 'lzlib' that all the data for this member have
already been written (with the 'LZ_compress_write' function). It is safe to call
'LZ_compress_finish' as many times as needed. After all the produced compressed
data have been read with 'LZ_compress_read' and 'LZ_compress_member_finished'
returns 1, a new member can be started with 'LZ_compress_restart_member'."
  (encoder :pointer))

(declaim (ftype (function (t u64) i32) lz-compress-restart-member))
(cffi:defcfun ("LZ_compress_restart_member" lz-compress-restart-member) :int
  "Use this function to start a new member in a multimember data stream. Call
this function only after 'LZ_compress_member_finished' indicates that the
current member has been fully read (with the 'LZ_compress_read' function)."
  (encoder :pointer)
  (member-size :unsigned-long-long))

(declaim (ftype (function (t) i32) lz-compress-sync-flush))
(cffi:defcfun ("LZ_compress_sync_flush" lz-compress-sync-flush) :int
  "Use this function to make available to 'LZ_compress_read' all the data
already written with the 'LZ_compress_write' function. First call
'LZ_compress_sync_flush'. Then call 'LZ_compress_read' until it returns 0.
Repeated use of 'LZ_compress_sync_flush' may degrade compression ratio, so use
it only when needed."
  (encoder :pointer))

(declaim (ftype (function (t t i32) i32) lz-compress-read))
(cffi:defcfun ("LZ_compress_read" lz-compress-read) :int
  "The 'LZ_compress_read' function reads up to size bytes from the stream
pointed to by encoder, storing the results in buffer. The return value is the
number of bytes actually read. This might be less than size; for example, if
there aren't that many bytes left in the stream or if more bytes have to be yet
written with the 'LZ_compress_write' function. Note that reading less than size
bytes is not an error."
  (encoder :pointer)
  (buffer :pointer)
  (size :int))

(declaim (ftype (function (t t i32) i32) lz-compress-write))
(cffi:defcfun ("LZ_compress_write" lz-compress-write) :int
  "The 'LZ_compress_write' function writes up to size bytes from buffer to the
stream pointed to by encoder. The return value is the number of bytes actually
written. This might be less than size. Note that writing less than size bytes is
not an error."
  (encoder :pointer)
  (buffer :pointer)
  (size :int))

(declaim (ftype (function (t) i32) lz-compress-write-size))
(cffi:defcfun ("LZ_compress_write_size" lz-compress-write-size) :int
  "The 'LZ_compress_write_size' function returns the maximum number of bytes
that can be immediately written through the 'LZ_compress_write' function. It is
guaranteed that an immediate call to 'LZ_compress_write' will accept a size up
to the returned number of bytes."
  (encoder :pointer))

(declaim (ftype (function (t) i32) lz-compress-errno))
(cffi:defcfun ("LZ_compress_errno" lz-compress-errno) :int
  "Returns the current error code for encoder."
  (encoder :pointer))

(declaim (ftype (function (t) i32) lz-compress-finished))
(cffi:defcfun ("LZ_compress_finished" lz-compress-finished) :int
  "Returns 1 if all the data have been read and 'LZ_compress_close' can be
safely called. Otherwise it returns 0. 'LZ_compress_finished' implies
'LZ_compress_member_finished'."
  (encoder :pointer))

(declaim (ftype (function (t) i32) lz-compress-member-finished))
(cffi:defcfun ("LZ_compress_member_finished" lz-compress-member-finished) :int
  "Returns 1 if the current member, in a multimember data stream, has been fully
read and 'LZ_compress_restart_member' can be safely called. Otherwise it returns
0."
  (encoder :pointer))

(declaim (ftype (function (t) u64) lz-compress-data-position))
(cffi:defcfun ("LZ_compress_data_position" lz-compress-data-position) :unsigned-long-long
  "Returns the number of input bytes already compressed in the current member."
  (encoder :pointer))

(declaim (ftype (function (t) u64) lz-compress-member-position))
(cffi:defcfun ("LZ_compress_member_position" lz-compress-member-position) :unsigned-long-long
  "Returns the number of compressed bytes already produced, but perhaps not yet
read, in the current member."
  (encoder :pointer))

(declaim (ftype (function (t) u64) lz-compress-total-in-size))
(cffi:defcfun ("LZ_compress_total_in_size" lz-compress-total-in-size) :unsigned-long-long
  "Returns the total number of input bytes already compressed."
  (encoder :pointer))

(declaim (ftype (function (t) u64) lz-compress-total-out-size))
(cffi:defcfun ("LZ_compress_total_out_size" lz-compress-total-out-size) :unsigned-long-long
  "Returns the total number of compressed bytes already produced, but perhaps
not yet read."
  (encoder :pointer))


;;;
;;; Decompression functions
;;;
;;; These are the functions used to decompress data. In case of error, all of
;;; them return -1 or 0, for signed and unsigned return values respectively,
;;; except 'LZ_decompress_open' whose return value must be verified by calling
;;; 'LZ_decompress_errno' before using it.
;;;

(declaim (ftype (function () t) lz-decompress-open))
(cffi:defcfun ("LZ_decompress_open" lz-decompress-open) :pointer
  "Initializes the internal stream state for decompression and returns a pointer
that can only be used as the decoder argument for the other LZ_decompress
functions, or a null pointer if the decoder could not be allocated. The returned
pointer must be verified by calling 'LZ_decompress_errno' before using it. If
'LZ_decompress_errno' does not return 'LZ_ok', the returned pointer must not be
used and should be freed with 'LZ_decompress_close' to avoid memory leaks.")

(declaim (ftype (function (t) i32) lz-decompress-close))
(cffi:defcfun ("LZ_decompress_close" lz-decompress-close) :int
  "Frees all dynamically allocated data structures for this stream. This
function discards any unprocessed input and does not flush any pending output.
After a call to 'LZ_decompress_close', decoder can no longer be used as an
argument to any LZ_decompress function."
  (decoder :pointer))

(declaim (ftype (function (t) i32) lz-decompress-finish))
(cffi:defcfun ("LZ_decompress_finish" lz-decompress-finish) :int
  "Use this function to tell 'lzlib' that all the data for this stream have
already been written (with the 'LZ_decompress_write' function). It is safe to
call 'LZ_decompress_finish' as many times as needed."
  (decoder :pointer))

(declaim (ftype (function (t) i32) lz-decompress-reset))
(cffi:defcfun ("LZ_decompress_reset" lz-decompress-reset) :int
  "Resets the internal state of decoder as it was just after opening it with the
'LZ_decompress_open' function. Data stored in the internal buffers is discarded.
Position counters are set to 0."
  (decoder :pointer))

(declaim (ftype (function (t) i32) lz-decompress-sync-to-member))
(cffi:defcfun ("LZ_decompress_sync_to_member" lz-decompress-sync-to-member) :int
  "Resets the error state of decoder and enters a search state that lasts until
a new member header (or the end of the stream) is found. After a successful call
to 'LZ_decompress_sync_to_member', data written with 'LZ_decompress_write' will
be consumed and 'LZ_decompress_read' will return 0 until a header is found. This
function is useful to discard any data preceding the first member, or to discard
the rest of the current member, for example in case of a data error. If the
decoder is already at the beginning of a member, this function does nothing."
  (decoder :pointer))

(declaim (ftype (function (t t i32) i32) lz-decompress-read))
(cffi:defcfun ("LZ_decompress_read" lz-decompress-read) :int
  "The 'LZ_decompress_read' function reads up to size bytes from the stream
pointed to by decoder, storing the results in buffer. The return value is the
number of bytes actually read. This might be less than size; for example, if
there aren't that many bytes left in the stream or if more bytes have to be yet
written with the 'LZ_decompress_write' function. Note that reading less than
size bytes is not an error. In case of decompression error caused by corrupt or
truncated data, 'LZ_decompress_read' does not signal the error immediately to
the application, but waits until all decoded bytes have been read. This allows
tools like tarlz to recover as much data as possible from each damaged member."
  (decoder :pointer)
  (buffer :pointer)
  (size :int))

(declaim (ftype (function (t t i32) i32) lz-decompress-write))
(cffi:defcfun ("LZ_decompress_write" lz-decompress-write) :int
  "The 'LZ_decompress_write' function writes up to size bytes from buffer to the
stream pointed to by decoder. The return value is the number of bytes actually
written. This might be less than size. Note that writing less than size bytes is
not an error."
  (decoder :pointer)
  (buffer :pointer)
  (size :int))

(declaim (ftype (function (t) i32) lz-decompress-write-size))
(cffi:defcfun ("LZ_decompress_write_size" lz-decompress-write-size) :int
  "The 'LZ_decompress_write_size' function returns the maximum number of bytes
that can be immediately written through the 'LZ_decompress_write' function. It
is guaranteed that an immediate call to 'LZ_decompress_write' will accept a size
up to the returned number of bytes."
  (decoder :pointer))

(declaim (ftype (function (t) i32) lz-decompress-errno))
(cffi:defcfun ("LZ_decompress_errno" lz-decompress-errno) :int
  "Returns the current error code for decoder."
  (decoder :pointer))

(declaim (ftype (function (t) i32) lz-decompress-finished))
(cffi:defcfun ("LZ_decompress_finished" lz-decompress-finished) :int
  "Returns 1 if all the data have been read and 'LZ_decompress_close' can be
safely called. Otherwise it returns 0."
  (decoder :pointer))

(declaim (ftype (function (t) i32) lz-decompress-member-finished))
(cffi:defcfun ("LZ_decompress_member_finished" lz-decompress-member-finished) :int
  "Returns 1 if the previous call to 'LZ_decompress_read' finished reading the
current member, indicating that final values for member are available through
'LZ_decompress_data_crc', 'LZ_decompress_data_position', and
'LZ_decompress_member_position'. Otherwise it returns 0."
  (decoder :pointer))

(declaim (ftype (function (t) i32) lz-decompress-member-version))
(cffi:defcfun ("LZ_decompress_member_version" lz-decompress-member-version) :int
  "Returns the version of current member from member header."
  (decoder :pointer))

(declaim (ftype (function (t) i32) lz-decompress-dictionary-size))
(cffi:defcfun ("LZ_decompress_dictionary_size" lz-decompress-dictionary-size) :int
  "Returns the dictionary size of current member from member header."
  (decoder :pointer))

(declaim (ftype (function (t) u32) lz-decompress-data-crc))
(cffi:defcfun ("LZ_decompress_data_crc" lz-decompress-data-crc) :unsigned-int
  "Returns the 32 bit Cyclic Redundancy Check of the data decompressed from the
current member. The returned value is valid only when
'LZ_decompress_member_finished' returns 1."
  (decoder :pointer))

(declaim (ftype (function (t) u64) lz-decompress-data-position))
(cffi:defcfun ("LZ_decompress_data_position" lz-decompress-data-position) :unsigned-long-long
  "Returns the number of decompressed bytes already produced, but perhaps not
yet read, in the current member."
  (decoder :pointer))

(declaim (ftype (function (t) u64) lz-decompress-member-position))
(cffi:defcfun ("LZ_decompress_member_position" lz-decompress-member-position) :unsigned-long-long
  "Returns the number of input bytes already decompressed in the current
member."
  (decoder :pointer))

(declaim (ftype (function (t) u64) lz-decompress-total-in-size))
(cffi:defcfun ("LZ_decompress_total_in_size" lz-decompress-total-in-size) :unsigned-long-long
  "Returns the total number of input bytes already decompressed."
  (decoder :pointer))

(declaim (ftype (function (t) u64) lz-decompress-total-out-size))
(cffi:defcfun ("LZ_decompress_total_out_size" lz-decompress-total-out-size) :unsigned-long-long
  "Returns the total number of decompressed bytes already produced, but perhaps
not yet read."
  (decoder :pointer))
