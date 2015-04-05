#|
Copyright 2014-2015 Guillaume LE VAILLANT

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <http://www.gnu.org/licenses/>.
|#

(in-package peercoin-vote)


;;; Use elliptic curve functions from openssl
(define-foreign-library libcrypto
  (t (:default "libcrypto")))

(use-foreign-library libcrypto)


;;; Constants

(defconstant +nid-secp256k1+ 714)
(defconstant +point-conversion-compressed+ 2)
(defconstant +point-conversion-uncompressed+ 4)


;;; Foreign types

(defctype bn-ctx :pointer)
(defctype bn :pointer)
(defctype ec-group :pointer)
(defctype ec-point :pointer)
(defctype ec-key :pointer)


;;; Foreign functions

(defcfun ("BN_CTX_new" bn-ctx-new)
    bn-ctx)

(defcfun ("BN_CTX_free" bn-ctx-free)
    :void
  (ctx bn-ctx))

(defcfun ("BN_CTX_start" bn-ctx-start)
    :void
  (ctx bn-ctx))

(defcfun ("BN_CTX_end" bn-ctx-end)
    :void
  (ctx bn-ctx))

(defcfun ("BN_CTX_get" bn-ctx-get)
    bn
  (ctx bn-ctx))

(defcfun ("BN_new" bn-new)
    bn)

(defcfun ("BN_clear_free" bn-clear-free)
    :void
  (a bn))

(defcfun ("BN_copy" bn-copy)
    bn
  (to bn)
  (from bn))

(defcfun ("BN_mul_word" bn-mul-word)
    :int
  (a bn)
  (w :unsigned-long))

(defcfun ("BN_add" bn-add)
    :int
  (r bn)
  (a bn)
  (b bn))

(defcfun ("BN_cmp" bn-cmp)
    :int
  (a bn)
  (b bn))

(defcfun ("BN_set_word" bn-set-word)
    :int
  (a bn)
  (w :unsigned-long))

(defcfun ("BN_mod_sub" bn-mod-sub)
    :int
  (r bn)
  (a bn)
  (b bn)
  (m bn)
  (ctx bn-ctx))

(defcfun ("BN_mod_inverse" bn-mod-inverse)
    bn
  (r bn)
  (a bn)
  (n bn)
  (ctx bn-ctx))

(defcfun ("BN_mod_mul" bn-mod-mul)
    :int
  (r bn)
  (a bn)
  (b bn)
  (m bn)
  (ctx bn-ctx))

(defcfun ("BN_bin2bn" bn-bin2bn)
    bn
  (s :pointer :unsigned-char)
  (len :int)
  (ret bn))

(defcfun ("BN_rshift" bn-rshift)
    :int
  (r bn)
  (a bn)
  (n :int))

(defcfun ("EC_POINT_new" ec-point-new)
    ec-point
  (group ec-group))

(defcfun ("EC_POINT_clear_free" ec-point-clear-free)
    :void
  (point ec-point))

(defcfun ("EC_POINT_mul" ec-point-mul)
    :int
  (group ec-group)
  (r ec-point)
  (n bn)
  (q ec-point)
  (m bn)
  (ctx bn-ctx))

(defcfun ("EC_POINT_set_compressed_coordinates_GFp" ec-point-set-compressed-coordinates-gfp)
    :int
  (group ec-group)
  (p ec-point)
  (x bn)
  (y_bit :int)
  (ctx bn-ctx))

(defcfun ("EC_GROUP_get_order" ec-group-get-order)
    :int
  (group ec-group)
  (order bn)
  (ctx bn-ctx))

(defcfun ("EC_GROUP_get_curve_GFp" ec-group-get-curve-gfp)
    :int
  (group ec-group)
  (p bn)
  (a bn)
  (b bn)
  (ctx bn-ctx))

(defcfun ("EC_GROUP_get_degree" ec-group-get-degree)
    :int
  (group ec-group))

(defcfun ("EC_KEY_new_by_curve_name" ec-key-new-by-curve-name)
    ec-key
  (nid :int))

(defcfun ("EC_KEY_free" ec-key-free)
    :void
  (key ec-key))

(defcfun ("EC_KEY_get0_group" ec-key-get0-group)
    ec-group
  (key ec-key))

(defcfun ("EC_KEY_set_public_key" ec-key-set-public-key)
    :int
  (key ec-key)
  (pub ec-point))

(defcfun ("EC_KEY_set_conv_form" ec-key-set-conv-form)
    :void
  (key ec-key)
  (form :int))

(defcfun ("i2o_ECPublicKey" i2o-ecpublickey)
    :int
  (key ec-key)
  (out :pointer :string))


;;; Hash functions

(defun sha256 (data)
  "Compute the sha256 hash of the byte sequence DATA."
  (ironclad:digest-sequence :sha256 data))

(defun sha256d (data)
  "Compute the double sha256 hash of the byte sequence DATA."
  (sha256 (sha256 data)))

(defun ripemd160 (data)
  "Compute the ripemd160 hash of the byte sequence DATA."
  (ironclad:digest-sequence :ripemd-160 data))


;;; Base58 functions

(defconstant +peercoin-version-byte+ 55)
(defconstant +peercoin-testnet-version-byte+ 111)
(defparameter *testnet* nil)
(defparameter +base58-symbols+ "123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz")

(defun set-testnet (b)
  "If B is NIL, use main net constants. If not, use test net constants."
  (setf *testnet* (if b t nil)))

(defun base58-encode (data)
  "Make a string containing the base58 encoding of the DATA."
  (let ((x 0)
        (len (length data))
        address)
    (dotimes (i len)
      (setf (ldb (byte 8 (* 8 i)) x) (aref data (- len 1 i))))

    (loop
       while (plusp x)
       do (multiple-value-bind (q r) (floor x 58)
            (setf x q)
            (push (elt +base58-symbols+ r) address)))

    (loop
       for i from 0 below len
       while (zerop (aref data i))
       do (push (elt +base58-symbols+ 0) address))

    (coerce address 'string)))

(defun pretty-print-address (hash)
  "Make a string containing the base58 encoding of an address given its HASH."
  (when hash
    (let ((script (vector (if *testnet*
                              +peercoin-testnet-version-byte+
                              +peercoin-version-byte+))))
      (setf script (concatenate '(vector (unsigned-byte 8)) script hash))
      (setf script (concatenate 'vector script (subseq (sha256d script) 0 4)))
      (base58-encode script))))


;;; Main functions

(defun pubkey-to-address (pubkey)
  "Compute the Peercoin address associated with the ecdsa public key PUBKEY."
  (pretty-print-address (ripemd160 (sha256 pubkey))))

(defun signature-recover-key (eckey sig-r sig-s msg recid)
  "Recover an ecdsa public key from a compact signature."
  (let (group ctx x e order sor eor field r q rr zero n i pubkey pmsg)
    (setf n 0)
    (setf i (floor recid 2))
    (setf group (ec-key-get0-group eckey))
    (setf ctx (bn-ctx-new))
    (bn-ctx-start ctx)
    (setf order (bn-ctx-get ctx))
    (ec-group-get-order group order ctx)
    (setf x (bn-ctx-get ctx))
    (bn-copy x order)
    (bn-mul-word x i)
    (bn-add x x sig-r)
    (setf field (bn-ctx-get ctx))
    (ec-group-get-curve-gfp group field (null-pointer) (null-pointer) ctx)
    (when (>= (bn-cmp x field) 0)
      (return-from signature-recover-key nil))
    (setf r (ec-point-new group))
    (ec-point-set-compressed-coordinates-gfp group r x (mod recid 2) ctx)
    (setf q (ec-point-new group))
    (setf n (ec-group-get-degree group))
    (setf e (bn-ctx-get ctx))
    (setf pmsg (foreign-alloc :unsigned-char :count (length msg) :initial-contents msg))
    (bn-bin2bn pmsg (length msg) e)
    (foreign-free pmsg)
    (when (> (* 8 (length msg)) n)
      (bn-rshift e e (- 8 (logand n 7))))
    (setf zero (bn-ctx-get ctx))
    (bn-set-word zero 0)
    (bn-mod-sub e zero e order ctx)
    (setf rr (bn-ctx-get ctx))
    (bn-mod-inverse rr sig-r order ctx)
    (setf sor (bn-ctx-get ctx))
    (bn-mod-mul sor sig-s rr order ctx)
    (setf eor (bn-ctx-get ctx))
    (bn-mod-mul eor e rr order ctx)
    (ec-point-mul group q eor r sor ctx)
    (ec-key-set-public-key eckey q)
    (let* ((len (i2o-ecpublickey eckey (null-pointer)))
           (ppubkey (foreign-alloc :unsigned-char :count len))
           (pppubkey (foreign-alloc :pointer :initial-element ppubkey)))
      (i2o-ecpublickey eckey pppubkey)
      (setf pubkey (loop
                      for i from 0 below len
                      collect (mem-aref ppubkey :unsigned-char i)))
      (setf pubkey (coerce pubkey '(vector (unsigned-byte 8))))
      (foreign-free pppubkey)
      (foreign-free ppubkey))
    (ec-point-clear-free r)
    (ec-point-clear-free q)
    (bn-ctx-end ctx)
    (bn-ctx-free ctx)
    pubkey))

(defun verify-compact-signature (hash signature address)
  "Check whether a compact ecdsa SIGNATURE was made on HASH by ADDRESS."
  (let (psig sig-r sig-s eckey nv pubkey ok)
    (when (/= (length signature) 65)
      (return-from verify-compact-signature ok))
    (setf nv (elt signature 0))
    (when (or (< nv 27) (>= nv 35))
      (return-from verify-compact-signature ok))
    (setf psig (foreign-alloc :unsigned-char :count (length signature) :initial-contents signature))
    (setf sig-r (bn-new))
    (setf sig-s (bn-new))
    (bn-bin2bn (inc-pointer psig 1) 32 sig-r)
    (bn-bin2bn (inc-pointer psig 33) 32 sig-s)
    (foreign-free psig)
    (setf eckey (ec-key-new-by-curve-name +nid-secp256k1+))
    (when (>= nv 31)
      (ec-key-set-conv-form eckey +point-conversion-compressed+)
      (decf nv 4))
    (setf pubkey (signature-recover-key eckey sig-r sig-s hash (- nv 27)))
    (bn-clear-free sig-r)
    (bn-clear-free sig-s)
    (ec-key-free eckey)
    (when (and pubkey (equal (pubkey-to-address pubkey) address))
      (setf ok t))
    ok))
