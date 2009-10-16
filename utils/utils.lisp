(in-package :gsll-util)

;; Utilities for the utilities. I resisted calling this file meta-utils.lisp

(defun all (pred list)
  "Tests whether every element of list satisfies pred."
  (and (mapcar pred list)))

(defun all-of-type (type objects)
  "Tests whether every obj in objects satisfies (typep obj type)"
  (all #'(lambda (obj) (typep obj type)) objects))

(defun sint-vec? (v)
  "Returns whether this is a gsll vector containing signed integers."
  (or (typep v 'gsll:vector-signed-byte-8)
      (typep v 'gsll:vector-signed-byte-16)
      (typep v 'gsll:vector-signed-byte-32)
      #+int64
      (typep v 'gsll:vector-signed-byte-64)))

(defun uint-vec? (v)
  "Returns whether this is a gsll vector containing unsigned integers."
  (or (typep v 'gsll:vector-unsigned-byte-8)
      (typep v 'gsll:vector-unsigned-byte-16)
      (typep v 'gsll:vector-unsigned-byte-32)
      #+int64
      (typep v 'gsll:vector-unsigned-byte-64)))

(defun int-vec? (v)
  "Returns whether this is a gsll vector containing integers"
  (or (uint-vec? v) (sint-vec? v)))

(defun sint-mtx? (m)
  "Returns whether this is a gsll matrix containing signed integers."
  (or (typep m 'gsll:matrix-signed-byte-8)
      (typep m 'gsll:matrix-signed-byte-16)
      (typep m 'gsll:matrix-signed-byte-32)
      #+int64
      (typep m 'gsll:matrix-signed-byte-64)))

(defun uint-mtx? (m)
  "Returns whether this is a gsll matrix containing unsigned integers."
  (or (typep m 'gsll:matrix-unsigned-byte-8)
      (typep m 'gsll:matrix-unsigned-byte-16)
      (typep m 'gsll:matrix-unsigned-byte-32)
      #+int64
      (typep m 'gsll:matrix-unsigned-byte-64)))