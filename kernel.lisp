(defpackage #:uf/kernel
  (:use #:cl)
  (:export #:cell))
(in-package #:uf/kernel)

(defvar types '(flag char number xt addr))

(defstruct cell
  type data)

(defstruct word
  id prev name code immediate)

(defstruct vm
  ip compiling dict pstack rstack cstack)
