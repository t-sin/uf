(defpackage #:uf/runtime
  (:use #:cl
        #:uf/kernel)
  (:export #:*vm*))
(in-package #:uf/runtime)

(defparameter *vm*
  (with-initial-vm ()
    ((:next nil nil)
     (print :next-called)
     (incf (vm-ip vm)))))
