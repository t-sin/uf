(defpackage #:uf/runtime
  (:use #:cl #:uf/core))
(in-package #:uf/runtime)

(defparameter *dictionary* nil)

;;; defining words...

(defun init-vm ()
  (let ((vm (make-vm)))
    (setf (vm-dict vm) (copy-tree *dictionary*))
    vm))
