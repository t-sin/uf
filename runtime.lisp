(defpackage #:tf/runtime
  (:use #:cl #:tf/core))
(in-package #:tf/runtime)

(defparameter *dictionary* nil)

;;; defining words...

(defun init-vm ()
  (let ((vm (make-vm)))
    (setf (vm-dict vm) (copy-tree *dictionary*))
    vm))
