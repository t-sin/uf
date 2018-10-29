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

(defun make-stack (size)
  (let ((stack (make-array size :element-type 'cell))
        (top 0))
    (lambda (method &optional val)
      (case method
        (:push (if (< size (1+ top))
                   (values nil :exausted)
                   (progn
                     (setf (svref stack top) val)
                     (incf top))))
        (:pop (progn
                (decf top)
                (cond ((< top 0) (progn
                                   (setf top 0)
                                   (values nil :empty)))
                      (t (values (svref stack top) t)))))))))
