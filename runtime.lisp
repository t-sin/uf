(defpackage #:uf/runtime
  (:use #:cl
        #:uf/kernel)
  (:export #:*vm*))
(in-package #:uf/runtime)

(defparameter *vm*
  (with-initial-vm ()
    ((:next nil nil)
     (print :next-called)
     (incf (vm-ip vm)))
    ((:nest nil nil)
     (print :nest-called)
     (funcall (vm-rstack vm) :push (cons (vm-ip vm) (vm-program vm)))
     (setf (vm-program vm) (funcall (vm-pstack vm) :pop)
           (vm-ip vm) 0))
    ((:unnest nil nil)
     (print :unnest-called)
     (destructuring-bind (ip . program)
         (funcall (vm-rstack vm) :pop)
       (setf (vm-program vm) program
             (vm-ip vm) ip)))))
