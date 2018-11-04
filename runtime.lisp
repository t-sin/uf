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
     (multiple-value-bind (value status)
         (funcall (vm-pstack vm) :pop)
       (if (eq status :empty)
           (error "stack underflow at :nest")
           (setf (vm-program vm) value
                 (vm-ip vm) 0))))

    ((:unnest nil nil)
     (print :unnest-called)
     (multiple-value-bind (value status)
         (funcall (vm-rstack vm) :pop)
       (if (eq status :empty)
           (error "stack underflow at :nest")
           (destructuring-bind (ip . program) value
             (setf (vm-program vm) program
                   (vm-ip vm) ip)))))))
