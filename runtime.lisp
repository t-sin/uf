(defpackage #:uf/runtime
  (:use #:cl
        #:uf/kernel)
  (:export #:*vm*))
(in-package #:uf/runtime)

(defmacro with-vm (() &body body)
  `(let ((,'vm (init-vm)))
     (declare (ignorable vm))
     ,@body))

(defmacro define-word ((name immediate data) &body body)
  (let (($vm (gensym "builtin-arg")))
    `(add-builtin-word ,'vm ',name
                       (lambda (,$vm) (declare (ignorable ,$vm))  ,@(substitute $vm 'vm body))
                       ,data ,immediate)))

(defmacro with-initial-vm (() &body worddefs)
  `(with-vm ()
     ,@(mapcar (lambda (def) `(define-word (,(caar def) ,(cadar def) ,(caddar def)) ,@(cdr def)))
               worddefs)
     ,'vm))

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
                   (vm-ip vm) ip)))))
    ((:|[| t nil)
     (print :enter-interpretaion-called)
     (setf (vm-compiling vm) nil)
     (incf (vm-ip vm)))
    ((:|]| t nil)
     (print :enter-compilation-called)
     (setf (vm-compiling vm) t)
     (incf (vm-ip vm)))))
