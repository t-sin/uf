(defpackage #:uf/kernel
  (:use #:cl)
  (:export #:cell))
(in-package #:uf/kernel)

(defvar types '(flag char number xt addr))

(defstruct cell
  type data)

(defstruct word
  prev name code immediate)

(defstruct vm
  ip compiling dict pstack rstack cstack)

(defun make-word% (dict name code immediate)
  (make-word :prev dict :name name :code code :immediate immediate))

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

(defvar pstack-size 1000)
(defvar rstack-size 1000)
(defvar cstack-size 1000)

(defun init-vm ()
  (let* ((vm (make-vm :ip nil
                      :compiling nil
                      :dict (make-word% nil nil nil nil)
                      :pstack (make-stack pstack-size)
                      :rstack (make-stack rstack-size)
                      :cstack (make-stack cstack-size))))
    vm))
