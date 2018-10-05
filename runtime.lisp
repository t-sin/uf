(defpackage #:uf/runtime
  (:use #:cl #:uf/core))
(in-package #:uf/runtime)

(defparameter *dictionary* nil)

(defmacro defword ((name) &body body)
  (let ((name* (intern (format nil "word:~a" (symbol-name name)) :uf/dict))
        ($word (gensym "uf")))
    `(progn
       (defun ,name* (vm) ,@body)
       (let ((,$word (find ',name* *dictionary* :key #'word-name)))
         (if ,$word
             (error "word ~s is already registered" (word-name ,$word))
             (push (make-word :name ',(intern (symbol-name name) :uf/dict) :code (function ,name*)) *dictionary*))))))

;;; defining words...

;; I/O
(defword (|.|) (format t "~a" (pop (vm-stack vm))))

;; arithmetic operation
(defword (|+|) (push (+ (pop (vm-stack vm)) (pop (vm-stack vm))) (vm-stack vm)))
(defword (|-|) (push (- (pop (vm-stack vm)) (pop (vm-stack vm))) (vm-stack vm)))
(defword (|*|) (push (* (pop (vm-stack vm)) (pop (vm-stack vm))) (vm-stack vm)))
(defword (|/|) (push (/ (pop (vm-stack vm)) (pop (vm-stack vm))) (vm-stack vm)))

(defun init-vm ()
  (let ((vm (make-vm)))
    (setf (vm-dict vm) (copy-tree *dictionary*))
    vm))
