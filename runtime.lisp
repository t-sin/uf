(defpackage #:uf/runtime
  (:use #:cl #:uf/core))
(in-package #:uf/runtime)

(defparameter *dictionary* nil)

;;; defining words...
(defun |w:.| (vm)
  (print (pop (vm-stack vm))))
(let ((word (find 'uf/dict::|.| *dictionary* :key #'word-name)))
  (if word
      (error "word ~s is already registered" (word-name word))
      (push (make-word :name 'uf/dict::|.| :code #'|w:.|) *dictionary*)))

(defun init-vm ()
  (let ((vm (make-vm)))
    (setf (vm-dict vm) (copy-tree *dictionary*))
    vm))
