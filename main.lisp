(defpackage #:uf
  (:use #:cl)
  (:import-from #:uf/condition
                #:uf/error)
  (:import-from #:uf/vm
                #:vm-stream
                #:vm-debug
                #:vm-compbuf
                #:make-vm*
                #:interpret)
  (:import-from #:uf/builtin/main
                #:*initial-word-list*)
  (:export #:init-vm
           #:interpret))
(in-package #:uf)

;;;;
;; runtime

(defun init-vm (stream &optional (debug nil) (word-list *initial-word-list*))
  (let ((vm (make-vm* nil)))
    (loop
      :for def :in (reverse word-list)
      :do (funcall def vm))
    (setf (vm-stream vm) stream
          (vm-debug vm) debug)
    vm))
