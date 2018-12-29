(defpackage #:uf
  (:use #:cl)
  (:import-from #:uf/condition
                #:uf/error)
  (:import-from #:uf/vm
                #:vm-stream
                #:vm-compbuf
                #:make-vm*
                #:interpret)
  (:import-from #:uf/builtin
                #:*initial-word-list*)
  (:export #:init-vm
           #:interpret))
(in-package #:uf)

;;;;
;; runtime

(defun init-vm (stream &optional (debug nil) (word-list *initial-word-list*))
  (let ((vm (make-vm* debug)))
    (loop
      :for def :in (reverse word-list)
      :do (funcall def vm))
    (setf (vm-stream vm) stream)
    vm))
