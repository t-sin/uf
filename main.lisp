(defpackage #:uf
  (:use #:cl)
  (:import-from #:uf/core
                #:make-vm
                #:vm-dict
                #:*dictionary*
                #:parse
                #:execute)
  (:import-from #:uf/runtime)
(:export #:init-vm
         #:parse
         #:execute))
(in-package #:uf)

(defun init-vm (code)
  (let ((vm (make-vm :code code :ip 0)))
    (setf (vm-dict vm) *dictionary*)
    vm))
