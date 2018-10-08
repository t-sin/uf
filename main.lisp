(defpackage #:uf
  (:use #:cl)
  (:import-from #:uf/core
                #:make-vm
                #:vm-ip
                #:vm-code
                #:vm-dict
                #:*dictionary*
                #:parse
                #:execute)
  (:import-from #:uf/runtime)
(:export #:init-vm
         #:parse
         #:execute
         #:start-repl))
(in-package #:uf)

(defun init-vm (code &optional debug-p)
  (let ((vm (make-vm :code code :ip 0 :debug-p debug-p)))
    (setf (vm-dict vm) *dictionary*)
    vm))

(defun start-repl (stream vm)
  (handler-bind
      ((condition (lambda (c) (error "error in repl: ~s!" c))))
    (loop
      :for line := (read-line stream nil :eof)
      :until (eq line :eof)
      :for code := (with-input-from-string (in line) (parse in))
      :do (handler-bind
              ((condition (lambda (c) (format t "not ok~%  ~s~%" c))))
            (setf (vm-code vm) code
                  (vm-ip vm) 0)
            (execute vm))
      :do (format t "ok~%"))))
