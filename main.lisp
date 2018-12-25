(defpackage #:uf
  (:use #:cl)
  (:import-from #:uf/condition
                #:uf/error)
  (:import-from #:uf/reader
                #:next-token
                #:read-to)
  (:import-from #:uf/stack
                #:stack-pop
                #:stack-push)
  (:import-from #:uf/vm
                #:word
                #:word-prev
                #:word-name
                #:word-builtin?
                #:word-immediate?
                #:word-builtin-fn
                #:word-code
                #:word-data

                #:vm
                #:vm-stream
                #:vm-program
                #:vm-dict
                #:vm-ip
                #:vm-comp?
                #:vm-compbuf
                #:vm-pstack
                #:vm-rstack

                #:make-vm*
                #:add-builtin-word
                #:add-word

                #:vm/find
                #:vm/create
                #:vm/name
                #:vm/compile
                #:vm/interpret
                #:vm/terminate-compile
                #:vm/nest
                #:vm/unnest
                #:vm/next)
  (:export))
(in-package #:uf)

;;;;
;; interpreter

(define-condition uf/empty-program (uf/error) ())

(defun execute (vm word &optional parent-word)
  (format t "; ~a~%" (word-name word))
  (if (word-builtin? word)
      (funcall (word-builtin-fn word) vm word parent-word)
      (progn
        (vm/nest vm (word-code word))
        (loop
          :while (< (vm-ip vm) (length (vm-program vm)))
          :for w := (svref (vm-program vm) (vm-ip vm))
          :do (execute vm w word)))))

(defun interpret-1 (vm atom)
  (let ((w (vm/find vm atom)))
    (if (null w)
        (error 'uf/undefined-word)
        (execute vm w))))

(defun compile-1 (vm atom)
  (let ((w (vm/find vm atom)))
    (if (null w)
        (error 'uf/undefined-word)
        (if (word-immediate? w)
            (execute vm w)
            (push w (vm-compbuf vm))))))

(defun interpret (vm)
  (loop
    :for atom := (next-token (vm-stream vm))
    :until (null atom)
    :if (vm-comp? vm)
    :do (compile-1 vm atom)
    :else
    :do (interpret-1 vm atom)))

;;;;
;; built-in words

(defparameter *initial-word-list* nil)

(defmacro defword ((name immediate? data) exec-code &optional comp-code)
  (let (($vm (gensym "defw/vm")))
    `(flet ()
       (push (lambda (,$vm)
               (add-builtin-word ,$vm ,name ,immediate? ,data
                                 (lambda (vm word parent)
                                   (declare (ignorable vm word parent))
                                   (if (vm-comp? vm) ,comp-code ,exec-code))))
             uf::*initial-word-list*))))

;; Low level words

(defword ("vm/sem" t nil)
  (progn
    (format t "execution semantics!~%")
    (vm/next vm))
  (progn
    (format t "compilation semantics!~%")
    (vm/next vm)))

(defword ("vm/next" nil nil)
  (vm/next vm))

(defword ("vm/self" nil nil)
  (progn
    (stack-push parent (vm-pstack vm))
    (vm/next vm)))

(defword ("vm/nest" nil nil)
  (let ((w (stack-pop (vm-pstack vm))))
    (vm/nest vm (word-code w))
    (vm/next vm)))

(defword ("vm/unnest" nil nil)
  (vm/unnest vm))

(defword ("vm/create" nil nil)
  (progn
    (vm/create vm)
    (vm/next vm)))

(defword ("vm/name" nil nil)
  (progn
    (vm/name vm (next-token (vm-stream vm)))
    (vm/next vm)))

(defword ("vm/find" nil nil)
  (progn
    (stack-push (vm/find vm (read-to #\space (vm-stream vm)))
                (vm-pstack vm))
    (vm/next vm)))

(defword ("vm/clearcomp" t nil)
  (progn
    (setf (vm-compbuf vm) nil)
    (vm/next vm)))

(defword ("vm/termcomp" t nil)
  (progn
    (vm/terminate-compile vm)
    (vm/next vm)))

(defword ("execute" t nil)
  (progn
    (execute vm (stack-pop (vm-pstack vm)))
    (vm/next vm)))

(defword ("[" t nil)
  nil
  (progn
    (vm/interpret vm)
    (vm/next vm)))

(defword ("]" t nil)
  (progn
    (vm/compile vm)
    (vm/next vm)))

(defword ("immediate" nil nil)
  (progn
    (setf (word-immediate? (vm-dict vm)) t)
    (vm/next vm)))

(defword ("postpone" t nil)
  nil
  (progn
    (push (vm/find vm (next-token (vm-stream vm))) (vm-compbuf vm))
    (vm/next vm)))

(defword (".s" nil nil)
  (progn
    (format t "~a~%" (vm-pstack vm))
    (vm/next vm)))

;;;;
;; runtime

(defun init-vm (stream &optional (word-list uf::*initial-word-list*))
  (let ((vm (make-vm*)))
    (loop
      :for def :in (reverse word-list)
      :do (funcall def vm))
    (setf (vm-stream vm) stream)
    vm))
