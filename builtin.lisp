(defpackage #:uf/builtin
  (:use #:cl #:uf/reader #:uf/stack #:uf/vm)
  (:export #:*initial-word-list*))
(in-package #:uf/builtin)

(defparameter *initial-word-list* nil)

(defmacro defword ((name immediate? data) exec-code &optional comp-code init-code)
  (let (($vm (gensym "defw/vm")))
    `(push (lambda (,$vm)
             (add-builtin-word ,$vm ,name ,immediate? ,data
                               (lambda (vm word parent)
                                 (declare (ignorable vm word parent)) ,init-code)
                               (lambda (vm word parent)
                                 (declare (ignorable vm word parent)) ,comp-code)
                               (lambda (vm word parent)
                                 (declare (ignorable vm word parent)) ,exec-code)))
           uf/builtin:*initial-word-list*)))

;; Low level words

(defword ("vm/nest" nil nil)
  (let ((w (stack-pop (vm-pstack vm))))
    (vm/nest vm (word-code w))
    (vm/next vm))
  nil
  (vm/nest vm (word-program word)))

(defword ("vm/unnest" nil nil)
  (vm/unnest vm))

(defword ("vm/next" nil nil)
  (vm/next vm))

(defword ("vm/sem" t nil)
  (progn
    (format t "execution semantics!~%")
    (vm/next vm))
  (progn
    (format t "compilation semantics!~%")
    (vm/next vm)))

(defword ("vm/self" nil nil)
  (progn
    (stack-push parent (vm-pstack vm))
    (vm/next vm)))

(defword ("vm/word" nil nil)
  (progn
    (vm/word vm)
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

;; VM

(defword ("execute" t nil)
  (progn
    (vm/execute vm (stack-pop (vm-pstack vm)))
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

;; this is equal to `vm/word vm/name NAME`...
(defword ("create" nil nil)
  (let ((name (next-token (vm-stream vm))))
    (setf (word-name (vm/word vm)) name)
    (vm/next vm)))

(defword ("does>" t nil)
  nil
  (let ((w (vm-dict vm)))
    (flet ((init-fn (vm word parent)
             (declare (ignorable vm word parent))
             (vm/nest vm (word-ecode w))
             (stack-push (word-data w) (vm-pstack vm))))
      (setf (vm-compbuf vm) nil
            (word-ifn w) #'init-fn))))

(defword ("immediate" nil nil)
  (progn
    (setf (word-immediate? (vm-dict vm)) t)
    (vm/next vm)))

(defword ("postpone" t nil)
  nil
  (progn
    (push (vm/find vm (next-token (vm-stream vm))) (vm-compbuf vm))
    (vm/next vm)))

;; I/O

(defword (".s" nil nil)
  (progn
    (format t "~a~%" (vm-pstack vm))
    (vm/next vm)))
