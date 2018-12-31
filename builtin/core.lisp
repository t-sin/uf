(defpackage #:uf/builtin/core
  (:use #:cl #:uf/reader #:uf/stack #:uf/vm)
  (:import-from #:uf/builtin/define
                #:defword))
(in-package #:uf/builtin/core)

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
