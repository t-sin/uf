(defpackage #:uf/builtin/vm
  (:use #:cl #:uf/reader #:uf/stack #:uf/vm)
  (:import-from #:uf/builtin/define
                #:defword))
(in-package #:uf/builtin/vm)

(defword ("vm/nest" nil nil)
  (let ((w (stack-pop (vm-pstack vm))))
    (vm/nest vm (word-ecode w))
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
