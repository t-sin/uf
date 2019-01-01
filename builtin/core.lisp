(defpackage #:uf/builtin/core
  (:use #:cl #:uf/reader #:uf/stack #:uf/vm)
  (:import-from #:uf/builtin/define
                #:defword
                #:exec))
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

(defword ("recurse" t nil)
  nil
  (progn
    (push (vm-compiling vm) (vm-compbuf vm))
    (vm/next vm)))

(exec "vm/word vm/name create ] postpone vm/word postpone vm/name vm/next [ vm/termcomp")
(exec "vm/word vm/name : ] create postpone ] [ vm/termcomp")
(exec "vm/word vm/name ; ] postpone [ postpone vm/termcomp vm/next [ vm/termcomp immediate")
