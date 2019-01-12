(defpackage #:uf/builtin/stack
  (:use #:cl #:uf/reader #:uf/stack #:uf/vm)
  (:import-from #:uf/builtin/define
                #:defword
                #:exec))
(in-package #:uf/builtin/stack)

(defword ("dup" nil nil)
  (progn
    (stack-push (stack-top (vm-pstack vm)) (vm-pstack vm))
    (vm/next vm)))

(defword ("drop" nil nil)
  (progn
    (stack-pop (vm-pstack vm))
    (vm/next vm)))

(defword ("swap" nil nil)
  (let* ((pstack (vm-pstack vm))
         (ptr (stack-ptr pstack)))
    (if (< ptr 2)
        (error "stack length is less than 2. cannot swap.")
        (let ((o1 (svref (stack-vec pstack) (- ptr 1))))
          (setf (svref (stack-vec pstack) (- ptr 1)) (svref (stack-vec pstack) (- ptr 2))
                (svref (stack-vec pstack) (- ptr 2)) o1)))
    (vm/next vm)))

(defword ("over" nil nil)
  (let* ((pstack (vm-pstack vm))
         (ptr (stack-ptr pstack)))
    (if (< ptr 2)
        (error "stack length is less than 2. cannot over.")
        (let ((o (svref (stack-vec pstack) (- ptr 2))))
          (stack-push o pstack)))
    (vm/next vm)))

(defword ("rot" nil nil)
  (let* ((pstack (vm-pstack vm))
         (ptr (stack-ptr pstack)))
    (if (< ptr 3)
        (error "stack length is less than 3. cannot rot.")
        (let ((o1 (svref (stack-vec pstack) (- ptr 1)))
              (o2 (svref (stack-vec pstack) (- ptr 2))))
          (setf (svref (stack-vec pstack) (- ptr 1)) (svref (stack-vec pstack) (- ptr 3))
                (svref (stack-vec pstack) (- ptr 2)) o1
                (svref (stack-vec pstack) (- ptr 3)) o2)))
    (vm/next vm)))
