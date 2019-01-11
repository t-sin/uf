(defpackage #:uf/builtin/math
  (:use #:cl #:uf/reader #:uf/stack #:uf/vm)
  (:import-from #:uf/builtin/define
                #:defword
                #:exec))
(in-package #:uf/builtin/math)

(defword ("+" nil nil)
  (let* ((pstack (vm-pstack vm))
         (n2 (stack-pop pstack))
         (n1 (stack-pop pstack)))
    (unless (eq (cell-type n1) :number)
      (error "~s is not a number" n1))
    (unless (eq (cell-type n2) :number)
      (error "~s is not a number" n2))
    (stack-push (make-num :n (+ (num-n n1) (num-n n2))) pstack)))

(defword ("-" nil nil)
  (let* ((pstack (vm-pstack vm))
         (n2 (stack-pop pstack))
         (n1 (stack-pop pstack)))
    (unless (eq (cell-type n1) :number)
      (error "~s is not a number" n1))
    (unless (eq (cell-type n2) :number)
      (error "~s is not a number" n2))
    (stack-push (make-num :n (- (num-n n1) (num-n n2))) pstack)))
