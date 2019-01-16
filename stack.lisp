(defpackage #:uf/stack
  (:use #:cl)
  (:import-from #:uf/condition
                #:uf/error)
  (:export #:stack
           #:stack-vec
           #:stack-ptr
           #:stack-len

           #:uf/stack
           #:uf/stack/full
           #:uf/stack/empty

           #:make-stack*
           #:stack-top
           #:stack-pop
           #:stack-push))
(in-package #:uf/stack)

(defstruct stack
  vec ptr len)

(defmethod print-object ((stack stack) stream)
  (loop
    :for n :from 0 :below (stack-ptr stack)
    :initially (format stream "[")
    :do (format stream "~s" (svref (stack-vec stack) n))
    :do (when (< n (1- (stack-ptr stack))) (format stream " "))
    :finally (format stream "]")))

(define-condition uf/stack (uf/error) ())
(define-condition uf/stack/full (uf/stack) ())
(define-condition uf/stack/empty (uf/stack) ())

(defun make-stack* (size)
  (make-stack :vec (coerce (make-array size) 'simple-vector)
              :ptr 0 :len size))

(defun stack-top (stack)
  (if (zerop (stack-ptr stack))
      (error 'uf/stack/empty :format-arguments "stack is empty!")
      (svref (stack-vec stack) (1- (stack-ptr stack)))))

(defun stack-pop (stack)
  (if (zerop (stack-ptr stack))
      (error 'uf/stack/empty :format-arguments "stack is empty!")
      (progn
        (decf (stack-ptr stack))
        (svref (stack-vec stack) (stack-ptr stack)))))

(defun stack-push (cell stack)
  (if (= (stack-ptr stack) (stack-len stack))
      (error 'uf/stack/full :format-arguments "stack is full!")
      (progn
        (setf (svref (stack-vec stack) (stack-ptr stack)) cell)
        (incf (stack-ptr stack)))))
