;; This software is Copyright (c) TANAKA Shinichi, 2018-10-08.
;; TANAKA Shinichi grants you the rights to distribute
;; and use this software as governed by the terms
;; of the Lisp Lesser GNU Public License
;; (http://opensource.franz.com/preamble.html),
;; known as the LLGPL.

(defpackage #:uf/runtime
  (:use #:cl #:uf/core))
(in-package #:uf/runtime)

(defmacro defword ((name) &body body)
  (let (($fn-name (intern (format nil "word:~a" (symbol-name name)) :uf/dict))
        ($word (gensym "uf")))
    `(progn
       (defun ,$fn-name (vm)
         (declare (ignorable vm))
         ,@body)
       (let ((,$word (find ',$fn-name uf/core:*dictionary* :key #'word-name)))
         (if ,$word
             (error "word ~s is already registered" (word-name ,$word))
             (push (make-word :name ',(intern (symbol-name name) :uf/dict)
                              :fn (function ,$fn-name) :system-p t) uf/core:*dictionary*))))))

;; I/O
(defword (|.|)
  (format t "~a" (pop (vm-stack vm))))
(defword (|cr|)
  (terpri))
(defword (|emit|)
  (format t "~a" (code-char (pop (vm-stack vm)))))
(defword (|.s|)
  (format t "~s" (vm-stack vm)))

;; stack maneuvers
(defword (|swap|)
  (let ((o1 (pop (vm-stack vm)))
        (o2 (pop (vm-stack vm))))
    (push o1 (vm-stack vm))
    (push o2 (vm-stack vm))))
(defword (|dup|)
  (let ((o (pop (vm-stack vm))))
    (push o (vm-stack vm))
    (push o (vm-stack vm))))
(defword (|over|)
  (let ((o1 (pop (vm-stack vm)))
        (o2 (pop (vm-stack vm))))
    (push o2 (vm-stack vm))
    (push o1 (vm-stack vm))
    (push o2 (vm-stack vm))))
(defword (|rot|)
  (let ((o1 (pop (vm-stack vm)))
        (o2 (pop (vm-stack vm)))
        (o3 (pop (vm-stack vm))))
    (push o1 (vm-stack vm))
    (push o2 (vm-stack vm))
    (push o3 (vm-stack vm))))
(defword (|drop|)
  (pop (vm-stack vm)))

;; arithmetic operation
(defword (|+|) (push (+ (pop (vm-stack vm)) (pop (vm-stack vm))) (vm-stack vm)))
(defword (|-|) (push (- (pop (vm-stack vm)) (pop (vm-stack vm))) (vm-stack vm)))
(defword (|*|) (push (* (pop (vm-stack vm)) (pop (vm-stack vm))) (vm-stack vm)))
(defword (|/|) (push (/ (pop (vm-stack vm)) (pop (vm-stack vm))) (vm-stack vm)))

;; arithmatic predicate
(defword (|=|) (if (= (pop (vm-stack vm)) (pop (vm-stack vm)))
                   (push -1 (vm-stack vm))
                   (push 0 (vm-stack vm))))
(defword (|<|) (if (< (pop (vm-stack vm)) (pop (vm-stack vm)))
                   (push -1 (vm-stack vm))
                   (push 0 (vm-stack vm))))

;; logical predicate
(defword (|or|) (if (or (= (pop (vm-stack vm)) -1)
                        (= (pop (vm-stack vm)) -1))
                    (push -1 (vm-stack vm))
                    (push 0 (vm-stack vm))))
(defword (|and|) (if (and (= (pop (vm-stack vm)) -1)
                          (= (pop (vm-stack vm)) -1))
                    (push -1 (vm-stack vm))
                    (push 0 (vm-stack vm))))
