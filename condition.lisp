(defpackage #:uf/condition
  (:use #:cl)
  (:export #:uf/error))
(in-package #:uf/condition)

(define-condition uf/error (simple-error) ())
