(defpackage #:uf/builtin/controll
  (:use #:cl #:uf/reader #:uf/stack #:uf/vm)
  (:import #:uf/builtin/define
           #:defword
           #:exec))
(in-package #:uf/builtin/controll)

(defword ("" nil nil)
  (progn
    ))