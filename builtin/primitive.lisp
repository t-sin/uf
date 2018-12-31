(defpackage #:uf/builtin
  (:use #:cl #:uf/reader #:uf/stack #:uf/vm)
  (:import-from #:uf/builtin/define
                #:defword))
(in-package #:uf/builtin)
;; VM

;; I/O

