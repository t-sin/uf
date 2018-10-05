(defpackage #:uf
  (:use #:cl)
  (:import-from #:uf/core
                #:parse
                #:execute*)
  (:import-from #:uf/runtime
                #:init-vm)
(:export #:parse
         #:execute*))
(in-package #:uf)
