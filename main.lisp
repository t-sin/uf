(defpackage #:uf
  (:use #:cl)
  (:import-from #:uf/kernel
                #:interpret)
  (:import-from #:uf/runtime
                #:*vm*)
  (:export #:interpret
           #:*vm*))
(in-package #:uf)
