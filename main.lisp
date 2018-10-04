(defpackage #:uf
  (:use #:cl)
  (:import-from #:uf/core
                #:parse
                #:evaluate)
  (:import-from #:uf/runtime
                #:init-vm)
(:export #:parse
         #:evaluate))
(in-package #:uf)
