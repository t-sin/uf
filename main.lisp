(defpackage #:uf
  (:use #:cl)
  (:import-from #:uf/core
                #:parse
                #:execute
                #:init-vm)
  (:import-from #:uf/runtime)
(:export #:init-vm
         #:parse
         #:execute))
(in-package #:uf)
