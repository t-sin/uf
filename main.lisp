(defpackage #:tf
  (:use #:cl)
  (:import-from #:tf/core
                #:parse
                #:evaluate)
  (:import-from #:tf/runtime
                #:init-vm)
(:export #:parse
         #:evaluate))
(in-package #:tf)
