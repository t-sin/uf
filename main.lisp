(defpackage #:tf
  (:use #:cl)
  (:import-from #:tf/core
                #:parse
                #:evaluate)
  (:export #:parse
           #:evaluate))
(in-package #:tf)
