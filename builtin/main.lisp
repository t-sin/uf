(defpackage #:uf/builtin/main
  (:use #:cl
        #:uf/builtin/vm
        #:uf/builtin/core
        #:uf/builtin/stack
        #:uf/builtin/io)
  (:import-from #:uf/builtin/define
                #:*initial-word-list*)
  (:export #:*initial-word-list*))
(in-package #:uf/builtin/main)
