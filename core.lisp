(defpackage #:uf/core
  (:use #:cl)
  (:export #:make-word
           #:word-name
           #:word-code

           #:make-vm
           #:vm-dict
           #:vm-stack

           #:parse
           #:evaluate-atom
           #:evaluate))
(in-package #:uf/core)

(defpackage #:uf/dict)

(defun parse (stream)
  (let (code buf atomp)
    (flet ((read-atom (ch)
             (unless atomp
               (setf atomp t)
               (setf buf nil))
             (push ch buf))
           (terminate-atom ()
             (when atomp
               (setf atomp nil)
               (push (intern (concatenate 'string (nreverse buf)) :uf/dict) code))))
      (loop
        :for ch := (read-char stream nil :eof)
        :until (eq ch :eof)
        :do (case ch
              (#\space (terminate-atom))
              (#\newline (terminate-atom))
              (t (read-atom ch)))
        :finally (progn
                   (terminate-atom)
                   (return (nreverse code)))))))

(defstruct word name code)
(defstruct vm dict stack)

(defmethod print-object ((word word) stream)
  (format stream "~a" (word-name word)))

(defmethod print-object ((vm vm) stream)
  (format stream "#<VM: ~s>" (vm-stack vm)))

(defun evaluate-atom (atom vm)
  (let ((word (find atom (vm-dict vm) :key #'word-name)))
    (if word
        (funcall (word-code word) vm)
        (push atom (vm-stack vm)))))

(defun evaluate (code vm)
  (loop
    :for atom :in code
    :do (evaluate-atom atom vm)))
