;; This software is Copyright (c) TANAKA Shinichi, 2018-10-08.
;; TANAKA Shinichi grants you the rights to distribute
;; and use this software as governed by the terms
;; of the Lisp Lesser GNU Public License
;; (http://opensource.franz.com/preamble.html),
;; known as the LLGPL.

(defpackage #:uf/core
  (:use #:cl)
  (:export #:make-word
           #:word-name
           #:word-code

           #:make-vm
           #:vm-dict
           #:vm-stack

           #:*dictionary*
           #:defword
           #:parse
           #:execute))
(in-package #:uf/core)

(defpackage #:uf/dict)

(defun parse (stream)
  (let (code buf atomp numberp)
    (flet ((read-atom (ch)
             (unless atomp
               (setf atomp t)
               (when (digit-char-p ch)
                 (setf numberp t))
               (setf buf nil))
             (unless (digit-char-p ch)
               (setf numberp nil))
             (push ch buf))
           (terminate-atom ()
             (when atomp
               (setf atomp nil)
               (let ((s (concatenate 'string (nreverse buf))))
                 (if numberp
                     (push (parse-integer s) code)
                     (push (intern s :uf/dict) code))))))
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

(defstruct word name fn start system-p)
(defstruct vm code ip dict stack rstack ifstack skip-p debug-p)
(defparameter *dictionary* nil)

(defmethod print-object ((word word) stream)
  (format stream "~a" (word-name word)))

(defmethod print-object ((vm vm) stream)
  (format stream "#<VM: ~s>" (vm-stack vm)))

(defun get-atom (vm)
  (prog1
      (nth (vm-ip vm) (vm-code vm))
    (incf (vm-ip vm))))

;; this may be an compilation state ...?
(defun define-word (vm)
  (let ((name (get-atom vm)))
    (when (null name)
      (error "invalid word definition : it doesn't have a name."))
    (let ((start-pos (vm-ip vm)))
      (loop
        :for atom := (get-atom vm)
        :until (eq atom 'uf/dict::|;|)
        :do (when (null atom)
              (error "invalid word definition '~a': it doesn't have ';'." name)))
      (let ((word (make-word :name name :system-p nil :start start-pos)))
        (let ((w (find name (vm-dict vm) :key #'word-name)))
          (if (and (not (null w)) (word-system-p w))
              (error "cannot overwrite the predefined word: ~s" name)
              (push word (vm-dict vm))))))))

;; this may be an interpretation state ...?
(defun execute (vm)
  (loop
    :for atom := (get-atom vm)
    :until (null atom)
    :do (when (vm-debug-p vm)
          (format t "; name = '~a', stack = ~s~%" atom (vm-stack vm)))
    :if (vm-skip-p vm)
    :do (cond ((eq atom 'uf/dict::|if|)
               (progn
                 (setf (vm-skip-p vm) nil)
                 (decf (vm-ip vm))))
              ((eq atom 'uf/dict::|else|)
               (if (null (vm-ifstack vm))
                   (error "unexpected `else`")
                   (if (eq (first (vm-ifstack vm)) :false)
                       (setf (vm-skip-p vm) nil)
                       (error "unexpected `else`"))))
              ((eq atom 'uf/dict::|then|)
               (if (null (vm-ifstack vm))
                   (error "unexpected `then`")
                   (pop (vm-ifstack vm)))))
    :else
    :do (cond ((eq atom 'uf/dict::|:|)
               (define-word vm))
              ((eq atom 'uf/dict::|;|)
               (if (null (vm-rstack vm))
                   (error "invalid syntax: reach ';' with empty rstack.")
                   (setf (vm-ip vm) (pop (vm-rstack vm)))))
              ((eq atom 'uf/dict::|if|)
               (if (= (pop (vm-stack vm)) -1)
                   (push :true (vm-ifstack vm))
                   (progn
                     (setf (vm-skip-p vm) t)
                     (push :false (vm-ifstack vm)))))
              ((eq atom 'uf/dict::|else|)
               (if (null (vm-ifstack vm))
                   (error "unexpected `else`")
                   (if (eq (first (vm-ifstack vm)) :true)
                       (setf (vm-skip-p vm) t)
                       (error "unexpected `else`"))))
              ((eq atom 'uf/dict::|then|)
               (if (null (vm-ifstack vm))
                   (error "unexpected `else`")
                   (pop (vm-ifstack vm))))
              (t (let ((word (find atom (vm-dict vm) :key #'word-name)))
                   (if word
                       (if (word-system-p word)
                           (funcall (word-fn word) vm)
                           (progn
                             (push (vm-ip vm) (vm-rstack vm))
                             (setf (vm-ip vm) (word-start word))))
                       (push atom (vm-stack vm))))))))
