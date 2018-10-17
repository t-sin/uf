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
(defstruct vm code ip dict stack rstack ifdepth skip-to debug-p)
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
          (format t "; name = '~a', stack = ~s, ifdepth = ~s, skip-to = ~s~%"
                  atom (vm-stack vm) (vm-ifdepth vm) (vm-skip-to vm)))
    :if (not (null (vm-skip-to vm)))
    :do (cond ((eq atom 'uf/dict::|if|)
               (incf (vm-ifdepth vm)))
              ((eq atom 'uf/dict::|else|)
               (cond ((zerop (vm-ifdepth vm)) (error "unexpected `else`"))
                     ((and (eq (car (vm-skip-to vm)) :false)
                           (= (1+ (cdr (vm-skip-to vm))) (vm-ifdepth vm)))
                      (setf (vm-skip-to vm) nil))))
              ((eq atom 'uf/dict::|then|)
               (if (zerop (vm-ifdepth vm))
                   (error "unexpected `then`")
                   (progn
                     (when (= (vm-ifdepth vm) (cdr (vm-skip-to vm)))
                       (setf (vm-skip-to vm) nil))
                     (decf (vm-ifdepth vm))))))
    :else
    :do (cond ((eq atom 'uf/dict::|:|)
               (define-word vm))
              ((eq atom 'uf/dict::|;|)
               (if (null (vm-rstack vm))
                   (error "invalid syntax: reach ';' with empty rstack.")
                   (setf (vm-ip vm) (pop (vm-rstack vm)))))
              ((eq atom 'uf/dict::|if|)
               (unless (= (pop (vm-stack vm)) -1)
                 (setf (vm-skip-to vm) (cons :false (vm-ifdepth vm))))
               (incf (vm-ifdepth vm)))
              ((eq atom 'uf/dict::|else|)
               (cond ((zerop (vm-ifdepth vm)) (error "unexpected `else`"))
                     ((null (vm-skip-to vm)) (setf (vm-skip-to vm) (cons :true (vm-ifdepth vm))))))
              ((eq atom 'uf/dict::|then|)
               (if (zerop (vm-ifdepth vm))
                   (error "unexpected `else`")
                   (progn
                     (setf (vm-skip-to vm) nil)
                     (decf (vm-ifdepth vm)))))
              (t (let ((word (find atom (vm-dict vm) :key #'word-name)))
                   (if word
                       (if (word-system-p word)
                           (funcall (word-fn word) vm)
                           (progn
                             (push (vm-ip vm) (vm-rstack vm))
                             (setf (vm-ip vm) (word-start word))))
                       (push atom (vm-stack vm))))))))
