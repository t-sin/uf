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
           #:execute
           #:init-vm))
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
(defstruct vm code ip dict stack rstack)
(defparameter *dictionary* nil)

(defmethod print-object ((word word) stream)
  (format stream "~a" (word-name word)))

(defmethod print-object ((vm vm) stream)
  (format stream "#<VM: ~s>" (vm-stack vm)))

(defun get-atom (vm)
  (prog1
      (nth (vm-ip vm) (vm-code vm))
    (incf (vm-ip vm))))

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

(defun execute (vm)
  (loop
    :for atom := (get-atom vm)
    :until (null atom)
    :do (cond ((eq atom 'uf/dict::|:|)
               (define-word vm))
              ((eq atom 'uf/dict::|;|)
               (if (null (vm-rstack vm))
                   (error "invalid syntax: reach ';' with empty rstack.")
                   (setf (vm-ip vm) (pop (vm-rstack vm)))))
              (t  (let ((word (find atom (vm-dict vm) :key #'word-name)))
                    (if word
                        (if (word-system-p word)
                            (funcall (word-fn word) vm)
                            (progn
                              (push (vm-ip vm) (vm-rstack vm))
                              (setf (vm-ip vm) (word-start word))))
                        (push atom (vm-stack vm))))))))

(defun init-vm (code)
  (let ((vm (make-vm :code code :ip 0)))
    (setf (vm-dict vm) *dictionary*)
    vm))
