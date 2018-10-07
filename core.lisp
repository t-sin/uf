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

(defstruct word name code system-p)
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
  (let* ((name (get-atom vm))
         (start-pos (vm-ip vm))
         (code (loop
                 :named collect-code
                 :for atom := (get-atom vm)
                 :with %code := nil
                 :until (eq atom 'uf/dict::|;|)
                 :do (if (null atom)
                         (return-from collect-code nil)
                         (push atom %code))
                 :finally (return-from collect-code (nreverse %code))))
         (end-pos (vm-ip vm)))
    (if (or (null name) (null code))
        (error "malformed word definition!")
        (let* ((word-fn #'(lambda (vm)
                            (let ((vm* (init-vm code)))
                              (setf (vm-stack vm*) (vm-stack vm)
                                    (vm-dict vm*) (vm-dict vm))
                              (execute vm*)
                              (setf (vm-stack vm) (vm-stack vm*)))))
               (word (make-word :name name :code word-fn)))
          (let ((w (find name (vm-dict vm) :key #'word-name)))
            (if (and (not (null w)) (word-system-p w))
                (error "cannot overwrite the predefined word: ~s" name)
                (push word (vm-dict vm))))))))

(defun execute (vm)
  (loop
    :for atom := (get-atom vm)
    :until (null atom)
    :do (if (eq atom 'uf/dict::|:|)
            (define-word vm)
            (let ((word (find atom (vm-dict vm) :key #'word-name)))
              (if word
                  (funcall (word-code word) vm)
                  (push atom (vm-stack vm)))))))

(defun init-vm (code)
  (let ((vm (make-vm :code code :ip 0)))
    (setf (vm-dict vm) *dictionary*)
    vm))
