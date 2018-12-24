(defpackage #:uf
  (:use #:cl)
  (:export))
(in-package #:uf)

(define-condition uf/error (simple-error) ())

;;;;
;; readers

(defvar +delimiters+ '(#\space #\tab #\newline #\page))

(defun skip-delimiters (stream &optional (num nil))
  (loop
    :for ch := (peek-char nil stream nil :eof)
    :for n := 0 :then (1+ n)
    :while (and (or (null num) (<= n num)) (member ch +delimiters+))
    :do (read-char stream)))

(defun next-token (stream)
  (let (buf)
    (skip-delimiters stream)
    (loop
      :for ch := (peek-char nil stream nil :eof)
      :until (or (eq ch :eof) (member ch +delimiters+ :test #'char=))
      :do (push (read-char stream) buf)
      :finally (unless (null buf)
                 (return-from next-token (coerce (nreverse buf) 'string))))))

(defun read-to (ch stream)
  (let (buf)
    (skip-delimiters stream 1)
    (loop
      :for ch* := (peek-char nil stream nil :eof)
      :if (or (eq ch* :eof) (char= ch* ch))
      :do (progn
            (unless (eq ch* :eof)
              (read-char stream))
            (return-from read-to (coerce (nreverse buf) 'string)))
      :else
      :do (push (read-char stream) buf))))

;;;;
;; stack

(defstruct stack
  vec ptr len)

(defmethod print-object ((stack stack) stream)
  (loop
    :for n :from 0 :below (stack-ptr stack)
    :initially (format stream "[")
    :do (format stream "~s " (svref (stack-vec stack) n))
    :finally (format stream "]")))

(define-condition uf/stack (uf/error) ())
(define-condition uf/stack/full (uf/stack) ())
(define-condition uf/stack/empty (uf/stack) ())

(defun make-stack* (size)
  (make-stack :vec (coerce (make-array size) 'simple-vector)
              :ptr 0 :len size))

(defun stack-pop (stack)
  (if (zerop (stack-ptr stack))
      (error 'uf/stack/empty :format-arguments "stack is empty!")
      (progn
        (decf (stack-ptr stack))
        (svref (stack-vec stack) (stack-ptr stack)))))

(defun stack-push (cell stack)
  (if (= (stack-ptr stack) (stack-len stack))
      (error 'uf/stack/full :format-arguments "stack is full!")
      (progn
        (setf (svref (stack-vec stack) (stack-ptr stack)) cell)
        (incf (stack-ptr stack)))))

;;;;
;; vm

(defvar +types+ '(:flag :char :number :xt :addr))
(defvar +stack-size+ 1000)

(define-condition  uf/undefined-word (uf/error) ())

(defstruct cell
  type data)

(defstruct word
  prev name builtin? immediate? builtin-fn code data)

(defmethod print-object ((word word) stream)
  (format stream "~a" (word-name word)))

(defstruct vm
  stream program dict ip comp? compbuf pstack rstack)

(defun make-vm* ()
  (make-vm :stream nil
           :program nil
           :dict (make-word)
           :pstack (make-stack* +stack-size+)
           :rstack (make-stack* +stack-size+)
           :ip nil
           :comp? nil))

(defun add-builtin-word (vm name immediate? data fn)
  (let ((w (make-word :name name
                      :builtin? t
                      :immediate? immediate?
                      :builtin-fn fn
                      :data data)))
    (setf (word-prev w) (vm-dict vm))
    (setf (vm-dict vm) w)))


(defun add-word (vm name immediate? data code)
  (let ((w (make-word :name name
                      :builtin? nil
                      :immediate? immediate?
                      :code code :data data)))
    (setf (word-prev w) (vm-dict vm))
    (setf (vm-dict vm) w)))

;;;;
;; vm instructions
(defun vm/find (vm name)
  (loop
    :for w := (vm-dict vm) :then (word-prev w)
    :until (eq w nil)
    :do (when (string= (word-name w) name)
          (return-from vm/find w))))

(defun vm/create (vm)
  (add-word vm :noname nil nil nil))

(defun vm/name (vm name)
  (setf (word-name (vm-dict vm)) name))

(defun vm/compile (vm)
  (setf (vm-comp? vm) t))

(defun vm/interpret (vm)
  (setf (vm-comp? vm) nil))

(defun vm/terminate-compile (vm)
  (setf (word-code (vm-dict vm)) (coerce (nreverse (vm-compbuf vm)) 'simple-vector)
        (vm-compbuf vm) nil))

(defun vm/nest (vm program)
  (if (null (vm-program vm))
      (stack-push nil (vm-rstack vm))
      (stack-push (cons (vm-program vm) (vm-ip vm)) (vm-rstack vm)))
  (setf (vm-program vm) program
        (vm-ip vm) 0))

(defun vm/unnest (vm)
  (let ((ip (stack-pop (vm-rstack vm))))
    (if (null ip)
        (setf (vm-program vm) nil
              (vm-ip vm) 0)
        (setf (vm-program vm) (car ip)
              (vm-ip vm) (cdr ip)))))

(defun vm/next (vm)
  (let ((ip (vm-ip vm)))
    (unless (null ip)
      (let ((next (1+ ip)))
        (if (< next (length (vm-program vm)))
            (setf (vm-ip vm) next)
            (vm/unnest vm))))))

;;;;
;; interpreter

(define-condition uf/empty-program (uf/error) ())

(defun execute (vm word)
  (format t "; ~a~%" (word-name word))
  (if (word-builtin? word)
      (funcall (word-builtin-fn word) vm word)
      (progn
        (vm/nest vm (word-code word))
        (loop
          :while (< (vm-ip vm) (length (vm-program vm)))
          :for w := (svref (vm-program vm) (vm-ip vm))
          :do (execute vm w))
        (vm/unnest vm))))

(defun interpret-1 (vm atom)
  (let ((w (vm/find vm atom)))
    (if (null w)
        (error 'uf/undefined-word)
        (execute vm w))))

(defun compile-1 (vm atom)
  (let ((w (vm/find vm atom)))
    (if (null w)
        (error 'uf/undefined-word)
        (if (word-immediate? w)
            (execute vm w)
            (push w (vm-compbuf vm))))))

(defun interpret (vm)
  (loop
    :for atom := (next-token (vm-stream vm))
    :until (null atom)
    :if (vm-comp? vm)
    :do (compile-1 vm atom)
    :else
    :do (interpret-1 vm atom)))

;;;;
;; built-in words

(defparameter *initial-word-list* nil)

(defmacro defword ((name immediate? data) exec-code &optional comp-code)
  (let (($vm (gensym "defw/vm")))
    `(flet ()
       (push (lambda (,$vm)
               (add-builtin-word ,$vm ,name ,immediate? ,data
                                 (lambda (vm word)
                                   (declare (ignorable vm word))
                                   (if (vm-comp? vm) ,comp-code ,exec-code))))
             uf::*initial-word-list*))))

;; Low level words

(defword ("vm/sem" t nil)
  (progn
    (format t "execution semantics!~%")
    (vm/next vm))
  (progn
    (format t "compilation semantics!~%")
    (vm/next vm)))

(defword ("vm/next" nil nil)
  (vm/next vm))

(defword ("vm/self" nil nil)
  (progn
    (stack-push word (vm-pstack vm))
    (vm/next vm)))

(defword ("vm/nest" nil nil)
  (let ((w (stack-pop (vm-pstack vm))))
    (vm/nest vm (word-code w))
    (vm/next vm)))

(defword ("vm/unnest" nil nil)
  (vm/unnest vm))

(defword ("vm/create" nil nil)
  (progn
    (vm/create vm)
    (vm/next vm)))

(defword ("vm/name" nil nil)
  (progn
    (vm/name vm (next-token (vm-stream vm)))
    (vm/next vm)))

(defword ("vm/find" nil nil)
  (progn
    (stack-push (vm/find vm (read-to #\space (vm-stream vm)))
                (vm-pstack vm))
    (vm/next vm)))

(defword ("vm/termcomp" t nil)
  (progn
    (vm/terminate-compile vm)
    (vm/next vm)))

(defword ("execute" t nil)
  (progn
    (execute vm (stack-pop (vm-pstack vm)))
    (vm/next vm)))

(defword ("[" t nil)
  nil
  (progn
    (vm/interpret vm)
    (vm/next vm)))

(defword ("]" t nil)
  (progn
    (vm/compile vm)
    (vm/next vm)))

(defword ("immediate" nil nil)
  (progn
    (setf (word-immediate? (vm-dict vm)) t)
    (vm/next vm)))

(defword ("postpone" t nil)
  nil
  (progn
    (push (vm/find vm (next-token (vm-stream vm))) (vm-compbuf vm))
    (vm/next vm)))

(defword (".s" nil nil)
  (progn
    (format t "~a~%" (vm-pstack vm))
    (vm/next vm)))

;;;;
;; runtime

(defun init-vm (stream &optional (word-list uf::*initial-word-list*))
  (let ((vm (make-vm*)))
    (loop
      :for def :in (reverse word-list)
      :do (funcall def vm))
    (setf (vm-stream vm) stream)
    vm))
