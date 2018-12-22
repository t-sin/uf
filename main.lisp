(defpackage #:uf
  (:use #:cl)
  (:export))
(in-package #:uf)

(defclass uf/error (simple-error) ())

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
      :if (or (eq ch :eof) (member ch +delimiters+ :test #'char=))
      :do (return-from next-token (coerce (nreverse buf) 'string))
      :else
      :do (push (read-char stream) buf))))

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

(defstruct vm
  program dict ip comp? compbuf dstack rstack)

(defun make-vm* ()
  (make-vm :program nil
           :dict (make-word)
           :dstack (make-stack* +stack-size+)
           :rstack (make-stack* +stack-size+)
           :ip nil
           :comp? nil))

(defun add-builtin-word (vm name immediate? fn data)
  (let ((w (make-word :name name
                      :builtin? t
                      :immediate? immediate?
                      :builtin-fn fn
                      :data data)))
    (setf (word-prev w) (vm-dict vm))
    (setf (vm-dict vm) w)))

(defun add-word (vm name immediate? code data)
  (let ((w (make-word :name name
                      :builtin? nil
                      :immediate? immediate?
                      :code code :data data)))
    (setf (word-prev w) (vm-dict vm))
    (setf (vm-dict vm) w)))

(defun find-word (vm name)
  (loop
    :for w := (vm-dict vm) :then (word-prev w)
    :until (eq w nil)
    :do (when (string= (word-name w) name)
          (return-from find-word w))))

;;;;
;; interpreter

(defun execute-word (vm word)
  (if (word-builtin? word)
      (funcall (word-builtin-fn word) vm word)
      (progn
        (stack-push (cons (vm-program vm) (vm-ip vm)) (vm-rstack vm))
        (setf (vm-program vm) (word-code word)
              (vm-ip vm) 0))))

(defun interpret-1 (vm atom)
  (if (word-p atom)
      (execute-word vm atom)
      (let ((w (find-word vm atom)))
        (if (null w)
            (error 'uf/undefined-word)
            (execute-word vm w)))))

(defun compile-1 (vm atom)
  (let ((w (find-word vm atom)))
    (if (null w)
        (error 'uf/undefined-word)
        (if (word-immediate? w)
            (execute-word vm w)
            (push w (vm-compbuf vm))))))

(defun interpret (vm)
  (loop
    :while (and (>= (vm-ip vm) 0) (< (vm-ip vm) (length (vm-program vm))))
    :for atom := (svref (vm-program vm) (vm-ip vm))
    :if (vm-comp? vm)
    :do (compile-1 vm atom)
    :else
    :do (interpret-1 vm atom)))
