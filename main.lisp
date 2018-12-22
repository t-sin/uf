(defpackage #:uf
  (:use #:cl)
  (:export))
(in-package #:uf)

(defvar +types+ '(:flag :char :number :xt :addr))

(defstruct cell
  type data)

(defstruct word
  prev name builtin? immediate? builtin-fn code data)

(defstruct vm
  program dict ip comp? dstack rstack)

(defvar +delimiters+ '(#\space #\tab #\newline #\page))

(defun skip-delimiters (stream)
  (loop
    :for ch := (peek-char nil stream nil :eof)
    :while (member ch +delimiters+)
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
