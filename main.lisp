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
