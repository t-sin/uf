(defpackage #:uf/vm
  (:use #:cl)
  (:import-from #:uf/condition
                #:uf/error)
  (:import-from #:uf/stack
                #:make-stack
                #:stack-pop
                #:stack-push)
  (:export #:word
           #:word-prev
           #:word-name
           #:word-builtin?
           #:word-immediate?
           #:word-builtin-fn
           #:word-code
           #:word-data

           #:vm
           #:vm-stream
           #:vm-program
           #:vm-dict
           #:vm-ip
           #:vm-comp?
           #:vm-compbuf
           #:vm-pstack
           #:vm-rstack

           #:make-vm*
           #:add-builtin-word
           #:add-word

           #:vm/find
           #:vm/create
           #:vm/name
           #:vm/compile
           #:vm/interpret
           #:vm/terminate-compile
           #:vm/nest
           #:vm/unnest
           #:vm/next
           #:vm/execute

           #:interpret))
(in-package #:uf/vm)

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
    (unless (or (null (vm-program vm)) (null ip))
      (let ((next (1+ ip)))
        (if (< next (length (vm-program vm)))
            (setf (vm-ip vm) next)
            (vm/unnest vm))))))

(defun vm/execute (vm word &optional parent-word)
  (format t "; ~a~%" (word-name word))
  (if (word-builtin? word)
      (funcall (word-builtin-fn word) vm word parent-word)
      (progn
        (vm/nest vm (word-code word))
        (loop
          :while (< (vm-ip vm) (length (vm-program vm)))
          :for w := (svref (vm-program vm) (vm-ip vm))
          :do (execute vm w word)))))

;;;;
;; interpreter

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
