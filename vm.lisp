(defpackage #:uf/vm
  (:use #:cl #:uf/reader)
  (:import-from #:uf/condition
                #:uf/error)
  (:import-from #:uf/stack
                #:make-stack*
                #:stack-pop
                #:stack-push)
  (:import-from #:uf/number
                #:to-number)
  (:export #:cell
           #:cell-type
           #:+types+

           #:num-n

           #:word
           #:word-prev
           #:word-name
           #:word-builtin?
           #:word-immediate?
           #:word-ifn
           #:word-cfn
           #:word-efn
           #:word-icode
           #:word-ccode
           #:word-ecode
           #:word-data

           #:vm
           #:vm-stream
           #:vm-program
           #:vm-dict
           #:vm-ip
           #:vm-comp?
           #:vm-compiling
           #:vm-compbuf
           #:vm-pstack
           #:vm-rstack
           #:vm-cstack

           #:make-vm*
           #:add-builtin-word
           #:add-word
           #:print-log

           #:vm/find
           #:vm/word
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

(defvar +types+ '(:nil :flag :char :number :xt :addr))
(defvar +stack-size+ 1000)

(define-condition  uf/undefined-word (uf/error) ())

(defstruct cell type)

(defstruct (num (:include cell)) n)
(defmethod print-object ((num num) stream)
  (format stream "~a" (num-n num)))

(defstruct (word (:include cell))
  prev name builtin? immediate? ifn cfn efn icode ccode ecode data)

(defmethod print-object ((word word) stream)
  (format stream "~a" (word-name word)))

(defstruct vm
  stream program dict ip comp? compiling compbuf pstack rstack cstack debug)

(defun make-vm* (debug)
  (make-vm :stream nil
           :program nil
           :dict (make-word)
           :pstack (make-stack* +stack-size+)
           :rstack (make-stack* +stack-size+)
           :cstack (make-stack* +stack-size+)
           :ip nil
           :comp? nil
           :debug debug))

(defun add-builtin-word (vm name immediate? data ifn cfn efn)
  (let ((w (make-word :name name
                      :type :xt
                      :builtin? t
                      :immediate? immediate?
                      :ifn ifn :cfn cfn :efn efn
                      :data (make-cell :type :nil))))
    (setf (word-prev w) (vm-dict vm))
    (setf (vm-dict vm) w)
    w))

(defun add-word (vm name immediate? data icode ccode ecode)
  (let ((w (make-word :name name
                      :type :xt
                      :builtin? nil
                      :immediate? immediate?
                      :icode icode :ccode ccode :ecode ecode
                      :data (make-cell :type :nil))))
    (setf (word-prev w) (vm-dict vm))
    (setf (vm-dict vm) w)
    w))

(defun print-log (vm fmt &rest args)
  (when (vm-debug vm)
    (apply #'format t fmt args)))

;; vm instructions

(defun vm/find (vm name)
  (loop
    :for w := (vm-dict vm) :then (word-prev w)
    :until (eq w nil)
    :do (when (string= (word-name w) name)
          (return-from vm/find w))))

(defun vm/compile (vm)
  (setf (vm-comp? vm) t
        (vm-compiling vm) (vm-dict vm)))

(defun vm/interpret (vm)
  (setf (vm-comp? vm) nil))

(defun vm/terminate-compile (vm)
  (setf (word-ecode (vm-compiling vm)) (coerce (nreverse (vm-compbuf vm)) 'simple-vector)
        (vm-compiling vm) nil
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
    (unless (null (vm-program vm))
      (let ((next (1+ ip)))
        (if (< next (length (vm-program vm)))
            (setf (vm-ip vm) next)
            (progn
              (vm/unnest vm)
              (incf (vm-ip vm))))))))

(flet ((nest (vm word parent)
         (declare (ignore parent))
         (vm/nest vm (word-ecode word))))
  (defun vm/word (vm)
    (let ((w (add-word vm :noname nil nil nil nil nil)))
      (setf (word-ifn w) #'nest)
      w)))

(defun vm/name (vm name)
  (setf (word-name (vm-dict vm)) name))

(defun vm/lit (vm type data)
  (flet ((init-fn (vm w p)
           (declare (ignorable vm w p))
           (stack-push (word-data w) (vm-pstack vm))
           (vm/next vm)))
    (let ((w (vm/word vm)))
      (setf (word-data w) (make-num :type type :n data)
            (word-ifn w) #'init-fn)
      w)))

(defun vm/execute (vm word &optional parent-word)
  (print-log vm "; ~a~%" (word-name word))
  (let ((ifn (word-ifn word)))
    (if ifn
        (funcall ifn vm word parent-word)
        ;; it's maybe a bug
        (vm/execute vm (word-icode word) parent-word)))
  (if (word-builtin? word)
      (if (vm-comp? vm)
          (funcall (word-cfn word) vm word parent-word)
          (funcall (word-efn word) vm word parent-word))
      (loop
        :while (< (vm-ip vm) (length (vm-program vm)))
        :for w := (svref (vm-program vm) (vm-ip vm))
        :do (vm/execute vm w word))))

;; TODO: make words cells
(defun vm/@ (vm cell)
  (let ((value (cell-data cell)))
    (stack-push value (vm-pstack vm))))

;;;;
;; interpreter

(defun interpret-1 (vm atom)
  (let ((w (vm/find vm atom)))
    (if (null w)
        (let ((n (to-number atom)))
          (if (null n)
              (error 'uf/undefined-word)
              (stack-push (make-num :type :number :n n)
                          (vm-pstack vm))))
        (vm/execute vm w))))

(defun compile-1 (vm atom)
  (let ((w (vm/find vm atom)))
    (if (null w)
        (let ((n (to-number atom)))
          (if (null n)
              (error 'uf/undefined-word)
              (push (vm/lit vm :number n) (vm-compbuf vm))))
        (if (word-immediate? w)
            (vm/execute vm w)
            (push w (vm-compbuf vm))))))

(defun interpret (vm)
  (loop
    :for atom := (next-token (vm-stream vm))
    :until (null atom)
    :if (vm-comp? vm)
    :do (compile-1 vm atom)
    :else
    :do (interpret-1 vm atom)))
