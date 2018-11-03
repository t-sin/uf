(defpackage #:uf/kernel
  (:use #:cl)
  (:export #:cell))
(in-package #:uf/kernel)

(defvar types '(flag char number xt addr))

;;; | type (8bit) | data (32bit) |
(defstruct cell
  type data)

(defun bytes->cell (bytes)
  (if (and (typep bytes '(array (unsigned-byte 8) (#.(+ 1 4))))
           (= (length bytes) #.(+ 1 4)))
      (let ((cell (make-cell :type (aref bytes 0)
                             :data (subseq bytes 1 5))))
        cell)
      (error "it's not a byte representation of cell")))

(defun cell->bytes (cell)
  (if (cell-p cell)
      (make-array 5 :element-type '(unsigned-byte 5)
                  :initial-contents (concatenate '(array (unsigned-byte 8) (#.(+ 1 4)))
                                                 (list (cell-type cell)) (cell-data cell)))
      (error "it's not a cell")))

(defstruct word
  prev name code fn data immediate builtin)

(defstruct vm
  ip compiling dict pstack rstack cstack)

(defun make-stack (size)
  (let ((stack (make-array size :element-type 'cell))
        (top 0))
    (lambda (method &optional val)
      (case method
        (:push (if (< size (1+ top))
                   (values nil :exausted)
                   (progn
                     (setf (svref stack top) val)
                     (incf top))))
        (:pop (progn
                (decf top)
                (cond ((< top 0) (progn
                                   (setf top 0)
                                   (values nil :empty)))
                      (t (values (svref stack top) t)))))))))

(defvar *pstack-size* 1000)
(defvar *rstack-size* 1000)
(defvar *cstack-size* 1000)

(defun init-vm ()
  (let* ((vm (make-vm :ip nil
                      :compiling nil
                      :dict (make-word :prev nil
                                       :name nil
                                       :code nil
                                       :data nil
                                       :immediate nil)
                      :pstack (make-stack *pstack-size*)
                      :rstack (make-stack *rstack-size*)
                      :cstack (make-stack *cstack-size*))))
    vm))

(defun add-word (vm name code data immediate)
  (let ((word (make-word :name name
                         :code code
                         :data data
                         :immediate immediate)))
    (setf (word-prev word) (vm-dict vm))
    (setf (vm-dict vm) word)))
