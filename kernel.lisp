(defpackage #:uf/kernel
  (:use #:cl)
  (:export #:vm-program
           #:vm-ip
           #:vm-compiling
           #:vm-dict
           #:vm-pstack
           #:vm-rstack
           #:vm-cstack
           #:vm

           #:init-vm
           #:add-word
           #:add-builtin-word
           #:find-word

           #:interpret
           #:with-initial-vm
           #:define-word))
(in-package #:uf/kernel)

(defvar types '(flag char number xt addr))

;;; | type (8bit) | data (32bit) |
(defstruct cell
  type data)

;; (defun bytes->cell (bytes)
;;   (if (and (typep bytes '(array (unsigned-byte 8) (#.(+ 1 4))))
;;            (= (length bytes) #.(+ 1 4)))
;;       (let ((cell (make-cell :type (aref bytes 0)
;;                              :data (subseq bytes 1 5))))
;;         cell)
;;       (error "it's not a byte representation of cell")))

;; (defun cell->bytes (cell)
;;   (if (cell-p cell)
;;       (make-array 5 :element-type '(unsigned-byte 5)
;;                   :initial-contents (concatenate '(array (unsigned-byte 8) (#.(+ 1 4)))
;;                                                  (list (cell-type cell)) (cell-data cell)))
;;       (error "it's not a cell")))

(defstruct word
  prev name code fn data immediate builtin)

(defstruct vm
  program ip compiling dict pstack rstack cstack)

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
  (let* ((vm (make-vm :program nil :ip nil
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

(defun add-builtin-word (vm name fn data immediate)
  (let ((word (make-word :name name
                         :fn fn
                         :data data
                         :immediate immediate
                         :builtin t)))
    (setf (word-prev word) (vm-dict vm))
    (setf (vm-dict vm) word)))

(defun find-word (vm name)
  (loop
    :for w := (vm-dict vm) :then (word-prev w)
    :until (eql w nil)
    :do (when (eql (word-name w) name)
          (return-from find-word w))))

(defun interpret (vm program)
  (setf (vm-ip vm) 0
        (vm-program vm) program)
  (flet ((execute-word (w)
           (if (word-builtin w)
               (funcall (word-fn w) vm)
               (progn
                 (funcall (vm-rstack vm)
                          :push (cons (vm-ip vm) (vm-program vm)))
                 (setf (vm-program vm) (word-code w)
                       (vm-ip vm) 0)))))
    (loop
      :while (and (>= (vm-ip vm) 0) (< (vm-ip vm) (length (vm-program vm))))
      :for atom := (aref (vm-program vm) (vm-ip vm))
      :if (vm-compiling vm)
      :do
         (progn)
      :else :do
         (if (word-p atom)
             (execute-word atom)
             (let ((w (find-word vm atom)))
               (if (null w)
                   (error "undefined word '~s'~%" atom)
                   (execute-word w)))))))
