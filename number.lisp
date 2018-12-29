(defpackage #:uf/number
  (:use #:cl)
  (:import-from #:alexandria
                #:iota)
  (:import-from #:uf/condition
                #:uf/error)
  (:export #:to-number))
(in-package #:uf/number)

(define-condition uf/number/invalid (uf/error) ())

(defparameter +decdigits+
  (mapcar (lambda (n) (char (write-to-string n) 0)) (iota 10)))
(defparameter +hexdigits+
  (append +decdigits+
          (iota 6 :start (char-code #\a))
          (iota 6 :start (char-code #\A))))
(defparameter +bindigits+
  (mapcar (lambda (n) (char (write-to-string n) 0)) (iota 2)))

(defmacro to-*num (s digits parse-exp)
  `(if (loop
         :for n :from (if (char= (char ,s 0) #\-) 1 0) :below (length s)
         :always (member (char ,s n) ,digits :test #'char=))
       ,parse-exp
       (error 'uf/number/invalid)))

(defun to-decnum (s)
  (to-*num s +decdigits+ (parse-integer s)))

(defun to-hexnum (s)
  (to-*num s +hexdigits+ (parse-integer s :radix 16)))

(defun to-binnum (s)
  (to-*num s +bindigits+ (parse-integer s :radix 2)))

(defun to-basenum (s)
  ;; TODO: concider `BASE`
  (to-decnum s))

(defun to-chnum (s)
  (if (and (= (length s) 3)
           (char= (char s 0) #\')
           (char= (char s 2) #\'))
      (char-code (char s 1))
      (error 'uf/number/invalid)))

(defun to-number (s)
  (let ((c (char s 0))
        (s* (and (> (length s) 0) (subseq s 1 ))))
    (cond ((char= c #\#) (to-decnum s*))
          ((char= c #\$) (to-hexnum s*))
          ((char= c #\%) (to-binnum s*))
          ((char= c #\') (to-chnum s*))
          ((member c `(#\- ,@(mapcar (lambda (n) (char (write-to-string n) 0))
                                     (alexandria:iota 10)))
                   :test #'char=)
           (to-basenum s))
          (t nil))))
