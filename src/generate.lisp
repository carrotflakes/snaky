(defpackage snaky.generate
  (:use :cl)
  (:export :generate
           :*text*
           :*text-length*
           :*pos*
           :*values*))
(in-package :snaky.generate)

(defvar *text*)
(defvar *text-length*)
(defvar *pos*)
(defvar *values*)

(defgeneric generate (operator succ fail))

(defmethod generate ((self snaky.operators:and) succ fail)
  (let ((pos (gensym "POS"))
        (values (gensym "VALUES"))
        (block (gensym "BLOCK")))
    `(block ,block
       (let ((,pos *pos*)
             (,values *values*))
         ,(let ((succ `(progn
                         ,succ
                         (return-from ,block))))
            (dolist (exp (reverse (snaky.operators:and-expressions self)))
              (setf succ (generate exp succ nil)))
            succ)
         (setf *pos* ,pos *values* ,values))
       ,fail)))

(defmethod generate ((self snaky.operators:or) succ fail)
  (let ((pos (gensym "POS"))
        (values (gensym "VALUES"))
        (block (gensym "BLOCK")))
    `(block ,block
       (let ((,pos *pos*)
             (,values *values*))
         ,(let ((fail `(progn
                         ,fail
                         (setf *pos* ,pos *values* ,values))))
            (dolist (exp (reverse (snaky.operators:or-expressions self)))
              (setf fail (generate exp nil fail)))
            fail))
       ,succ)))

(defmethod generate ((self snaky.operators:str) succ fail)
  (let ((string (snaky.operators:str-string self)))
    `(if (and (<= (+ *pos* ,(length string)) *text-length*)
              (string= *text* ,string
                       :start1 *pos*
                       :end1 (+ *pos* ,(length string))))
         (progn
           (incf *pos* ,(length string))
           ,succ)
         ,fail)))

(defmethod generate ((self snaky.operators:call) succ fail)
  `(if (,(snaky.operators:call-symbol self))
       ,succ
       ,fail))

(defmethod generate ((self snaky.operators:capture) succ fail)
  (let ((pos (gensym "POS")))
    `(let ((,pos *pos*))
       ,(generate (snaky.operators:capture-expression self)
                  `(progn
                     (push (subseq *text* ,pos *pos*) *values*)
                     ,succ)
                  fail))))
