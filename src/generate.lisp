(defpackage snaky.generate
  (:use :cl)
  (:export :generate
           :*text*
           :*text-length*
           :pos
           :values))
(in-package :snaky.generate)

(defvar *text*)
(defvar *text-length*)

(defgeneric generate (operator succ fail))

(defmethod generate ((self snaky.operators:and) succ fail)
  (let ((pos (gensym "POS"))
        (values (gensym "VALUES"))
        (block (gensym "BLOCK")))
    `(block ,block
       (let ((,pos pos)
             (,values values))
         ,(let ((succ `(progn
                         ,succ
                         (return-from ,block))))
            (dolist (exp (reverse (snaky.operators:and-expressions self)))
              (setf succ (generate exp succ nil)))
            succ)
         (setf pos ,pos values ,values))
       ,fail)))

(defmethod generate ((self snaky.operators:or) succ fail)
  (let ((pos (gensym "POS"))
        (values (gensym "VALUES"))
        (block (gensym "BLOCK")))
    `(block ,block
       (let ((,pos pos)
             (,values values))
         ,(let ((fail `(progn
                         ,fail
                         (setf pos ,pos values ,values))))
            (dolist (exp (reverse (snaky.operators:or-expressions self)))
              (setf fail (generate exp nil fail)))
            fail))
       ,succ)))

(defmethod generate ((self snaky.operators:str) succ fail)
  (let ((string (snaky.operators:str-string self)))
    `(if (and (<= (+ pos ,(length string)) *text-length*)
              (string= *text* ,string
                       :start1 pos
                       :end1 (+ pos ,(length string))))
         (progn
           (incf pos ,(length string))
           ,succ)
         ,fail)))

(defmethod generate ((self snaky.operators:class) succ fail)
  (let* ((negative (snaky.operators:class-negative self))
         (chars (snaky.operators:class-chars self))
         (ranges (snaky.operators:class-ranges self))
         (conditions `(or ,@(mapcar (lambda (char) `(eq char ,char))
                                    chars)
                          ,@(mapcar (lambda (range) `(char<= ,(car range) char ,(cdr range)))
                                    ranges)))
         (condition (if negative
                        `(not ,conditions)
                        conditions)))
    `(if (and (<= (1+ pos) *text-length*)
              (let ((char (aref *text* pos)))
                ,condition))
         (progn
           (incf pos)
           ,succ)
         ,fail)))
 
(defmethod generate ((self snaky.operators:any) succ fail)
  `(if (<= (1+ pos) *text-length*)
       (progn
         (incf pos)
         ,succ)
       ,fail))

(defmethod generate ((self snaky.operators:repeat) succ fail)
  (let ((expression (snaky.operators:repeat-expression self))
        (min (snaky.operators:repeat-min self))
        (max (snaky.operators:repeat-max self)))
    (let ((pos (gensym "POS"))
          (values (gensym "VALUES"))
          (block (gensym "BLOCK"))
          (i (gensym "I")))
      `(let ((,pos pos)
             (,values values))
         (if (loop
               named ,block for ,i from 0 ,@(if max `(below ,max) '())
               do ,(generate expression
                             nil
                             `(return-from ,block ,(if min `(<= ,min ,i) 't)))
               finally (return-from ,block t))
             ,succ
             (progn
               (setf pos ,pos values ,values)
               ,fail))))))

(defmethod generate ((self snaky.operators:call) succ fail)
  `(multiple-value-bind (succ pos* values*)
       (,(snaky.operators:call-symbol self) pos)
     (if succ 
         (progn
           (setf pos pos*
                 values (concatenate 'list values* values))
           ,succ)
         ,fail)))

(defmethod generate ((self snaky.operators:capture) succ fail)
  (let ((pos (gensym "POS")))
    `(let ((,pos pos))
       ,(generate (snaky.operators:capture-expression self)
                  `(progn
                     (push (subseq *text* ,pos pos) values)
                     ,succ)
                  fail))))

(defmethod generate ((self snaky.operators:&) succ fail)
  (let ((pos (gensym "POS"))
        (values (gensym "VALUES")))
    `(let ((,pos pos)
           (,values values))
       ,(generate (snaky.operators:&-expression self)
                  `(progn
                    (setf pos ,pos values ,values)
                    ,succ)
                  fail))))

(defmethod generate ((self snaky.operators:!) succ fail)
  (let ((pos (gensym "POS"))
        (values (gensym "VALUES")))
    `(let ((,pos pos)
           (,values values))
       ,(generate (snaky.operators:!-expression self)
                  `(progn
                    (setf pos ,pos values ,values)
                    ,fail)
                  succ))))

(defmethod generate ((self snaky.operators:@) succ fail)
  (let ((values (gensym "VALUES")))
    `(let ((,values values))
       (setf values nil)
       ,(generate (snaky.operators:@-expression self)
                  `(progn
                    (setf values (cons (reverse values) ,values))
                    ,succ)
                  `(progn
                     (setf values ,values)
                     ,fail)))))

(defmethod generate ((self snaky.operators:ret) succ fail)
  `(progn
     (push ,(snaky.operators:ret-value self) values)
     ,succ))

(defmethod generate ((self snaky.operators:->) succ fail)
  (let ((values (gensym "VALUES")))
    `(let ((,values values))
       (setf values nil)
       ,(generate (snaky.operators:->-expression self)
                  `(progn
                    (setf values
                          (cons (apply ,(snaky.operators:->-function self)
                                       values)
                                ,values))
                    ,succ)
                  `(progn
                     (setf values ,values)
                     ,fail)))))
