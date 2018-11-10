(defpackage snaky.generate
  (:use :cl
        :snaky.operators)
  (:export :generate
           :*text*
           :*text-length*
           :*failed-matches*
           :*failed-pos*
           :fail
           :pos
           :values))
(in-package :snaky.generate)

(defvar *text*)
(defvar *text-length*)
(defvar *failed-matches*)
(defvar *failed-pos*)
(defvar *notify-failure*)

(defgeneric generate (operator succ fail))

(defmethod generate ((self seq) succ fail)
  (let ((pos (gensym "POS"))
        (values (gensym "VALUES"))
        (block (gensym "BLOCK")))
    `(block ,block
       (let ((,pos pos)
             (,values values))
         ,(let ((succ `(progn
                         ,succ
                         (return-from ,block))))
            (dolist (exp (reverse (seq-expressions self)))
              (setf succ (generate exp succ nil)))
            succ)
         (setf pos ,pos values ,values))
       ,fail)))

(defmethod generate ((self ordered-choice) succ fail)
  (let ((pos (gensym "POS"))
        (values (gensym "VALUES"))
        (block (gensym "BLOCK")))
    `(block ,block
       (let ((,pos pos)
             (,values values))
         ,(let ((fail `(progn
                         ,fail
                         (setf pos ,pos values ,values))))
            (dolist (exp (reverse (ordered-choice-expressions self)))
              (setf fail (generate exp nil fail)))
            fail))
       ,succ)))

(defmethod generate ((self str) succ fail)
  (let ((string (str-string self)))
    `(if (and (<= (+ pos ,(length string)) *text-length*)
              (string= *text* ,string
                       :start1 pos
                       :end1 (+ pos ,(length string))))
         (progn
           (incf pos ,(length string))
           ,succ)
         (progn
           (fail pos ,(format nil "~s" string))
           ,fail))))

(defmethod generate ((self charactor-class) succ fail)
  (let* ((negative (charactor-class-negative self))
         (chars (charactor-class-chars self))
         (ranges (charactor-class-ranges self))
         (conditions `(or ,@(mapcar (lambda (char) `(eq char ,char))
                                    chars)
                          ,@(mapcar (lambda (range) `(char<= ,(car range) char ,(cdr range)))
                                    ranges)))
         (condition (if negative
                        `(not ,conditions)
                        conditions)))
    `(if (and (< pos *text-length*)
              (let ((char (aref *text* pos)))
                ,condition))
         (progn
           (incf pos)
           ,succ)
         (progn
           (fail pos ,(format nil "[~a]" (charactor-class-source self)))
           ,fail))))
 
(defmethod generate ((self any) succ fail)
  `(if (< pos *text-length*)
       (progn
         (incf pos)
         ,succ)
       (progn
         (fail pos ".")
         ,fail)))

(defmethod generate ((self repeat) succ fail)
  (let ((expression (repeat-expression self))
        (min (repeat-min self))
        (max (repeat-max self)))
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

(defmethod generate ((self call) succ fail)
  `(multiple-value-bind (succ pos* values*)
       (,(call-symbol self) pos)
     (if succ 
         (progn
           (setf pos pos*
                 values (concatenate 'list values* values))
           ,succ)
         ,fail)))

(defmethod generate ((self capture) succ fail)
  (let ((pos (gensym "POS")))
    `(let ((,pos pos))
       ,(generate (capture-expression self)
                  `(progn
                     (push (subseq *text* ,pos pos) values)
                     ,succ)
                  fail))))

(defmethod generate ((self &) succ fail)
  (let ((pos (gensym "POS"))
        (values (gensym "VALUES")))
    `(let ((,pos pos)
           (,values values))
       ,(generate (&-expression self)
                  `(progn
                    (setf pos ,pos values ,values)
                    ,succ)
                  fail))))

(defmethod generate ((self !) succ fail)
  (let ((pos (gensym "POS"))
        (values (gensym "VALUES")))
    `(let ((,pos pos)
           (,values values))
       ,(generate (!-expression self)
                  `(progn
                    (setf pos ,pos values ,values)
                    ,fail)
                  succ))))

(defmethod generate ((self @) succ fail)
  (let ((values (gensym "VALUES")))
    `(let ((,values values))
       (setf values nil)
       ,(generate (@-expression self)
                  `(progn
                    (setf values (cons (reverse values) ,values))
                    ,succ)
                  `(progn
                     (setf values ,values)
                     ,fail)))))

(defmethod generate ((self ret) succ fail)
  `(progn
     (push ,(ret-value self) values)
     ,succ))

(defmethod generate ((self modify) succ fail)
  (let ((values (gensym "VALUES")))
    `(let ((,values values))
       (setf values nil)
       ,(generate (modify-expression self)
                  `(progn
                    (setf values
                          (cons (apply ,(modify-function self)
                                       (reverse values))
                                ,values))
                    ,succ)
                  `(progn
                     (setf values ,values)
                     ,fail)))))
