(defpackage snaky.generate
  (:use :cl
        :snaky.operators)
  (:export :fixnum-+
           :generate
           :*text*
           :*text-length*
           :*failed-matches*
           :*failed-pos*
           :fail
           :pos
           :values))
(in-package :snaky.generate)

(defmacro fixnum-+ (x y)
  `(locally (declare (optimize (safety 0)))
     (the fixnum (+ ,x ,y))))

(defvar *text*)
(defvar *text-length*)
(defvar *failed-matches*)
(defvar *failed-pos*)
(defvar *notify-failure*)

(defgeneric generate (operator succ fail))

(defmethod generate ((self seq) succ fail)
  (let ((pos (gensym "POS"))
        (values (gensym "VALUES"))
        (fail-tag (gensym "FAIL-TAG"))
        (break-tag (gensym "BREAK-TAG")))
    `(let ((,pos (the fixnum pos))
           (,values values))
       (tagbody
         ,(let ((succ `(progn
                         ,succ
                         (go ,break-tag))))
            (dolist (exp (reverse (seq-expressions self)))
              (setf succ (generate exp succ `(go ,fail-tag))))
            succ)
         ,fail-tag
         (setf pos ,pos values ,values)
         ,fail
         ,break-tag))))

(defmethod generate ((self ordered-choice) succ fail)
  (let ((pos (gensym "POS"))
        (values (gensym "VALUES"))
        (succ-tag (gensym "SUCC-TAG"))
        (break-tag (gensym "BREAK-TAG")))
    `(let ((,pos (the fixnum pos))
           (,values values))
       (declare (ignorable ,pos ,values))
       (tagbody
         ,(let ((fail `(progn
                         ,fail
                         (setf pos ,pos values ,values)
                         (go ,break-tag))))
            (dolist (exp (reverse (ordered-choice-expressions self)))
              (setf fail (generate exp `(go ,succ-tag) fail)))
            fail)
         ,succ-tag
         ,succ
         ,break-tag))))

(defmethod generate ((self str) succ fail)
  (let ((string (str-string self)))
    `(if (and (<= (fixnum-+ pos ,(length string)) *text-length*)
              (string= *text* ,string
                       :start1 pos
                       :end1 (fixnum-+ pos ,(length string))))
         (progn
           (setf pos (fixnum-+ pos ,(length string)))
           ,succ)
         (progn
           (fail pos ,(format nil "~s" string))
           ,fail))))

(defmethod generate ((self character-class) succ fail)
  (let* ((negative (character-class-negative self))
         (chars (character-class-chars self))
         (ranges (character-class-ranges self))
         (conditions `(or ,@(mapcar (lambda (char) `(eq char ,char))
                                    chars)
                          ,@(mapcar (lambda (range) `(char<= ,(car range) char ,(cdr range)))
                                    ranges)))
         (condition (if negative
                        `(not ,conditions)
                        conditions)))
    `(if (and (< pos *text-length*)
              (let ((char (schar *text* pos)))
                ,condition))
         (progn
           (setf pos (fixnum-+ pos 1))
           ,succ)
         (progn
           (fail pos ,(format nil "[~a]" (character-class-source self)))
           ,fail))))
 
(defmethod generate ((self any) succ fail)
  `(if (< pos *text-length*)
       (progn
         (setf pos (fixnum-+ pos 1))
         ,succ)
       (progn
         (fail pos ".")
         ,fail)))

(defmethod generate ((self repeat) succ fail)
  (let ((expression (repeat-expression self))
        (min (repeat-min self))
        (max (repeat-max self)))
    (when (and (null min) (= max 1))
      (return-from generate
        (let ((block (gensym "BLOCK")))
          `(progn
             (block ,block
               (generate expression '(return-from ,block) '(return-from ,block)))
             ,succ))))
    (let ((pos (gensym "POS"))
          (values (gensym "VALUES"))
          (block (gensym "BLOCK"))
          (i (gensym "I")))
      `(let ((,pos (the fixnum pos))
             (,values values))
         (if (loop
               named ,block
               for ,i fixnum from 0 ,@(if max `(below ,max) '())
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
    `(let ((,pos (the fixnum pos))
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

(defmethod generate ((self guard) succ fail)
  (let ((values (gensym "VALUES"))
        (block (gensym "BLOCK")))
    `(let ((,values values))
       (block ,block
         (setf values nil)
         ,(generate (guard-expression self)
                    `(cond
                       ((apply ,(guard-function self) (reverse values))
                        (setf values (concatenate 'list values ,values))
                        ,succ)
                       (t
                        (return-from ,block)))
                    `(return-from ,block)))
       (setf values ,values)
       ,fail)))

(defmethod generate ((self waste) succ fail)
  (let ((values (gensym "VALUES")))
    `(let ((,values values))
       ,(generate (waste-expression self)
                  `(progn
                     (setf values ,values)
                     ,succ)
                  fail))))

(defmethod generate ((self group) succ fail)
  (let ((block (gensym "BLOCK")))
    `(progn
       (block ,block
         (let ((*failed-pos* (fixnum-+ *text-length* 1)))
           ,(generate (group-expression self)
                      succ
                      `(return-from ,block))))
       (progn
         (fail pos ',(group-name self))
         ,fail))))
