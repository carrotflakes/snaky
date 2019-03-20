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

(defgeneric generate (operator succ))

(defmethod generate ((self seq) succ)
  (let ((pos (gensym "POS"))
        (values (gensym "VALUES")))
    (dolist (exp (reverse (seq-expressions self)))
      (setf succ (generate exp succ)))
    `(let ((,pos pos)
           (,values values))
       ,succ
       (setf pos ,pos values ,values))))

(defmethod generate ((self ordered-choice) succ)
  (let ((pos (gensym "POS"))
        (values (gensym "VALUES"))
        (succ-tag (gensym "SUCC-TAG"))
        (fail-tag (gensym "FAIL-TAG")))
    `(let ((,pos pos)
           (,values values))
       (tagbody
         ,@(loop
             for exp in (ordered-choice-expressions self)
             collect (generate exp `(go ,succ-tag)))
         (go ,fail-tag)
         ,succ-tag
         ,succ
         ,fail-tag
         (setf pos ,pos values ,values)))))

(defmethod generate ((self str) succ)
  (let ((string (str-string self)))
    `(if (and (<= (fixnum-+ pos ,(length string)) *text-length*)
              (string= *text* ,string
                       :start1 pos
                       :end1 (fixnum-+ pos ,(length string))))
         (progn
           (setf pos (fixnum-+ pos ,(length string)))
           ,succ)
         (fail pos ,(format nil "~s" string)))))

(defmethod generate ((self character-class) succ)
  (let* ((negative (character-class-negative self))
         (chars (character-class-chars self))
         (ranges (character-class-ranges self))
         (conditions `(or (member char ',chars :test 'char=)
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
         (fail pos ,(format nil "[~a]" (character-class-source self))))))

(defmethod generate ((self any) succ)
  `(if (< pos *text-length*)
       (progn
         (setf pos (fixnum-+ pos 1))
         ,succ)
       (fail pos ".")))

(defmethod generate ((self repeat) succ)
  (let ((expression (repeat-expression self))
        (min (repeat-min self))
        (max (repeat-max self)))
    (cond
      ((and (zerop min) (eq max 1))
       (let ((block (gensym "BLOCK")))
         `(progn
            (block ,block
              ,(generate expression `(return-from ,block)))
            ,succ)))
      ((and (zerop min) (null max))
       (let ((loop-tag (gensym "LOOP")))
         `(tagbody
            ,loop-tag
            ,(generate expression `(go ,loop-tag))
            ,succ)))
      (t
       (let ((pos (gensym "POS"))
             (values (gensym "VALUES"))
             (succ-tag (gensym "SUCC"))
             (fail-tag (gensym "FAIL"))
             (loop-tag (gensym "LOOP"))
             (i (gensym "I")))
         `(let ((,pos pos)
                (,values values)
                (,i 0))
            (tagbody
              ,loop-tag
              ,(generate expression
                 (if max
                     `(if (< (incf ,i) ,max)
                          (go ,loop-tag)
                          (go ,succ-tag))
                     `(progn (incf ,i) (go ,loop-tag))))
              ,@(when (< 0 min)
                  `((when (< ,i ,min)
                      (go ,fail-tag))))
              ,succ-tag
              ,succ
              ,fail-tag
              (setf pos ,pos values ,values))))))))

(defmethod generate ((self call) succ)
  `(multiple-value-bind (succ pos* values*)
       (,(call-symbol self) pos)
     (when succ
       (setf pos pos*
             values (concatenate 'list values* values))
       ,succ)))

(defmethod generate ((self capture) succ)
  (let ((pos (gensym "POS")))
    `(let ((,pos pos))
       ,(generate (capture-expression self)
                  `(progn
                     (push (subseq *text* ,pos pos) values)
                     ,succ)))))

(defmethod generate ((self &) succ)
  (let ((pos (gensym "POS"))
        (values (gensym "VALUES")))
    `(let ((,pos pos)
           (,values values))
       ,(generate (&-expression self)
                  `(progn
                    (setf pos ,pos values ,values)
                    ,succ)))))

(defmethod generate ((self !) succ)
  (let ((pos (gensym "POS"))
        (values (gensym "VALUES"))
        (block (gensym "BLOCK")))
    `(block ,block
       (let ((,pos pos)
             (,values values))
         ,(generate (!-expression self)
                    `(progn
                       (setf pos ,pos values ,values)
                       (return-from ,block)))
         ,succ))))

(defmethod generate ((self @) succ)
  (let ((values (gensym "VALUES")))
    `(let ((,values values))
       (setf values nil)
       ,(generate (@-expression self)
                  `(progn
                    (setf values (cons (reverse values) ,values))
                    ,succ))
       (setf values ,values))))

(defmethod generate ((self ret) succ)
  `(progn
     (push ,(ret-value self) values)
     ,succ))

(defmethod generate ((self modify) succ)
  (let ((values (gensym "VALUES")))
    `(let ((,values values))
       (setf values nil)
       ,(generate (modify-expression self)
                  `(progn
                    (setf values
                          (cons (apply ,(modify-function self)
                                       (reverse values))
                                ,values))
                    ,succ))
       (setf values ,values))))

(defmethod generate ((self guard) succ)
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
                        (return-from ,block)))))
       (setf values ,values))))

(defmethod generate ((self waste) succ)
  (let ((values (gensym "VALUES")))
    `(let ((,values values))
       ,(generate (waste-expression self)
                  `(progn
                     (setf values ,values)
                     ,succ)))))

(defmethod generate ((self group) succ)
  `(progn
     (let ((*failed-pos* (fixnum-+ *text-length* 1)))
       ,(generate (group-expression self) succ))
     (fail pos ',(group-name self))))


(defun fail (pos match)
  (declare (optimize (speed 3) (space 0) (safety 0)))
  (cond
    ((< *failed-pos* pos)
     (setf *failed-pos* pos
           *failed-matches* (list match)))
    ((= *failed-pos* pos)
     (push match *failed-matches*))))
