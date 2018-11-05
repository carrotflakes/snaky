(defpackage snaky
  (:use :cl
        :snaky.generate))
(in-package :snaky)

(defun read-expression (exp)
  (cond
    ((stringp exp)
     (snaky.operators:str exp))
    ((listp exp)
     (case (first exp)
       (and (apply #'snaky.operators:and (mapcar #'read-expression (cdr exp))))
       (or (apply #'snaky.operators:or (mapcar #'read-expression (cdr exp))))
       (str (snaky.operators:str (second exp)))
       (any (snaky.operators:any))
       (repeat (snaky.operators:repeat (read-expression (second exp))
                                       (third exp) (fourth exp)))
       (call (snaky.operators:call (second exp)))
       (capture (snaky.operators:capture (read-expression (second exp))))))))

(defmacro defrule (name exp)
  `(defun ,name ()
     ,(generate (read-expression exp)
               `(return-from ,name 1)
               `(return-from ,name 2))))

(defun parse (name text &optional (pos 0))
  (let ((*text* text)
        (*text-length* (length text))
        (*pos* pos)
        (*values* nil))
    (funcall (symbol-function name))
    (if (= (length *text*) *pos*)
        (first *values*)
        (error "parse failed"))))

(print (generate (read-expression '(or (and (str "aaaa") (call hoge))
                                    (capture (str "yo"))))
               `(return-from name 'succ)
               `(return-from name 'fail)))

(eval
 '(progn
   (defrule hoge "hoge")
   (defrule start
     (or (and (str "aaaa") (call hoge))
         (capture (str "yo"))))

   (print (parse 'start "yo"))
   (print (parse 'start "aaaahoge"))))
