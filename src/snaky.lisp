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
       (? (snaky.operators:? (read-expression (second exp))))
       (* (snaky.operators:* (read-expression (second exp))))
       (+ (snaky.operators:+ (read-expression (second exp))))
       (call (snaky.operators:call (second exp)))
       (capture (snaky.operators:capture (read-expression (second exp))))))))

(defmacro defrule (name exp)
  `(defun ,name ()
     ,(print (generate (read-expression exp)
               `(return-from ,name t)
               `(return-from ,name nil)))))

(defun parse (name text &optional (pos 0))
  (let ((*text* text)
        (*text-length* (length text))
        (*pos* pos)
        (*values* nil))
    (funcall (symbol-function name))
    (if (= (length *text*) *pos*)
        (first *values*)
        (error "parse failed"))))

(eval
 '(progn
   (defun safe-parse (name text)
     (handler-case (parse name text)
       (error (c)
         (return-from safe-parse 'failed))))
   (defrule hoge "hoge")
   (defrule start
     (or (and (str "aaaa") (call hoge))
         (capture (str "yo"))))
   (defrule fuga
     (capture (repeat (str "a") 2 4)))

   (print (safe-parse 'start "yo"))
   (print (safe-parse 'start "aaaahoge"))
   (print (safe-parse 'start "aaa"))
   (print (safe-parse 'start "aaaa"))
   (print (safe-parse 'fuga "a"))
   (print (safe-parse 'fuga "aa"))
   (print (safe-parse 'fuga "aaa"))
   (print (safe-parse 'fuga "aaaa"))
   (print (safe-parse 'fuga "aaaaa"))
))
;; charactor-class & ! ` @ -> -? -| ~ \V $input $pos $row $colu,m