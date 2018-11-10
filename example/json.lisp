(ql:quickload :snaky)

(use-package :snaky)

(defrule json (and ws value ws))

(defrule begin-array (and ws "[" ws))
(defrule begin-object (and ws "{" ws))
(defrule end-array (and ws "]" ws))
(defrule end-object (and ws "}" ws))
(defrule name-separator (and ws ":" ws))
(defrule value-separator (and ws "," ws))

(defrule ws (* (cc #.(format nil " ~a~a~a" #\cr #\lf #\tab))))

(defrule value
  (or false %null true object array number str))

(defrule false (and "false" (ret 'false)))
(defrule %null (and "null" (ret 'null)))
(defrule true (and "true" (ret 'true)))

(defrule object
  (and begin-object
       (@ (and (ret :obj) (? %member) (* (and value-separator %member))))
       end-object))

(defrule %member
  (@ (and str name-separator value)))

(defrule array
  (and begin-array
       (@ (and (ret :arr) (? value) (* (and value-separator value))))
       end-array))

(defrule number
  (mod (cap (and (? minus) int (? frac) (? %exp)))
       #'read-from-string))

(defrule decimal-point ".")
(defrule digit1-9 (cc "1-9"))
(defrule e (cc "eE"))
(defrule %exp (and e (? (or minus plus)) (+ digit)))
(defrule frac (and decimal-point (+ digit)))
(defrule int (or zero (and digit1-9 (* digit))))
(defrule minus "-")
(defrule plus "+")
(defrule zero "0")

(defrule str
  (and quotation-mark
       (mod (@ (* %char)) (lambda (x) (format nil "~{~a~}" x)))
       quotation-mark))

(defrule %char
  (or (cap unescaped)
      (and escape
           (or (cap "\"")
               (cap "\\")
               (cap "/")
               (and "b" (ret #.(format nil "~a" #\bs)))
               (and "f" (ret #.(format nil "~a" #\page)))
               (and "n" (ret #.(format nil "~a" #\lf)))
               (and "r" (ret #.(format nil "~a" #\cr)))
               (and "t" (ret #.(format nil "~a" #\tab)))
               (and "u" (mod (cap (rep hexdig 4 4))
                             (lambda (x) (code-char (parse-integer x :radix 16)))))))))

(defrule escape "\\")
(defrule quotation-mark "\"")
(defrule unescaped (cc #.(format nil "^~a-~a\\\"" (code-char #x0000) (code-char #x001f))))

(defrule digit (cc "0-9"))
(defrule hexdig (cc "0-9a-fA-F"))

(defparser parse-json json)

(print (parse-json "true"))
(print (parse-json "null"))
(print (parse-json "\"hello\""))
(print (parse-json "[true]"))
(print (parse-json "[true, 1, 1.0, -1e-10]"))
(print (parse-json "{}"))
(print (parse-json "[{\"name\": \"foo\", \"age\": 20}]"))
