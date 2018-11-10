(ql:quickload :snaky)

(use-package :snaky)

(defrule arithmetic (and ws additive ws))

(defrule additive
  (or (mod (and additive ws "+" ws multiplicative) #'+)
      (mod (and additive ws "-" ws multiplicative) #'-)
      multiplicative))

(defrule multiplicative
  (or (mod (and multiplicative ws "*" ws primary) #'*)
      (mod (and multiplicative ws "/" ws primary) #'/)
      (mod (and multiplicative ws "%" ws primary) #'mod)
      primary))

(defrule primary (or integer (and "(" ws additive ws ")")))

(defrule integer (grp "integer" (mod (cap (and (? "-") (+ (cc "0-9")))) #'parse-integer)))

(defrule ws (* (cc #.(format nil " ~a~a~a" #\cr #\lf #\tab))))

(defparser parse-arithmetic arithmetic)

(format t "(1 + 2) * 3 * 4 - (5 / 6) = ~a~%" (parse-arithmetic "(1 + 2) * 3 * 4 - (5 / 6)"))
