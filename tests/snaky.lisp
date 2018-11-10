(defpackage snaky-test
  (:use :cl
        :snaky
        :prove))
(in-package :snaky-test)

;; NOTE: To run this test file, execute `(asdf:test-system :snaky)' in your Lisp.

(plan nil)


(diag "str")

(defrule str (str "yo"))

(is (parse 'str "yo")
    'nil)
(is-error (parse 'str "y")
          'simple-error)
(is-error (parse 'str "a")
          'simple-error)
(is-error (parse 'str "yoo")
          'simple-error)


(diag "repeat")

(defrule rep (rep (str "a") 1 2))

(is (parse 'rep "a")
    'nil)
(is (parse 'rep "aa")
    'nil)
(is-error (parse 'rep "")
          'simple-error)
(is-error (parse 'rep "aaa")
          'simple-error)


(diag "any")

(defrule any (rep (any) 1 2))

(is (parse 'any "a")
    'nil)
(is (parse 'any "ab")
    'nil)


(diag "charactor-class")

(defrule cc1 (rep (cc "a1-3") 1 2))
(defrule cc2 (rep (cc "^a1-3") 1 2))

(is (parse 'cc1 "a")
    'nil)
(is (parse 'cc1 "12")
    'nil)
(is-error (parse 'cc1 "b")
          'simple-error)
(is-error (parse 'cc1 "4")
          'simple-error)
(is (parse 'cc2 "b")
    'nil)
(is (parse 'cc2 "4")
    'nil)
(is-error (parse 'cc2 "a")
          'simple-error)
(is-error (parse 'cc2 "12")
          'simple-error)


(diag "capture")

(defrule cap (cap (rep (any) 1 2)))

(is (parse 'cap "a")
    "a")
(is (parse 'cap "ab")
    "ab")


(diag "lookahead")

(defrule pla (and (& (cc "a-d")) (any)))
(defrule nla (and (! (cc "a-d")) (any)))

(is (parse 'pla "a")
    'nil)
(is-error (parse 'pla "e")
          'simple-error)
(is (parse 'nla "e")
    'nil)
(is-error (parse 'nla "a")
          'simple-error)


(diag "ret")

(defrule ret (ret 'foo))

(is (parse 'ret "")
    'foo)


(diag "@")

(defrule @ (@ (and (ret 'foo) (ret 'bar))))

(is (parse '@ "")
    '(foo bar))


(diag "call")

(defrule call1 (and (ret 1) (ret 2) "a"))
(defrule call2 (@ (and call1 call1)))

(is (parse 'call2 "aa")
    '(1 2 1 2))


(diag "mod")

(defrule modify (mod (mod (cap (rep (any) 0 nil)) #'intern) (lambda (x) x)))

(is (parse 'modify "ABC")
    'abc)


(diag "cache")

(defrule cache1 (and (ret 1) (ret 2) "a"))
(defrule cache2 (@ (and (& cache1) cache1)))

(is (parse 'cache2 "a")
    '(1 2))


(diag "arithmetic")

(defrule arithmetic (and ws additive ws))
(defrule additive (or (@ (and additive ws (cap "+") ws multiplicative))
                      (@ (and additive ws (cap "-") ws multiplicative))
                      multiplicative))
(defrule multiplicative (or (@ (and multiplicative ws (cap "*") ws primary))
                            (@ (and multiplicative ws (cap "/") ws primary))
                            primary))
(defrule primary (or int
                     (and "(" ws additive ws ")")))
(defrule int (cap (and (? "-") (+ (cc "0-9")))))
(defrule ws (* (cc " ")))

(is (parse 'arithmetic "1 + 2 * (3 + 4 + 5) / 2")
    '("1" "+" (("2" "*" (("3" "+" "4") "+" "5")) "/" "2")))

(finalize)
