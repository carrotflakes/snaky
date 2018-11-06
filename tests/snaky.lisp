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

(defrule repeat (repeat (str "a") 1 2))

(is (parse 'repeat "a")
    'nil)
(is (parse 'repeat "aa")
    'nil)
(is-error (parse 'repeat "")
          'simple-error)
(is-error (parse 'repeat "aaa")
          'simple-error)


(diag "any")

(defrule any (repeat (any) 1 2))

(is (parse 'any "a")
    'nil)
(is (parse 'any "ab")
    'nil)


(diag "charactor-class")

(defrule class1 (repeat (class "a1-3") 1 2))
(defrule class2 (repeat (class "^a1-3") 1 2))

(is (parse 'class1 "a")
    'nil)
(is (parse 'class1 "12")
    'nil)
(is-error (parse 'class1 "b")
          'simple-error)
(is-error (parse 'class1 "4")
          'simple-error)
(is (parse 'class2 "b")
    'nil)
(is (parse 'class2 "4")
    'nil)
(is-error (parse 'class2 "a")
          'simple-error)
(is-error (parse 'class2 "12")
          'simple-error)


(diag "capture")

(defrule capture (capture (repeat (any) 1 2)))

(is (parse 'capture "a")
    "a")
(is (parse 'capture "ab")
    "ab")


(diag "lookahead")

(defrule pla (and (& (class "a-d")) (any)))
(defrule nla (and (! (class "a-d")) (any)))

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


(finalize)
