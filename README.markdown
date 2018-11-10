# snaky
snaky is a parser generator, a kind of packrat parser.

## Usage
``` lisp
;; Define rules
(defrule start (cap any-string))
(defrule any-string (* (any)))

;; Parse instantly
(parse 'start "hello")
;; => "hello"

;; Define parser
(defparser parse-start start)

;; and use
(parse-start "hello")
;; => "hello"
```

## API
### Macro: (defrule rule-name expression)
Defines a rule.

### Macro: (defparser name rule-name)
Defines a function named `name` that parses by rule `rule-name`.

### Function: (parse rule-name string)
Parses `string` by rule `rule-name`.

## Operators
```
(and ...)
(or ...)
(str <string>)
<string> ; Shorthand for (str <string>)
(cc <string>) ; a.k.a. charactor class.
(any)
(rep <expression> min max)
(? <expression>)
(* <expression>)
(+ <expression>)
(& <expression>) ; Positive lookahead
(! <expression>) ; Negative lookahead
(call <symbol>)
<symbol> ; Shorthand for (call <symbol>)
(cap <expression>) ; Substrings between <expression> and returns the string.
(ret <form>) ; Returns the form constantly.
(@ <expression>) ; Returns the values in <expression> as a list.
(mod <expression> <function>) ; Modifies the value from <expression> by <function>.
(grd <expression> <function>) ; Fails when <function> with <expression> value is `nil`.
(wst <expression>) ; Wastes the returned value from <expression>
(grp name <expression>) ; Names <expression> by the name.The name will be used in error message.
```

## Installation
```
$ ros install carrotflakes/snaky
```

## Author

* carrotflakes (carrotflakes@gmail.com)

## Copyright

Copyright (c) 2018 carrotflakes (carrotflakes@gmail.com)

## License

Licensed under the LLGPL License.
