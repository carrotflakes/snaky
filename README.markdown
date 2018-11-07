# snaky

## Usage
``` lisp
;; Define rules
(defrule start ...)
(defrule foo ...)

;; Parse instantly
(parse 'start "hello")

;; Define parser
(defparser parse-start start)

;; and use
(parse-start "hello")
```

## Installation

## Author

* carrotflakes (carrotflakes@gmail.com)

## Copyright

Copyright (c) 2018 carrotflakes (carrotflakes@gmail.com)

## License

Licensed under the LLGPL License.
