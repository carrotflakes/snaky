#|
  This file is a part of snaky project.
  Copyright (c) 2018 carrotflakes (carrotflakes@gmail.com)
|#

(defsystem "snaky-test"
  :defsystem-depends-on ("prove-asdf")
  :author "carrotflakes"
  :license "LLGPL"
  :depends-on ("snaky"
               "prove")
  :components ((:module "tests"
                :components
                ((:test-file "snaky"))))
  :description "Test system for snaky"

  :perform (test-op (op c) (symbol-call :prove-asdf :run-test-system c)))
