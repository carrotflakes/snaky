#|
  This file is a part of snaky project.
  Copyright (c) 2018 carrotflakes (carrotflakes@gmail.com)
|#

#|
  Author: carrotflakes (carrotflakes@gmail.com)
|#

(defsystem "snaky"
  :version "0.1.0"
  :author "carrotflakes"
  :license "LLGPL"
  :depends-on ()
  :components ((:module "src"
                :components
                ((:file "snaky" :depends-on ("core"))
                 (:file "core" :depends-on ("generate" "util"))
                 (:file "generate" :depends-on ("operators"))
                 (:file "operators")
                 (:file "util"))))
  :description "PEG-based parser generator"
  :long-description
  #.(read-file-string
     (subpathname *load-pathname* "README.markdown"))
  :in-order-to ((test-op (test-op "snaky-test"))))
