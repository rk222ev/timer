(asdf:defsystem #:timer
  :name "CLI timer"
  :version "0.0.1-SNAPSHOT"
  :license "MIT"
  :author "Rpkn"
  :class :package-inferred-system
  :depends-on (#:cl-ppcre)
  :components ((:file "packages")
               (:file "timer")))
