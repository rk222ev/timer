(asdf:defsystem #:timer
  :name "CLI timer"
  :version "0.0.1-SNAPSHOT"
  :license "MIT"
  :author "Rpkn"
  :class :package-inferred-system
  :depends-on (#:cl-ppcre #:sb-posix)
  :components ((:file "packages")
               (:file "timer"))
  :build-operation "program-op"
  :build-pathname "timer"
  :entry-point "timer:cli-main")

#+sb-core-compression
(defmethod asdf:perform ((o asdf:image-op) (c asdf:system))
  (uiop:dump-image (asdf:output-file o c) :executable T :compression T))
