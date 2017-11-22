(load (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname)))

(ql:quickload "split-sequence")

(defpackage :timer
  (:use #:cl)
  (:import-from #:split-sequence #:split-sequence))
