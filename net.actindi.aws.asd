;;;; -*- Mode: LISP; -*-
(asdf:defsystem :net.actindi.aws
  :version "0.0.0"
  :serial t
  :components ((:file "package")
               (:file "aws"))
  :depends-on (info.read-eval-print.series-ext
               alexandria
               bordeaux-threads
               cl-ppcre
               trivial-shell))
