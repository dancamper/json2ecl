;;;; json2ecl.asd

(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload '(:with-user-abort
                  :adopt
                  :com.inuoe.jzon)
                :silent t))

(asdf:defsystem #:json2ecl
  :description "Describe json2ecl here"
  :author "Dan S. Camper"
  :license  "MIT"
  :version "0.0.1"
  :serial t
  :depends-on (#:adopt #:com.inuoe.jzon #:with-user-abort)
  :components ((:file "package")
               (:file "ecl_keywords")
               (:file "json2ecl")))
