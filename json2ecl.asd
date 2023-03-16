;;;; json2ecl.asd

(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload '(:with-user-abort
                  :adopt
                  :com.inuoe.jzon)
                :silent t))

(asdf:defsystem #:json2ecl
  :description "Examines JSON data and deduces the ECL RECORD definitions necessary to parse it."
  :author "Dan S. Camper"
  :license  "MIT"
  :version "0.0.2"
  :serial t
  :depends-on (#:adopt #:com.inuoe.jzon #:with-user-abort)
  :components ((:file "package")
               (:file "ecl_keywords")
               (:file "userio")
               (:file "json2ecl")))
