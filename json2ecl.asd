;;;; json2ecl.asd

(asdf:defsystem #:json2ecl
  :description "Examines JSON data and deduces the ECL RECORD definitions necessary to parse it."
  :author "Dan S. Camper"
  :license  "Apache 2.0"
  :version "0.1.0"
  :serial t
  :depends-on (#:adopt #:com.inuoe.jzon #:with-user-abort)
  :components ((:file "package")
               (:file "ecl_keywords")
               (:file "json2ecl")
               (:file "userio")))
