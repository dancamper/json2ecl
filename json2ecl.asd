;;;; json2ecl.asd

(asdf:defsystem #:json2ecl
  :description "Describe json2ecl here"
  :author "Dan S. Camper"
  :license  "MIT"
  :version "0.0.1"
  :serial t
  :depends-on (#:com.inuoe.jzon)
  :components ((:file "package")
               (:file "ecl_keywords")
               (:file "json2ecl")))
