;;;; json2ecl.lisp
;;;;
;;;; See https://github.com/Zulu-Inuoe/jzon

(in-package #:json2ecl)

;; (declaim (optimize (debug 3)))

;;;

(defvar *layout-names* nil
  "Used while ECL record definitions are being emitted.  Tracks the names
of the record definitions created, so that subsequent creations don't reuse
previously-defined names.")

(defparameter *ecl-string-type* "UTF8"
  "The ECL data type to be used for JSON string types.  Can be overridden
with an option.")

;;;

(defclass array-item ()
  ((object-prototype :accessor object-prototype :initform nil)
   (element-type :accessor element-type :initform nil)))

(defclass object-item ()
  ((keys :accessor keys :initform (make-hash-table :test 'equalp :size 25))))

;;;

(defun is-ecl-keyword-p (name)
  "Test if NAME (which should be a lowercase string) is an ECL keyword."
  (member name *ecl-keywords* :test 'equalp))

(defun remove-illegal-chars (name)
  "Return a copy of NAME with characters illegal for ECL attribute names
substituted with an underscore."
  (substitute-if #\_ (lambda (c) (not (or (alphanumericp c)
                                          (member c '(#\_)))))
                 name))

;;;

(defun as-ecl-field-name (name)
  "Return a copy of NAME that is suitable to be used as an ECL attribute."
  (let* ((lowername (string-downcase name))
         (no-dashes (remove-illegal-chars lowername))
         (legal (if (or (not (alpha-char-p (elt no-dashes 0)))
                        (is-ecl-keyword-p no-dashes))
                    (format nil "f_~A" no-dashes)
                    no-dashes)))
    legal))

(defun legal-layout-subname (name)
  "Return a copy of NAME that can be used within a RECORD name."
  (string-upcase (remove-illegal-chars name)))

(defun as-layout-name (name)
  "Construct a string that is a suitable ECL RECORD attribute, based on NAME."
  (let* ((legal-name (legal-layout-subname name))
         (name-count (count-if #'(lambda (x) (equalp x legal-name)) *layout-names*))
         (interstitial (if (< name-count 2) "" (format nil "_~3,'0D" name-count))))
    (format nil "~A~A_LAYOUT" legal-name interstitial)))

(defun register-layout-subname (name)
  "Push layout subname NAME to a special variable list so we can track usage."
  (let ((legal-name (legal-layout-subname name)))
    (push legal-name *layout-names*)))

(defun as-ecl-xpath (name)
  "Construct an ECL XPATH directive for NAME (typically an as-is JSON key)."
  (format nil "{XPATH('~A')}" name))

(defun as-dataset-type (name)
  "Construct an ECL DATASET datatype, given NAME."
  (format nil "DATASET(~A)" (as-layout-name name)))

(defun as-ecl-type (value-type)
  "Given a symbol representing an internal data type, return the corresponding ECL data type."
  (case value-type
    (boolean "BOOLEAN")
    (null-value "STRING")
    (string "STRING")
    (default-string *ecl-string-type*)
    (number "INTEGER")
    (float "REAL")))

;;;

(defgeneric as-ecl-field-def (value-obj name)
  (:documentation "Create an ECL field definition from an object or array class."))

(defmethod as-ecl-field-def ((value-obj t) name)
  (let ((ecl-type (as-ecl-type value-obj)))
    (format nil "~4T~A ~A ~A;~%" ecl-type (as-ecl-field-name name) (as-ecl-xpath name))))

(defmethod as-ecl-field-def ((obj object-item) name)
  (format nil "~4T~A ~A ~A;~%" (as-dataset-type name) (as-ecl-field-name name) (as-ecl-xpath name)))

(defmethod as-ecl-field-def ((obj array-item) name)
  (if (element-type obj)
      (format nil "~4TSET OF ~A ~A ~A;~%"
              (as-ecl-type (element-type obj))
              (as-ecl-field-name name)
              (as-ecl-xpath name))
      (format nil "~4T~A ~A ~A;~%"
              (as-dataset-type name)
              (as-ecl-field-name name)
              (as-ecl-xpath name))))

(defgeneric as-ecl-record-def (obj name)
  (:documentation "Create an ECL RECORD definition from an object or array class."))

(defmethod as-ecl-record-def ((obj t) name)
  (declare (ignore obj name))
  "")

(defmethod as-ecl-record-def ((obj object-item) name)
  (let* ((result-str "")
         (my-str (with-output-to-string (s)
                   (register-layout-subname name)
                   (format s "~A := RECORD~%" (as-layout-name name))
                   (loop for field-name being the hash-keys of (keys obj)
                           using (hash-value field-value)
                         do (let ((child-recdef (as-ecl-record-def field-value field-name)))
                              (when (string/= child-recdef "")
                                (setf result-str (format nil "~A~A" result-str child-recdef)))
                              (format s "~A" (as-ecl-field-def field-value field-name))))
                   (format s "END;~%~%")
                   )))
    (format nil "~A~A" result-str my-str)))

(defmethod as-ecl-record-def ((obj array-item) name)
  (if (object-prototype obj)
      (as-ecl-record-def (object-prototype obj) name)
      ""))

;;;

(defmacro reuse-object (place classname)
  "Return object found in PLACE if it is an instance of CLASSNAME, or create a
new instance of CLASSNAME in place and return that."
  `(progn
     (cond ((or (null ,place) (eql ,place 'null-value))
            (setf ,place (make-instance ,classname)))
           ((not (typep ,place ,classname))
            (error "Mismatching object types")))
     ,place))

(defmacro parse-simple (place value)
  "Insert the common type of VALUE and PLACE into PLACE."
  `(unless (or (typep ,place 'array-item) (typep ,place 'object-item))
     (setf ,place (common-type (base-type ,value) ,place))))

(defmacro parse-complex (place classname parser)
  "Reuse object in PLACE if possible, or create a new instance of CLASSNAME,
then kick off a new depth of parsing with the result."
  `(progn
     (reuse-object ,place ,classname)
     (parse-obj ,place ,parser nil)))

;;;

(defun base-type (jzon-atom)
  "Convert a JZON data type to an internal symbol to common-up some types."
  (etypecase jzon-atom
    ((eql t) 'boolean)
    ((eql nil) 'boolean)
    ((eql null) 'null-value)
    (integer 'number)
    (double-float 'float)
    (string 'default-string)))

(defun common-type (new-type old-type)
  "Given two internal symbols, return an internal type that can encompass both."
  (cond ((not old-type)
         new-type)
        ((eql new-type old-type)
         new-type)
        ((or (eql old-type 'default-string) (eql new-type 'default-string))
         'default-string)
        ((or (eql old-type 'string) (eql new-type 'string))
         'string)
        ((or (and (eql new-type 'number) (eql old-type 'float))
             (and (eql new-type 'float) (eql old-type 'number)))
         'float)
        (t
         'string)))

;;;

(defgeneric parse-obj (obj parser is-toplevel-p)
  (:documentation "Parses JZON-provided tokens into an internal object representation."))

(defmethod parse-obj ((obj t) parser (is-toplevel-p (eql t)))
  (loop named parse
        do (multiple-value-bind (event value) (jzon:parse-next parser)
             (cond ((null event)
                    (return-from parse))
                   ((eql event :begin-array)
                    (reuse-object obj 'array-item)
                    (parse-obj obj parser nil))
                   ((eql event :begin-object)
                    (reuse-object obj 'object-item)
                    (parse-obj obj parser nil))
                   (t
                    (error "Unknown object at toplevel: (~A,~A)" event value)))))
  obj)

(defmethod parse-obj ((obj array-item) parser (is-toplevel-p (eql nil)))
  (loop named parse
        do (multiple-value-bind (event value) (jzon:parse-next parser)
             (cond ((null event)
                    (error "Unexpected end of file"))
                   ((eql event :end-array)
                    (return-from parse))
                   ((eql event :value)
                    (parse-simple (element-type obj) value))
                   ((eql event :begin-array)
                    (parse-complex (object-prototype obj) 'array-item parser))
                   ((eql event :begin-object)
                    (parse-complex (object-prototype obj) 'object-item parser))
                   (t
                    (error "Unknown object while parsing array: (~A,~A)" event value)))))
  obj)

(defmethod parse-obj ((obj object-item) parser (is-toplevel-p (eql nil)))
  (loop named parse
        do (multiple-value-bind (event value) (jzon:parse-next parser)
             (cond ((null event)
                    (error "Unexpected end of file"))
                   ((eql event :end-object)
                    (return-from parse))
                   ((eql event :object-key)
                    (multiple-value-bind (key-event key-value) (jzon:parse-next parser)
                      (case key-event
                        ((:value)
                         (parse-simple (gethash value (keys obj)) key-value))
                        ((:begin-array)
                         (parse-complex (gethash value (keys obj)) 'array-item parser))
                        ((:begin-object)
                         (parse-complex (gethash value (keys obj)) 'object-item parser))
                        (otherwise
                         (error "Unknown object while parsing object value: (~A,~A)" key-event key-value)))))
                   (t
                    (error "Unknown object while parsing object: (~A,~A)" event value)))))
  obj)

;;;

(defun process-file-or-stream (input parsed-obj)
  "Entry point for parsing a single JSON data blob; INPUT can be a pathname
or a file stream; PARSED-OBJ should be a toplevel object."
  (jzon:with-parser (parser input)
    (setf parsed-obj (parse-obj parsed-obj parser t)))
  parsed-obj)

