;;;; json2ecl.lisp
;;;;
;;;; See https://github.com/Zulu-Inuoe/jzon

(in-package #:json2ecl)

;; (declaim (optimize (debug 3)))

;;;

(defvar *layout-names* nil)

;;;

(defclass array-item ()
  ((object-prototype :accessor object-prototype :initform nil)
   (element-type :accessor element-type :initform nil)))

(defclass object-item ()
  ((keys :accessor keys :initform (make-hash-table :test 'equalp :size 25))))

;;;

(defmethod pp ((obj t) &optional (depth 0))
  (declare (ignore depth))
  (with-output-to-string (s)
    (format s "~A~%" obj)))

(defmethod pp ((obj array-item) &optional (depth 0))
  (with-output-to-string (s)
    (format s "[~%")
    (format s "~vT~A" (* (1+ depth) 4) (pp (or (object-prototype obj) (element-type obj)) (1+ depth)))
    (format s "~vT]~%" (* depth 4))))

(defmethod pp ((obj object-item) &optional (depth 0))
  (with-output-to-string  (s)
    (format s "{~%")
    (loop for key being the hash-keys of (keys obj)
            using (hash-value value)
          do (format s "~vT~A: ~A" (* (1+ depth) 4) key (pp value (1+ depth))))
    (format s "~vT}~%" (* depth 4))))

;;;

(defun ecl-keyword-p (name)
  (member name *ecl-keywords* :test 'equalp))

(defun ecl-name (name)
  (let* ((lowername (string-downcase name))
         (no-dashes (substitute #\_ #\- lowername))
         (legal (if (or (not (alpha-char-p (elt no-dashes 0)))
                        (ecl-keyword-p no-dashes))
                    (format nil "f_~A" no-dashes)
                    no-dashes)))
    legal))

(defun legal-layout-name (name)
  (string-upcase (substitute #\_ #\- name)))

(defun layout-name (name)
  (let* ((legal-name (legal-layout-name name))
         (name-count (count-if #'(lambda (x) (equalp x legal-name)) *layout-names*))
         (interstitial (if (zerop name-count) "" (format nil "_~3,'0D" name-count))))
    (format nil "~A~A_LAYOUT" legal-name interstitial)))

(defun register-layout-name (name)
  (let ((legal-name (legal-layout-name name)))
    (push legal-name *layout-names*)))

(defun ecl-xpath (name)
  (format nil "{XPATH('~A')}" name))

(defun ecl-type (value-type)
  (case value-type
    (boolean "BOOLEAN")
    (null "STRING")
    (string "STRING")
    (utf8 "UTF8")
    (number "INTEGER")))

(defun dataset-datatype-name (name)
  (format nil "DATASET(~A)" (layout-name name)))

;;;

(defmethod as-ecl-fielddef ((value-obj t) name)
  (let ((ecl-type (ecl-type value-obj)))
    (format nil "~4T~A ~A ~A;~%" ecl-type (ecl-name name) (ecl-xpath name))))

(defmethod as-ecl-fielddef ((obj object-item) name)
  (format nil "~4T~A ~A ~A;~%" (dataset-datatype-name name) (ecl-name name) (ecl-xpath name)))

(defmethod as-ecl-fielddef ((obj array-item) name)
  (if (element-type obj)
      (format nil "~4TSET OF ~A ~A ~A;~%" (ecl-type (element-type obj)) (ecl-name name) (ecl-xpath name))
      (format nil "~4T~A ~A ~A;~%" (dataset-datatype-name name) (ecl-name name) (ecl-xpath name))))

;;;

(defmethod as-ecl-recdef ((obj t) name)
  (declare (ignore obj name))
  "")

(defmethod as-ecl-recdef ((obj object-item) name)
  (let* ((result-str "")
         (my-str (with-output-to-string (s)
                   (register-layout-name name)
                   (format s "~A := RECORD~%" (layout-name name))
                   (loop for field-name being the hash-keys of (keys obj)
                           using (hash-value field-value)
                         do (let ((child-recdef (as-ecl-recdef field-value field-name)))
                              (when (string/= child-recdef "")
                                (setf result-str (format nil "~A~A" result-str child-recdef)))
                              (format s "~A" (as-ecl-fielddef field-value field-name))))
                   (format s "END;~%~%")
                   )))
    (format nil "~A~A" result-str my-str)))

(defmethod as-ecl-recdef ((obj array-item) name)
  (if (object-prototype obj)
      (as-ecl-recdef (object-prototype obj) name)
      ""))

;;;

(defmacro reuse-object (place classname)
  `(progn
     (cond ((or (null ,place) (eql ,place 'null))
            (setf ,place (make-instance ,classname)))
           ((not (typep ,place ,classname))
            (error "Mismatching object types")))
     ,place))

(defmacro parse-simple (place value)
  `(unless (eql (base-type ,value) 'null)
     (setf ,place (common-type (base-type ,value) ,place))))

(defmacro parse-complex (place classname parser)
  `(progn
     (reuse-object ,place ,classname)
     (parse-obj ,place ,parser)))

;;;

(defun base-type (thing)
  (etypecase thing
    ((eql t) 'boolean)
    ((eql nil) 'boolean)
    ((eql null) 'null)
    (integer 'number)
    (double-float 'number)
    (string 'utf8)))

(defun common-type (new-type old-type)
  (cond ((not old-type) new-type)
        ((or (eql old-type 'utf8) (eql new-type 'utf8)) 'utf8)
        ((not (eql old-type new-type)) 'string)
        (t new-type)))

;;;

(defmethod parse-obj ((obj array-item) parser &optional (startingp nil))
  (loop named parse
        do (multiple-value-bind (event value) (jzon:parse-next parser)
             (cond (startingp
                    (cond ((null event)
                           (return-from parse))
                          ((eql event :begin-array)
                           (reuse-object obj 'array-item)
                           (parse-obj obj parser))
                          ((eql event :begin-object)
                           (reuse-object obj 'object-item)
                           (parse-obj obj parser))
                          (t
                           (error "Unknown object at toplevel: (~A,~A)" event value))))
                   ((null event)
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

(defmethod parse-obj ((obj object-item) parser &optional (startingp nil))
  (loop named parse
        do (multiple-value-bind (event value) (jzon:parse-next parser)
             (cond (startingp
                    (cond ((null event)
                           (return-from parse))
                          ((eql event :begin-array)
                           (reuse-object obj 'array-item)
                           (parse-obj obj parser))
                          ((eql event :begin-object)
                           (reuse-object obj 'object-item)
                           (parse-obj obj parser))
                          (t
                           (error "Unknown object at toplevel: (~A,~A)" event value))))
                   ((null event)
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

(defmethod parse-obj ((obj t) parser &optional (startingp nil))
  (declare (ignore obj startingp))
  (let ((top-object nil))
    (loop named parse
          do (multiple-value-bind (event value) (jzon:parse-next parser)
               (cond ((null event)
                      (return-from parse))
                     ((eql event :begin-array)
                      (reuse-object top-object 'array-item)
                      (parse-obj top-object parser))
                     ((eql event :begin-object)
                      (reuse-object top-object 'object-item)
                      (parse-obj top-object parser))
                     (t
                      (error "Unknown object at toplevel: (~A,~A)" event value)))))
    top-object))

;;;

(defun process-file (input &optional (parsed-obj nil))
  (jzon:with-parser (parser input)
    (setf parsed-obj (parse-obj parsed-obj parser t)))
  parsed-obj)

