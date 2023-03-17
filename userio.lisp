;;;; userio.lisp

(in-package #:json2ecl)

;; Adopt: https://docs.stevelosh.com/adopt/usage/

(defparameter *option-version*
  (adopt:make-option 'version
                     :result-key 'version
                     :help "Display version and exit."
                     :long "version"
                     :short #\v
                     :reduce (constantly t)))

(defparameter *option-help*
  (adopt:make-option 'help
                     :result-key 'help
                     :help "Display help and exit."
                     :long "help"
                     :short #\h
                     :reduce (constantly t)))

(adopt:define-string *help-text*
  "json2ecl examines JSON data and deduces the ECL RECORD definitions necessary to parse it. ~
The resulting ECL definitions are returned via standard out, suitable for piping or copying ~
and pasting into your favorite IDE.~@
~@
JSON data can be supplied as one or more files or via standard input.~@
~@
Multiple files, if provided, are parsed as if they should have the same record structure. ~
This is useful for cases where you suspect that not all JSON key/value objects are fully ~
defined in one file, but other files may contain the missing data.")

(defparameter *ui*
  (adopt:make-interface :name "json2ecl"
                        :usage "[OPTIONS] [FILE...]"
                        :summary (format nil "analyze JSON data and emit ECL record ~
                                              definitions that can parse that data")
                        :help *help-text*
                        :contents (list
                                   *option-version*
                                   *option-help*)))

;;;

(define-condition user-error (error)
  ())

(define-condition missing-file (user-error)
  ((path :initarg :path))
  (:report
   (lambda (c s)
     (format s "missing file '~A'" (slot-value c 'path)))))

;;;

(defmacro exit-on-ctrl-c (&body body)
  `(handler-case (with-user-abort:with-user-abort (progn ,@body))
     (with-user-abort:user-abort () (adopt:exit 130))))

(defun run (args)
  (let* ((argc (length args))
         (args (if (plusp argc) args (list *standard-input*))))
    ;; Verify that files exist
    (when (plusp argc)
      (loop for input in args
            do (unless (uiop:probe-file* input)
                 (error 'missing-file :path input))))
    (let ((toplevel-name (if (= argc 1)
                             (pathname-name (uiop:probe-file* (car args)))
                             (format nil "~A" (gensym "toplevel_"))))
          (result-obj nil))
      ;; Parse files or standard input
      (loop for input in args
            do (let ((one-item (or (uiop:probe-file* input) input)))
                 (setf result-obj (process-file-or-stream one-item result-obj))))
      ;; Emit ECL record definitions
      (setf *layout-names* nil)
      (format t "~A" (as-ecl-recdef result-obj toplevel-name)))))

(defun toplevel (argv)
  (sb-ext:disable-debugger)
  (exit-on-ctrl-c
    (multiple-value-bind (arguments options) (adopt:parse-options-or-exit *ui* argv)
      (cond ((gethash 'version options)
             (format t "~A~%" #.(slot-value (asdf:find-system 'json2ecl) 'asdf:version))
             (adopt:exit))
            ((gethash 'help options)
             (adopt:print-help-and-exit *ui*)))
      (handler-case (run (cdr arguments))
        (user-error (e) (adopt:print-error-and-exit e))
        (jzon:json-error (e) (adopt:print-error-and-exit e)))))
  (adopt:exit))
