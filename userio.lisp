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

(defparameter *ui*
  (adopt:make-interface :name "json2ecl"
                        :usage "[OPTIONS] FILE ..."
                        :summary (format nil "analyze JSON data and emit ECL record ~
                                              definitions that can parse that data")
                        :help (format nil "json2ecl examines JSON data and deduces ~
                                           the ECL RECORD definitions necessary to parse it.")
                        :contents (list
                                   *option-version*
                                   *option-help*)))

;;;

(define-condition user-error (error) ())

;;;

(defmacro exit-on-ctrl-c (&body body)
  `(handler-case (with-user-abort:with-user-abort (progn ,@body))
     (with-user-abort:user-abort () (adopt:exit 130))))

(defun run (args)
  (let ((argc (length args)))
    (when (plusp argc)
      (let ((toplevel-name (if (= argc 1)
                               (pathname-name (uiop:probe-file* (car args)))
                               (format nil "~A" (gensym "toplevel_"))))
            (result-obj nil))
        (loop for input in args
              do (setf result-obj (process-file (uiop:probe-file* input) result-obj)))
        (setf *layout-names* nil)
        (format t "~A" (as-ecl-recdef result-obj toplevel-name))))))

(defun toplevel (argv)
  (sb-ext:disable-debugger)
  (exit-on-ctrl-c
    (multiple-value-bind (arguments options) (adopt:parse-options-or-exit *ui* argv)
      (cond ((gethash 'version options)
             (format t "~A~%" (slot-value (asdf:find-system 'json2ecl) 'asdf:version))
             (adopt:exit))
            ((gethash 'help options)
             (adopt:print-help-and-exit *ui*)))
      (handler-case (run (cdr arguments))
        (user-error (e) (adopt:print-error-and-exit e)))))
  (adopt:exit))
