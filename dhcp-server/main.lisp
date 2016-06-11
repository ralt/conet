(in-package #:conet-dhcp-server)

(opts:define-opts
  (:name :help
   :description "print this help text"
   :short #\h
   :long "help")
  (:name :log-level
   :description "log level. Available: trace, debug, info, warn, error, fatal"
   :short #\l
   :long "log-level"
   :arg-parser #'identity
   :meta-var "LEVEL"))

(defvar *log-levels* '(:trace :debug :info :warn :error :fatal))

(defun fatal (msg &rest args)
  (apply #'format t (format nil "fatal: ~A~%" msg) args)
  (uiop:quit -1))

(defmacro when-option ((options opt) &body body)
  `(let ((it (getf ,options ,opt)))
     (when it
       ,@body)))

(defun help ()
  (opts:describe
   :prefix "DHCP server"
   :usage-of "conet-dhcp-server"))

(defun main (args)
  (multiple-value-bind (options _)
      (handler-case
          (opts:get-opts args)
        (opts:unknown-option (condition)
          (fatal "option ~S is unknown" (opts:option condition)))
        (opts:missing-arg (condition)
          (fatal "option ~S needs an argument" (opts:option condition)))
        (opts:arg-parser-failed (condition)
          (fatal "cannot parse ~S as argument of ~S"
                 (opts:raw-arg condition)
                 (opts:option condition))))
    (declare (ignore _))
    (when (= (length args) 1)
      (help))
    (when-option (options :help)
      (help))
    (when-option (options :log-level)
      (let ((log-level (intern (string-upcase it) :keyword)))
        (unless (member log-level *log-levels*)
          (fatal "unknown log level: ~S" it))
        (log:config log-level)))))
