(defsystem "trivial-form-type"
  :license "MIT"
  :serial t
  :depends-on ("introspect-environment"
               "cl-environments"
               "alexandria"
               "fiveam"
               ;; https://github.com/digikar99/trivial-types
               ;; This solely for FUNCTION-NAME
               "trivial-types")
  :components ((:file "package")
               (:file "inferrers")
               (:file "form-type"))
  :perform (test-op (o c)
             (declare (ignore o c))
             (eval (read-from-string "(LET ((5AM:*ON-ERROR* :DEBUG)
                                            (5AM:*ON-FAILURE* :DEBUG))
                                        (5AM:RUN :TRIVIAL-FORM-TYPE))"))))
