(defpackage :trivial-form-type
  (:use :cl :cl-environments.cltl2 :alexandria)
  (:import-from
   :introspect-environment
   :constant-form-value)
  (:import-from
   :fiveam
   :def-test
   :is)
  (:export
   :form-type
   :form-typep))
