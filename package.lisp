(defpackage :trivial-form-type
  (:use :cl :cl-environments.cltl2 :alexandria)
  (:import-from
   :introspect-environment
   :constant-form-value)
  (:shadow :type=)
  (:import-from
   :fiveam
   :def-test
   :is)
  (:export
   :form-type
   :form-typep))

(in-package :trivial-form-type)

(defun type= (type1 type2 &optional env)
  (and (subtypep type1 type2 env)
       (subtypep type2 type1 env)))
