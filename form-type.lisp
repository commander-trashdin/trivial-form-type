(in-package :trivial-form-type)

(5am:in-suite :trivial-form-type)

(defun form-typep (form type &optional env)
  (if (eq t type)
      (values t t)
      (multiple-value-bind (form-type known) (form-type form env)
        (if known
            (values (subtypep form-type type env)
                    t)
            (values nil nil)))))

(defun form-type (form &optional env)
  "Returns two values: the first value is the TYPE of FORM if the second value is T"
  (cond ((constantp form env)
         (values `(eql ,(constant-form-value form env))
                 t))
        ((symbolp form)
         (multiple-value-bind (type local-p declarations) (variable-information form env)
           (declare (ignore local-p))
           (if (eq type :symbol-macro)
               (form-type (macroexpand form env) env)
               (let ((type (cdr (assoc 'type declarations))))
                 (if type
                     (values type t)
                     (values t nil))))))
        ((listp form)
         (%form-type (first form) form env))
        (t
         (error "~%We shouldn't have reached here!"))))

(def-test form-type ()
  (5am:is-false (nth-value 1 (form-type '#:unknown-symbol)))
  (5am:is-true  (nth-value 0 (form-type '#:unknown-symbol)))
  (5am:is-true  (nth-value 1 (form-type '(the t #:unknown-symbol))))
  (5am:is-false (nth-value 1 (form-type '(#:unknown-function 5))))
  (5am:is-true  (nth-value 0 (form-type '(#:unknown-function 5))))
  (5am:is-true  (nth-value 1 (form-type '(the t (#:unknown-function 5))))))
