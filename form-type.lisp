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



(defun expand (type-specifier &optional env)
  "Expands the TYPE-SPECIFIER to a canonical representation in the ENV."
  ;; TODO Fix for other then SBCL to accept the environment.
  ;; Since this isn't my code, I'm unsure what exactly "fix" was supposed to mean
  (declare (ignorable env))
  #+abcl          (system::expand-deftype type-specifier)
  #+xcl           (system::expand-deftype type-specifier)
  #+allegro       (excl:normalize-type type :default type-specifier)
  #+ccl           (ccl::type-expand type-specifier)
  #+clisp         (ext:type-expand type-specifier)
  #+cmu           (kernel:type-expand type-specifier)
  #+ecl           (si::expand-deftype type-specifier)
  #+mkcl          (si::expand-deftype type-specifier)
  #+lispworks     (type:expand-user-type type-specifier)
  #+sbcl          (sb-ext:typexpand-all type-specifier env)
  #-(or abcl allegro ccl clisp cmu ecl lispworks mkcl sbcl xcl)
  (assert nil nil "EXPAND unimplemented."))
