(in-package :trivial-form-type)

(5am:def-suite* :trivial-form-type)

(defmacro lm (&rest var-body)
  `(lambda ,(butlast var-body)
     ,@(last var-body)))

(defun maybe-constant-form-value (form &key env default)
  (if (constantp form env)
      (values (constant-form-value form env) t)
      (values default nil)))


(defgeneric %form-type (first form &optional env))

(defmethod %form-type (first form &optional env)
  (cond ((typep first 'trivial-types:function-name)
         ;; These could be SETF functions
         ;; or just symbols that are not even bound to a function yet
         (cond ((macro-function first env)
                (form-type (macroexpand form env)
                           env))
               ((compiler-macro-function first env)
                (let* ((compiler-macro-function
                         (compiler-macro-function first env))
                       (expansion
                         (let ((*error-output* (make-string-output-stream)))
                           (funcall compiler-macro-function form env))))
                  (if (equalp form expansion)
                      (values t nil)
                      (form-type expansion env))))
               ((special-operator-p first)
                ;; This case should be handled by specializing %FORM-TYPE
                ;; for different FIRSTs
                (values t nil))
               ((fboundp first)
                (let* ((decl  (nth-value 2 (function-information first env)))
                       (ftype (cdr (assoc 'ftype decl))))
                  (if ftype
                      (values (third ftype) t)
                      (values t nil))))
               (t (values t nil))))
        ((eq 'lambda (first first))
         (form-type (lastcar first) env))
        (t (values t nil))))

(defmethod %form-type ((first (eql 'function)) form &optional env)
  (let* ((name  (second form))
         (decl  (nth-value 2 (function-information name env)))
         (ftype (cdr (assoc 'ftype decl))))
    (values (or ftype 'function)
            t)))

(defmethod %form-type ((first (eql 'the)) form &optional env)
  (values `(and ,(second form)
                ,(form-type (third form) env))
          t))

(def-test the ()
  (is (type= `(eql 5)
             (form-type '(the number 5))))
  (is (type= `number
             (form-type '(the number a)))))

(defmethod %form-type ((first (eql 'make-instance)) form &optional env)
  (maybe-constant-form-value (second form) :env env :default t))


(defmethod %form-type ((first (eql 'make-array)) form &optional env)
  (let* ((dimensions (let ((dim-form (second form)))
                       (if (constantp dim-form env)
                           (ensure-list (constant-form-value dim-form env))
                           'cl:*)))
         ;; Make sure that a keyword argument does not
         ;; come from a non-constant form, for example:
         ;;   (make-array 2 (identity :displaced-to) #(0 0 0))
         (constant-key-forms-p (loop :for i :from 0
                                     :for sub-form :in (nthcdr 2 form)
                                     ;; :do (print (list i sub-form
                                     ;;                  (constantp sub-form *environment*)))
                                     :always (or (oddp i)
                                                 (constantp sub-form env)))))
    (if (not constant-key-forms-p)
        `(array * ,dimensions)
        (let* ((element-type (let ((key-pos
                                     (position
                                      :element-type form
                                      :key (lm form (maybe-constant-form-value form :env env)))))
                               (if (not key-pos)
                                   t
                                   (let ((elt-form (elt form (1+ key-pos))))
                                     (maybe-constant-form-value elt-form env 'cl:*)))))

               ;; Assume that we can know if or not the array is adjustable

               (adjustable-p-p t)
               (adjustable-p   (let ((key-pos (position :adjustable form)))
                                 (if (not key-pos)
                                     nil
                                     (let ((value-form (elt form (+ 1 key-pos))))
                                       (if (constantp value-form env)
                                           ;; It may be simple if it is not adjustable
                                           (constant-form-value value-form env)
                                           ;; In this case, we do not even know if or
                                           ;; not the array is adjustable
                                           (setq adjustable-p-p nil))))))
               (fill-pointer-p (find :fill-pointer form
                                     :key (lm form (maybe-constant-form-value form :env env))))
               (displaced-p    (intersection
                                '(:displaced-index-offset :displaced-to)
                                form
                                :key (lm form (maybe-constant-form-value form :env env)))))

          (let ((simple-p (if (null adjustable-p-p)
                              nil ; we cannot tell if or not the array is adjustable
                              (and (null adjustable-p)
                                   (null fill-pointer-p)
                                   (null displaced-p)))))

            (if simple-p
                `(simple-array ,element-type ,dimensions)
                `(array ,element-type ,dimensions)))))))

(def-test make-array ()
  (is (equalp '(array * (3)) (form-type '(make-array 3 (first a) #(0 0 0)))))
  (is (equalp '(simple-array t (3))
              (form-type '(make-array 3 :initial-contents #(0 0 0)))))
  (is (equalp '(simple-array t *)
              (form-type '(make-array a :initial-contents #(0 0 0)))))
  (is (equalp '(array t *)
              (form-type '(make-array a :displaced-to #(0 0 0))))))


(defmethod %form-type ((first (eql 'make-string)) form &optional env)
  (let* ((length (let ((length-form (second form)))
                   (if (constantp length-form env)
                       (constant-form-value length-form env)
                       'cl:*)))
         ;; Make sure that a keyword argument does not
         ;; come from a non-constant form, for example:
         ;;   (make-array 2 (identity :displaced-to) #(0 0 0))
         (constant-key-forms-p (loop :for i :from 0
                                     :for sub-form :in (nthcdr 2 form)
                                     ;; :do (print (list i sub-form
                                     ;;                  (constantp sub-form env)))
                                     :always (or (oddp i)
                                                 (constantp sub-form env)))))
    (if (not constant-key-forms-p)
        `(simple-string ,length)
        (let* ((element-type (if-let (key-pos
                                      (position
                                       :element-type form
                                       :key (lm form (maybe-constant-form-value form :env env))))
                               (let ((elt-form (elt form (1+ key-pos))))
                                 (maybe-constant-form-value elt-form :env env :default 'cl:*))
                               'character)))
          (if (type= element-type 'base-char)
              `(simple-base-string ,length)
              `(simple-string ,length))))))


(def-test make-string ()
  (is (equalp '(simple-string 3) (form-type '(make-string 3))))
  (is (equalp '(simple-string cl:*) (form-type '(make-string a))))
  (is (equalp '(simple-base-string 3)
              (form-type '(make-string 3 :element-type 'base-char))))
  (is (equalp '(simple-base-string cl:*)
              (form-type '(make-string a :element-type 'base-char)))))
