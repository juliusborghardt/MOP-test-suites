(defpackage :mop-test
  (:use :cl))
(in-package :mop-test)

;;; -----------------------------
;;; Test registry
;;; -----------------------------

(defvar *tests* (make-hash-table :test #'equal))
(defvar *test-counter* 0)

(defun define-test (name fn)
  (setf (gethash name *tests*) fn))

;;; -----------------------------
;;; Symbol lookup
;;; -----------------------------

(defun find-symbol-anywhere (name)
  (let* ((name-str (etypecase name
                     (symbol (symbol-name name))
                     (string name)))
         (target (string-upcase name-str)))
    (block found
      (do-all-symbols (sym)
        (when (and (string= (symbol-name sym) target)
		   (not (eq (symbol-package sym) (find-package :mop-test))))
	  ;; ignore anything declared here
	  
          (return-from found (values sym (symbol-package sym)))))
      (values nil nil))))

(defun ensure-symbol-found (name)
  (multiple-value-bind (sym pkg)
      (find-symbol-anywhere name)
    (declare (ignore pkg))
    (if sym
        sym
        (error "Symbol ~A not found in any package." name))))

;;; -----------------------------
;;; Test helpers
;;; -----------------------------

(defun test-exists (name)
  (handler-case
      (let ((sym (ensure-symbol-found name)))
        (format t "[OK] Symbol ~A in ~A~%"
                name (package-name (symbol-package sym)))
        t)
    (error (e)
      (format t "[FAIL] Missing ~A (~A)~%" name e)
      nil)))

(defun test-fbound (name)
  (multiple-value-bind (sym pkg) (find-symbol-anywhere name)
    (if (and sym (fboundp sym))
        (progn (format t "[OK] Fbound: ~A in ~A~%" name pkg) t)
        (progn (format t "[FAIL] Not fbound: ~A~%" name) nil))))

(defun test-classp (name)
  (let ((sym (etypecase name
		    (symbol name)
		    (string (intern name)))))
  (if (find-class sym nil)
      (progn (format t "[OK] Class: ~A in ~A~%" name (symbol-package sym)) t)
      (progn (format t "[FAIL] Not a class: ~A~%" name) nil))))

(defun run-test (name fn)
  (declare (ignore name))
  (handler-case
      (if (funcall fn)
          (values :pass nil)
          (values :fail nil))
    (error (e)
      (values :fail e))))

;;; -----------------------------
;;; MOP test definitions
;;; -----------------------------

(defparameter *required-mop-entities*
  '(
    ;; -------------------------------
    ;; Core MOP Classes
    ;; -------------------------------
    ("METAOBJECT"                         :class)
    ("STANDARD-CLASS"                     :class)
    ("METHOD-COMBINATION"                 :class)
    ("STANDARD-METHOD-COMBINATION"        :class)
    ("SHORT-METHOD-COMBINATION"           :class)
    ("LONG-METHOD-COMBINATION"            :class)
    ("GENERIC-FUNCTION"                   :class)
    ("STANDARD-GENERIC-FUNCTION"          :class)
    ("METHOD"                             :class)
    ("STANDARD-METHOD"                    :class)

    ;; -------------------------------
    ;; generic functions
    ;; -------------------------------
    ("FIND-METHOD-COMBINATION"           :function)
    ("COMPUTE-EFFECTIVE-METHOD"          :function)
    ("MAKE-INSTANCE"                     :function)
    ("CHANGE-CLASS"                      :function)
    ("REINITIALIZE-INSTANCE"             :function)
    ("UPDATE-INSTANCE-FOR-DIFFERENT-CLASS" :function)
    ("SHARED-INITIALIZE"                 :function)
    ("ENSURE-GENERIC-FUNCTION"           :function)
    ("ADD-METHOD"                        :function)
    ("REMOVE-METHOD"                     :function)
    ("SLOT-VALUE-USING-CLASS"            :function)
    ("CLASS-OF"                          :function)
    ("FINALIZE-INHERITANCE"              :function)
    ("ENSURE-CLASS-USING-CLASS"          :function)  ;; optional

   

    ;; -------------------------------
    ;; Symbols (not really needed, but supported anyway)
    ;; -------------------------------
    ;("*STANDARD-METHOD-COMBINATION*"     :symbol)
    
    
    ;; -------------------------------
    ;; Macros
    ;; -------------------------------
    ("DEFINE-METHOD-COMBINATION"         :macro)
    ("DEFGENERIC"                        :macro)
    ("DEFCLASS"                          :macro)
    ("DEFMETHOD"                         :macro)
    ))


(defun build-tests ()
  "create a test of each role a required entity has"
  (clrhash *tests*)
  (setf *test-counter* 0)
  (dolist (entry *required-mop-entities*)
    (destructuring-bind (name &rest roles) entry

      ;; Existence
      (when (member :symbol roles)
	(let ((t-exists (format nil "~A-EXISTS?" name)))
          (define-test t-exists (lambda () (test-exists name)))))

      ;; Functions / GFs
      (when (member :function roles)
        (let ((t-fbound (format nil "~A-IS-FBOUND?" name)))
          (define-test t-fbound (lambda () (test-fbound name)))))

      ;; Classes
      (when (member :class roles)
        (let ((t-class (format nil "~A-IS-CLASS?" name)))
          (define-test t-class (lambda () (test-classp name))))))))

;;; -----------------------------
;;; Test runner
;;; -----------------------------

(defun run-mop-tests ()
  (build-tests)
  (let ((total 0)
        (passed 0)
        (failed 0)
        (failed-tests '())
        (start-time (get-internal-real-time)))
    (maphash
     (lambda (name fn)
       (incf total)
       (multiple-value-bind (result err)
           (run-test name fn)
         (case result
           (:pass (incf passed))
           (:fail
            (incf failed)
            (push name failed-tests)
            (when err
              (format t "Error in ~A: ~A~%" name err))))))

     *tests*)

    (let* ((end-time (get-internal-real-time))
           (sec (/ (- end-time start-time)
                   internal-time-units-per-second)))
      (format t "~%===== MOP TEST SUMMARY =====~%")
      (format t "Tests run: ~D~%" total)
      (format t "Passed:    ~D~%" passed)
      (format t "Failed:    ~D~%" failed)
      (format t "Runtime:   ~,3F seconds~%" sec)

      ;; Print failed test list
      (when failed-tests
        (format t "~%Failed tests (~D):~%" (length failed-tests))
        (dolist (tname (reverse failed-tests))
          (format t "  - ~A~%" tname))))))



;; run
(run-mop-tests)
