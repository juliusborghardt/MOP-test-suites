(defpackage :mop-dep-test
  (:use :cl))
(in-package :mop-dep-test)


;; these have to be global so sbcl stops giving style warnings
(defparameter *mc-test-name* 'mc-test)
(defparameter *mc2-test-name* 'mc2-test)

(defparameter *gf1-name* 'dep-gf)
(defparameter *gf2-name* 'dep2-gf)

;;; ----------------------------------------------------------
;;; Helpers
;;; ----------------------------------------------------------

(defmacro quiet (&body body)
  "Run body and ignore noise and sbcl complaints."
  `(handler-case (progn ,@body)
     (warning () nil)
     (error (e) (error e))))

(defun show-state (label gf-symbol)
  "Show current mc version for gf."
  (let ((val (funcall (symbol-function gf-symbol))))
    ;; Some Lisps (ecl again)  wrap the effective method result in additional lambdas
    (loop while (functionp val)
          do (setf val (funcall val)))
    (format t "~A => ~A~%" label val)
    val))


;;; ----------------------------------------------------------
;;; Versioned MC generator
;;; ----------------------------------------------------------

(defun define-versioned-mc (name version)
  (format t "Defining method combination ~A as version ~A~%" name version)
  (quiet
    (eval
     `(define-method-combination ,name ()
        ((* () :required t))
        ;; some lisps (ecl!) need a callable form here
        (lambda (&rest args)
          (declare (ignore args))
          ',version)))))


;;; ----------------------------------------------------------
;;; Test 1: MC -> GF propagation
;;; ----------------------------------------------------------

(defun test-mc-updates-gf ()
  (format t "~%=== Test 1: Method Combination → Generic Function ===~%")

  ;; Step 1: MC v1
  (define-versioned-mc *mc-test-name* :v1)
  ;;expands to a simple constant, more complex combination doesnt really matter
  
  ;; Step 2: Define the GF using that MC
  (quiet
    (eval
     `(defgeneric ,*gf1-name* ()
        (:method-combination ,*mc-test-name*)
        (:method () :dummy))))

  (format t "~%Initial state:~%")
  (show-state "  dep-gf returns" *gf1-name*)

  ;; Step 3: Redefine MC -> v2
  (format t "~%Redefining MC ~A => :v2~%" *mc-test-name*)
  (define-versioned-mc *mc-test-name* :v2)

  (format t "State after MC redefinition:~%")
  (let ((res (show-state "  dep-gf returns" *gf1-name*)))
    (unless (eq res :v2)
      (format t "[FAIL] GF did not update after MC changed; got ~A~%" res)
      (return-from test-mc-updates-gf nil)))

  (format t "[SUCCESS] MC -> GF dependency works.~%")
  t)

;;; ----------------------------------------------------------
;;; Test 2: GF -> MC propagation
;;; ----------------------------------------------------------

(defun test-gf-updates-mc ()
  (format t "~%=== Test 2: Generic Function → Method Combination ===~%")

  ;; Step 1: MC v1
  (define-versioned-mc *mc2-test-name* :v1)

  ;; Step 2: Define GF using MC v1
  (quiet
    (eval
     `(defgeneric ,*gf2-name* ()
        (:method-combination ,*mc2-test-name*)
        (:method () :dummy))))

  (format t "~%Initial state:~%")
  (show-state "  dep2-gf returns" *gf2-name*)

  ;; Step 3: Redefine the GF
  (format t "~%Redefining GF dep2-gf (same MC)...~%")
  (quiet
    (eval
     `(defgeneric ,*gf2-name* ()
        (:method-combination ,*mc2-test-name*)
        (:method () :other))))

  ;; Step 4: Redefine MC -> v2
  (format t "~%Redefining MC ~A => :v2~%" *mc2-test-name*)
  (define-versioned-mc *mc2-test-name* :v2)

  (format t "State after GF and MC redefinitions:~%")
  (let ((res (show-state "  dep2-gf returns" *gf2-name*)))
    (unless (eq res :v2)
      (format t "[FAIL] MC did not propagate after GF redefinition; got ~A~%" res)
      (return-from test-gf-updates-mc nil)))

  (format t "[SUCCESS] GF -> MC dependency works.~%")
  t)

;;; ----------------------------------------------------------
;;; Runner
;;; ----------------------------------------------------------

(defun run-dependency-tests (&optional (debug nil))
  (format t "~%============================================~%")
  (format t " Running Dependency Propagation Test Suite~%")
  (format t "============================================~%~%")

  (let ((ok1 (if debug ;;dev only, test will not work with this on
                 (test-mc-updates-gf)
                 (ignore-errors (test-mc-updates-gf))))
        (ok2 (if debug
                 (test-gf-updates-mc)
                 (ignore-errors (test-gf-updates-mc)))))
    (format t "~%=== Summary ===~%")
    (format t "MC -> GF update: ~A~%" (if ok1 "PASS" "FAIL"))
    (format t "GF -> MC update: ~A~%" (if ok2 "PASS" "FAIL"))))


;; run it
(run-dependency-tests)
