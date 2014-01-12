(in-package :cl-user)
(defpackage on-lisp.chap8
  (:use :cl))
(in-package :on-lisp.chap8)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Chapter 8. When to Use Macros
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; By default we should use functions. We should use macros only where they
;;; bring us some specific advantage.

;;;
;;; 8.1 When Nothing Else Will Do
;;;

;;; Macros can control (or prevent) the evaluation of their arguments, and they
;;; are expanded right into the calling context.

;;; 1. Transformation (setf ...).
;;; 2. Binding (let ...).
;;; 3. Conditional evaluation (when ...).
;;; 4. Multiple evaluation (do ...).
;;; 5. Using the calling environment.
;;; 6. Wrapping a new environment.
;;; 7. Saving function calls.

;;;
;;; 8.2 Macro or Function?
;;;

;;; Any operator that needs access to its parameters before they are evaluated
;;; should be written as a macro.

;;; At compile-time we may not know the values of the arguments, but we do know
;;; how many there are.
;;; (defun avg (&rest args)
;;;   (/ (apply #'+ args) (length args)))
;;; ->
(defmacro avg (&rest args)
  `(/ (+ ,@args) ,(length args)))

;;; The Pros:
;;; 1. Computation at compile-time.
;;; 2. Integration with Lisp.
;;; 3. Saving function calls.

;;; The Cons:
;;; 4. Functions are data, while macros are more like instructions to the
;;; compiler.
;;; 5. Clarity of source code.
;;; 6. Clarity at runtime.
;;; 7. Recursion.

;;;
;;; 8.3 Applications for Macros
;;;

;;; Macros allow you to write whole programs in a language distinctly different
;;; from Lisp. Macros used in this way are said to implement embedded languages.

;;; When you notice a pattern in your code, consider turning it into a utility.

(defmacro our-defun (name parms &body body)
  `(progn
     (setf (symbol-function ',name)
           #'(lambda ,parms
               (block ,name ,@body)))
     ',name))
