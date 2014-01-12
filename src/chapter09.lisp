(in-package :cl-user)
(defpackage on-lisp.chap9
  (:use :cl))
(in-package :on-lisp.chap9)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Chapter 9. Variable Capture
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;
;;; 9.1 Macro Argument Capture
;;;

;;; Instances variable capture can be traced to one of two situations:
;;; macro arugment capture and free symbol capture.

;;;
;;; 9.2 Free Symbol Capture
;;;

;;;
;;; 9.3 When Capture Occurs
;;;

;;; Free: A symbol s occurs free in an expression when it is used as a variable
;;; in that expression, but the expression does not create a binding for it.

;;; Skeleton: The skeleton of a macro expansion is the whole expansion, minus
;;; anything which was part of an argument in the macro call.

;;; Capturable: A symbol is capturable in some macro expansion if (a) it occurs
;;; free in the skeleton of the macro expansion, or (b) it is bound by a part
;;; of the skeleton in which arguments passed to the macro are either bound or
;;; evaluated.

;;;
;;; 9.4 Avoiding Capture with Better Names
;;;

;;;
;;; 9.5 Avoiding Capture by Prior Evaluation
;;;

;; ;; Vulnerable to capture.
;; (defmacro for ((var start stop) &body body)
;;   `(do ((,var ,start (1+ ,var))
;;         (limit ,stop))
;;        ((> ,var limit))
;;      ,@body))

;; ;; Correct version using closure.
;; (defmacro for ((var start stop) &body body)
;;   `(do ((b #'(lambda (,var)
;;                ,@body))
;;         (count ,start (1+ count))
;;         (limit ,stop))
;;        ((> count limit))
;;      (funcall b count)))

;;;
;;; 9.6 Avoiding Capture with Gensyms
;;;

;; Correct version using gensym.
(defmacro for ((var start stop) &body body)
  (let ((gstop (gensym)))
    `(do ((,var ,start (1+ ,var))
          (,gstop ,stop))
         ((> ,var ,gstop))
       ,@body)))

;;;
;;; 9.7 Avoiding Capture with Packages
;;;

;;;
;;; 9.8 Capture in Other Name-Spaces
;;;

;;;
;;; 9.9 Why Bother?
;;;
