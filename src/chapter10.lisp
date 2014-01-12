(in-package :cl-user)
(defpackage on-lisp.chap10
  (:use :cl))
(in-package :on-lisp.chap10)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Chapter 10. Other Macro Pitfalls
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;
;;; 10.1 Number of Evaluations
;;;

;;;
;;; 10.2 Order of Evaluation
;;;

;;;
;;; 10.3 Non-functional Expanders
;;;

;;; As a general rule, expander code shouldn't depend on anything except its
;;; arguments.

;;;
;;; 10.4 Recursion
;;;

;;; It's find for macros to contain references to other macros, so long as
;;; expansion terminates somewhere.

(defun ntha (n lst)
  (if (= n 0)
      (car lst)
      (ntha (- n 1) (cdr lst))))

;; This won't compile
;; (defmacro nthb (n lst)
;;   `(if (= ,n 0)
;;        (car ,lst)
;;        (nthb (- ,n 1) (cdr ,lst))))

;; A tail-recursive function can easily be transformed into an iterative
;; equivalent, and then used as a model for macro.
(defmacro nthc (n lst)
  `(do ((n2 ,n (1- n2))
        (lst2 ,lst (cdr lst2)))
       ((= n2 0) (car (lst2)))))

(defmacro nthd (n lst)
  `(nth-fn ,n ,lst))

(defun nth-fn (n lst)
  (if (= n 0)
      (car lst)
      (nth-fn (- n 1) (cdr lst))))

(defmacro nthe (n lst)
  `(labels ((nth-fn (n lst)
              (if (= n 0)
                  (car lst)
                  (nth-fn (- n 1) (cdr lst)))))
     (nth-fn ,n ,lst)))

;; Recursive expansion functions.
(defmacro ora (&rest args)
  (or-expand args))

(defun or-expand (args)
  (if (null args)
      nil
      (let ((sym (gensym)))
        `(let ((,sym ,(car args)))
           (if ,sym
               ,sym
               ,(or-expand (cdr args)))))))

(defmacro orb (&rest args)
  (if (null args)
      nil
      (let ((sym (gensym)))
        `(let ((,sym ,(car args)))
           (if ,sym
               ,sym
               (orb ,@(cdr args)))))))
