(in-package :cl-user)
(defpackage on-lisp.chap14
  (:use :cl))
(in-package :on-lisp.chap14)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Chapter 14. Anaphoric Macros
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;
;;; 14.1 Anaphoric Variants
;;;

;;; An anaphor, it turns out, is a lot like a captured symbol. We can use anaphora
;;; in programs by designating certain symbols to serve as pronouns, and then
;;; writing macros intentionally to capture these symbols.

;; Anaphoric variants of Common Lisp operators.
(defmacro aif (test-form then-form &optional else-form)
  `(let ((it ,test-form))
     (if it
         ,then-form
         ,else-form)))

(defmacro awhen (test-form &body body)
  `(aif ,test-form
        (progn ,@body)))

(defmacro awhile (expr &body body)
  `(do ((it ,expr ,expr))
       ((not it))
     ,@body))

(defmacro aand (&rest args)
  (cond
    ((null args) t)
    ((null (cdr args)) (car args))
    (t `(aif ,(car args)
             (aand ,@(cdr args))))))

(defmacro acond (&rest clauses)
  (if (null clauses)
      nil
      (let ((cl1 (car clauses))
            (sym (gensym)))
        `(let ((,sym ,(car cl1)))
           (if ,sym
               (let ((it ,sym))
                 ,@(cdr cl1))
               (acond ,@(cdr clauses)))))))

(defmacro alambda (parms &body body)
  `(labels ((self ,parms ,@body))
     #'self))

(defmacro ablock (tag &rest args)
  `(block ,tag
     ,(funcall (alambda (args)
                        (case (length args)
                          (0 nil)
                          (1 (car args))
                          (t `(let ((it ,(car args)))
                                ,(self (cdr args)))))) args)))

;;;
;;; 14.2 Failure
;;;

;; Multiple-value anaphoric macros.
(defmacro aif2 (test &optional then else)
  (let ((win (gensym)))
    `(multiple-value-bind (it ,win) ,test
       (if (or it ,win)
           ,then
           ,else))))

(defmacro awhen2 (test &body body)
  `(aif2 ,test
         (progn ,@body)))

(defmacro awhile2 (test &body body)
  (let ((flag (gensym)))
    `(let ((,flag t))
       (while ,flag
         (aif2 ,test
               (progn ,@body)
               (setf ,flag nil))))))

(defmacro acond2 (&rest clauses)
  (if (null clauses)
      nil
      (let ((cl1 (car clauses))
            (val (gensym))
            (win (gensym)))
        `(multiple-value-bind (,val ,win) ,(car cl1)
           (if (or ,val ,win)
               (let ((it ,val))
                 ,@(cdr cl1))
               (acond2 ,@(cdr clauses)))))))

;; File utilities.
(let ((g (gensym)))
  (defun read2 (&optional (str *standard-input*))
    (let ((val (read str nil g)))
      (unless (equal val g)
        (values val t)))))

(defmacro do-file (filename &body body)
  (let ((str (gensym)))
    `(with-open-file (,str ,filename)
       (awhile2 (read2 ,str)
         ,@body))))

;;;
;;; 14.3 Referential Transparency
;;;

;;; A language is referentially transparent if (a) every subexpression can be
;;; replaced by any other that's equal to it in value and (b) all occurrences
;;; of an expression within a given context yield the same value.

;;; No language with assignment is referentially transparent.
