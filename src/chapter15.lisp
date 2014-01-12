(in-package :cl-user)
(defpackage on-lisp.chap15
  (:use :cl)
  (:import-from :on-lisp.chap4
                :mklist
                :map1-n)
  (:import-from :on-lisp.chap5
                :lrec
                :trec))
(in-package :on-lisp.chap15)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Chapter 15. Macros Returning Functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;
;;; 15.1 Building Functions
;;;

;; General function-building macro.
(defmacro fn (expr)
  `#',(rbuild expr))

(defun rbuild (expr)
  (if (or (atom expr) (eq (car expr) 'lambda))
      expr
      (if (eq (car expr) 'compose)
          (build-compose (cdr expr))
          (build-call (car expr) (cdr expr)))))

(defun build-call (op fns)
  (let ((g (gensym)))
    `(lambda (,g)
       (,op ,@(mapcar #'(lambda (f)
                          `(,(rbuild f) ,g)) fns)))))

(defun build-compose (fns)
  (let ((g (gensym)))
    `(lambda (,g)
       ,(labels ((rec (fns)
                      (if fns
                          `(,(rbuild (car fns)) ,(rec (cdr fns)))
                          g)))
                (rec fns)))))

;;;
;;; 15.2 Recursion on Cdrs
;;;

;; Macros for list recursion.
(defmacro alrec (rec &optional base)
  (let ((gfn (gensym)))
    `(lrec #'(lambda (it ,gfn)
               (symbol-macrolet ((rec (funcall ,gfn)))
                 ,rec)) ,base)))

(defmacro on-cdrs (rec base &rest lsts)
  `(funcall (alrec ,rec #'(lambda ()
                            ,base)) ,@lsts))

;; Common Lisp functions defined with on-cdrs.
(defun our-copy-list (lst)
  (on-cdrs (cons it rec) nil lst))

(defun our-remove-duplicates (lst)
  (on-cdrs (adjoin it rec) nil lst))

(defun our-find-if (fn lst)
  (on-cdrs (if (funcall fn it)
               it
               rec) nil lst))

(defun our-some (fn lst)
  (on-cdrs (or (funcall fn it) rec) nil lst))

;; New utilities defined with on-cdrs.
(defun unions (&rest sets)
  (on-cdrs (union it rec) (car sets) (cdr sets)))

(defun intersections (&rest sets)
  (unless (some #'null sets)
    (on-cdrs (intersection it rec) (car sets) (cdr sets))))

(defun maxmin (args)
  (when args
    (on-cdrs (multiple-value-bind (mx mn) rec
               (values (max mx it) (min mn it)))
             (values (car args) (cdr args))
             (cdr args))))

;;;
;;; 15.3 Recursion on Subtrees
;;;

;; Macros for recursion on trees.
(defmacro atrec (rec &optional (base 'it))
  (let ((lfn (gensym))
        (rfn (gensym)))
    `(trec #'(lambda (it ,lfn ,rfn)
               (symbol-macrolet ((left (funcall ,lfn))
                                 (right (funcall ,rfn)))
                 ,rec))
           #'(lambda (it)
               ,base))))

(defmacro on-trees (rec base &rest trees)
  `(funcall (atrec ,rec ,base) ,@trees))

;; Functions defined using on-trees.
(defun our-copy-tree (tree)
  (on-trees (cons left right) it tree))

(defun count-leaves (tree)
  (on-trees (+ left (or right 1)) 1 tree))

(defun flatten (tree)
  (on-trees (nconc left right) (mklist it) tree))

(defun rfind-if (fn tree)
  (on-trees (or left right)
            (and (funcall fn it) it)
            tree))

;;;
;;; 15.4 Lazy Evaluation
;;;

;; Implementation of force and delay.
(defparameter *unforced* (gensym))

(defstruct delay
  forced
  closure)

(defmacro delay (expr)
  (let ((self (gensym)))
    `(let ((,self (make-delay :forced *unforced*)))
       (setf (delay-closure ,self) #'(lambda ()
                                       (setf (delay-forced ,self) ,expr)))
       ,self)))

(defun force (x)
  (if (delay-p x)
      (if (eq (delay-forced x) *unforced*)
          (funcall (delay-closure x))
          (delay-forced x))
      x))
