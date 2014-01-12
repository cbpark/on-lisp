(in-package :cl-user)
(defpackage on-lisp.chap12
  (:use :cl)
  (:import-from :on-lisp.chap11
                :with-gensyms
                :nif)
  (:export :_f))
(in-package :on-lisp.chap12)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Chapter 12. Generalized Variables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;
;;; 12.1 The Concept
;;;

;;; If the first argument (after macroexpansion) is a symbol, the setf just
;;; expands into a setq. But if the first argument is s query, the setf
;;; expands into the corresponding assertion.

;;; An expression which can serve as the first argument to setf is called a
;;; generalized variable.

;;;
;;; 12.2 The Multiple Evaluation Problem
;;;

;; Macros which operate on generalized variables.
(defmacro allf (val &rest args)
  (with-gensyms (gval)
    `(let ((,gval ,val))
       (setf ,@(mapcan #'(lambda (a)
                           (list a gval)) args)))))

(defmacro nilf (&rest args)
  `(allf nil ,@args))

(defmacro tf (&rest args)
  `(allf t ,@args))

(defmacro toggle (&rest args)
  `(progn
     ,@(mapcar #'(lambda (a)
                   `(toggle2 ,a)) args)))

(define-modify-macro toggle2 () not)

;;;
;;; 12.3 New Utilities
;;;

;; List operations on generalized variables.
(define-modify-macro concf (obj) nconc)

(define-modify-macro conc1f (obj)
  (lambda (place obj)
    (nconc place (list obj))))

(define-modify-macro concnew (obj &rest args)
  (lambda (place obj &rest args)
    (unless (apply #'member obj place args)
      (nconc place (list obj)))))

;;; If you're planning to build a list by adding elements to the end, it may be
;;; preferable to use push, and then nreverse the list. It is cheaper to do
;;; something to the front of a list than to the end, because to do something
;;; to the end you have to get there first.

;;;
;;; 12.4 More Complex Utilities
;;;

(defmacro _f (op place &rest args)
  (multiple-value-bind (vars forms var set access)
      (get-setf-expansion place)
    `(let* (,@(mapcar #'list vars forms)
            (,(car var) (,op ,access ,@args)))
       ,set)))

(defmacro pull (obj place &rest args)
  (multiple-value-bind (vars forms var set access)
      (get-setf-expansion place)
    (let ((g (gensym)))
      `(let* ((,g ,obj)
              ,@(mapcar #'list vars forms)
              (,(car var) (delete ,g ,access ,@args)))
         ,set))))

(defmacro pull-if (test place &rest args)
  (multiple-value-bind (vars forms var set access)
      (get-setf-expansion place)
    (let ((g (gensym)))
      `(let* ((,g ,test)
              ,@(mapcar #'list vars forms)
              (,(car var) (delete-if ,g ,access ,@args)))
         ,set))))

(defmacro popn (n place)
  (multiple-value-bind (vars forms var set access)
      (get-setf-expansion place)
    (with-gensyms (gn glst)
      `(let* ((,gn ,n)
              ,@(mapcar #'list vars forms)
              (,glst ,access)
              (,(car var) (nthcdr ,gn ,glst)))
         (prog1 (subseq ,glst 0 ,gn)
           ,set)))))

(defmacro sortf (op &rest places)
  (let* ((meths (mapcar #'(lambda (p)
                            (multiple-value-list (get-setf-expansion p)))
                        places))
         (temps (apply #'append (mapcar #'third meths))))
    `(let* ,(mapcar #'list
                    (mapcan #'(lambda (m)
                                (append (first m) (third m))) meths)
                    (mapcan #'(lambda (m)
                                (append (second m) (list (fifth m)))) meths))
       ,@(mapcon #'(lambda (rest)
                     (mapcar #'(lambda (arg)
                                 `(unless (,op ,(car rest) ,arg)
                                    (rotatef ,(car rest) ,arg)))
                             (cdr rest))) temps)
       ,@(mapcar #'fourth meths))))

;;;
;;; 12.5 Defining Inversions
;;;
