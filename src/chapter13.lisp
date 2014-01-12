(in-package :cl-user)
(defpackage on-lisp.chap13
  (:use :cl)
  (:import-from :on-lisp.chap11
                :with-gensyms)
  (:import-from :on-lisp.chap4
                :map0-n))
(in-package :on-lisp.chap13)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Chapter 13. Computation at Compile-Time
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;
;;; 13.1 New Utilities
;;;

;; (defun most-of (&rest args)
;;   (let ((all 0)
;;         (hits 0))
;;     (dolist (a args)
;;       (incf all)
;;       (if a
;;           (incf hits)))
;;     (> hits (/ all 2))))

;; Shifting and avoiding computation.
(defmacro most-of (&rest args)
  (let ((need (floor (/ (length args) 2)))
        (hits (gensym)))
    `(let ((,hits 0))
       (or ,@(mapcar #'(lambda (a)
                         `(and ,a (> (incf ,hits) ,need))) args)))))

;; Use of arguments known at compile-time.
;; (defun nthmost (n lst)
;;   (nth n (sort (copy-list lst) #'>)))

(defmacro nthmost (n lst)
  (if (and (integerp n) (< n 20))
      (with-gensyms (glst gi)
        (let ((syms (map0-n #'(lambda (x)
                                (declare (ignore x))
                                (gensym)) n)))
          `(let ((,glst ,lst))
             (unless (< (length ,glst) ,(1+ n))
               ,@(gen-start glst syms)
               (dolist (,gi ,glst)
                 ,(nthmost-gen gi syms t))
               ,(car (last syms))))))
      `(nth ,n (sort (copy-list ,lst) #'>))))

(defun gen-start (glst syms)
  (reverse
   (maplist #'(lambda (syms)
                (let ((var (gensym)))
                  `(let ((,var (pop ,glst)))
                     ,(nthmost-gen var (reverse syms))))) (reverse syms))))

(defun nthmost-gen (var vars &optional long?)
  (if (null vars)
      nil
      (let ((else (nthmost-gen var (cdr vars) long?)))
        (if (and (not long?) (null else))
            `(setq ,(car vars) ,var)
            `(if (> ,var ,(car vars))
                 (setq ,@(mapcan #'list (reverse vars) (cdr (reverse vars)))
                       ,(car vars) ,var)
                 ,else)))))

;;;
;;; 13.2 Example: Bezier Curves
;;;

;;;
;;; 13.3 Applications
;;;
