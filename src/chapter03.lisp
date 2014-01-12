(in-package :cl-user)
(defpackage on-lisp.chap3
  (:use :cl))
(in-package :on-lisp.chap3)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Chapter 3. Functional Programming
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;
;;; 3.1 Functional Design
;;;

;; A function to reverse lists: destructive.
;; -- nreverse
(defun bad-reverse (lst)
  (let* ((len (length lst))
         (ilimit (truncate (/ len 2))))
    (do ((i 0 (1+ i))
         (j (1- len) (1- j)))
        ((>= i ilimit))
      (rotatef (nth i lst) (nth j lst)))))

;;; A destructive function is one that can alter ther arguments passed to it.

;; A function to return reversed lists.
;; -- reverse
(defun good-reverse (lst)
  (labels ((rev (lst acc)
             (if (null lst)
                 acc
                 (rev (cdr lst) (cons (car lst)
                                      acc)))))
    (rev lst nil)))

;;; Having functional programming as an ideal doesn't imply that programs should
;;; never have side-effects. It just means that they should have no more than
;;; necessary.

(defun powers (x)
  (values x (sqrt x) (expt x 2)))

;; (multiple-value-bind (base root square) (powers 4)
;;         (list base root square)) ;-> (4 2 16)

;;;
;;; 3.2 Imperative Outside-In
;;;
;;; A functional program tells you what it wants; an imperative program tells
;;; you what to do.

;; Functional style: Return a list of a and the square of the first element of x.
(defun fun (x)
  (list 'a (expt (car x) 2)))

;; Imperative style: Get the first element of x, then square it, then return
;; a list of a and the square.
(defun imp (x)
  (let (y sqr) ; uninitialized variables.
    (setf y (car x))
    (setf sqr (expt y 2))
    (list 'a sqr))) ; <- inside-out since sqr is set outside.

;;; An imperative program is a functional program turned inside-out.

;;;
;;; 3.3 Functional Interfaces
;;;
;; It preserves referential transparency.
(defun qualify (expr)
  (nconc (copy-list expr) (list 'maybe)))

;;; Informally, we could say that it's harmless for a function to modify
;;; something that no one else owns.

(defun ok (x)
  (nconc (list 'a x) (list 'c)))

(defun not-ok (x)
  (nconc (list 'a) x (list 'c)))

;;; Functions can't share objects with other code that doesn't follow the rules.

;;; Code written so that each invocation only modifies what it owns is almost as
;;; good as purely functional code.

;; One should avoid writing functions whose return values incorporate
;; quoted objects.
;; Exception: the functions which generate macro expansions.
(defun exclaim (expression)
  ;; (append expression '(oh my)))
  (append expression (list 'oh 'my)))

;;;
;;; 3.4 Interactive Programming
;;;

;;; 1. They try to segregate side-effects in a new functions, allowing the
;;; greater part of the program to be written in a purely functional style.
;;; 2. If a function perform side-effects, they try at least to give it a
;;; functional interface.
;;; 3. They give each function a singlet, well-defined purpose.
