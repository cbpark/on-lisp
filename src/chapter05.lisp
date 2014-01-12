(in-package :cl-user)
(defpackage on-lisp.chap5
  (:use :cl)
  (:import-from :on-lisp.chap4
                :mklist)
  (:export :lrec
           :trec
           :compose))
(in-package :on-lisp.chap5)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Chapter 5. Returning Functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;
;;; 5.1 Common Lisp Evolves
;;;

;;; (remove-if-not #'pred lst)
;;; = (remove-if #'(lambda (x) (not (pred x))) lst)
;;; = (remove-if (complement #'pred) lst)

;; (defun complement (fn)
;;   #'(lambda (&rest args)
;;       (not (apply fn args))))

;;; Under lexical scope, instead of merely choosing among a group of constant
;;; functions, we can build new closures at runtime.

;;;
;;; 5.2 Orthogonality
;;;

;;; An orthogonal language is one in which you can express a lot by combining
;;; a small number of operators in a lot of different ways.

;;; By defining an operator to return the destructive counterpart of a function,
;;; we would not have to refer to the destructive functions directly.

;;; Returning destructive equivalents.

(defparameter *!equivs* (make-hash-table))

(defun ! (fn)
  (or (gethash fn *!equivs*) fn))

(defun def! (fn fn!)
  (setf (gethash fn *!equivs*) fn!))

;;;
;;; 5.3 Memoizing
;;;

;;; Memoize: to cache the return values of all the previous calls, and each time
;;; the function is about to be called, to look first in the cache to see if the
;;; value is already known.

;; Memoizing utility.
(defun memoize (fn)
  (let ((cache (make-hash-table :test #'equal)))
    #'(lambda (&rest args)
        (multiple-value-bind (val win)
            (gethash args cache)
          (if win
              val
              (setf (gethash args cache)
                    (apply fn args)))))))

;;;
;;; 5.4 Composing functions
;;;

;;; If f and g are functions, then f . g is also a function, and
;;; f . g(x) = f(g(x)).

;; An operator for functional composition.
(defun compose (&rest fns)
  (if fns
      (let ((fn1 (car (last fns)))
            (fns (butlast fns)))
        #'(lambda (&rest args)
            (reduce #'funcall fns
                    :from-end t
                    :initial-value (apply fn1 args))))
      #'identity))

;; (defun complement (pred)
;;   (compose #'not pred))

;;; Function builders.

(defun fif (if then &optional else)
  #'(lambda (x)
      (if (funcall if x)
          (funcall then x)
          (if else
              (funcall else x)))))

(defun fint (fn &rest fns)
  (if (null fns)
      fn
      (let ((chain (apply #'fint fns)))
        #'(lambda (x)
            (and (funcall fn x) (funcall chain x))))))

(defun fun (fn &rest fns)
  (if (null fns)
      fn
      (let ((chain (apply #'fun fns)))
        #'(lambda (x)
            (or (funcall fn x) (funcall chain x))))))

;;;
;;; 5.5 Recursion of Cdrs
;;;

(defun our-length (lst)
  (if (null lst)
      0
      (1+ (our-length (cdr lst)))))

(defun our-every (fn lst)
  (if (null lst)
      t
      (and (funcall fn (car lst))
           (our-every fn (cdr lst)))))

;; Function to define flat list recursers.
(defun lrec (rec &optional base)
  (labels ((self (lst)
             (if (null lst)
                 (if (functionp base)
                     (funcall base)
                     base)
                 (funcall rec (car lst) #'(lambda ()
                                            (self (cdr lst)))))))
    #'self))

;; (funcall (lrec #'(lambda (x f) (1+ (funcall f))) 0) '(1 2 3 4 5))
;; (funcall (lrec #'(lambda (x f) (and (oddp x) (funcall f))) t) '(1 2 3 4 5))

;; copy-list
;; (lrec #'(lambda (x f) (cons x (funcall f))))

;; remove-duplicates
;; (lrec #'(lambda (x f) (adjoin x (funcall f))))

;; find-if, for some function fn
;; (lrec #'(lambda (x f) (if (fn x) x (funcall f))))

;; some, for some function fn
;; (lrec #'(lambda (x f) (or (fn x) (funcall f))))

;;; They are best suited for use in initial version of a program, or in parts
;;; where speed is not critical, for they tend to one away from tail-recursive
;;; solutions.

;;;
;;; 5.6 Recursion on Subtrees
;;;

(defun our-copy-tree (tree)
  (if (atom tree)
      tree
    (cons (our-copy-tree (car tree))
          (if (cdr tree)
              (our-copy-tree (cdr tree))))))

;; The leaves of a tree are all the atoms you can see when you look at the tree
;; in its dotted-pair representation.
(defun count-leaves (tree)
  (if (atom tree)
      1
    (+ (count-leaves (car tree))
       (or (if (cdr tree)
               (count-leaves (cdr tree)))
           1))))

(defun flatten (tree)
  (if (atom tree)
      (mklist tree)
    (nconc (flatten (car tree))
           (if (cdr tree)
               (flatten (cdr tree))))))

(defun rfind-if (fn tree)
  (if (atom tree)
      (and (funcall fn tree) tree)
      (or (rfind-if fn (car tree))
          (if (cdr tree)
              (rfind-if fn (cdr tree))))))

;;; 1. In the base case it returns its argument.
;;; 2. In the recursive case, it applies cons to the recursion down the left
;;; (car) and right (cdr) subtrees.

;; Tree traverser.
(defun ttrav (rec &optional (base #'identity))
  (labels ((self (tree)
             (if (atom tree)
                 (if (functionp base)
                     (funcall base tree)
                     base)
                 (funcall rec (self (car tree))
                          (if (cdr tree)
                              (self (cdr tree)))))))
    #'self))

;; our-copy-tree.
(ttrav #'cons)

;; count-leaves.
(ttrav #'(lambda (l r)
           (+ l (or r l))) 1)

;; flatten
(ttrav #'nconc #'mklist)

;; Function for recursion on trees.
(defun trec (rec &optional (base #'identity))
  (labels ((self (tree)
             (if (atom tree)
                 (if (functionp base)
                     (funcall base tree)
                     base)
                 (funcall rec tree
                          #'(lambda ()
                              (self (car tree)))
                          #'(lambda ()
                              (if (cdr tree)
                                  (self (cdr tree))))))))
    #'self))

;; flatten.
(trec #'(lambda (o l r)
          (declare (ignore o))
          (nconc (funcall l) (funcall r)))
      #'mklist)

;; rfind-if for oddp.
(trec #'(lambda (o l r)
          (declare (ignore o))
          (or (funcall l) (funcall r)))
      #'(lambda (tree)
          (and (oddp tree) tree)))

;;;
;;; 5.7 When to Build Functions
;;;

;;; By using #., the sharp-dot read macro, we can have the new functions
;;; built at read-time.
;;; ex) (find-if #.(compose #'oddp #'truncate) lst)
