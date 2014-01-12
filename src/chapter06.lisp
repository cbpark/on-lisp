(in-package :cl-user)
(defpackage on-lisp.chap6
  (:use :cl))
(in-package :on-lisp.chap6)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Chapter 6. Functions as Representation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;
;;; 6.1 Networks
;;;

;; Representation and definition of nodes.
(defstruct node contents yes no)

(defparameter *nodes* (make-hash-table))

(defun defnode (name conts &optional yes no)
  (setf (gethash name *nodes*) (make-node :contents conts
                                          :yes      yes
                                          :no       no)))

;; Sample network.
(defnode 'people "Is the person a man?" 'male 'female)

(defnode 'male "Is he living?" 'liveman 'deadman)

(defnode 'deadman "Was he American?" 'us 'them)

(defnode 'us "Is he on a coin?" 'coin 'cidence)

(defnode 'coin "Is the coin a penny?" 'penny 'coins)

(defnode 'penny 'lincoln)

;; Function for traversing networks.
(defun run-node (name)
  (let ((n (gethash name *nodes*)))
    (cond
      ((node-yes n) (format t "~A~%>> " (node-contents n))
       (case (read)
         (yes (run-node (node-yes n)))
         (t   (run-node (node-no  n)))))
      (t (node-contents n)))))

;;;
;;; 6.2 Compiling Networks
;;;

;; A network compiled into closures.
;; (defun defnode (name conts &optional yes no)
;;   (setf (gethash name *nodes*) (if yes
;;                                    #'(lambda ()
;;                                        (format t "~A~%>> " conts)
;;                                        (case (read)
;;                                          (yes (funcall (gethash yes *nodes*)))
;;                                          (t   (funcall (gethash no  *nodes*)))))
;;                                    #'(lambda ()
;;                                        conts))))

;; Compilation with static references.
;; (defparameter *nodes* nil)

;; (defun defnode (&rest args)
;;   (push args *nodes*)
;;   args)

;; (defun compile-net (root)
;;   (let ((node (assoc root *nodes*)))
;;     (if (null node)
;;         nil
;;         (let ((conts (second node))
;;               (yes   (third  node))
;;               (no    (fourth node)))
;;           (if yes
;;               (let ((yes-fn (compile-net yes))
;;                     (no-fn  (compile-net no)))
;;                 #'(lambda ()
;;                     (format t "~A~%>> " conts)
;;                     (funcall (if (eq (read) 'yes)
;;                                  yes-fn
;;                                  no-fn))))
;;               #'(lambda ()
;;                   conts))))))

;;;
;;; 6.3 Looking Forward
;;;

;;; Closures are data objects, and they can be used to represent things just
;;; as structres can.

;;; "To represent with closures" is another way of saying "to compile," and
;;; since macros do their work at compile-time, they are a natural vehicle
;;; for this technique.