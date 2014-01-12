(in-package :cl-user)
(defpackage on-lisp.chap17
  (:use :cl)
  (:import-from :on-lisp.chap4
                :mapa-b)
  (:import-from :on-lisp.chap5
                :compose))
(in-package :on-lisp.chap17)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Chapter 17. Read-Macros
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;
;;; 17.1 Macro Characters
;;;

;;; A macro character is a character which exacts special treatement from the
;;; Lisp reader.

;;;
;;; 17.2 Dispatching Macro Characters
;;;

;; A read-macro for constant functions.
(set-dispatch-macro-character #\# #\?
                              #'(lambda (stream char1 char2)
                                  `#'(lambda (&rest ,(gensym))
                                       ,(read stream t nil t))))

;;;
;;; 17.3 Delimiters
;;;

(set-macro-character #\] (get-macro-character #\)))

(set-dispatch-macro-character #\# #\[
                              #'(lambda (stream char1 char2)
                                  (let ((accum nil)
                                        (pair (read-delimited-list #\] stream t)))
                                    (do ((i (ceiling (car pair)) (1+ i)))
                                        ((> i (floor (cadr pair)))
                                         (list 'quote (nreverse accum)))
                                      (push i accum)))))

;; A macro for defining delimiter read-macros.
(defmacro defdelim (left right parms &body body)
  `(ddfn ,left ,right #'(lambda ,parms
                          ,@body)))

(let ((rpar (get-macro-character #\))))
  (defun ddfn (left right fn)
    (set-macro-character right rpar)
    (set-dispatch-macro-character
     #\# left
     #'(lambda (stream char1 char2)
         (apply fn
                (read-delimited-list right stream t))))))

;;;
;;; 17.4 When What Happens
