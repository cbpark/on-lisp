(in-package :cl-user)
(defpackage on-lisp.chap7
  (:use :cl))
(in-package :on-lisp.chap7)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Chapter 7. Macros
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;
;;; 7.1 How Macros Work
;;;

;;; A function produce results, but a macro produces expressions -- which, when
;;; evaluated, produce results.

;; Whenever you see an expression of the form (nil! var), turn it into one of
;; the form (setf var nil) before evaluating it.
;; (defmacro nil! (var)
;;   (list 'setf var nil))

;;; 1. Macroexpansion:
;;;   builds the expression specified by the definition above, then
;;; 2. Evaluation:
;;;   evaluates that expression in place of the original macro call.

;;; Evaluation does not always come immediately after expansion, as it does at
;;; the top level.

;;; The macroexpansion step deals with expressions, and the evaluation step
;;; deals with their values.

;;; paser --> compiler: the output of the parser consists of lists of Lisp
;;; objects.

;;; Being able to change what compiler sees is almost like bein able to write it.

;;;
;;; 7.2 Backquote
;;;

;;; `(a b c) is equivalent to (list 'a 'b 'c).

;;; A comma surrounded by n commas must be surrounded by at least n+1 backquotes.

(defmacro nil! (var)
  `(setf ,var nil))

(defmacro nif (expr pos zero neg)
  `(case (truncate (signum ,expr))
     (1 ,pos)
     (0 ,zero)
     (-1 ,neg)))

;; (mapcar #'(lambda (x)
;;             (nif x 'p 'z 'n)) '(0 2.5 -8))

;;; Comma-at (,@)
;;; 1. In order for its argument to be spliced, comma-at must occur within a
;;; sequence. It's an error to say something like `,@b because there is nowhere
;;; to splice the value of b.
;;; 2. The object to be spliced must be a list, unless it occurs last. The
;;; expression `(a ,@1) will evaluate to (a . 1), but attempting to splice an
;;; atom into the middle of a list, as in `(a ,@1 b), will cause an error.

;;; Comma-at tends to be used in macros which take an indeterminate number of
;;; arguments and pass them on to functions or macros which also take an
;;; indeterminate number of arguments.

(defmacro our-when (test &body body)
  `(if ,test
       (progn
         ,@body)))

;;; `(a ,@b c) = (cons 'a (append b (list 'c)))

(defun greet (name)
  `(hello ,name))

;;;
;;; 7.3 Defining Simple Macros
;;;

;;; 1. If there is no line connecting it with the macro call, then write down
;;; the expression itself.
;;; 2. If there is a connection to one of the arguments in the macro call, write
;;; down the symbol which occurs in the corresponding position in the macro
;;; parameter list, preceded by a comma.
;;; 3. If there is a connection from a series of expressions in the expansion to
;;; a series of the arguments in the macro call, write down the corresponding
;;; &rest or &body parameter, preceded by a comma-at.

;; (memq x choices) -> (member x choices :test #'eq).
(defmacro memq (obj lst)
  `(member ,obj ,lst :test #'eq))

(defmacro while (test &body body)
  `(do ()
       ((not ,test))
     ,@body))

;;;
;;; 7.4 Testing Macroexpansion
;;;

(defmacro mac (expr)
  `(pprint (macroexpand-1 ',expr)))

;;;
;;; 7.5 Destructing in Parameter Lists
;;;

;;; (destructuring-bind (x (y) . z) '(a (b) c d) (list x y z))

(defmacro our-dolist ((var list &optional result) &body body)
  `(progn
     (mapc #'(lambda (,var)
               ,@body) ,list)
     (let ((,var nil))
       ,result)))

(defmacro when-bind ((var expr) &body body)
  `(let ((,var ,expr))
     (when ,var
       ,@body)))

;;;
;;; 7.6 A Model of Macros
;;;

;;;
;;; 7.7 Macros as Programs
;;;

;;; A macro is a function which transforms one sort of expression into another.

;; Implementing do.
(defmacro our-do (bindforms (test &rest result) &body body)
  (let ((label (gensym)))
    `(prog ,(make-initforms bindforms)
        ,label
        (if ,test
            (return (progn ,@result)))
        ,@body
        ;; psetf: parellel setf
        (psetf ,@(make-stepforms bindforms))
        (go ,label))))

(defun make-initforms (bindforms)
  (mapcar #'(lambda (b)
              (if (consp b)
                  (list (car b) (cadr b))
                  (list b nil)))
          bindforms))

(defun make-stepforms (bindforms)
  (mapcan #'(lambda (b)
              (if (and (consp b) (third b))
                  (list (car b) (third b))
                  nil))
          bindforms))

;;;
;;; 7.8 Macro Style
;;;

;;; Expander code can favor clarity over efficiency, and expansion code can favor
;;; efficiency over clarity.

;; Macros equivalent to and.
(defmacro our-and (&rest args)
  (case (length args)
    (0 t)
    (1 (car args))
    (t `(if ,(car args)
            (our-and ,@(cdr args))))))

(defmacro our-andb (&rest args)
  (if (null args)
      t
      (labels ((expander (rest)
                 (if (cdr rest)
                     `(if ,(car rest)
                          ,(expander (cdr rest)))
                     (car rest))))
        (expander args))))

;;; Macros are often used to implement general-purpose utilities, which are often
;;; called everywhere in a program. Something used so often can't afford to be
;;; inefficient.

;;;
;;; 7.9 Dependence on Macros
;;;

;;; 1. Define macros before functions (or macros) which call them.
;;; 2. When a macro is redefined, also recompile all the functions (or macros)
;;; which call it -- directly or via other macros.

;;;
;;; 7.10 Macros from Functions
;;;

;;; The easiest class to translate are the functions which
;;; 1. Have a body consisting of a single expression.
;;; 2. Have a parameter list consisting only of parameter names.
;;; 3. Create no new variables (except the paramters).
;;; 4. Are not recursive (nor part of mutually recursive group).
;;; 5. Have no parameter which occurs more than once in the body.
;;; 6. Have no parameter whose value is used before that of another parameter
;;; occurring before it in the parameter list.
;;; 7. Contain no free variables.

;;; (defun second (x) (cadr x)) -> (defmacro second (x) `(cadr ,x))

;;; If condition 1 doesn't hold, you have to add a progn.
;;; (defun noisy-second (x)
;;;   (princ "Someone is taking a cadr!")
;;;   (cadr x))
;;; ->
;;; (defmacro noisy-second (x)
;;;   `(progn
;;;      (princ "Someone is taking a cadr!")
;;;      (cadr ,x)))

;;; When the function doesn't meet condition 2 because it has an &rest or &body
;;; parameter, the rules are the same, except that the parameter, instead of
;;; simply having a comma before it, must be spliced into a call to list.
;;; (defun sum (&rest args)
;;;   (apply #'+ args))
;;; ->
;;; (defmacro sum (&rest args)
;;;   `(apply #'+ (list ,@args)))
;;; ->
;;; (defmacro sum (&rest args)
;;;   `(+ ,@args))

;;; When condition 3 doesn't hold -- when new variables are created within the
;;; function body -- the rule about the insertion of commas must be modified.
;;; Instead of putting commas before all symbols in the paramter list, we only
;;; put them before those which will refer to the paramters.
;;; (defun foo (x y z)
;;;   (list x (let ((x y))
;;;             (list x z))))
;;; ->
;;; (defmacro foo (x y z)
;;;   `(list ,x (let ((x ,y))
;;;               (list x ,z))))

;;;
;;; 7.11 Symbol Macros
;;;
