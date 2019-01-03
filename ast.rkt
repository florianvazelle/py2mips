#lang racket/base

(provide (all-defined-out))


;;; Syntactic structures representing the parsed syntax
;;; ---------------------------------------------------

;;; Definition d'une variable
(struct Passign (id val)     #:transparent)

;;; Valeur
(struct Pconst  (type value) #:transparent)

;;; Appelle d'operation (operation argument1 argument2)
(struct Pop     (op v1 v2)   #:transparent)

;;; Identifiant
(struct Pid     (id)         #:transparent)

;;; Boolean
(struct Pcond   (id v1 v2)   #:transparent)

;;; And et Or
(struct Pcondop (id v1 v2)   #:transparent)

;; Condition
(struct Pif   (bool)   #:transparent)
(struct Pelif (bool)   #:transparent)
(struct Pelse ()       #:transparent)

;; Tablature
(struct Ptab  ()   #:transparent)


;;; Syntactic structures for our internal representation (AST)
;;; ----------------------------------------------------------

(struct Assign (id val)     #:transparent)
(struct Const  (type value) #:transparent)
(struct Op     (op v1 v2)   #:transparent)
(struct Id     (id)         #:transparent)
(struct Cond   (id v1 v2)   #:transparent)
(struct Condop (id v1 v2)   #:transparent)
(struct If     (bool)       #:transparent)
(struct Elif   (bool)       #:transparent)
(struct Else   ()           #:transparent)
(struct End    (nb struct)  #:transparent)
