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
