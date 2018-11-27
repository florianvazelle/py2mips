#lang racket/base

(provide (all-defined-out))

;;;;; MIPS

(struct Move    (rd rs))
(struct Li      (r i))
(struct La      (r a))
(struct Addi    (rd rs i))
(struct Sw      (r loc))
(struct Lw      (r loc))
(struct Syscall ())
(struct Jr      (r))
(struct Label   (l))

;; addresses
(struct Lbl     (l))   ;; label (souvent présent dans .data)
(struct Mem     (b r)) ;; emplacement mémoire à l'adresse b + valeur du registre r
