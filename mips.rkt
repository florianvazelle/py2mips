#lang racket/base

(provide (all-defined-out))

;;;;; MIPS
(struct Lw      (out src))       ;; Load word
(struct Add     (out val1 val2)) ;; Add (with overflow)
(struct Sub     (out val1 val2)) ;; Subtract
(struct Mult    (val1 val2))
(struct Div     (val1 val2))
(struct Mflo    (out))
(struct Mfhi    (out))

(struct Com     (str))          ;; Commentaire

;; Boolean
(struct Beq     (val1 val2 offset))
(struct Bne     (val1 val2 offset))
(struct Blt     (val1 val2 offset))
(struct Bgt     (val1 val2 offset))
(struct Ble     (val1 val2 offset))
(struct Bge     (val1 val2 offset))
(struct B       (offset))

(struct Move    (rd rs))
(struct Li      (r i))
(struct La      (r a))
(struct Addi    (rd rs i))
(struct Sw      (r loc))
(struct Syscall ())
(struct Jr      (r))
(struct Label   (l))

;; addresses
(struct Lbl     (l))   ;; label (souvent présent dans .data)
(struct Mem     (b r)) ;; emplacement mémoire à l'adresse b + valeur du registre r
