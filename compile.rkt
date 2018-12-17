#lang racket/base

(require "mips.rkt"
         "ast.rkt"
         racket/match
         racket/hash ;; pour hash-union
         racket/list) ;; pour flatten

(provide compile2mips)

(define (comp ast env) ;; le décalage entre sp et fp est fp - sp
  (match ast
    ((Pconst type val)
     (match type
      ;; Pointeur dans .data mis dans v0
      ('str (list (La 'v0 (Lbl val))))
      ;; Constante entière mise dans v0
      ('num (list (Li 'v0 val)))))

    ((Pid id)
      (list (Lw 'v0 (hash-ref env id))))
      ;; Pour faire ceci (hash-ref env id) doit retoruner 0(sp) ou 4(sp) ...

    ((Passign id val)
      (set! fp-sp (- fp-sp 4))
      (hash-set! env id (Mem fp-sp 'fp))
      (append
         ;; on compile val pour avoir sa valeur dans v0 :
         (list (comp val env))

         ;; on empile la variable locale :
         (list (Addi 'sp 'sp -4)
               (Sw 'v0 (Mem 0 'sp)))))

    ((Pop op v1 v2)
      (append
        (comp v1 env)
        (list (Move 't0 'v0))
        (comp v2 env)
        (match op          ;; op match avec add, sub, mul et mod (valeur defini dans le parser, 'add $1 $3)
          ('add
            (list (Add 'v0 't0 'v0)))
          ('sub
            (list (Sub 'v0 't0 'v0)))
          ('mul
            (list (Mult 't0 'v0)
                  (Mflo 'v0)))
          ('div
            (list (Div 't0 'v0)
                  (Mflo 'v0)))
          ('mod
            (list (Div 't0 'v0)
                  (Mfhi 'v0))))))
  ))

(define (mips-loc loc)
  (match loc
    ((Lbl l)   (format "~a" l))
    ((Mem b r) (format "~a($~a)" b r))))

(define (mips-emit instr)
;(displayln instr)
  (match instr
    ((Move rd rs)   (printf "move $~a, $~a\n" rd rs))
    ((Li r i)       (printf "li $~a, ~a\n" r i))
    ((La r a)       (printf "la $~a, ~a\n" r (mips-loc a)))
    ((Addi rd rs i) (printf "addi $~a, $~a, ~a\n" rd rs i))

    ;; operation
    ((Add out val1 val2) (printf "add $~a, $~a, $~a\n" out val1 val2))
    ((Sub out val1 val2) (printf "sub $~a, $~a, $~a\n" out val1 val2))
    ((Mult val1 val2)    (printf "mult $~a, $~a\n" val1 val2))
    ((Div val1 val2)     (printf "div $~a, $~a\n" val1 val2))

    ((Mflo out)          (printf "mflo $~a\n" out))
    ((Mfhi out)          (printf "mfhi $~a\n" out))

    ((Sw r loc)     (printf "sw $~a, ~a\n" r (mips-loc loc)))
    ((Lw r loc)     (printf "lw $~a, ~a\n" r (mips-loc loc)))
    ((Syscall)      (printf "syscall\n"))
    ((Jr r)         (printf "jr $~a\n" r))
    ((Label l)      (printf "\t~a:\n" l))))

(define (mips-data data)
  (printf ".data\n")
  (hash-for-each data
    (lambda (k v)
      (printf "~a: .asciiz ~s\n" k v)))
  (printf "\n.text\n.globl main\nmain:\n"))

(define fp-sp 0)
(define (compile2mips data env)
  (mips-data env)
  (hash-clear! env)
  (for-each mips-emit
    (append
      ;; On initialise notre environnement local :
      (list (Move 'fp 'sp))
      ;; On compile une expression :
      (flatten (map (lambda (i)
            (comp i env))
            data))
      ;; On affiche le résultat, qui est dans v0
      (list (Move 'a0 'v0)
            (Li 'v0 1) ;; 4 pour print_string qui est le type du résultat
            (Syscall)
            ;; affichage retour à la ligne :
            (La 'a0 (Lbl 'nl))
            (Li 'v0 4)
            (Syscall)
            ;; main return 0
            (Li 'v0 0)
            (Jr 'ra)))))
