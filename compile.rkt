#lang racket/base

(require "mips.rkt"
         "ast.rkt"
         racket/match)

(provide compile2mips)

(define (comp ast env fp-sp) ;; le décalage entre sp et fp est fp - sp
  (match ast
    ((Pconst type val)
     (match type
      ;; Pointeur dans .data mis dans v0
      ('str (list (La 'v0 (Lbl val))))
      ;; Constante entière mise dans v0
      ('num (list (Li 'v0 val)))))

    ((Pid id)
     (hash-ref env id))

    ((Passign id val)
      (append
       ;; on compile v pour avoir sa valeur dans v0 :
       (comp val env fp-sp)
       ;; on empile la variable locale :
       (list (Addi 'sp 'sp -4)
             (Sw 'v0 (Mem 0 'sp)))

       ;; en associant n à son adresse dans la pile par rapport
       ;; à fp, pour que cette adresse soit fixe
       (hash-set env id (Mem fp-sp 'fp))))
))

(define (mips-loc loc)
  (match loc
    ((Lbl l)   (format "~a" l))
    ((Mem b r) (format "~a($~a)" b r))))

(define (mips-emit instr)
  (match instr
    ((Move rd rs)   (printf "move $~a, $~a\n" rd rs))
    ((Li r i)       (printf "li $~a, ~a\n" r i))
    ((La r a)       (printf "la $~a, ~a\n" r (mips-loc a)))
    ((Addi rd rs i) (printf "addi $~a, $~a, ~a\n" rd rs i))
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

(define (compile data env fp-sp)
  (match data
    ((list expr)
     (comp expr env fp-sp))
    ((list decl prog-rest)
     (compile prog-rest (comp decl env fp-sp) fp-sp))))

(define (compile2mips data env)
  (mips-data env)
  (for-each mips-emit
    (append
      ;; On initialise notre environnement local :
      (list (Move 'fp 'sp))
      ;; On compile une expression :
      (comp data
        ;; avec un environnement vide :
        env
        ;; et fp-sp = 0 (vu que fp = sp à ce moment là) :
        0)
      ;; On affiche le résultat, qui est dans v0
      (list (Move 'a0 'v0)
            (Li 'v0 4) ;; 4 pour print_string qui est le type du résultat
            (Syscall)
            ;; affichage retour à la ligne :
            (La 'a0 (Lbl 'nl))
            (Syscall)
            ;; main return 0
            (Li 'v0 0)
            (Jr 'ra)))))
