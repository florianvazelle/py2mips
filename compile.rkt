#lang racket/base

(require "mips.rkt"
         "ast.rkt"
         racket/match
         racket/hash ;; pour hash-union
         racket/list) ;; pour flatten

(provide compile2mips)

(define (comp-and ast env fp-sp)
  ;; on veut obtenir un 1 ou un 0
  (match ast
    ((Pconst type val)
      (match type
        ('str (list (Li 'v0 1)))
        ('num
          (if (= val 0) ;; j'interprete un peu mais c'est pour une valeur fixe
            (list (Li 'v0 0))
            (list (Li 'v0 1))))))
    ((Pid id)
      (comp (Pcond '!= (Pid id) (Pconst 'num 0)) env fp-sp))
    ((Pop op v1 v2)
      (comp (Pcond '!= (Pop op v1 v2) (Pconst 'num 0)) env fp-sp))
    ((Pcond id v1 v2)
      (comp (Pcond id v1 v2) env fp-sp))
    ((Pcondop id v1 v2)
      (comp (Pcondop id v1 v2) env fp-sp))))

(define (comp ast env fp-sp) ;; le décalage entre sp et fp est fp - sp
  (match ast
    ((Pconst type val)
     (match type
      ;; Pointeur dans .data mis dans v0
      ('str (list (La 'v0 (Lbl val))))
      ;; Constante entière mise dans v0
      ('num (list (Li 'v0 val)))))

    ((Pid id)
      (list (Lw 'v0 (Mem (hash-ref env id) 'fp))))
      ;; Pour faire ceci (hash-ref env id) doit retoruner 0(sp) ou 4(sp) ...

    ((Passign id val)
      (if (hash-has-key? env id)
          (let ((tmp_sp-fp (hash-ref env id)))
                (let ((decalage (- tmp_sp-fp global_fp-sp)))
                     (list
                        (comp val env (- global_fp-sp 4))
                        (Com (string-append "redefinition de " (symbol->string id)))
                        (Addi 'sp 'sp decalage)
                        (Sw 'v0 (Mem 0 'sp))
                        (Addi 'sp 'sp (- 0 decalage)))))

          (begin
            (set! global_fp-sp (- global_fp-sp 4))
            (hash-set! env id global_fp-sp)
            ;; on compile val pour avoir sa valeur dans v0 :
            (list
               (comp val env global_fp-sp)
               ;; on empile la variable locale :
               (Com (string-append "vardef " (symbol->string id)))
               (Addi 'sp 'sp -4)
               (Sw 'v0 (Mem 0 'sp))))))

    ((Pop op v1 v2)
        (append
            (list
              (Addi 'sp 'sp -4)                 ;; on decale sp
              (comp v1 env (- fp-sp 4))         ;; on compile la premiere valeur
              (Sw 'v0 (Mem 0 'sp))              ;; on empile la valeur v0 (resultat de la compilation de la premiere valeur)
              (comp v2 env (- fp-sp 4))         ;; on compile la second valeur
              (Lw 't0 (Mem fp-sp 'fp))          ;; on recupere la valeur, precedement empiler, dans t0
              (Addi 'sp 'sp 4))                 ;; on remet sp a son etat initial

            (match op          ;; op match avec add, sub, mul et mod (valeur defini dans le parser, 'add $1 $3)
              ('add
                (list (Com "addition")
                  (Add 'v0 't0 'v0)))
              ('sub
                (list (Com "soustraction")
                  (Sub 'v0 't0 'v0)))
              ('mul
                (list (Com "multiplication")
                  (Mult 't0 'v0)
                  (Mflo 'v0)))
              ('div
                (list (Com "division")
                  (Div 't0 'v0)
                  (Mflo 'v0)))
              ('mod
                (list (Com "modulo")
                  (Div 't0 'v0)
                  (Mfhi 'v0))))))

    ((Pcond id v1 v2)
      (set! offset (+ offset 1))
      (append
        (list
          (Addi 'sp 'sp -4)
          (comp v1 env (- fp-sp 4))
          (Sw 'v0 (Mem 0 'sp))
          (comp v2 env (- fp-sp 4))
          (Lw 't0 (Mem fp-sp 'fp))
          (Addi 'sp 'sp 4))

        (match id
          ('==
            (list (Com "egale a")
              (Beq 't0 'v0 (string-append "target" (number->string offset)))))
          ('!=
            (list (Com "n'est pas egale a")
              (Bne 't0 'v0 (string-append "target" (number->string offset)))))
          ('<
            (list (Com "inferieur a")
              (Blt 't0 'v0 (string-append "target" (number->string offset)))))
          ('>
            (list (Com "superieur a")
              (Bgt 't0 'v0 (string-append "target" (number->string offset)))))
          ('<=
            (list (Com "inferieur ou egale a")
              (Ble 't0 'v0 (string-append "target" (number->string offset)))))
          ('>=
            (list (Com "superieur ou egale a")
              (Bge 't0 'v0 (string-append "target" (number->string offset))))))

        (list (Li 'v0 0)
          (B (string-append "suite" (number->string offset)))
          (Label (string-append "target" (number->string offset)))
          (Li 'v0 1)
          (B (string-append "suite" (number->string offset)))
          (Label (string-append "suite" (number->string offset))))))

    ((Pcondop id v1 v2)
      (append
        (list (Com " Debut Pcondop")
          (Addi 'sp 'sp -4)
          (comp-and v1 env (- fp-sp 4))
          (Sw 'v0 (Mem 0 'sp))
          (comp-and v2 env (- fp-sp 4))
          (Lw 't0 (Mem fp-sp 'fp))
          (Addi 'sp 'sp 4))

        (match id
          ('and
            (set! offset (+ offset 1))
            (list (Com "et")
              (Li 't1 1)
              (Beq 'v0 't1 (string-append "target_" (number->string offset)))
              (Li 'v0 0)
              (B (string-append "suite" (number->string offset)))
              (Label (string-append "target_" (number->string offset)))
              (Beq 't0 't1 (string-append "target" (number->string offset)))))
          ('or
            (list (Com "ou"))))

        (list (Li 'v0 0)
          (B (string-append "suite" (number->string offset)))
          (Label (string-append "target" (number->string offset)))
          (Li 'v0 1)
          (B (string-append "suite" (number->string offset)))
          (Label (string-append "suite" (number->string offset))))))

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

    ((Com str)      (printf "\t#~a\n" str))

    ;; Boolean
    ((Beq val1 val2 offset) (printf "beq $~a, $~a, ~a\n" val1 val2 offset))
    ((Bne val1 val2 offset) (printf "bne $~a, $~a, ~a\n" val1 val2 offset))
    ((Blt val1 val2 offset) (printf "blt $~a, $~a, ~a\n" val1 val2 offset))
    ((Bgt val1 val2 offset) (printf "bgt $~a, $~a, ~a\n" val1 val2 offset))
    ((Ble val1 val2 offset) (printf "ble $~a, $~a, ~a\n" val1 val2 offset))
    ((Bge val1 val2 offset) (printf "bge $~a, $~a, ~a\n" val1 val2 offset))
    ((B offset)             (printf "b ~a\n" offset))

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

(define global_fp-sp 0)
(define offset 0)
(define (compile2mips data env)
  (mips-data env)
  (hash-clear! env)
  (for-each mips-emit
    (append
      ;; On initialise notre environnement local :
      (list (Move 'fp 'sp))
      ;; On compile une expression :
      (flatten (map (lambda (i)
            (comp i env global_fp-sp))
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
