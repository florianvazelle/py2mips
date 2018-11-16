#lang racket/base

(require racket/match
         "ast.rkt")

(provide myeval)

(define (eval-instr instr env)
  (match instr
    ((Pconst type val)
     val)
    ((Pid id)
     (hash-ref env id))
    ((Passign id val)
     (hash-set env id (eval-instr val env)))
    ((Pop op v1 v2)
     (match op					;; op match avec add, sub, mul et mod (valeur defini dans le parser, 'add $1 $3)
      ('add (+ (eval-instr v1 env) (eval-instr v2 env)))
      ('sub (- (eval-instr v1 env) (eval-instr v2 env)))
      ('mul (* (eval-instr v1 env) (eval-instr v2 env)))
      ('div (/ (eval-instr v1 env) (eval-instr v2 env)))
      ('mod (modulo (eval-instr v1 env) (eval-instr v2 env)))
      ))


      ))

(define (myeval prog env)
  (match prog
    ((list expr)
     (eval-instr expr env))
    ((cons decl prog-rest)
     (myeval prog-rest (eval-instr decl env))))) ;; on attend un environnement avec l'appelle (eval-instr decl env)
