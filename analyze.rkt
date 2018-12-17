#lang racket/base

(require racket/match
         "ast.rkt")

(provide analyze)

(define (analyze-instr instr env)
  (match instr
    ((Pconst type val)
     #t)
    ((Pid id)
     (if (hash-has-key? env id) ;; renvoie true si elle existe
         #t
         (begin
           (printf "Analyze: Undefined variable ~a\n" id)
           (exit 1))))
    ((Passign id val)
     (hash-set env id (analyze-instr val env)))
    ((Pop op v1 v2)
     (if (and (analyze-instr v1 env) (analyze-instr v2 env))
          env
          (begin
            (printf "Analyze: Operation impossible ~a ~a ~a\n" v1 op v2)
            (exit 1))))
    ((Pcond op v1 v2)
    ;; au final pas besoin de tester cette egalite car deja fait par le parser
     (if (regexp? #px"and|or|==|!=|<|>|<=|>=")
         (and (analyze-instr v1 env) (analyze-instr v2 env))
         (begin
           (eprintf 'Analyze "Undefined operator: ~a" op)
           (exit 1))))))

(define (analyze prog env)
  (match prog
    ((list expr)
     (analyze-instr expr env))
    ((cons decl prog-rest)
     (analyze prog-rest (analyze-instr decl env)))))
