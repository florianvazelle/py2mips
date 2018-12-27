#lang racket/base

(require racket/match
         "ast.rkt")

(provide analyze)

(define (get-type struct env)
  (match struct

    ;; termes finaux
    ((Const type val)
     type)
    ((Id id)
     (hash-ref env id))

    ;; termes non-finaux
    ((Op op v1 v2)
     (get-type (car v1) env))
    ((Cond id v1 v2)
     (get-type (car v1) env))))

(define (analyze-instr instr env)
  (match instr

    ((Pconst type val)
     (cons (Const type val) env))

    ((Pid id)
     (if (hash-has-key? env id) ;; renvoie true si elle existe
         (cons (Id id) env)
         (begin
           (printf "Analyze: Undefined variable ~a\n" id)
           (exit 1))))

    ((Passign id val)
      (let ((val (analyze-instr val env)))
        (cons (Assign id (car val))
              (hash-set env id (get-type (car val) env)))))

    ((Pop op v1 v2)
      (let ((v1 (analyze-instr v1 env))
            (v2 (analyze-instr v2 env)))
        (let ((t1 (get-type (car v1) env))
              (t2 (get-type (car v2) env)))
        (if (eqv? t1 t2)
            (cons (Op op v1 v2) env)
            (begin
              (printf "Analyze: Operation impossible entre different type: ~a ~a ~a\n" t1 op t2)
              (exit 1))))))

    ((Pcond id v1 v2)
     (cons (Cond id (analyze-instr v1 env) (analyze-instr v2 env)) env))

))

(define (analyze prog env)
  (match prog
    ((list expr)
     (analyze-instr expr env))
    ((cons decl prog-rest)
      (analyze prog-rest (cdr (analyze-instr decl env))))))
