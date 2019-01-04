#lang racket/base

(require racket/match
         racket/list ;; pour flatten
         "ast.rkt")

(provide call-analyze)

(define (get-type struct env)
  (match struct

    ;; termes finaux
    ((Const type val)
     type)
    ((Id id)
     (hash-ref env id))

    ;; termes non-finaux
    ((Op op v1 v2)
     (get-type v1 env))
    ;; verifier le type d'une condition est bizarre
    ((Cond id v1 v2)
     (get-type v1 env))
    ((Condop id v1 v2)
     (get-type v1 env))
    ;; fin des trucs bizarres
    ((End nb struct)
     (get-type struct env))))

(define (verify-tab l_indent l_nbind)
  (set! instr-is-verify #t)
  (set! nbind 0)
  (cond [(< l_indent l_nbind) -1]
        [(> l_indent l_nbind)
         (let ((diff (- l_indent l_nbind)))
               (begin
                 (set! indentation l_nbind)
                 diff))]
        [(= l_indent l_nbind) 0]))

;; pour terme finaux (les termes non finaux ont d'autre verification
;; et la verification de la tablature doit se faire avant l'analyze de leur terme finaux)
(define (call-verify-tab r_struct r_env)
  (if instr-is-verify
      (cons r_struct r_env)
      (let ((res (verify-tab indentation nbind)))
           (cond [(= res 0) (cons r_struct r_env)]
                 [(> res 0) (cons (End res r_struct) r_env)]
                 [(< res 0)
                  ((printf "Analyze: Probleme d'indentation: Une tablature en trop.\n")
                   (exit 1))]))))

(define (analyze-instr instr env)
  (match instr

    ((Pconst type val)
     (call-verify-tab (Const type val) env))

    ((Pid id)
     (if (hash-has-key? env id) ;; renvoie true si elle existe
         (call-verify-tab (Id id) env)
         (begin
           (printf "Analyze: Undefined variable ~a\n" id)
           (exit 1))))

    ((Passign id val)
     (if instr-is-verify
         (let ((l_val (car (analyze-instr val env))))
              (cons
               (Assign id l_val)
               (hash-set env id (get-type l_val env))))
         (let ((res (verify-tab indentation nbind)))
              (let ((l_val (car (analyze-instr val env))))
                   (cond [(= res 0) (cons
                                     (Assign id l_val)
                                     (hash-set env id (get-type l_val env)))]
                         [(> res 0) (cons
                                     (End res
                                       (Assign id l_val))
                                       (hash-set env id (get-type l_val env)))]
                         [(< res 0)
                          ((printf "Analyze: Probleme d'indentation: Une tablature en trop.\n")
                           (exit 1))])))))

    ((Pop op v1 v2)
     (if instr-is-verify
       (let ((v1 (analyze-instr v1 env))
             (v2 (analyze-instr v2 env)))
         (let ((t1 (get-type (car v1) env))
               (t2 (get-type (car v2) env)))
         (if (eqv? t1 t2)
             (cons (Op op (car v1) (car v2)) env)
             (begin
               (printf "Analyze: Operation impossible entre different type: ~a ~a ~a\n" t1 op t2)
               (exit 1)))))
       (let ((res (verify-tab indentation nbind)))
            (let ((v1 (analyze-instr v1 env))
                  (v2 (analyze-instr v2 env)))
                 (let ((t1 (get-type (car v1) env))
                       (t2 (get-type (car v2) env)))
                      (if (eqv? t1 t2)
                          (cond [(= res 0) (cons (Op op (car v1) (car v2)) env)]
                                [(> res 0) (cons (End res (Op op (car v1) (car v2))) env)]
                                [(< res 0)
                                 ((printf "Analyze: Probleme d'indentation: Une tablature en trop.\n")
                                  (exit 1))])
                          (begin
                            (printf "Analyze: Operation impossible entre different type: ~a ~a ~a\n" t1 op t2)
                            (exit 1))))))))

    ((Pcond id v1 v2)
     (if instr-is-verify
         (cons (Cond id (car (analyze-instr v1 env)) (car (analyze-instr v2 env))) env)
         (let ((res (verify-tab indentation nbind)))
              (let ((l_v1 (car (analyze-instr v1 env)))
                    (l_v2 (car (analyze-instr v2 env))))
                   (cond [(= res 0) (cons (Cond id l_v1 l_v2) env)]
                         [(> res 0) (cons (End res (Cond id l_v1 l_v2)) env)]
                         [(< res 0)
                          ((printf "Analyze: Probleme d'indentation: Une tablature en trop.\n")
                           (exit 1))])))))

    ((Pcondop id v1 v2)
    (if instr-is-verify
        (cons (Condop id (car (analyze-instr v1 env)) (car (analyze-instr v2 env))) env)
        (let ((res (verify-tab indentation nbind)))
             (let ((l_v1 (car (analyze-instr v1 env)))
                   (l_v2 (car (analyze-instr v2 env))))
                  (cond [(= res 0) (cons (Condop id l_v1 l_v2) env)]
                        [(> res 0) (cons (End res (Condop id l_v1 l_v2)) env)]
                        [(< res 0)
                         ((printf "Analyze: Probleme d'indentation: Une tablature en trop.\n")
                          (exit 1))])))))

    ((Ptab)
     (set! nbind (+ nbind 1))
     (cons '() env))

    ((Pif bool)
      (if instr-is-verify
          (let ((l_bool (car (analyze-instr bool env))))
               (begin
                 (set! indentation (+ indentation 1))
                 (displayln "here if")
                 (cons (If l_bool) env)))
          (let ((res (verify-tab indentation nbind)))
               (let ((l_bool (car (analyze-instr bool env))))
                    (begin
                      (set! indentation (+ indentation 1))
                      (cond [(= res 0) (cons (If l_bool) env)]
                            [(> res 0) (cons (End res (If l_bool)) env)]
                            [(< res 0)
                             ((printf "Analyze: Probleme d'indentation: Une tablature en trop.\n")
                              (exit 1))]))))))

    ((Pelif bool)
      (if instr-is-verify
          (let ((l_bool (car (analyze-instr bool env))))
               (begin
                 (set! indentation (+ indentation 1))
                 (displayln "here elif")
                 (cons (Elif l_bool) env)))
          (let ((res (verify-tab indentation nbind)))
               (let ((l_bool (car (analyze-instr bool env))))
                    (begin
                      (set! indentation (+ indentation 1))
                      (cond [(= res 0)
                             ((printf "Analyze: Mauvaise utilisation du 'elif'.\n")
                              (exit 1))]
                            [(> res 0) (cons (End (- res 1) (Elif l_bool)) env)]
                            [(< res 0)
                             ((printf "Analyze: Probleme d'indentation: Une tablature en trop.\n")
                              (exit 1))]))))))

    ((Pelse)
     (if instr-is-verify
         (begin
           (set! indentation (+ indentation 1))
           (displayln "here else")
           (cons (Else) env))
         (let ((res (verify-tab indentation nbind)))
              (begin
                (set! indentation (+ indentation 1))
                (cond [(= res 0)
                       ((printf "Analyze: Mauvaise utilisation du 'else'.\n")
                        (exit 1))]
                      [(> res 0) (cons (End (- res 1) (Else)) env)]
                      [(< res 0)
                       ((printf "Analyze: Probleme d'indentation: Une tablature en trop.\n")
                        (exit 1))])))))

))

(define indentation 0)
(define nbind 0)
(define instr-is-verify #f)

(define (analyze prog env)
  (set! instr-is-verify #f)
  (match prog
    ((list expr)
     (car (analyze-instr expr env)))
    ((cons decl prog-rest)
     (let ((res-decl (analyze-instr decl env)))
          (list (car res-decl) (analyze prog-rest (cdr res-decl)))))))

(define (call-analyze prog env)
  (let ((res-prog (flatten (analyze prog env))))
       (map (lambda (i)
              (if (null? i) (remove '() res-prog) i))
            res-prog)))
