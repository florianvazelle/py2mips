#lang racket/base

(require parser-tools/yacc
         ;; ast est le fichier qui contient l'ensemble des structure aui serviront au parser
         "ast.rkt"
         ;; pour pouvoir utiliser les tokens (peut etre faire un fichier speciale avec la definition des tokens)
         "lexer.rkt")

(provide py-parse)

(define myparser
  (parser
   (src-pos)
   (tokens autres ponctuations fonctions types operateurs)
   (start prog)
   (end Leof)
   (grammar
    (prog
     ((instr)            (list $1))
     ((instr Lnl prog)   (cons $1 $3)))

    (instr
     ((Lvar Lassign sexpr)        (Passign $1 $3)))

    (sexpr ;; single-expr
     ((atom)              $1)
     ((operation)         $1)
     ((Lopar sexpr Lcpar) $2))

    (atom
     ((Lstr)   (Pconst 'str $1)) ;; string or char
     ((Lnum)   (Pconst 'num $1)) ;; nombre
     ((Lvar)   (Pid $1)) ;; variable (deja existante)
    )

    (operation
     ((sexpr Ladd sexpr) (Pop 'add $1 $3))
     ((sexpr Lsub sexpr) (Pop 'sub $1 $3))
     ((sexpr Lmul sexpr) (Pop 'mul $1 $3))
     ((sexpr Ldiv sexpr) (Pop 'div $1 $3))
     ((sexpr Lmod sexpr) (Pop 'mod $1 $3))
    ))

   (precs
    (left Lmod)
    (left Ladd)
    (left Lsub)
    (left Lmul)
    (left Ldiv))
   (debug "yacc.dbg")
   (error
    (lambda (ok? name value s-pos e-pos)
      (eprintf "Parser: ~a: ~a~a on line ~a col ~a.\n"
               (substring (symbol->string name) 1)
               (if ok? "syntax error" "unexpected token")
               (if value (format " near '~a'" value) "")
               (position-line s-pos)
               (position-col s-pos))
      (exit 1)))))

(define (py-parse in)
  (myparser (lambda () (mylexer in))))
