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
     ((instr Lnl prog)   (cons $1 $3))
     ((instr Lnl)        (list $1)))

    (instr
      ; tout se qui est la, est possible (on peut l'ecrire) mais inutile
      ; en effet on ne stocke ni le chiffre, ni le resultat de l'operation
      ; donc je vais noter ces operations inutiles pour quelle soit supprimer
      ; dans un optimisateur
      ((Lnum)   (Pconst 'num $1))
      ((operation) $1)
      ((Lvar)   (Pid $1))

     ((Lvar Lassign sexpr)        (Passign $1 $3)))

    (sexpr ;; single-expr
     ((atom)              $1)
     ((operation)         $1)
     ((Lopar sexpr Lcpar) $2))

    (atom
      ((Ltrue)  (Pconst 'num 1))
      ((Lfalse) (Pconst 'num 0))

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

     ((sexpr Leq sexpr)  (Pcond '== $1 $3))
     ((sexpr Lneq sexpr) (Pcond '!= $1 $3))
     ((sexpr Llt sexpr)  (Pcond '<  $1 $3))
     ((sexpr Lgt sexpr)  (Pcond '>  $1 $3))
     ((sexpr Llte sexpr) (Pcond '<= $1 $3))
     ((sexpr Lgte sexpr) (Pcond '>= $1 $3))

     ((sexpr Land sexpr) (Pcondop 'and $1 $3))
     ((sexpr Lor sexpr)  (Pcondop 'or  $1 $3))
    ))

   (precs
     (left Lor)
     (left Land)

     (left Leq)
     (left Lneq)
     (left Llt)
     (left Lgt)
     (left Llte)
     (left Lgte)

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
