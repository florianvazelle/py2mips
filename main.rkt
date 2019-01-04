#lang racket/base

(require "parser.rkt"
	       "analyze.rkt"
				 "compile.rkt"
				 racket/list)

(define debug #f)

;; debut du programme

(define argv (current-command-line-arguments))
;; si
(cond
  ;; la taille de argv est egale a 1
  ((= (vector-length argv) 1)
   ;; on execute le code suivant
   (define in (open-input-file (vector-ref argv 0)))
   (port-count-lines! in)

   ;; on definit la variable parsed, resultat de l'appelle de mon parser a la suite de l'appelle de mon lexer
   (define parsed (py-parse in))

   (close-input-port in)
   ;; on indique a l'utilisateur que le parsing c'est bien deroule
   (cond (debug (printf "Parsing ok.\n")))

   ;;pour le debug on va affiche le resultat du parser
   (cond (debug (write parsed)))
	 (cond (debug (newline)))

   (define analyzed (call-analyze parsed (make-immutable-hash)))

   (cond (debug (printf "Typing ok.\n")))

	 (cond (debug (write analyzed)))
	 (cond (debug (newline)))

	 (compile2mips analyzed (make-hash '((nl . "\n"))))

   ;; on quitte
   (exit 0)
   )
  ;; sinon
  (else
   ;; on affiche l'utilisation du code
   (eprintf "Usage: racket main.rkt <file>\n")
   ;; et on quitte
   (exit 1)))
