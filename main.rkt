#lang racket/base

(require "parser.rkt"
	       "analyze.rkt"
				 "compile.rkt")

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
   (printf "Parsing ok.\n")

   ;;pour le debug on va affiche le resultat du parser
   ;(write parsed)

   (analyze parsed (make-immutable-hash))
   ;;(write prog)
   (printf "Typing ok.\n")

	 (compile2mips parsed (make-immutable-hash))

   ;; on quitte
   (exit 0)
   )
  ;; sinon
  (else
   ;; on affiche l'utilisation du code
   (eprintf "Usage: racket main.rkt <file>\n")
   ;; et on quitte
   (exit 1)))
