#lang racket/base

(require parser-tools/lex
         (prefix-in : parser-tools/lex-sre))

(provide mylexer
         autres
         fonctions
         ponctuations
         types
         operateurs
         (struct-out position))

;; definitions des tokens vide (donc qui ne sont pas rattache a une valeur)

(define-empty-tokens autres
  (Leof            ;; end of file
   Lif Lelif Lelse ;; if, elif, else
   Land Lor        ;; and or
   ))

(define-empty-tokens operateurs
  (Ladd Lsub Lmul Ldiv Lmod        ;; addition, soustraction, multiplication, division, modulo
        Leq Lneq Llt Lgt Llte Lgte ;; ==, !=, <, >, <=, >=
        Lassign        	        ;; =
))

(define-empty-tokens fonctions
  (Lwhile))

(define-empty-tokens ponctuations
  (Lopar Lcpar ;; parenthese ouvrante et fermante
         Lnl   ;; newline
         Lspa  ;; space
         Ltab  ;; tablature
         Lcol  ;; :
         ))

;; definitions des tokens

(define-tokens types
  (Lstr Lnum Lvar))

;; definition des abreviations

(define-lex-abbrevs
  [letter     (:or (:/ "a" "z") (:/ #\A #\Z) )])

;; regexp abbreviations

(define-lex-abbrev latin_
  (:or letter "_"))

(define-lex-abbrev latin_num
  (:or latin_ numeric))

(define-lex-abbrev variable
  (:: latin_ (:* latin_num)))

(define-lex-abbrev number
  (:: (:+ numeric) (:? "." (:+ numeric))))

(define mylexer
  (lexer-src-pos
   ((eof)        (token-Leof))

   (#\tab	       (token-Ltab))
   (#\newline	   (token-Lnl))
   (#\space      (return-without-pos (mylexer input-port)))

   ;; boucle
   ;("while"    (token-Lwhile))

   ;; condition
   ;("if"       (token-Lif))
   ;("elif"     (token-Lelif))
   ;("else"     (token-Lelse))
   ;(":"        (token-Lcol))

   ;; boolean
   ("=="       (token-Leq))
   ("!="       (token-Lneq))
   ("<"        (token-Llt))
   (">"        (token-Lgt))
   ("<="       (token-Llte))
   (">="       (token-Lgte))

   ("and"      (token-Land))
   ("or"       (token-Lor))

   ;; other
   ("("        (token-Lopar))
   (")"        (token-Lcpar))
   ("\""       (token-Lstr (apply string-append (string-mylexer input-port))))
   ("#"        (return-without-pos (comment-mylexer input-port))) ;; pour ignorer les commentaires

   ;; operation
   ("+"        (token-Ladd))
   ("-"        (token-Lsub))
   ("*"        (token-Lmul))
   ("/"        (token-Ldiv))
   ("%"        (token-Lmod))

   ;; assignement
   ("="        (token-Lassign))

   (number   (token-Lnum (string->number lexeme)))
   (variable (token-Lvar (string->symbol lexeme)))

   (any-char     (begin
                   (eprintf "Lexer: ~a: unrecognized char at line ~a col ~a.\n"
                            lexeme (position-line start-pos) (position-col start-pos))
                   (exit 1)))))

(define string-mylexer
  (lexer
   ("\\\""   (cons "\"" (string-mylexer input-port)))
   ("\\\\"   (cons "\\" (string-mylexer input-port)))
   ("\\n"    (cons "\n" (string-mylexer input-port)))
   ("\\t"    (cons "\t" (string-mylexer input-port)))
   ("\""     '())
   (any-char (cons lexeme (string-mylexer input-port)))))

(define comment-mylexer
  (lexer
   (#\newline (mylexer input-port))
   (any-char  (comment-mylexer input-port))))
