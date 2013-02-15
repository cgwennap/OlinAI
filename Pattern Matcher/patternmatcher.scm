;patternmatcher.scm

;Does pattern matching as described in assignment.txt
;Achieves minimum deliverable, but is sufficiently robust to include more patterns fairly easily

;display with new line attached at end
(define println
  (lambda (str)
   (display str)
   (newline)))
  
;if int, convert read from symbol to string, then replace the matching int in list with it (symbol->string )
(define sym-fill
  (lambda (mixlist intvar symvar)
    (if (null? mixlist)
        mixlist
        (if (integer? (car mixlist))
            (if (= intvar (car mixlist))
                (cons (symbol->string symvar) (cdr mixlist))
                (cons (car mixlist) (sym-fill (cdr mixlist) intvar symvar)))
            (cons (car mixlist) (sym-fill (cdr mixlist) intvar symvar))))))
;(sym-fill (list (list) 'sup "Hi " 1 2) 2 'Chaz)

;if string, print string merged with all consecutive strings (string-append "Apple " "Banana")
(define string-mash
  (lambda (stringlist)
    (if (null? stringlist)
        ""
        (if (null? (cdr stringlist))
            (car stringlist)
            (string-append (car stringlist) (string-mash (cdr stringlist)))))))
;(string-mash (list "My name is " "Chaz"))

;takes dictvar (possible outcomes) and symvar (symbol read), and iterates until happiness
(define pattern-matcher 
  (lambda (dictvar symvar)
    (cond
      ((null? dictvar) (display "Don't give me an empty list, fool!"))
      ((list? (car dictvar)) (cond
                               ((null? (car dictvar)) (display "LETDOWN"))
                               ((null? symvar) (pattern-matcher dictvar (read)))
                               ((eq? symvar (car (car dictvar))) (pattern-matcher (cdr (car dictvar)) '()))
                               ((null? (cdr dictvar)) (println "I don't understand what you're saying"))
                               (else (pattern-matcher (cdr dictvar) symvar))))
      ((symbol? (car dictvar)) (cond
                                 ((null? symvar) (pattern-matcher dictvar (read)))
                                 ((eq? symvar (car dictvar)) (pattern-matcher (cdr dictvar) '()))
                                 (else (println "I don't QUITE understand what you're saying"))))
      ((integer? (car dictvar)) (cond
                                 ((null? symvar) (pattern-matcher dictvar (read)))
                                 (else (pattern-matcher (sym-fill (cdr dictvar) (car dictvar) symvar) '()))))
      ((string? (car dictvar)) (println (string-mash dictvar)))
      (else (display "not null dict, but I'm not sure what it is...")))))
;(pattern-matcher (list (list 'a)) 'hello)
      
;;MAIN FUNC;;
(define main 
  (lambda ()
    (define pattern-dict (list (list 'hello "hi")
                               (list 'how 'are 'you "I am fine, thank you")
                               (list 'my 'name 'is 1 "Hi " 1)
                               (list 'i 'like 1 'and 2 "Do you prefer " 1 " or " 2 "?")))
    (pattern-matcher pattern-dict '())
    (main)))
(main)
