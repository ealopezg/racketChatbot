#!r6rs
(import (rnrs base (6))                        ; R6RS chapter 9
        (rnrs unicode (6))                     ; R6RS library chapter 1
        (rnrs bytevectors (6))                 ; R6RS library chapter 2
        (rnrs lists (6))                       ; R6RS library chapter 3
        (rnrs sorting (6))                     ; R6RS library chapter 4
        (rnrs control (6))                     ; R6RS library chapter 5
        (rnrs exceptions (6))                  ; R6RS library section 7.1
        (rnrs conditions (6))                  ; R6RS library sections 7.2 and 7.3
        (rnrs io ports (6))                    ; R6RS library sections 8.1 and 8.2
        (rnrs io simple (6))                   ; R6RS library sections 8.1 and 8.3
        (rnrs files (6))                       ; R6RS library chapter 9
        (rnrs programs (6))                    ; R6RS library chapter 10
        (rnrs arithmetic fixnums (6))          ; R6RS library section 11.2
        (rnrs arithmetic flonums (6))          ; R6RS library section 11.3
        (rnrs arithmetic bitwise (6))          ; R6RS library section 11.4
        (rnrs syntax-case (6))                 ; R6RS library chapter 12
        (rnrs hashtables (6))                  ; R6RS library chapter 13
        (rnrs enums)                           ; R6RS library chapter 14
        (rnrs (6))                             ; R6RS library chapter 15
        (rnrs eval (6))                        ; R6RS library chapter 16
        (rnrs mutable-pairs (6))               ; R6RS library chapter 17
        (rnrs mutable-strings (6))             ; R6RS library chapter 18
        (rnrs r5rs (6))                        ; R6RS library chapter 19
        (rnrs records procedural (6))          ; R6RS library section 6.3
        (rnrs records inspection (6))          ; R6RS library section 6.4
        (rnrs records syntactic (6))           ; R6RS library section 6.2
        (srfi :19)
        (srfi :27))

#|
 Descripcion :
 Dominio     :
 Recorrido   : 
|#

(define log1 (list (list 1 (list "frase11" "frase21" "frase31" "frase41"))
                   (list 2 (list "frase12" "frase22" "frase32" "frase42"))
                   (list 3 (list "frase13" "frase23" "frase33" "frase43"))
                   ))





(define diccionario
  (list (list "*CONCHETUMARE" (list "shhh andai shoro que wea"))
        (list "HOLA*" (list "Hola, ¿Cómo estas?" "2" "3"))
        (list "BIEN Y TU*" (list "Bien, con ganas de ayudarte"))
        (list "ADIOS*" (list "Que te vaya bien, fue un placer ayudarte"))
        (list "QUE CUENTAS" (list "1 2 3 4 5 6....."))
        (list "*REVISAR VUELO" (list "Veamos que tenemos aqui"))
        )
  )


#|
 Descripcion : Crea el saludo inicial a partir de la hora.
 Dominio     : nada
 Recorrido   : string con el saludo dependiendo de la hora actual.
|#
(define (saludoInicial)
     (cond ((>= (string->number (date->string (current-date) "~H") ) 21) "Buenas noches, en que lo puedo ayudar ?")
           ((<= (string->number (date->string (current-date) "~H") ) 6) "Buenas noches, en que lo puedo ayudar a esta hora de la madrugada?")
           ((<= (string->number (date->string (current-date) "~H") ) 11) "Buenos dias, en que lo puedo ayudar a esta hora?")
           ((<= (string->number (date->string (current-date) "~H") ) 20) "Buenos tardes, en que lo puedo ayudar a esta hora?")
           )
  )


#|
 Descripcion : Genera un chatbot a partir de su nombre y personalidad
 Dominio     : String con el nombre y entero con la personalidad
 Recorrido   : Arreglo
|#

(define (genChatbot nombre id)
  '(nombre id))

(define (chatbot? cbot)
  (if (list? cbot)
      (if (and (string? (car cbot)) (number? (cadr cbot)))
          #t
          #f)
      #f))

(define (getName cbot)
  (if (chatbot? cbot)
      (car cbot)
      #f))

(define (getID cbot)
  (if (chatbot? cbot)
      (cadr cbot)
      0))


(define (genID cbot)
  (list (getName cbot) (random-integer 10000)))


#|
 Descripcion : Devuelve la hora actual en formato de string
 Dominio     : nada
 Recorrido   : Hora actual como string DIA/MES/AÑO HORA:MINUTOS:SEGUNDOS
|#
(define (hora)
  (date->string (current-date) "~d/~m/~Y ~H:~M:~S")) 

#|
 Descripcion : Comienza una nueva conversacion
 Dominio     : Estructura chatbot, log y la semilla para respuestas pseudoaleatorias
 Recorrido   : log modificado con el delimitador BEGINDIALOG y el saludo inicial
|#
(define (beginDialog chatbot log seed)
  (if (= (getID chatbot) 0)
      (beginDialog (genID chatbot) log seed)
      (addToLog (getID chatbot) (addToLog (getID chatbot) (addIDToLog (getID chatbot) log) (string-append "beginDialog a las " (hora))) (string-append (hora) " CHATBOT:" (saludoInicial)))
      )
  )
      



#|
(define (genRespuesta msg chatbot log seed)
  (if (chatbot? chatbot)
      (let ((lista (split msg)))
      (if (= (getCbotPer chatbot) 0)
          (let ((lista (split msg)))
            )
|#

(define (genRespuesta msg chatbot log seed) #t)

(define (split str)
(define (split1 lista aux)
     (if (= (length lista) 0)
         (list (list->string aux))
         (if (eq? (car lista) #\space)
             (append (list (list->string aux)) (split1 (cdr lista) '()))
             (split1 (cdr lista) (append aux (list (car lista))))
             )
         )
  ) (split1 (string->list str) '()))


(define (searchWord str palabra)
  (define (searchWord1 lista palabra)
    (if (= (length lista) 0)
        #f
        (if (eq? (car lista) palabra)
            #t
            (searchWord1 (cdr lista) palabra)
            )
        )
    )
(searchWord1 (split str) palabra))


       
#|
 Descripcion : Envia el mensaje del usuario
 Dominio     : mensaje, estructura chatbot, log y seed
 Recorrido   : Log modificado con el mensaje del usuario y seguido de la respuesta del chatbot
|#
(define (sendMessage msg chatbot log seed)
  (addToLog (getID chatbot) (addToLog (getID chatbot) log msg) (devolverFrase msg seed diccionario)))

  


#|
 Descripcion : Genera una lista de numeros pseudoaleatorios en base al algoritmo descrito en
               https://es.wikipedia.org/wiki/Generador_de_n%C3%BAmeros_aleatorios
 Dominio     : enteros semilla , a, c, m y i=0
 Recorrido   : lista de m numeros pseudoaleatorios
|#
(define (genNumeros semilla a c m i)
  (if (= i m)
      '()
      (if (= i 0)
          (append (list semilla) (genNumeros semilla a c m (+ 1 i)))
          (append (list (modulo (+ (* a semilla) c) m)) (genNumeros (modulo (+ (* a semilla) c) m) a c m (+ 1 i))))))



#|
 Descripcion : Llama a la funcion genNumeros(),genera una lista de numeros pseudoaleatorios en base al algoritmo descrito en
               https://es.wikipedia.org/wiki/Generador_de_n%C3%BAmeros_aleatorios , utilizando los enteros:
               a = 1103515245
               c = 12345
               m = 32768

 Dominio     : semilla, si esta es 0 se ocupa (time-second (current-time))
 Recorrido   : lista de m números pseudoaleatorios
|#
(define (randomList seed)
  (if (= seed 0)
      (genNumeros (time-second (current-time)) 1103515245 12345 32768 0)      
      (genNumeros seed 1103515245 12345 32768 0)))

#|
 Descripcion : Devuelve un numero pseudoaleatorio en el rango [0,n[
 Dominio     : semilla y n
 Recorrido   : entero
|#
(define (randomInt seed n)
  (modulo (list-ref (randomList seed) (/ 32768 2)) n))

(random-integer 10)


#|
 Descripcion : Devuelve un string con unicamente los n últimos elementos
 Dominio     : string y entero n
 Recorrido   : entero
|#
(define (substring-reverse str n)
  (define (substring1 lista n i)
    (if (= i n)
        lista
        (substring1 (cdr lista) n (- i 1))))
  (list->string (substring1 (string->list str) n  (string-length str))))



#|
 Descripcion : Funcionque devuelve el ultimo elemento de un arreglo
 Dominio     : Lista
 Recorrido   : Elemento que contenga la lista
|#
(define (last lista)
  (if (= (length lista) 1)
      (car lista)
      (last (cdr lista))))


#|
 Descripcion : Comprueba si un string contiene al elemento frasedicc
               Ejemplo: El mensaje "bueno, quiero una pizza" contiene el elemento "*quiero una pizza".
 Dominio     : string en mensaje y frasedicc, frasedicc debe tener un * antes o despues de lo escrito
 Recorrido   : Booleano
|#
(define (parte? mensaje frasedicc)
  (if (eq? (last (string->list frasedicc)) #\*)
      (if (equal? (substring mensaje 0 (- (string-length frasedicc) 1)) (substring frasedicc 0 (- (string-length frasedicc) 1)))
          #t
          #f)
      (if (equal? (car  (string->list frasedicc)) #\*)
          (if (equal? (substring-reverse mensaje (string-length frasedicc)) (string-append " " (substring-reverse frasedicc (- (string-length frasedicc) 1))))
          #t
          #f)
          #f)))



#|
 Descripcion :
 Dominio     :
 Recorrido   : 
|#
(define (devolverFrase mensaje seed diccionario)
  (if (eq? diccionario '())
      #f
      (if (parte? (string-upcase mensaje) (caar diccionario))
          (list-ref (cadar diccionario) (random-integer (length (cadar diccionario))))
          (devolverFrase mensaje seed (cdr diccionario))
          )))

(define (devolverAccion mensaje seed diccionario)
  (if (eq? diccionario '())
      #f
      (if (parte? (string-upcase mensaje) (caar diccionario))
          (caar diccionario)
          (devolverFrase mensaje seed (cdr diccionario))
          )))


(define (revisarVuelos id log)
(define (revisarVuelos1 id lista)
  (if (equal? lista '())
      #f
      (if (equal? (car lista) id)
          (if ( "VUELO:")
              #t
              #f)
          #f)))
  (revisarVuelos1 id (split log)))



    



(define (chat chatbot log seed)
  (cond ((eq? chatbot #f) (display log))
        (else (let ((msg (get-line (current-input-port))))
                      (begin
                        (display (string-append "USUARIO: " msg "\n"))
                        (display log)
                        (chat chatbot (sendMessage msg chatbot log seed) seed )
                        )
                )
              )
        )
  )



(define (log? lg)
  (if (equal? lg '())
      #t
      (if (list? (car lg))
          (if (number? (caar lg))
              (if (list? (cdar lg))
                  (log? (cdr lg))
                  #f)
              #f)
          #f)
      )
  )


(define (addIDToLog id log)
  (append log (list (list id (list )))))


(define (addToLog id log str)
  (if (equal? log '())
      '()
  (if (= (caar log) id)
      (append (list (list (caar log) (append (cadar log) (list str)))) (addToLog id (cdr log) str))
      (append (list (car log)) (addToLog id (cdr log) str)))))



(define cbot '("CHATBOT" 0))


(define (lines->string lista)
  (if (equal? lista '())
      ""
      (string-append (car lista) "\n" (lines->string (cdr lista)))))




#|
 Descripcion : Funcion que transforma un log en string
 Dominio     : log
 Recorrido   : string
|#
(define (log->string log)
  (if (equal? log '())
      ""
      (if (log? log)
          (string-append "ID: " (number->string (caar log)) "\n" (lines->string (cadar log)) "\n" (log->string (cdr log)))
          "")))
      
      


(define log2 (beginDialog (list "CBOT" 1) '() 0))
(display (devolverFrase "hola" 0 diccionario))


      
