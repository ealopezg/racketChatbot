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
        (srfi :19))

#|
 Descripcion :
 Dominio     :
 Recorrido   : 
|#


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

(define (genChatbot nombre personalidad)
  '(nombre personalidad))

(define (chatbot? cbot)
  (if (list? cbot)
      (if (and (string? (car cbot) (number? (cdr cbot))))
          #t
          #f)
      #f))

(define (getCbotName cbot)
  (if (chatbot? cbot)
      (car cbot)
      #f))

(define (getCbotPer cbot)
  (if (chatbot? cbot)
      (cdr cbot)
      #f))



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
  (string-append log "\n\n" "BEGINDIALOG " (hora) "\n\n" (getCbotName chatbot) ": " (saludoInicial)))



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
  (string-append log "\n\n" (hora) " Usuario: " msg "\n" (genRespuesta msg chatbot log seed)))
  







(define cbot '("CHATBOT" 0))



      
