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

(define (crearChatbot nombre personalidad)
  '(nombre personalidad))

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
  (string-append log "\n\n" "BEGINDIALOG" (hora) "\n\n" (car chatbot) ": " (saludoInicial)))

#|
 Descripcion : Envia el mensaje del usuario
 Dominio     : mensaje, estructura chatbot, log y seed
 Recorrido   : Log modificado con el mensaje del usuario y seguido de la respuesta del chatbot
|#
(define (sendMessage msg chatbot log seed)
  (string-append log "\n\n" (hora) " Usuario: " msg))
  







(define cbot '("CHATBOT" 0))



      
