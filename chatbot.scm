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






(define diccionario
  (list (list "HOLA*" (list "Hola, ¿Cómo estas?" "2" "3"))
        (list "BIEN Y TU*" (list "Bien, con ganas de ayudarte"))
        (list "ADIOS*" (list "Que te vaya bien, fue un placer ayudarte"))
        (list "RECOMIENDA*" (list "Tengo muchas peliculas para recomendarte, pero primero necesito saber tus gustos"))
        
        )
  )

(define cartelera
  (list (list "ACCION"(list (cons "Ready Player One: Comienza el Juego" 7.9)
                            (cons "Pantera Negra" 7.8)
                            (cons "Rampage: Devastación" 6.6)
                            (cons "Deseo de Matar" 6.5)
                            (cons "Mazinger Z: INFINITY" 6.1)
                            (cons "Titanes del Pacífico: La Insurrección" 5.3)
                            (cons "Asalto en el Huracán" 4.9)
                            (cons "Un Viaje en el Tiempo" 3.3)
                            ))
        (list "ANIMACION"(list (cons "Coco" 8.5)
                               (cons "En Este Rincón del Mundo" 7.9)
                               ))
        (list "DRAMA"(list (cons "...Y de pronto el amanecer" 8.2)
                           ))
        (list "INFANTIL"(list (cons "Coco" 8.5)
                              (cons "Las travesuras de Peter Rabbit" 6.6)
                              ))
        (list "COMEDIA"(list (cons "Swing" 6.8)))
        (list "TERROR"(list (cons "Un Lugar en Silencio" 7.2)))
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

(define (genID cbot log)
  (define (genID1 cbot log n)
    (if (equal? log '())
        (list (getName cbot) n)
        (genID1 cbot (cdr log) (+ n 1))
        )) (genID cbot log 0))

           


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
      (addMsgToLog (getID chatbot) (addMsgToLog (getID chatbot) (addIDToLog (getID chatbot) log) (string-append "beginDialog a las " (hora))) (string-append (hora) " " (getName chatbot) ": " (saludoInicial)))
      )
  )
      



#|
 Descripcion : Funcion que transforma un string en una lista de palabras
 Dominio     : string
 Recorrido   : Lista de palabras que conforman el string de entrada
|#
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


       
#|
 Descripcion : Envia el mensaje del usuario
 Dominio     : mensaje, estructura chatbot, log y seed
 Recorrido   : Log modificado con el mensaje del usuario y seguido de la respuesta del chatbot
|#

(define (sendMessage msg chatbot log seed)
  (addMsgToLog (getID chatbot) (addMsgToLog (getID chatbot) log (string-append (hora) " USUARIO: " msg)) (string-append (hora) " " (getName chatbot) ": " (devolverFrase msg seed diccionario)) )
  )






  


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
   (if (>= (string-length mensaje) (- (string-length frasedicc) 1))
       (cond ( (and (equal? (last (string->list frasedicc)) #\*) (equal? (substring mensaje 0 (- (string-length frasedicc) 1)) (substring frasedicc 0 (- (string-length frasedicc) 1)))) #t)
             ( (and (equal? (car  (string->list frasedicc)) #\*) (equal? (substring-reverse mensaje (- (string-length frasedicc) 1)) (substring-reverse frasedicc (- (string-length frasedicc) 1)))) #t)
             (else #f)
             )
       #f))
  



#|
 Descripcion : Funcion que devuelve una frase que se encuentra en el diccionario, como respuesta a un mensaje
 Dominio     : string mensaje, seed y diccionario
 Recorrido   : string con la respuesta
|#
(define (devolverFrase mensaje seed diccionario)
  (if (eq? diccionario '())
      "Lo siento, no te entendí muy bien lo que querias decir"
      (if (parte? (string-upcase mensaje) (caar diccionario))
          (list-ref (cadar diccionario) (random-integer (length (cadar diccionario))))
          (devolverFrase mensaje seed (cdr diccionario))
          )))

#|
 Descripcion : Funcion que genera un usuario
 Dominio     : strings nombre,genero,pelicula,cine, dia, hora
 Recorrido   : lista
|#
(define (genUsr nombre genero pelicula cine dia hora)
  (list nombre genero pelicula cine (list dia hora)))

#|
 Descripcion : Funcion que comprueba si existe un usuario
 Dominio     : usuario
 Recorrido   : Booleano
|#
(define (usuario? usr)
  (if (list? usr)
      (if (and (string? (car usr)) (string? (cadr usr)) (string? (caddr usr)) (string? (cadddr usr)) (list? (car (cddddr usr))))
          #t
          #f)
      #f))

#|
 Descripcion : Funcion que genera un usuario vacio
 Dominio     : nada
 Recorrido   : usuario 
|#
(define (genEmptyUsr)
  (list "" "" "" "" (list "" "")))

#|
 Descripcion : Funcion que retorna el nombre del usuario
 Dominio     : usuario
 Recorrido   : string con el nombre
|#
(define (getUsrName usr)
  (if (usuario? usr)
      (car usr)
      #f))

#|
 Descripcion : Funcion que retorna el genero elegido por el usuario
 Dominio     : usuario
 Recorrido   : string con el genero elegido por el usuario
|#
(define (getUsrGen usr)
  (if (usuario? usr)
      (cadr usr)
      #f))

#|
 Descripcion : Funcion que retorna la pelicula elegida por el usuario
 Dominio     : usuario
 Recorrido   : string con la pelicula elegida por el usuario
|#
(define (getUsrPel usr)
  (if (usuario? usr)
      (caddr usr)
      #f))

#|
 Descripcion : Funcion que retorna el cine elegido por el usuario
 Dominio     : usuario
 Recorrido   : string con el cine elegido por el usuario
|#
(define (getUsrCin usr)
  (if (usuario? usr)
      (cadddr usr)
      #f))

#|
 Descripcion : Funcion que retorna el dia elegido por el usuario
 Dominio     : usuario
 Recorrido   : string con el dia elegido por el usuario
|#
(define (getUsrDia usr)
  (if (usuario? usr)
      (caar (cddddr usr))
      #f))

#|
 Descripcion : Funcion que retorna la hora elegida por el usuario
 Dominio     : usuario
 Recorrido   : string con la hora elegida por el usuario
|#
(define (getUsrHor usr)
  (if (usuario? usr)
      (cadar (cddddr usr))
      #f))


#|
 Descripcion : Funcion que agrega el nombre al usuario 
 Dominio     : usuario y string con el nombre
 Recorrido   : usuario
|#
(define (addUsrName usr nombre)
  (if (usuario? usr)
      (append (list nombre) (cdr usr))
      (genEmptyUsr))
  )

#|
 Descripcion : Funcion que agrega el genero al usuario 
 Dominio     : usuario y string con el genero
 Recorrido   : usuario
|#
(define (addUsrGen usr genero)
  (if (usuario? usr)
      (append (list (car usr)) (list genero) (cddr usr))
      (genEmptyUsr))
  )

#|
 Descripcion : Funcion que agrega el cine al usuario 
 Dominio     : usuario y string con el cine
 Recorrido   : usuario
|#
(define (addUsrCin usr cine)
  (if (usuario? usr)
      (append (list (car usr)) (list (cadr usr)) (list (caddr usr)) (list cine) (cddddr usr))
      (genEmptyUsr))
  )

#|
 Descripcion : Funcion que agrega la pelicula al usuario
 Dominio     : usuario y string con la pelicula
 Recorrido   : usuario
|#
(define (addUsrPel usr pelicula)
  (if (usuario? usr)
      (append (list (car usr)) (list (cadr usr)) (list pelicula) (cdddr usr))
      (genEmptyUsr))
  )

#|
 Descripcion : Funcion que agrega el dia al usuario
 Dominio     : usuario y string con el dia
 Recorrido   : usuario
|#
(define (addUsrDia usr dia)
  (if (usuario? usr)
      (append (list (car usr)) (list (cadr usr)) (list (caddr usr)) (list (cadddr usr)) (list (list dia (cadar (cddddr usr)))))
      (genEmptyUsr))
  )

#|
 Descripcion : Funcion que agrega el horario al usuario
 Dominio     : usuario y string con el horario
 Recorrido   : usuario
|#
(define (addUsrHor usr horario)
  (if (usuario? usr)
      (append (list (car usr)) (list (cadr usr)) (list (caddr usr)) (list (cadddr usr)) (list (list (caar (cddddr usr)) horario)))
      (genEmptyUsr))
  )

(define (accion mensaje log id diccionario actual)
  (if (= (caaar log) id)
      (






#|
 Descripcion : Funcion que comprueba si es o no un log
 Dominio     : Log
 Recorrido   : Booleano
|#
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

#|
 Descripcion : Funcion que añade el ID al log
 Dominio     : numero id, y log
 Recorrido   : log modificado
|#
(define (addIDToLog id log)
  (append log (list (list (list id (genEmptyUsr)) (list )))))

#|
 Descripcion : Funcion que añade mensajes al log
 Dominio     : numero id, log y el mensaje str
 Recorrido   : log modificado
|#
(define (addMsgToLog id log str)
  (if (equal? log '())
      '()
  (if (= (caaar log) id)
      (append (list (list (caar log) (append (cadar log) (list str)))) (addMsgToLog id (cdr log) str))
      (append (list (caar log)) (addMsgToLog id (cdr log) str)))))



(define cbot '("CHATBOT" 0))


(define (lines->string lista)
  (if (equal? lista '())
      ""
      (string-append (car lista) "\n" (lines->string (cdr lista)))))



#|
 Descripcion : Funcion que recomienda la pelicula mejor calificada del genero escogido
 Dominio     : string genero, y cartelera
 Recorrido   : par donde el primer elemento es el nombre de la pelicula y el segundo es el puntaje
|#
(define (recomendarPelicula genero cartelera)
  (if (equal? cartelera '())
              #f
              (if (equal? (caar cartelera) genero)
                  (caadar cartelera)
                  (recomendarPelicula genero (cdr cartelera))
                  )
              )
  )

#|
 Descripcion : Funcion que devuelve una lista con todas las peliculas del genero
 Dominio     : string genero y cartelera
 Recorrido   : lista de pares donde el primer elemento del par es el nombre de la pelicula y el segundo es el puntaje
|#
(define (devolverPeliculas genero cartelera)
  (if (equal? cartelera '())
              #f
              (if (equal? (caar cartelera) genero)
                  (cadar cartelera)
                  (devolverPeliculas genero (cdr cartelera))
                  )
              )
  )

#|
 Descripcion : Funcion que retorna un string en base a una lista de peliculas (recorrido de la funcion (devolverPeliculas)
 Dominio     : lista de peliculas
 Recorrido   : string
|#
(define (listaPeliculas->string lista)
  (define (pe->s lista n)
    (if (equal? lista '())
        ""
        (string-append (number->string n) ".- " (caar lista) " (" (number->string (cdar lista)) ")" "\n" (pe->s (cdr lista) (+ n 1)))))
  (pe->s lista 1))

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
      
      




(define log3 (sendMessage "Me llamo Esteban" (list "CBOT" 1) (sendMessage "que wea te pasa conchetumare" (list "CBOT" 1) (beginDialog (list "CBOT" 1) '() 0) 0) 0))



      
