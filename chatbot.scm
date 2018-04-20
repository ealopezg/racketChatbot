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
        (list "*TU NOMBRE" (list "Mi nombre es secreto" "Chupame el pico"))
        
        )
  )









(define cartelera
  (list (list "ACCION"(list (list "Ready Player One: Comienza el Juego" 7.9 )
                            (list "Pantera Negra" 7.8)
                            (list "Rampage: Devastación" 6.6)
                            (list "Deseo de Matar" 6.5)
                            (list "Mazinger Z: INFINITY" 6.1)
                            (list "Titanes del Pacífico: La Insurrección" 5.3)
                            (list "Asalto en el Huracán" 4.9)
                            (list "Un Viaje en el Tiempo" 3.3)
                            ))
        (list "ANIMACION"(list (list "Coco" 8.5)
                               (list "En Este Rincón del Mundo" 7.9)
                               ))
        (list "DRAMA"(list (list "...Y de pronto el amanecer" 8.2)
                           ))
        (list "INFANTIL"(list (list "Coco" 8.5)
                              (list "Las travesuras de Peter Rabbit" 6.6)
                              ))
        (list "COMEDIA"(list (list "Swing" 6.8)))
        (list "TERROR"(list (list "Un Lugar en Silencio" 7.2)))
        )
  )

;#################################################################################################################################
;                                         Creacion de horarios pseudoaleatorios
                            
(define listaHorarios (list (cons 13 15) (cons 09 15) (cons 22 15) (cons 09 50) (cons 21 30)  (cons 11 30)  (cons 14 20) (cons 17 50)  (cons 23 40)))

#|
 Descripcion : Funcion que revisa si un elemento es parte de una lista
 Dominio     : Elemento y lista
 Recorrido   : Booleano
|#
(define (member? elemento lista)
  (if (equal? lista '())
      #f
      (if (equal? (car lista) elemento)
          #t
          (member? elemento (cdr lista)))))
      

#|
 Descripcion : Funcion que genera horarios para una lista de peliculas
 Dominio     : Lista de peliculas y seed
 Recorrido   : lista de Peliculas modificada con su horario
|#
(define (generarHorariosListaPeliculas listaPeliculas seed)
  (if (equal? listaPeliculas '())
      '()
      (append (list (append (car listaPeliculas) (list (generarHorarios seed)))) (generarHorariosListaPeliculas (cdr listaPeliculas) (random seed)))))


#|
 Descripcion : Funcion que genera una cartelera con los horarios disponibles
 Dominio     : cartelera y semilla
 Recorrido   : cartelera con horarios
|#
(define (generarCarteleraConHorarios cartelera seed)
  (if (equal? cartelera '())
      '()
      (append (list (list (caar cartelera) (generarHorariosListaPeliculas (cadar cartelera) seed))) (generarCarteleraConHorarios (cdr cartelera) (random seed)))))


#|
 Descripcion : Funcion que elige n elementos al azar de una lista
 Dominio     : lista n y semilla
 Recorrido   : lista de n elementos de la lista original
|#
(define (elegirAlgunos lista n seed)
  (define (elegirAlgun1 lista n i seed listaAux)
    (if (= i n)
        listaAux
        (let ((seleccion (elegir seed lista)))
          (if (member? seleccion listaAux)
              (elegirAlgun1 lista n i (random seed) listaAux)
              (elegirAlgun1 lista n (+ 1 i) (random seed) (append listaAux (list seleccion)))
              )
          )
        )
    )(elegirAlgun1 lista n 0 seed '()))

#|
 Descripcion : Funcion que genera el horario de una pelicula
 Dominio     : semilla
 Recorrido   : lista de horarios para cada dia de la semana
|#
(define (generarHorarios seed)
  (define (generarHor1 seed dia)
    (if (= dia 7)
        '()
        (append (list (quickSort (elegirAlgunos listaHorarios (remainder (random seed) 6) (random seed))) ) (generarHor1 (random seed) (+ dia 1)))))
  (generarHor1 seed 0))
      
#|
 Descripcion : Funcion que devuelve los horarios más tardes que n
 Dominio     : lista de horarios y horario n
 Recorrido   : lista de horarios 
|#
(define (mayores lista n)
  (if (= (length lista) 0)
      '()
      (if (or (> (caar lista) (car n)) (and (= (caar lista) (car n)) (>= (cdar lista) (cdr n))))
          (append (list (car lista)) (mayores (cdr lista) n))
          (mayores (cdr lista) n))))
#|
 Descripcion : Funcion que devuelve los horarios más tempranos que n
 Dominio     : lista de horarios y horario n
 Recorrido   : lista de horarios 
|#
(define (menores lista n)
  (if (= (length lista) 0)
      '()
      (if (<= (caar lista) (car n))
          (append (list (car lista)) (menores (cdr lista) n))
          (menores (cdr lista) n))))



#|
 Descripcion : Funcion que ordena un arreglo de pares
 Dominio     : Lista de pares
 Recorrido   : Lista de pares
|#
(define (quickSort lista)
    (if (= (length lista) 0)
        '()
        (if (= (length lista) 1)
            lista
        (if (= (length lista) 2)
            (if (or (> (caadr lista) (caar lista)) (and (= (caadr lista) (caar lista)) (>= (cdadr lista) (cdar lista))))
                lista
                (list (cadr lista) (car lista)))
            (append (quickSort (menores (cdr lista) (car lista))) (list (car lista)) (quickSort (mayores (cdr lista) (car lista))))))))

#|
 Descripcion : Funcion que devuelve la lista de horarios de la semana de una pelicula
 Dominio     : lista de peliculas y string con el nombre de la pelicula
 Recorrido   : lista de horarios de la semana
|#
(define (devolverListaHorarios listaPeliculas pelicula)
  (if (equal? listaPeliculas '())
      ""
      (if (equal? (caar listaPeliculas) pelicula)
          (caddar listaPeliculas)
          (devolverListaHorarios (cdr listaPeliculas) pelicula))))

#|
 Descripcion : Funcion que transforma una lista de horarios diarios en un string para que el chatbot se lo muestre al usuario
 Dominio     : lista de horarios diarios
 Recorrido   : string
|#
(define (listaHorariosDiaString listaHorarios)
  (if (equal? listaHorarios '())
      "No hay funciones ese dia"
      (if (= (length listaHorarios) 1)
          (string-append (number->string (caar listaHorarios)) ":" (number->string (cdar listaHorarios)))
          (string-append (number->string (caar listaHorarios)) ":" (number->string (cdar listaHorarios)) " - " (listaHorariosDiaString (cdr listaHorarios))))))

#|
 Descripcion : Funcion que transforma una lista de horarios diarios en un string para que el chatbot se lo muestre al usuario
 Dominio     : chatbot log y el dia de la semana
 Recorrido   : string (llamado a la funcion listaHorariosDiaString)
|#
(define (stringHorarios chatbot log dia)
  (cond ((equal? dia "Lunes") (listaHorariosDiaString (list-ref (devolverListaHorarios (devolverPeliculas (getUsrGen (getUsrLog log (getID chatbot))) (getCartelera chatbot)) (getUsrPel (getUsrLog log (getID chatbot)))) 0)))
        ((equal? dia "Martes") (listaHorariosDiaString (list-ref (devolverListaHorarios (devolverPeliculas (getUsrGen (getUsrLog log (getID chatbot))) (getCartelera chatbot)) (getUsrPel (getUsrLog log (getID chatbot)))) 1)))
        ((equal? dia "Miercoles") (listaHorariosDiaString (list-ref (devolverListaHorarios (devolverPeliculas (getUsrGen (getUsrLog log (getID chatbot))) (getCartelera chatbot)) (getUsrPel (getUsrLog log (getID chatbot)))) 2)))
        ((equal? dia "Jueves") (listaHorariosDiaString (list-ref (devolverListaHorarios (devolverPeliculas (getUsrGen (getUsrLog log (getID chatbot))) (getCartelera chatbot)) (getUsrPel (getUsrLog log (getID chatbot)))) 3)))
        ((equal? dia "Viernes") (listaHorariosDiaString (list-ref (devolverListaHorarios (devolverPeliculas (getUsrGen (getUsrLog log (getID chatbot))) (getCartelera chatbot)) (getUsrPel (getUsrLog log (getID chatbot)))) 4)))
        ((equal? dia "Sabado") (listaHorariosDiaString (list-ref (devolverListaHorarios (devolverPeliculas (getUsrGen (getUsrLog log (getID chatbot))) (getCartelera chatbot)) (getUsrPel (getUsrLog log (getID chatbot)))) 5)))
        ((equal? dia "Domingo") (listaHorariosDiaString (list-ref (devolverListaHorarios (devolverPeliculas (getUsrGen (getUsrLog log (getID chatbot))) (getCartelera chatbot)) (getUsrPel (getUsrLog log (getID chatbot)))) 6)))
        (else "")
        ))





                                    

#|
 Descripcion : Crea el saludo inicial a partir de la hora.
 Dominio     : nada
 Recorrido   : string con el saludo dependiendo de la hora actual.
|#
(define (saludoInicial seed)
     (cond ((>= (string->number (date->string (current-date) "~H") ) 21) (elegir seed (list "Buenas noches, en que lo puedo ayudar ?" "Hola que tal, que buena noche para ver ir al cine o no?, Que necesitas?")))
           ((<= (string->number (date->string (current-date) "~H") ) 6) (elegir seed (list "Buenas noches, en que lo puedo ayudar a esta hora de la madrugada?" "Aunque sea tarde igual te puedo atender, las maquinas no duermen.Dime...En que te ayudo?")))
           ((<= (string->number (date->string (current-date) "~H") ) 11) (elegir seed (list "Buenos dias, en que lo puedo ayudar a esta hora?" "Hola, muy buenos dias, estoy aqui para ayudarte. Qué necesitas?")))
           ((<= (string->number (date->string (current-date) "~H") ) 20) (elegir seed (list "Buenos tardes, en que lo puedo ayudar a esta hora?" "Hola, muy buenas tardes, estoy aqui para ayudarte. Qué necesitas?")))
           )
  )



;##  TDA CHATBOT

#|
 Descripcion : Genera un chatbot a partir de su nombre y personalidad
 Dominio     : String con el nombre y entero con el id
 Recorrido   : chatbot
|#
(define (genChatbot nombre id cartelera diccionario)
  (list nombre id cartelera diccionario))

#|
 Descripcion : Funcion que comprueba si la entrada es un chatbot o no
 Dominio     : cualquier cosa
 Recorrido   : chatbot
|#
(define (chatbot? cbot)
  (if (list? cbot)
      (if (and (string? (car cbot)) (number? (cadr cbot)) (list? (caddr cbot)) (list? (cadddr cbot)))
          #t
          #f)
      #f))

(define (getDiccionario cbot)
  (if (chatbot? cbot)
      (cadddr cbot)
      #f))
  

#|
 Descripcion : Funcion que devuelve la cartelera de un chatbot
 Dominio     : chatbot
 Recorrido   : cartelera
|#

(define (getCartelera cbot)
  (if (chatbot? cbot)
      (caddr cbot)
      #f))

#|
 Descripcion : Funcion que devuelve el nombre del chatbot
 Dominio     : chatbot
 Recorrido   : string con el nombre
|#
(define (getName cbot)
  (if (chatbot? cbot)
      (car cbot)
      #f))


#|
 Descripcion : Funcion que devuelve el id de un chatbot
 Dominio     : Chatbot
 Recorrido   : string con el id
|#
(define (getID cbot)
  (if (chatbot? cbot)
      (cadr cbot)
      0))

#|
 Descripcion : Funcion que genera un nuevo id para el chatbot a partir de un log
 Dominio     : chatbot y log
 Recorrido   : chatbot
|#
(define (genID cbot log)
  (define (genID1 cbot log n)
    (if (equal? log '())
        (list (getName cbot) n)
        (genID1 cbot (cdr log) (+ n 1))
        )) (genID1 cbot log 0))





        
        


(define (elegir seed lista)
  (define (elegir1 lista i n)
    (if (= i n)
        (car lista)
        (elegir1 (cdr lista) (+ i 1) n)))
  (elegir1 lista 0 (remainder (random seed) (length lista)))) 

           


#|
 Descripcion : Devuelve la hora actual en formato de string
 Dominio     : nada
 Recorrido   : Hora actual como string DIA/MES/AÑO HORA:MINUTOS:SEGUNDOS
|#
(define (hora)
  (date->string (current-date) "[~d/~m/~Y ~H:~M:~S]")) 

#|
 Descripcion : Comienza una nueva conversacion
 Dominio     : Estructura chatbot, log y la semilla para respuestas pseudoaleatorias
 Recorrido   : log modificado con el delimitador BEGINDIALOG y el saludo inicial
|#
(define (beginDialog chatbot log seed)
  (if (= (getID chatbot) -1)
      (beginDialog (genID chatbot log) log seed)
      (addMsgToLog (getID chatbot) (addMsgToLog (getID chatbot) (addIDToLog (getID chatbot) log) (string-append "beginDialog a las " (hora))) (string-append (hora) " " (getName chatbot) ": " (saludoInicial seed)))
      )
  )

#|
 Descripcion : Funcion que finaliza el chat
 Dominio     : chatbot log y seed
 Recorrido   : log modificado
|#
(define (endDialog chatbot log seed)
  (addMsgToLog (getID chatbot) log (string-append "endDialog a las " (hora))))
      



#|
 Descripcion : Funcion que transforma un string en una lista de palabras
 Dominio     : string
 Recorrido   : Lista de palabras que conforman el string de entrada
|#
(define (split str e)
(define (split1 lista aux e)
     (if (= (length lista) 0)
         (list (list->string aux))
         (if (eq? (car lista) e)
             (append (list (list->string aux)) (split1 (cdr lista) '() e))
             (split1 (cdr lista) (append aux (list (car lista))) e)
             )
         )
  ) (split1 (string->list str) '() e))


       
#|
 Descripcion : Envia el mensaje del usuario
 Dominio     : mensaje, estructura chatbot, log y seed
 Recorrido   : Log modificado con el mensaje del usuario y seguido de la respuesta del chatbot******CAMBIAR ESTA WEA
|#

(define (sendMessage msg chatbot log seed)
  (accion msg chatbot log seed)
  )


#|
 Descripcion : Devuelve un numero pseudoaleatorio 
 Dominio     : semilla
 Recorrido   : entero
|#
(define (random seed)
  (if (equal? seed "time")
      (remainder (+ (* 1103515245 (time-second (current-time))) 12345) 2147483648)
      (remainder (+ (* 1103515245 seed) 12345) 2147483648))
  )


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
 Descripcion : Funcion que devuelve el ultimo elemento de un arreglo
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
          (let ((ans (list-ref (cadar diccionario) (random-integer (length (cadar diccionario))))))
            ans)
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


(define (getUsrLog log id)
  (if (equal? log '())
      #f
      (if (= id (caaar log))
          (cadaar log)
          (getUsrLog (cdr log) id))))
      

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

(define (addUsrNameL id nombre log)
   (if (equal? log '())
        '()
       (if (= (caaar log) id)
           (append (list (list (list id (addUsrName (cadaar log) nombre)) (cadar log))) (addUsrNameL id nombre (cdr log)))
           (append (list (car log)) (addUsrNameL id nombre (cdr log)))
           )
       )
  )

#|
 Descripcion : Funcion que devuelve el nombre de usuario de la conversacion id
 Dominio     : id y log
 Recorrido   : string con el nombre de usuario, en el caso de que no exista nombre se utiliza usuario
|#
(define (getUsrNameL id log)
  (let ((nombre (getUsrName (getUsrLog log id))))
  (if (equal? nombre "")
      "DESCONOCIDO"
      nombre
  )))

#|
 Descripcion : Funcion que agrega el genero al usuario 
 Dominio     : usuario y string con el genero
 Recorrido   : usuario
|#
(define (addUsrGene usr genero)
  (if (usuario? usr)
      (append (list (car usr)) (list genero) (cddr usr))
      (genEmptyUsr))
  )



#|
 Descripcion : Funcion que agrega el genero al usuario id del log
 Dominio     : log id y string con el genero
 Recorrido   : log modificado
|#
(define (addUsrGeneL id genero log)
   (if (equal? log '())
        '()
       (if (= (caaar log) id)
           (append (list (list (list id (addUsrGene (cadaar log) genero)) (cadar log))) (addUsrGeneL id genero (cdr log)))
           (append (list (car log)) (addUsrGeneL id genero (cdr log)))
           )
       )
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

(define (addUsrCinL id cine log)
   (if (equal? log '())
        '()
       (if (= (caaar log) id)
           (append (list (list (list id (addUsrCin (cadaar log) cine)) (cadar log))) (addUsrCinL id cine (cdr log)))
           (append (list (car log)) (addUsrCinL id cine (cdr log)))
           )
       )
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


(define (addUsrPelL id pelicula log)
   (if (equal? log '())
        '()
       (if (= (caaar log) id)
           (append (list (list (list id (addUsrPel (cadaar log) pelicula)) (cadar log))) (addUsrPelL id pelicula (cdr log)))
           (append (list (car log)) (addUsrPelL id pelicula (cdr log)))
           )
       )
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


(define (addUsrDiaL dia id log)
   (if (equal? log '())
        '()
       (if (= (caaar log) id)
           (append (list (list (list id (addUsrDia (cadaar log) dia)) (cadar log))) (addUsrDiaL dia id (cdr log)))
           (append (list (car log)) (addUsrDiaL dia id (cdr log)))
           )
       )
  )


#|
 Descripcion : Funcion que agrega el horario al usuario
 Dominio     : usuario y string con la hora
 Recorrido   : usuario
|#
(define (addUsrHor usr hora)
  (if (usuario? usr)
      (append (list (car usr)) (list (cadr usr)) (list (caddr usr)) (list (cadddr usr)) (list (list (caar (cddddr usr)) hora)))
      (genEmptyUsr))
  )

(define (addUsrHorL id hora log)
   (if (equal? log '())
        '()
       (if (= (caaar log) id)
           (append (list (list (list id (addUsrHor (cadaar log) hora)) (cadar log))) (addUsrHorL  id hora (cdr log)))
           (append (list (car log)) (addUsrHorL  id hora (cdr log)))
           )
       )
  )

(define diccionarioFormal (list
                     (list (list "¿Qué genero te gusta?" "¿Cual es el tipo de películas que más le agrada?" "¿Que genero suele ver?")                     ;RECOMENDACION
                           (list "Le recomiendo la siguiente película " "Mis algoritmos detectan que la mejor película para usted es " "Tengo una película para usted, es ")
                           (list "¿Desea comprar entradas?" "¿Le gustaria pasar al cine a verla?" "¿Le apetece reservar una entrada?")
                           (list "Fue un placer ayudarlo que tenga un gran día" "Bueno, cualquier cosa me puede volver a hablar" "Una lastima...Pero bueno, otro día hablaremos de nuevo");Negacion comprar entrada
                           (list "¿Qué dia le gustaria ir?" "¿Cuando tiene pensado en ir a verla?" "¿Que día asistirá al cine?");Pregunta sobre el dia
                           (list "Para esta película le puedo ofrecer estos horarios.." "Tengo estos horarios " "Le ofrezco estos horarios..")
                           (list "¿A qué hora sería su visita?" "¿A que hora quiere que reserve la entrada?" "La entrada..¿A qué hora?")
                           (list "¿Qué cine le queda mas cerca, Norte, Centro o Sur?" "¿A qué cine va a ir, Norte, Centro o Sur?" "¿A qué cine se dirige, Norte, Centro o Sur?");Pregunta sobre la hora
                           (list "¿Su nombre cuál es?" "¿Su nombre?" "Vaya..todavia no le he pedido su nombre ¿Cómo se llama?");Pregunta sobre el nombre
                           (list "Bueno, está lista la reserva a nombre de " "Listo, ya he hecho la reserva a nombre de" "Su entrada ha sido reservada con exito, se encuentra a nombre de")
                           )
                     (list (list "¿Cual es la película que quiere ver?" "¿De qué pelicula estamos hablando?" "Dificil elegir entre tantas películas, ¿Cual es el nombre de su película elegida?") ;COMPRAR ENTRADA
                           (list "hdf1ujdf" "fhj1dfhjd" "dhfd1gfg")
                           (list "hdfu1jdf" "fhjd1fhjd" "dhfdgf1g")
                           )
                     
                     (list (list "¿Para que dia quieres que le muestre la cartelera?" "¿Qué día quiere ver la cartelera?" "Permitame saber el día") ;Mostrar cartelera
                           (list "Estas son las peliculas para el día " "Estas películas le puedo ofrecer el dia " "Tenemos estas peliculas para el día")
                           (list "Dime el nombre de la pelicula para detallarle los horarios" "Digame como se llama la película para poder mostrarle los horarios" "Permitame saber el nombre de la película")
                           
                           )
                     (list (list "Adios, fue un placer haberlo ayudado" "Que tenga un buen día...hasta luego" "Gracias por utilizar nuestros servicios...Nos vemos pronto"))
                     
                     )
  )
                           

(define (respuesta modo etapa diccionario seed)
  (elegir seed (list-ref (list-ref diccionario modo) etapa)))

(define (respuesta? modo etapa mensaje diccionario)
  (member? mensaje (list-ref (list-ref diccionario modo) etapa))) 
  
              


;(define (encontrarPelicula chatbot mensaje)
;  (define (eP cartelera mensaje)
;    (if (equal? cartelera '())
;        #f
;        (if (equal? (cadar cartelera) '())
;             (eP (cdr cartelera) mensaje)
;             (if (equal? (string-upcase (car (caadar cartelera))) (string-upcase mensaje))
;                 (cons (caar cartelera) (car (caadar cartelera)))
;                 (eP (append (list (list (caar cartelera) (cdadar cartelera))) (cdr cartelera)) mensaje)
;                 )
;            )
;        )
;    )
;    
;        
;  (eP (getCartelera chatbot) mensaje ))

(define (devolverCarteleraPorDia cartelera dia)
  (cond ((equal? dia "Lunes")     (disponible? cartelera 0))
        ((equal? dia "Martes")    (disponible? cartelera 1))
        ((equal? dia "Miercoles") (disponible? cartelera 2))
        ((equal? dia "Jueves")    (disponible? cartelera 3))
        ((equal? dia "Viernes")   (disponible? cartelera 4))
        ((equal? dia "Sabado")    (disponible? cartelera 5))
        ((equal? dia "Domingo")   (disponible? cartelera 6))
        (else #f)))

(define (disponible? cartelera dia)
  (if (equal? cartelera '())
      '()
      (if (equal? (cadar cartelera) '())
          (disponible?  (cdr cartelera) dia)
      (if (not (equal? (list-ref (caddr (caadar cartelera)) dia) '()))
          (append  (list (car (caadar cartelera))) (disponible? (append (list (list (caar cartelera) (cdadar cartelera))) (cdr cartelera)) dia))
          (disponible? (append (list (list (caar cartelera) (cdadar cartelera))) (cdr cartelera)) dia)))))

(define (listaPeliculas->String lista)
  (if (= (length lista) 1)
      (car lista)
      (string-append (car lista) " | " (listaPeliculas->String (cdr lista)))))
          
#|
 Descripcion : Funcion que devuelve el ultimo mensaje enviado de cualquiera de los 2 participantes
 Dominio     : id y log
 Recorrido   : string con el mensaje
|#
(define (lastMsg id log)
  (if (equal? log '())
      ""
      (if (= (caaar log) id)
          (substring (last (cadar log)) 22)
          (lastMsg id (cdr log)))))

#|
 Descripcion : Funcion que devuelve el ultimo mensaje enviado por name (puede se
 Dominio     :
 Recorrido   : 
|#
(define (lastMsgFromNameList name lista)
  (if (equal? lista '())
      ""
      (if (parte? (substring (car lista) 22) (string-append name "*"))
          (substring (car lista) (+ 22 (string-length name) 2))
          (lastMsgFromNameList name (cdr lista)))))

(define (lastMsgFrom name id log)
  (if (equal? log '())
      ""
      (if (= (caaar log) id)
          (lastMsgFromNameList name (reverse (cadar log)))
          (lastMsg id (cdr log)))))
  
          



#|
 Descripcion : Funcion que controla las acciones que el chatbot debe hacer segun lo que responde el usuario
 Dominio     : mensaje chatbot log seed
 Recorrido   : log modificado
|#
(define (accion mensaje chatbot log seed)
  (let ((mensajeUP (string-upcase mensaje)))
  (cond ( (parte? mensajeUP "RECOMIENDA*")
          (addListMsgToLog (getID chatbot) log (list (string-append (hora) " " (getUsrNameL (getID chatbot) log) ": " mensaje)
                                                     (string-append (hora) " " (getName chatbot) ": "(respuesta 0 0 (getDiccionario chatbot) seed)))
          ))

        ( (and (not (equal? (encontrarGenero mensaje) #f)) (respuesta? 0 0 (lastMsgFrom (getName chatbot) (getID chatbot) log) (getDiccionario chatbot)))
          (let ((log (addUsrGeneL (getID chatbot) (encontrarGenero mensaje) log)))
          (addUsrPelL (getID chatbot) (car (recomendarPelicula (getUsrGen (getUsrLog log (getID chatbot))) chatbot)) (addListMsgToLog (getID chatbot) log (list (string-append (hora) " " (getUsrNameL (getID chatbot) log) ": " mensaje)
                                                                                                                                                                (string-append (hora) " " (getName chatbot) ": " (respuesta 0 1 (getDiccionario chatbot) seed) "'" (car (recomendarPelicula (encontrarGenero mensaje) chatbot)) "' que tiene una puntuacion de " (number->string (cdr (recomendarPelicula (encontrarGenero mensaje) chatbot))))
                                                                                                                                                                (string-append (hora) " " (getName chatbot) ": " (respuesta 0 2 (getDiccionario chatbot) seed))
                                                                                                                                                                )
                                                                                                                                      )
                      )
            )
          )

        ( (and (respuesta? 0 2 (lastMsgFrom (getName chatbot) (getID chatbot) log) (getDiccionario chatbot)) (parte? mensajeUP "NO*"))
          (endDialog chatbot (addListMsgToLog (getID chatbot) log (list (string-append (hora) " " (getUsrNameL (getID chatbot) log) ": " mensaje)
                                                                        (string-append (hora) " " (getName chatbot) ": "(respuesta 0 3 (getDiccionario chatbot) seed)))) seed))
        
        ( (and (respuesta? 0 2 (lastMsgFrom (getName chatbot) (getID chatbot) log) (getDiccionario chatbot)) (parte? mensajeUP "SI*"))
          (addListMsgToLog (getID chatbot) log (list (string-append (hora) " " (getUsrNameL (getID chatbot) log) ": " mensaje)
                                                     (string-append (hora) " " (getName chatbot) ": "(respuesta 0 4 (getDiccionario chatbot) seed)))))

        ( (and (not (equal? (encontrarDia mensaje) #f)) (respuesta? 0 4 (lastMsgFrom (getName chatbot) (getID chatbot) log) (getDiccionario chatbot)))
          (addUsrDiaL (encontrarDia mensaje) (getID chatbot) (addListMsgToLog (getID chatbot) log (list (string-append (hora) " " (getUsrNameL (getID chatbot) log) ": " mensaje)
                                                                                        (string-append (hora) " " (getName chatbot) ": "(respuesta 0 5 (getDiccionario chatbot) seed) (stringHorarios chatbot log (encontrarDia mensaje) ))
                                                                                        (string-append (hora) " " (getName chatbot) ": "(respuesta 0 6 (getDiccionario chatbot) seed))
                                                                                        )
                                                                              )
                      )
          )
        
        

        ( (and (not (equal? (encontrarCine mensaje) #f)) (respuesta? 0 7 (lastMsgFrom (getName chatbot) (getID chatbot) log) (getDiccionario chatbot)))
          (addUsrCinL (getID chatbot) (encontrarCine mensaje) (addListMsgToLog (getID chatbot) log (list (string-append (hora) " " (getUsrNameL (getID chatbot) log) ": " mensaje)
                                                                                                         (string-append (hora) " " (getName chatbot) ": "(respuesta 0 8 (getDiccionario chatbot) seed))
                                                                                                         ))))

        ( (respuesta? 0 8 (lastMsgFrom (getName chatbot) (getID chatbot) log) (getDiccionario chatbot))
          (let ( (log (addUsrNameL (getID chatbot) mensaje log)))
           (addListMsgToLog (getID chatbot) log (list (string-append (hora) " " (getUsrNameL (getID chatbot) log) ": " mensaje)
                                                      (string-append (hora) " " (getName chatbot) ": "(respuesta 0 9 (getDiccionario chatbot) seed) " " (getUsrNameL (getID chatbot) log) " para la pelicula '" (getUsrPel (getUsrLog log (getID chatbot))) "' el dia " (getUsrDia (getUsrLog log (getID chatbot))) " a las " (getUsrHor (getUsrLog log (getID chatbot))) " en el cine " (getUsrCin (getUsrLog log (getID chatbot))) )))))
        


        ;##################################################################################

        ( (parte? mensajeUP "*LA CARTELERA") 
          (addListMsgToLog (getID chatbot) log (list (string-append (hora) " " (getUsrNameL (getID chatbot) log) ": " mensaje)
                                                     (string-append (hora) " " (getName chatbot) ": "(respuesta 2 0 (getDiccionario chatbot) seed))))
          )

         ( (and (not (equal? (encontrarDia mensaje) #f)) (respuesta? 2 0 (lastMsgFrom (getName chatbot) (getID chatbot) log) (getDiccionario chatbot)))
          (addUsrDiaL (encontrarDia mensaje) (getID chatbot) (addListMsgToLog (getID chatbot) log (list (string-append (hora) " " (getUsrNameL (getID chatbot) log) ": " mensaje)
                                                                                                        (string-append (hora) " " (getName chatbot) ": "(respuesta 2 1 (getDiccionario chatbot) seed) " " (encontrarDia mensaje) ": " (listaPeliculas->String (devolverCarteleraPorDia (getCartelera chatbot) (encontrarDia mensaje) )))
                                                                                                        (string-append (hora) " " (getName chatbot) ": "(respuesta 2 2 (getDiccionario chatbot) seed))))))
         
         ( (and (not (equal? (encontrarPelicula chatbot mensaje) #f)) (respuesta? 2 2 (lastMsgFrom (getName chatbot) (getID chatbot) log) (getDiccionario chatbot)))
          (let ((log (addUsrGeneL (getID chatbot) (car (encontrarPelicula chatbot mensaje))  (addUsrPelL (getID chatbot) (cdr (encontrarPelicula chatbot mensaje)) log))))
           (endDialog chatbot (addListMsgToLog (getID chatbot) log (list (string-append (hora) " " (getUsrNameL (getID chatbot) log) ": " mensaje)
                                                      (string-append (hora) " " (getName chatbot) ": "(respuesta 0 5 (getDiccionario chatbot) seed) (stringHorarios chatbot log (getUsrDia (getUsrLog log (getID chatbot)))))
                                                      (string-append (hora) " " (getName chatbot) ": "(respuesta 3 0 (getDiccionario chatbot) seed)))) seed)))

        ;##################################################################################

        ( (parte? mensajeUP "*COMPRAR UNA ENTRADA")
          (addListMsgToLog (getID chatbot) log (list (string-append (hora) " " (getUsrNameL (getID chatbot) log) ": " mensaje)
                                                     (string-append (hora) " " (getName chatbot) ": ¿Para qué pelicula?"))))
        

        ( (and (not (equal? (encontrarPelicula chatbot mensaje) #f)) (respuesta? 1 0 (lastMsgFrom (getName chatbot) (getID chatbot) log) (getDiccionario chatbot)))
          (let ((log (addUsrGeneL (getID chatbot) (car (encontrarPelicula chatbot mensaje))  (addUsrPelL (getID chatbot) (cdr (encontrarPelicula chatbot mensaje)) log))))
           (addListMsgToLog (getID chatbot) log (list (string-append (hora) " " (getUsrNameL (getID chatbot) log) ": " mensaje)
                                                      (string-append (hora) " " (getName chatbot) ": ¿Para que día?")))))
        

        ( (and (respuesta? 0 2 (lastMsgFrom (getName chatbot) (getID chatbot) log) (getDiccionario chatbot)) (parte? mensajeUP "NO*"))
          (endDialog chatbot (addListMsgToLog (getID chatbot) log (list (string-append (hora) " " (getUsrNameL (getID chatbot) log) ": " mensaje)
                                                                        (string-append (hora) " " (getName chatbot) ": "(respuesta 0 3 (getDiccionario chatbot) seed)))) seed))
        

        ( (and (not (equal? (encontrarDia mensaje) #f)) (equal? (lastMsg (getID chatbot) log) (string-append (getName chatbot) ": ¿Para que día?")))
          (addUsrDiaL (encontrarDia mensaje) (getID chatbot) (addListMsgToLog (getID chatbot) log (list (string-append (hora) " " (getUsrNameL (getID chatbot) log) ": " mensaje)
                                                                                        (string-append (hora) " " (getName chatbot) ": Para ese dia tenemos los siguientes horarios: " (stringHorarios chatbot log (encontrarDia mensaje) ))
                                                                                        (string-append (hora) " " (getName chatbot) ": ¿Qué horario eliges?")))))

        ( (and (not (equal? (encontrarHorario  log chatbot mensaje (getUsrDia (getUsrLog log (getID chatbot)))) #f)) (respuesta? 0 6 (lastMsgFrom (getName chatbot) (getID chatbot) log) (getDiccionario chatbot)))
          (addUsrHorL  (getID chatbot) mensaje (addListMsgToLog (getID chatbot) log (list (string-append (hora) " " (getUsrNameL (getID chatbot) log) ": " mensaje)
                                                                                          (string-append (hora) " " (getName chatbot) ": "(respuesta 0 7 (getDiccionario chatbot) seed))
                                                                                        ))))

        ( (or (parte? mensajeUP "ADIOS*") (parte? mensajeUP "CHAO*"))
          (endDialog chatbot (addListMsgToLog (getID chatbot) log (list (string-append (hora) " " (getUsrNameL (getID chatbot) log) ": " mensaje)
                                                                        (string-append (hora) " " (getName chatbot) ": Adios, fue un placer ayudarte"))) seed)
          )
        
        (else (addListMsgToLog (getID chatbot) log (list (string-append (hora) " " (getUsrNameL (getID chatbot) log) ": " mensaje)
                                                         (string-append (hora) " " (getName chatbot) ": No te entendí muy bien....Te repito la pregunta" )
                                                         (string-append (hora) " " (getName chatbot) ": " (lastMsgFrom (getName chatbot) (getID chatbot) log))
                                                         )
                               )
              
              )
        )
    )
  )
  

       

#|
 Descripcion : Funcion que encuentra si el usuario dijo un genero
 Dominio     : string con el mensaje
 Recorrido   : string con el genero o falso si es que no existe
|#
(define (encontrarGenero mensaje)
  (cond ( (parte? (string-upcase mensaje) "TERROR*") "TERROR")
        ( (parte? (string-upcase mensaje) "ACCION*") "ACCION")
        ( (parte? (string-upcase mensaje) "ANIMACION*") "ANIMACION")
        ( (parte? (string-upcase mensaje) "DRAMA*") "DRAMA")
        ( (parte? (string-upcase mensaje) "COMEDIA*") "COMEDIA")
        (else #f)))

#|
 Descripcion : Funcion que encuentra si el usuario dijo un cine
 Dominio     : string con el mensaje
 Recorrido   : string con el nombre del cine o falso si es que no existe
|#
(define (encontrarCine mensaje)
  (cond ( (or (parte? (string-upcase mensaje) "CINE NORTE*")  (parte? (string-upcase mensaje) "NORTE*")) "NORTE")
        ( (or (parte? (string-upcase mensaje) "CINE SUR*")  (parte? (string-upcase mensaje) "SUR*")) "SUR")
        ( (or (parte? (string-upcase mensaje) "CINE CENTRO*")  (parte? (string-upcase mensaje) "CENTRO*")) "CENTRO")
        (else #f)))

#|
 Descripcion : Funcion que encuentra si el usuario dijo un dia de la semana
 Dominio     : string con el mensaje
 Recorrido   : string con el dia de la semana o falso si es que no existe
|#
(define (encontrarDia mensaje)
  (cond ( (parte? (string-upcase mensaje) "*LUNES") "Lunes")
        ( (parte? (string-upcase mensaje) "*MARTES") "Martes")
        ( (parte? (string-upcase mensaje) "*MIERCOLES") "Miercoles")
        ( (parte? (string-upcase mensaje) "*JUEVES") "Viernes")
        ( (parte? (string-upcase mensaje) "*VIERNES") "Viernes")
        ( (parte? (string-upcase mensaje) "*SABADO") "Sabado")
        ( (parte? (string-upcase mensaje) "*DOMINGO") "Domingo")
        (else #f)))

#|
 Descripcion : Funcion que encuentra si el usuario dijo un horario
 Dominio     : string con el mensaje
 Recorrido   : string con el horario o falso si es que no existe
|#
(define (encontrarHorario log chatbot mensaje dia)
  (define (eH listaHorarios mensaje)
  (if (and (member? #\: (string->list mensaje)) (>= 5 (string-length mensaje)) (member? (horarioS->P mensaje) listaHorarios))
      mensaje
      #f))
  (cond ((equal? dia "Lunes")     (eH (list-ref (devolverListaHorarios (devolverPeliculas (getUsrGen (getUsrLog log (getID chatbot) )) (getCartelera chatbot)) (getUsrPel (getUsrLog log (getID chatbot)))) 0) mensaje))
        ((equal? dia "Martes")    (eH (list-ref (devolverListaHorarios (devolverPeliculas (getUsrGen (getUsrLog log (getID chatbot) )) (getCartelera chatbot)) (getUsrPel (getUsrLog log (getID chatbot)))) 1) mensaje))
        ((equal? dia "Miercoles") (eH (list-ref (devolverListaHorarios (devolverPeliculas (getUsrGen (getUsrLog log (getID chatbot) )) (getCartelera chatbot)) (getUsrPel (getUsrLog log (getID chatbot)))) 2) mensaje))
        ((equal? dia "Jueves")    (eH (list-ref (devolverListaHorarios (devolverPeliculas (getUsrGen (getUsrLog log (getID chatbot) )) (getCartelera chatbot)) (getUsrPel (getUsrLog log (getID chatbot)))) 3) mensaje))
        ((equal? dia "Viernes")   (eH (list-ref (devolverListaHorarios (devolverPeliculas (getUsrGen (getUsrLog log (getID chatbot) )) (getCartelera chatbot)) (getUsrPel (getUsrLog log (getID chatbot)))) 4) mensaje))
        ((equal? dia "Sabado")    (eH (list-ref (devolverListaHorarios (devolverPeliculas (getUsrGen (getUsrLog log (getID chatbot) )) (getCartelera chatbot)) (getUsrPel (getUsrLog log (getID chatbot)))) 5) mensaje))
        ((equal? dia "Domingo")   (eH (list-ref (devolverListaHorarios (devolverPeliculas (getUsrGen (getUsrLog log (getID chatbot) )) (getCartelera chatbot)) (getUsrPel (getUsrLog log (getID chatbot)))) 6) mensaje))
        (else #f)))

#|
 Descripcion : Funcion que encuentra si el usuario dijo una pelicula de la cartelera
 Dominio     : string con el mensaje
 Recorrido   : par con el genero de la pelicula y nombre de la pelicula o falso si es que no existe
|#
(define (encontrarPelicula chatbot mensaje)
  (define (eP cartelera mensaje)
    (if (equal? cartelera '())
        #f
        (if (equal? (cadar cartelera) '())
             (eP (cdr cartelera) mensaje)
             (if (equal? (string-upcase (car (caadar cartelera))) (string-upcase mensaje))
                 (cons (caar cartelera) (car (caadar cartelera)))
                 (eP (append (list (list (caar cartelera) (cdadar cartelera))) (cdr cartelera)) mensaje)
                 )
            )
        )
    )
    
        
  (eP (getCartelera chatbot) mensaje ))


#|
 Descripcion : Funcion que transforma un string con formato de horario a par
 Dominio     : string con el horario
 Recorrido   : par
|#
(define (horarioS->P str)
  (if (equal? (list-ref (string->list str) 1) #\:)
      (cons (string->number (substring str 0 1)) (string->number (substring str 2 4)))
      (if (equal? (list-ref (string->list str) 2) #\:)
          (cons (string->number (substring str 0 2)) (string->number (substring str 3 5)))
          (cons 0 0))))

;## MANEJO DE LOGS


#|
 Descripcion : Funcion que comprueba si es o no un log
 Dominio     : Log
 Recorrido   : Booleano
|#
(define (log? lg)
  (if (and (= (length lg) 1) (list? (car lg)) (number? (caaar lg)) (usuario? (cadaar lg)) (list? (cadar lg)))
      #t
      (if (and (= (length lg) 1) (list? (car lg)) (number? (caaar lg)) (usuario? (cadaar lg)) (list? (cadar lg)))
          (log? (cdr lg))
          #f)))

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

#|
 Descripcion : Funcion que añade una lista de mensajes al log en orden.
 Dominio     : id log y lista de mensajes
 Recorrido   : log modificado
|#          
(define (addListMsgToLog id log lista)
  (if (equal? lista '())
      log
      (addListMsgToLog id (addMsgToLog id log (car lista))  (cdr lista))))

#|
 Descripcion : Funcion que transforma una lista de strings en una string parrafo
 Dominio     : lista de mensajes
 Recorrido   : string
|#  
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
          (string-append "ID: " (number->string (caaar log)) "\n" (lines->string (cadar log)) "\n" (log->string (cdr log)))
          "")))


#|
 Descripcion : Funcion que muestra el log en pantalla
 Dominio     : log
 Recorrido   : nada
|#
(define (showLog log)
  (display (log->string log)))




;## MANEJO DE PELICULAS


#|
 Descripcion : Funcion que recomienda la pelicula mejor calificada del genero escogido
 Dominio     : string genero, y cartelera
 Recorrido   : par donde el primer elemento es el nombre de la pelicula y el segundo es el puntaje
|#
(define (recomendarPelicula genero chatbot)
  (define (rP1 genero cartelera)
    (if (equal? cartelera '())
        #f
        (if (equal? (caar cartelera) genero)
            (cons (car(caadar cartelera)) (cadr (caadar cartelera)))
            (rP1 genero (cdr cartelera))
            )
        )
    )(rP1 genero (getCartelera chatbot)))


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






(define (test user chatbot log seed)
  (define (test1 usr cbot lg sd)
    (if (equal? usr '())
        lg
        (test1 (cdr usr) cbot (sendMessage (car usr) cbot lg sd) sd)))
  (test1 user chatbot (beginDialog chatbot log seed) seed))
  


(define user1 (list "revisar los horarios" "coco" "Jueves" "9:50" "Norte" "Esteban López" "Adios")) ;semilla 110
(define user2 (list "recomiendame una pelicula" "terror" "si" "Sabado" "11:30" "cine sur" "Mauricio Israel")) ;semilla 110
(define user3 (list "Muestrame la cartelera" "Lunes" "Swing"))
(define cbot2 (list "Joaquin" 1 (generarCarteleraConHorarios cartelera 110) diccionarioFormal))



      
