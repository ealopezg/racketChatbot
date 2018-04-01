#lang scheme

;CATEGORIAS
;
;0: Saludo
;1: Estado de animo

(define (encontrarPalabra mensaje palabra)
  #t)

(define (verCategoria mensaje)
  (let ((lista (string-split mensaje)))))


(define (responder mensaje)
 (encontrarFrase (verCategoria mensaje)))



(define (leerArchivo nombre)
  (let ((lista (file->lines nombre)))))

(define (organizarFrases lista)
  (if (eq? lista '())
      '()
      (if (eq? (caar (string->list lista)) #\:)
          (append (list ( (organizarFrases (
      
