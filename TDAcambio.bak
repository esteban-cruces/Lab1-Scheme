#lang racket
(provide(all-defined-out))
(require "TDAarchivo.rkt")
(require racket/date)

;TDA cambio

;un cambio será una lista con 3 elementos, el primero será el archivo, el segundo el numero de la linea editada, y el ultimo la nueva linea editada

;constructor
;dom: archivo x entero x string
;rec: TDA cambio
;objetivo: crear una TDA cambio
(define (crearCambio archivo num newLine)
  (list archivo (list num) (list newLine) (date->string (current-date) second)))

;pertenencia
;dom: cambio
;rec: bollean
;objetivo: verificar si una lista pertenece a un TDA cambio
(define (cambio? cambio)
  (and (= (length cambio) 3) (archivo? (car cambio))))

;selector
;dom: cambio
;rec: archivo
;objetivo: selecionar el archivo dentro de cambio
(define (getArchCambio cambio)
  (car cambio))

;selector
;dom: cambio
;rec: entero
;objetivo: selecionar el numero de la linea donde se aplico el cambio
(define (getNumLineCambio cambio)
  (cadr cambio))

;selector
;dom: cambio
;rec: string
;objetivo: selecionar la nueva linea
(define (getNewLineCambio cambio)
  (caddr cambio))

      
