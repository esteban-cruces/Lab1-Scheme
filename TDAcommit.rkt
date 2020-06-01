#lang racket
(provide(all-defined-out))
(require "TDAcambio.rkt")

;TDA commit

;un cambio será una lista donde el primer elemento es un string indicando el nombre del commit, seguido de la cantidad de cambios que se desee agregar

;constructor
;dom: string
;rec: TDA commit
;objetivo: crear una TDA cambio
(define (crearCommit nombreCommit)
  (list nombreCommit))

;pertenencia
;dom: commit
;rec: bollean
;objetivo: verificar si una lista pertenece a un TDA cambio
(define (cambio? commit)
  (if (> (length commit) 1)
      (and (string? (car commit)) (cambio? (cdr commit)))
      (string? (car commit))))

;modificador
;dom: commit x cambio
;rec: commit
(define (addCambioCommit commit cambio)
  (if (and (commit? commit) (cambio? cambio))
      (append (commit) (cambio))
      null))
