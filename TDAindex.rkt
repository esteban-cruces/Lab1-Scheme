#lang racket
(provide(all-defined-out))
(require "TDAarchivo.rkt")
(require "TDAworkspace.rkt")
(require racket/date)

;TDA index
;Este tda guarda los cambios generados entre el TDA repositorio remoto y el TDA workspace
;El TDA index se representará como una lista de TDA commit

;constructor
;dom: null
;rec: TDA rec
;funcion que construye el TDA index
(define (createIndex)
  (list "Index"))

;pertenencia
;dom: TDA index
;rec: valor booleano (#t #f)
;funcion que verifica si un elemento es un TDA index
(define (index? index)
  (if (equal? (car index) "Index")
      #t
      #f))

;modificador
;dom: TDA index x TDA workspace x string x entero x string
;rec: lista de TDA commit
;funcion agrega los cambios hechos en un TDA archivo en el index
(define (addChangeIndex index workspace nameArch numLine newLine)
  (append index (list(list workspace (date->string (current-date) second) nameArch numLine newLine))))

;modificador
;dom: 
;rec: 
;funcion: agregar varios cambios al index
(define (addCambios index workspace . lista)
  (append index (list(list workspace (date->string (current-date) second) (car lista) (cadr lista) (caddr lista)))))
       
(define (cambio index workspace)
  (if (null? (cdr index))
      (append index (list (cdr workspace)))
      #t))




  
  