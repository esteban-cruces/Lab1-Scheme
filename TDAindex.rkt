#lang racket
(provide(all-defined-out))
(require "TDAarchivo.rkt")
(require "TDAworkspace.rkt")

;TDA index
;Este tda guarda los cambios generados entre el TDA repositorio remoto y el TDA workspace
;El TDA index se representará como una lista con los siguientes elementos el primero será un string indicadando que se trata de un TDA index ("Index") y el resto de elementos serás listas con la siguente composicion
;el primer elemento el TDA workspace, el segundo será el nombre archivo editado, el tercero será el numero de la linea que se edito, y el ultimo será la linea editada

;constructor
;dom: string
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
;rec: TDA index
;funcion agrega los cambios hechos en un TDA archivo en el index
(define (addChangeIndex index workspace nameArch numLine newLine)
  (append index (list workspace nameArch numLine newLine)))
