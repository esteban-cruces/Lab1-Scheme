#lang racket
(provide(all-defined-out))
;TDA Archivo

;El primer elemento será el que represente el nombre del archivo
;todos los elementos restantes representaran cada linea del texto, a medida que se agreguen

;constructor
;crea un archivo, representado en una lista, donde la primera pos. es el nombre, y el resto de elementos son los lineas del archivo
;dom: varias cadenas de caracteres
;rec: TDA Archivo
(define (createArchivo nombreArch . textoArch)
  (append (list nombreArch) textoArch))

;pertenecia
;dom: TDAarchivo
;rec: valor booleano
;esta funcion revisa si el elemnto pertenece a un TDA archivo
(define (archivo? arch)
  (if (and (>= (length arch) 2) (list? arch))
      #t
      #f))

;selector
;funcion que seleciona el nombre del archivo
;dom: TDA Archivo
;recorrido: un string
(define (getName arch)
  (car arch))

;Selector
;selecciona la linea n del archivo
;dom: TDA archivo, entero+
;rec: cadena de caracteres
;recursion de cola
(define (getLine arch n)
  (if (= n 0)
      (car arch)
      (getLine (cdr arch) (- n 1))))

;seleciona la cola las lineas restantes despues de la linea n de un TDA archivo
;dom : TDA archivo
;rec : lista
;recursion de cola
(define (tailLine arch n)
  (if (= n 1)
      (cdr arch)
      (tailLine (cdr arch) (- n 1))))

;modificador
;dom: TDA arch x entero x string
;rec: lista
;esta funcion permite agregar una fila de caracteres entre la fila n y n-1
(define (addLine arch n newLine)
  (append (reverse (tailLine (reverse arch) (- (length arch) (+ n 1)))) (list newLine) (tailLine arch (+ n 1))))

;Modificador
;dom: TDA archivo
;rec: TDA archivo
;funcion que permite editar una linea n de informacion del TDA archivo
(define (editLine arch n lineN)
  (append (reverse (tailLine (reverse arch) (- (length arch) (+ n 1)))) (list lineN) (tailLine arch (+ n 2))))

;modificador
;dom: TDA archivo x entero
;rec: TDA archivo
;esta funcion remueve la linea n de informacion del tda archivo
(define (removeArch arch n)
  (append (reverse (tailLine (reverse arch) (- (length arch) n))) (tailLine arch (+ n 1))))