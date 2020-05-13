#lang racket
(provide(all-defined-out))
;TDA Archivo

;El primer elemento será el que represente el nombre del archivo
;todos los elementos restantes representaran cada linea del texto, a medida que se agreguen

;constructor
;crea un archivo, representado en una lista, donde la primera pos. es el nombre, y el resto de elementos son los lineas del archivo
;dom: varias cadenas de caracteres
;rec: TDA Archivo
(define (crearArchivo nombreArch . textoArch)
  (append (list nombreArch) textoArch))


;pertenecia

;dom: TDAarchivo
;rec: valor booleano
;esta funcion revisa si el elemnto pertenece a un TDA archivo
(define (archivo? arch)
  (if (and (>= (length arch) 2) (list? arch))
      #t
      #f)
  )
          

;selector
;funcion que seleciona el nombre del archivo
;dom: TDA Archivo
;recorrido: un string
(define (tomarNombre arch)
  (car arch))

;Selector
;selecciona la linea n del archivo
;dom: TDA archivo, entero+
;rec: cadena de caracteres
(define (getLine arch n)
  (if (= n 0)
      (car arch)
      (getLine (cdr arch) (- n 1))
      )
   )


;seleciona la cola las lineas restantes despues de la linea n de un TDA archivo
;dom : TDA archivo
;rec : lista
(define (tailLine arch n)
  (if (= n 0)
      (cdr arch)
      (tailLine (cdr arch) (- n 1))
      )
   )

;dar vuelta un archivo, dejando el nombre de este al final y su ultima linea al inicio
;dom: TDA archivo
;rec: lista
(define (reverseArch arch)
  (reverse arch))

;seleciona los elementos previos a la fila n de un TDA
;dom : TDA archivo
;rec : lista
(define (prevLine arch n)
  (tailLine ((reverseArch arch) (- (length arch) n))
            )
  )

;Modificador
;dom: TDA archivo
;rec: TDA archivo
;funcion que agrega una linea a un TDA archivo
;arch es el archivo a edita, n es la linea que se desea agregar la nueva linea y lineN a agregar
;(define (addLine arch n lineN)
 ; (if
  





;Modificador
;dom: TDA archivo
;rec: TDA archivo
;funcion que permite editar una linea de informacion del TDA archivo
;arch es el archivo a edita, n es la linea que se desea editar y lineN es la linea ya editada
(define (editLine arch n lineN)
  (append (tailLine arch n) lineN (prevLine arch n))
  )
  
  
          
     
  








