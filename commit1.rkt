#lang racket
;tda archvo de texto plano, el primer elemento será el que represente el nombre del archivo
;todos los elementos restantes representaran cada linea del texto, a medida que se agreguen

;funcion para asignar el nombre y contenido del archivo, donde el primer argumento representará el nombre
;todos los argumentos posteriores representan las lineas del texto
(define (archivoTexto nombreArch . textoArch)
  (append (list nombreArch) textoArch))


;funcion que seleciona el nombre del archivo
;dominio: una lista de strings
;recorrido: un string
(define (tomarNombre arch)
  (car arch))

;función que verifica si el un archivo de texto es un una lista 
;dominio: una lista de strings
;recorrido: un valor booleano
(define (archivoTexto? arch)
  (if(list? arch)
     #T
     #F))

