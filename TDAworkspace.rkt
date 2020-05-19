#lang racket
(provide(all-defined-out))
(require "TDAarchivo.rkt")

;TDA Workspace
;a través de una lista se creará una simulación de Workspace
;este consiste en tener una lista, donde el primer elemento indica que es el Workspace, y el resto de elementos serán los TDA archivos que se guarden en este
;en este tda inicialmente se creará una lista con 1 elemnto "Workspace" a la cual, a medida que se creen TDA Archivo, se le irán agregando a este TDA Workspace

;constructor
;dom: tda Archivo
;rec: TDA Workspace
;en esta funcion se crea zona de trabajo workspace
(define (crearWorkspace)
  (list "Workspace"))

;pertenecia
;dom: TDA Workspace
;rec: valor booleano (#t o #f)
;vrificar si el archivo pertenece a un TDA Worksapce
(define (workspace? workspace)
  (if (and (= (car workspace) "Workspace") (list? workspace))
      #t
      #f))

;selector
;dom: TDA workspace x string
;rec: lista
;esta funcion entregra los elementos restantes a partir de el atchivo al que pertenesca el nombre ingresado
;recursion de cola
(define (tailWorkspace workspace nameArch)
  (if (equal? nameArch (car (car (cdr workspace))))
      (cdr workspace)
      (tailWorkspace (cdr workspace) nameArch)))

;selector
;seleciona un archivo en el workspace
;dom: TDA workspace x string (indicando el nombre del archivo)
;rec: TDA archivo
;recursion: de cola
(define (getArch workspace nameArch)
   (if (equal? nameArch (car (car (cdr workspace))))
      (car (cdr workspace))
      (getArch (cdr workspace) nameArch)))

;modificador
;funcion que agrega un TDA archivo a un TDA workspace
;dom: TDA workspace x TDA archivo
;rec: TDA workspace
(define (addWorkspace workspace . arch)
  (append workspace arch))

;modificador
;funcion que remueve (borra) un TDA archivo del TDA workspace
;dom: TDA workspace x string
;rec: TDA workspace
(define (removeWorkspace workspace nameArch)
  (append (reverse (cdr (tailWorkspace (reverse workspace) nameArch))) (cdr (tailWorkspace workspace nameArch))))
      
;modificador
;funcion que edita un TDA archivo en un TDA workspace
;dom: TDA workspace x string x string x entero
;rec: TDA workspace
(define (editWorkspace workspace nameArch newLineArch numLine)
  (append (reverse (cdr (tailWorkspace (reverse workspace) nameArch))) (list (editLine (getArch workspace nameArch) (- numLine 1) newLineArch)) (cdr (tailWorkspace workspace nameArch))))