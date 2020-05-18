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
;funcion que edita un TDA archivo del TDA workspace
;dom: TDA workspace x string x string
;rec: TDA workspace
(define (editWorkspace workpace nameArch newLine n)
  (editLine (getArch workspace nameArch) n newLine))
  
  