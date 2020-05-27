#lang racket
(provide(all-defined-out))
(require "TDAarchivo.rkt")
(require "TDAworkspace.rkt")

;TDA local repository
;se crea a traves de una lista, la cual indica en el primer elemnto q es el "local repository" y el resto de elemntos pertenecen tda commit 

;constructor
;dom: null
;rec: TDA local repository
;en esta funcion se crea zona de local repository
(define (crearRepositoryL)
  (list "LocalRepository"))

;pertenecia
;dom: TDA local repository
;rec: valor booleano (#t o #f)
;vrificar si el elemento pertenece a un TDA local repository
(define (localRepository? localRepository)
  (if (and (equal? (car localRepository) "LocalRepository") (list? localRepository))
      #t
      #f))

;modificador
;dom: TDA local repository x TDA index 
;rec: TDA local repository
;objetivo: añadir commits al repositorio local
(define commit (lambda (localRepository)
                 (lambda (index)
                   (append list localRepository index))))