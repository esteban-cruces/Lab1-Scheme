#lang racket
(provide(all-defined-out))
(require "TDAarchivo.rkt")
(require "TDAworkspace.rkt")

;TDA romote repository
;se crea a traves de una lista, la cual indica en el primer elemnto q es el "remote repository" y el resto de elemntos pertenecen tda commit 

;constructor
;dom: null
;rec: TDA remote repository
;en esta funcion se crea zona de remote repository
(define (crearRepositoryR)
  (list "RemoteRepository"))

;pertenecia
;dom: TDA remote repository
;rec: valor booleano (#t o #f)
;vrificar si el elemento pertenece a un TDA remote repository
(define (remoteRepository? remoteRepository)
  (if (and (equal? (car remoteRepository) "RemoteRepository") (list? remoteRepository))
      #t
      #f))

;modificador
;dom: repositorio remoto
;rec: TDA remote repository
;objetivo: eliminar el primer cambio presente en este repositorio
(define (deleteCambio repositoryR)
  (append (list "RemoteRepository") (cddr repositoryR)))
