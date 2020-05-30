#lang racket
(provide(all-defined-out))
(require "TDAarchivo.rkt")
(require "TDAworkspace.rkt")
(require "TDAcambio.rkt")

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


;operador
;dom: repository remote x archivo x lista
;rec: lista de cambios
;objetivo: ver los cambios de un archivo que no estén guardados en el repository remote
(define (hayCambio? repositoryR archivo2 archivo listaCambios)
  (if (null? (cdr repositoryR))
      (crearListaCambiosArch archivo2 archivo listaCambios)
      (if (null? archivo)
          listaCambios
          (if (getLineBool archivo (getNewLineCambio (cadr repositoryR)))
              (hayCambio? (deleteCambio repositoryR) archivo2 (removeArch (removeArch archivo (- (numLine archivo 0 (getNewLineCambio (cadr repositoryR))) 1 )) (- (numLine archivo 0 (getNewLineCambio (cadr repositoryR))) 1 )) listaCambios)
              (hayCambio? (deleteCambio repositoryR) archivo2 archivo listaCambios)))))


;selector
;dom: reposity remote string x entero
;rec: entero
;objetivoentrega el numero del primer cambio perteneciente a un archivo
;recursion de cola
(define (numCambioArch repositoryR nombreArch num)
  (if (null? (cdr repositoryR))
      null
      (if (equal? (getArchCambio (cadr repositoryR)) nombreArch)
          num
          (numCambioArch (deleteCambio repositoryR) nombreArch (+ 1 num)))))

;modificador
;dom: reposity remote x string
;rec: reposity remote
;objetivo: elimina todos los cambios que hayan de un archivo
;recursion de cola
(define (deleteCambioArch repositoryR nombreArchivo)
  (if (null? (numCambioArch repositoryR nombreArchivo 1))
      repositoryR
      (deleteCambioArch (removeArch repositoryR (numCambioArch repositoryR nombreArchivo 1)) nombreArchivo)))
  