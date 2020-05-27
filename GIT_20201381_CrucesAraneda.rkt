#lang racket
(provide(all-defined-out))
(require "TDAarchivo.rkt")
(require "TDAworkspace.rkt")
(require "TDArepositorioremoto.rkt")
(require "TDArepositoriolocal.rkt")
(require "TDAindex.rkt")
(require "TDAcambio.rkt")

;comandos de git
;aqui se crearan las funciones las cuales serás los comandos de git

;TDA zonas
;consta de una lista con 4 elementos, el primero el workspace, segundo index, tercero local repository, y cuarto remote repository

;constructor
;dom: TDAworspace x TDAindex x TDAlocalRepository x TDAremoteRepository
;rec: TDA zonas, esta es una lista con las 4 zonas de git
;objetivo: crear una "zona" que contenga todas las zonas
(define (crearZonas worspace index localRepository remoteRepository)
  (append (list worspace) (list index) (list localRepository) (list remoteRepository)))

;pertenencia
;dom: zonas
;rec: bollean
;objetivo: verificar si el elemento pertenece a zonas
(define (zonas? zonas)
  (and (list? zonas) (workspace? (car zonas)) (index? (cadr zonas)) (localRepository? (caddr zonas)) (remoteRepository? (car (reverse zonas)))))

;selector
;dom: zonas
;rec: workspace
;objetivo: obtener el workspace dentro de zonas
(define (getWorkspace zonas)
  (car zonas))

;selector
;dom: zonas
;rec: index
;objetivo: obtener el index dentro de zonas
(define (getIndex zonas)
  (cadr zonas))

;selector
;dom: zonas
;rec: Local Repository
;objetivo: obtener el Local Repository dentro de zonas
(define (getRepositoryL zonas)
  (caddr zonas))

;selector
;dom: zonas
;rec: Remote Repository
;objetivo: obtener el Remote Repository dentro de zonas
(define (getRepositoryR zonas)
  (car (reverse zonas)))

;dom: string x zonas
;rec: zonas
;objetivo:Función que permite aplicar los comandos (funciones en este caso) en el contexto de git sobre las zonas de trabajo.
(define git (lambda (comando)
                (if (equal? pull comando)
                    pull
                    (if (equal? "add" comando)
                        (list "add")
                        (if (equal? "commit" comando)
                            (list "commit")
                            (if (equal? "push" comando)
                                (list "push")
                                (list "no")))))))

;dom: zonas
;rec: zonas
;objetivo: Función que retorna una lista con todos los cambios (commits) desde el RemoteRepository al Workspace
;recursion de cola
(define pull (lambda (zonas)
               (crearZonas (getWorkspace (moverRRaWS zonas)) (getIndex (moverRRaWS zonas)) (getRepositoryL (moverRRaWS zonas)) (getRepositoryR zonas))))

;dom: zonas
;rec: zonas
;objetivo: esta funcion se encarga de "mover los cambio" del repository remote al workspace, se usará dentro de "pull"
;recursion de cola
(define moverRRaWS (lambda (zonas)
               (if (zonas? zonas)
                   (if (null? (cdr (getRepositoryR zonas)))
                       zonas
                       (if (null? (getArch (getWorkspace zonas) (getName(getArchCambio (cadr (getRepositoryR zonas))))))
                           (moverRRaWS (crearZonas (getWorkspace zonas) (getIndex zonas) (getRepositoryL zonas) (deleteCambio (getRepositoryR zonas))))
                           (if (equal? (getLine (getArch (getWorkspace zonas) (getName(getArchCambio (cadr (getRepositoryR zonas))))) (car (getNumLineCambio (cadr (getRepositoryR zonas))))) (getNewLineCambio (cadr (getRepositoryR zonas))))
                               (moverRRaWS (crearZonas (getWorkspace zonas) (getIndex zonas) (getRepositoryL zonas) (deleteCambio (deleteCambio (getRepositoryR zonas)))))
                               (moverRRaWS (crearZonas (append (reverse (cdr (tailWorkspace (reverse (getWorkspace zonas)) (getName(getArchCambio (cadr (getRepositoryR zonas))))))) (list (editLine (getArch (getWorkspace zonas) (getName(getArchCambio (cadr (getRepositoryR zonas))))) (- (car (getNumLineCambio (cadr (getRepositoryR zonas)))) 1) (car (getNewLineCambio (cadr (getRepositoryR zonas)))))) (cdr (tailWorkspace (getWorkspace zonas) (getName(getArchCambio (cadr (getRepositoryR zonas))))))) (getIndex zonas) (getRepositoryL zonas) (deleteCambio (getRepositoryR zonas)))))))
                   null)))






              
              











               
