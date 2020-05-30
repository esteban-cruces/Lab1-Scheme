#lang racket
(provide(all-defined-out))
(require "TDAarchivo.rkt")
(require "TDAworkspace.rkt")
(require "TDArepositorioremoto.rkt")
(require "TDArepositoriolocal.rkt")
(require "TDAindex.rkt")
(require "TDAcambio.rkt")
(require racket/date)

;comandos de git
;aqui se crearan las funciones las cuales serás los comandos de git

;---------------------------TDA-ZONAS------------------------------------------------
;TDA zonas
;consta de una lista con 5 elementos, el primero el workspace, segundo index, tercero local repository,  cuarto remote repository y el ultimo un historial de comandos aplicados

;constructor
;dom: TDAworspace x TDAindex x TDAlocalRepository x TDAremoteRepository x lista
;rec: TDA zonas, esta es una lista con las 4 zonas de git
;objetivo: crear una "zona" que contenga todas las zonas
(define (crearZonas worspace index localRepository remoteRepository historial)
  (append (list worspace) (list index) (list localRepository) (list remoteRepository) (list historial)))

;pertenencia
;dom: zonas
;rec: bollean
;objetivo: verificar si el elemento pertenece a zonas
(define (zonas? zonas)
  (and (list? zonas) (workspace? (getWorkspace zonas)) (index? (getIndex zonas)) (localRepository? (getRepositoryL zonas)) (remoteRepository? (getRepositoryR zonas))))

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
  (cadddr  zonas))

;selector
;dom: zonas
;rec: lista con Historial de comandos
(define (getHistorial zonas)
  (car (reverse zonas)))

;----------------------------------GIT---------------------------------------------------------------------------
;dom: string x zonas
;rec: zonas
;objetivo:Función que permite aplicar los comandos (funciones en este caso) en el contexto de git sobre las zonas de trabajo.
(define git (lambda (comando)
                (if (equal? pull comando)
                    pull
                    (if (equal? add comando)
                        add
                        (if (equal? commit comando)
                            commit
                            (if (equal? push comando)
                                push
                                null))))))

;---------------------------------------PULL--------------------------------------------------------------------
;dom: zonas
;rec: zonas
;objetivo: Función que retorna una lista con todos los cambios (commits) desde el RemoteRepository al Workspace
(define pull (lambda (zonas)
               (crearZonas (getWorkspace (moverRRaWS zonas)) (getIndex (moverRRaWS zonas)) (getRepositoryL (moverRRaWS zonas)) (getRepositoryR zonas) (append (getHistorial zonas) (list (date->string (current-date) second) "PULL" )))))

;dom: zonas
;rec: zonas
;objetivo: esta funcion se encarga de "mover los cambio" del repository remote al workspace, se usará dentro de "pull"
;recursion de cola
(define moverRRaWS (lambda (zonas)
               (if (zonas? zonas)
                   (if (null? (cdr (getRepositoryR zonas)))
                       zonas
                       (if (string? (cadr (getRepositoryR zonas)))
                           (moverRRaWS (crearZonas (getWorkspace zonas) (getIndex zonas) (getRepositoryL zonas) (deleteCambio (getRepositoryR zonas)) (getHistorial zonas)))
                           (if (null? (getArch (getWorkspace zonas) (getArchCambio (cadr (getRepositoryR zonas)))))
                               (moverRRaWS (crearZonas (getWorkspace zonas) (getIndex zonas) (getRepositoryL zonas) (deleteCambio (getRepositoryR zonas)) (getHistorial zonas)))
                               (if (equal? (getLine (getArch (getWorkspace zonas) (getArchCambio (cadr (getRepositoryR zonas)))) (- (getNumLineCambio (cadr (getRepositoryR zonas))) 1)) (getNewLineCambio (cadr (getRepositoryR zonas))))
                                   (moverRRaWS (crearZonas (getWorkspace zonas) (getIndex zonas) (getRepositoryL zonas) (deleteCambio (deleteCambio (getRepositoryR zonas))) (getHistorial zonas)))
                                   (moverRRaWS (crearZonas (append (reverse (cdr (tailWorkspace (reverse (getWorkspace zonas)) (getArchCambio (cadr (getRepositoryR zonas)))))) (list (editLine (getArch (getWorkspace zonas) (getArchCambio (cadr (getRepositoryR zonas)))) (- (getNumLineCambio (cadr (getRepositoryR zonas))) 1) (getNewLineCambio (cadr (getRepositoryR zonas))))) (cdr (tailWorkspace (getWorkspace zonas) (getArchCambio (cadr (getRepositoryR zonas)))))) (getIndex zonas) (getRepositoryL zonas) (deleteCambio (getRepositoryR zonas)) (getHistorial zonas)))))))
                   null)))
                   

;-----------------------------------ADD-------------------------------------------------------------------------
;dom: lista x zonas
;rec: zonas
;objetivo: agregar los cambios generados en al workspace al index
;recursion natural
(define add (lambda (listaNombreArchivos)
              (lambda (zonas)
                (crearZonas (getWorkspace zonas) (append (getIndex zonas) ((add-aux listaNombreArchivos)zonas)) (getRepositoryL zonas) (getRepositoryR zonas) (append (getHistorial zonas) (list (date->string (current-date) second) "ADD"))))))

;dom: lista x zonas
;rec: lista de cambios
;objetivo: obtener los cambios creados en el workspace
;recursion Natural
(define add-aux (lambda (listaNombreArchivos)
              (lambda (zonas)
                  (if (null? listaNombreArchivos)
                      null
                      (if (list? listaNombreArchivos)
                          (if (workspace? (getWorkspace zonas))
                               (if (null? (getArch (getWorkspace zonas) (car listaNombreArchivos)))
                                   null
                                   (append (hayCambio? (getRepositoryR zonas) (addNumArch (getArch (getWorkspace zonas) (car listaNombreArchivos)) 1 (list )) (addNumArch (getArch (getWorkspace zonas) (car listaNombreArchivos)) 1 (list )) (list)) ((add-aux (cdr listaNombreArchivos))(crearZonas (getWorkspace zonas) (getIndex zonas) (getRepositoryL zonas) (deleteCambioArch (getRepositoryR zonas) (car listaNombreArchivos)) (getHistorial zonas)))))
                               null)
                          null)))))

;--------------------------------------COMMIT----------------------------------------------------------------------
