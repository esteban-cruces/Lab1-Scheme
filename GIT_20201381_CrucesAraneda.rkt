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
;dom: TDAworspace x TDAindex x TDAlocalRepository x TDAremoteRepository 
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
;dom: string x zonas
;rec: zonas
;objetivo: subir los cambios del index al repository Local, como commit, con un mensaje
(define commit (lambda (nombreCommit)
                 (lambda (zonas)
                   (crearZonas (getWorkspace zonas) (createIndex) (append (getRepositoryL zonas) (list nombreCommit) (cdr (getIndex zonas))) (getRepositoryR zonas) (append (getHistorial zonas) (list (date->string (current-date) second) "COMMIT"))))))

;---------------------------------------PUSH------------------------------------------------------------------------
;dom: zonas
;rec: zonas
;objetivo: subir los commit guardados en el repositorio local al reposirotio remoto
(define push (lambda (zonas)
               (crearZonas (getWorkspace zonas) (getIndex zonas) (crearRepositoryL) (append (getRepositoryR zonas) (cdr (getRepositoryL zonas))) (append (getHistorial zonas) (list (date->string (current-date) second) "PUSH")))))

;----------------------------------------ZONAS->STRING--------------------------------------------------------------
;dom: zonas
;rec: string
;objetivo: entregar una representación de las zonas como un string posible de visualizar de forma comprensible al usuario.
(define zonas->string (lambda (zonas)
                        (string-append ((workspaceString (getWorkspace zonas))"") ((indexString (getIndex zonas))"") ((repositoryLString (getRepositoryL zonas))"") ((repositoryRString (getRepositoryR zonas))"") ((historialString (getHistorial zonas))""))))
                        
;dom: workspace x string
;rec: string 
;objetivo: pasar el worksspace a un string comprencible por el usuario
;recursion de cola
(define workspaceString (lambda (workspace)
                          (lambda (string)
                            (if (null? workspace)
                                string
                                (if (equal? "" string)
                                    ((workspaceString (cdr workspace))"----------WORKSPACE---------- \n")
                                    ((workspaceString (cdr workspace))(string-append string (((archivoString (car workspace))"")1)"\n")))))))

;dom: archivo x string x entero
;rec: string
;objetivo: transformas un archivo a un string
;recursion de cola
(define archivoString (lambda (archivo)
                        (lambda (string)
                          (lambda (num)
                            (if (null? archivo)
                                string
                                (((archivoString (cdr archivo))(string-append string (number->string num) " " (car archivo) " \n"))(+ num 1 )))))))
                                  
;dom: index x string
;rec: string
;objetivo: pasar el index a un string comprencible por el usuario
;recursion de cola
(define indexString (lambda (index)
                          (lambda (string)
                            (if (null? index)
                                string
                                (if (equal? "" string)
                                    ((indexString (cdr index))"----------INDEX---------- \n")
                                    ((indexString (cdr index))(string-append string ((cambioString (car index))"") "\n")))))))

;dom: cambios x string
;rec: string
;objetivo: transformar un cambio a string
(define cambioString (lambda (cambio)
                        (lambda (string)
                          (string-append string (getArchCambio cambio) ", linea: " (number->string (getNumLineCambio cambio)) ", el cambio es: " (getNewLineCambio cambio)))))

;dom: Local Repository x string
;rec: string
;objetivo: pasar el Local Repository a un string comprencible por el usuario
;recursion de cola
(define repositoryLString (lambda (repositoryL)
                          (lambda (string)
                            (if (null? repositoryL)
                                string
                                (if (equal? "" string)
                                    ((repositoryLString (cdr repositoryL))"----------LOCAL-REPOSITORY---------- \n")
                                    (if (string? (car repositoryL))
                                        ((repositoryLString (cdr repositoryL))(string-append string "--" (car repositoryL) "-- \n"))
                                        ((repositoryLString (cdr repositoryL))(string-append string ((cambioString (car repositoryL))"") " \n"))))))))

;dom: Remote Repository x string
;rec: string
;objetivo: pasar el Remote Repository a un string comprencible por el usuario
;recursion de cola
(define repositoryRString (lambda (repositoryR)
                          (lambda (string)
                            (if (null? repositoryR)
                                string
                                (if (equal? "" string)
                                    ((repositoryRString (cdr repositoryR))"----------REMOTE-REPOSITORY---------- \n")
                                    (if (string? (car repositoryR))
                                        ((repositoryRString (cdr repositoryR))(string-append string "--" (car repositoryR) "-- \n"))
                                        ((repositoryRString (cdr repositoryR))(string-append string ((cambioString (car repositoryR))"") " \n"))))))))

;dom: historial x string
;rec: string
;objetivo: pasar el historial a un string entendible por el usuario
(define historialString (lambda (historial)
                          (lambda (string)
                            (if (null? historial)
                                string
                                (if (string? historial)
                                    "----------HISTORIAL-------- \n"
                                    (if (equal? "" string)
                                        ((historialString (cdr historial))"----------HISTORIAL-------- \n")
                                        ((historialString (cddr historial))(string-append string "Fecha: " (car historial) ", el comando ejecutado fue: " (cadr historial) "\n"))))))))


