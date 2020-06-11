(defpackage "CLEXT.QUEUE"
  (:use "COMMON-LISP"
        "BORDEAUX-THREADS")
  (:export "QUEUE" "QUEUEP"
           "MAKE-QUEUE"
           "QUEUE-NAME"
           "QUEUE-COUNT"
           "QUEUE-EMPTYP"
           "ENQUEUE"
           "DEQUEUE"
           "TEST-QUEUE")
  (:documentation "Implements a thread-safe message queue."))

(defpackage #:nglview
  (:use #:cl)
  (:nicknames #:nglv)
  (:local-nicknames (:j :jupyter)
                    (:jw :jupyter-widgets))
  (:shadow #:structure)
  (:export
    #:add-component
    #:add-representation
    #:add-structure
    #:add-trajectory
    #:append-coordinates
    #:clear-representations
    #:coordinates-dict
    #:ext
    #:file-structure
    #:fm
    #:get-coordinates
    #:get-structure-name
    #:get-structure-string
    #:hide-components
    #:id
    #:make-nglwidget
    #:n-frames
    #:nglwidget
    #:params
    #:path
    #:pdbid
    #:pdb-id-structure
    #:remote-call-thread-queue
    #:remove-all-components
    #:remove-components
    #:send-binary
    #:set-coordinates
    #:shape
    #:show-aggregate
    #:show-components
    #:shown
    #:show-pdbid
    #:show-structure-file
    #:structure
    #:text
    #:text-structure
    #:trajectory
    #:url
    #:%wait-until-finished))

(defpackage #:pythread
  (:use #:cl)
  (:shadow #:set)
  (:export
   #:remote-call-callback
   #:make-remote-call-callback
   #:callback
   #:method-name
   #:description
   #:fire-callback
   #:remote-call-add
   #:remote-call-thread-run
   #:event
   #:event-set
   #:clear
   #:is-set
   #:*remote-call-thread*
   #:*remote-call-thread-queue*))
