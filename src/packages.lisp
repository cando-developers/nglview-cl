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
  (:nicknames :nglv)
  (:shadow #:structure)
  (:export
    #:add-component
    #:add-representation
    #:add-structure
    #:add-trajectory
    #:append-coordinates
    #:center
    #:clear-representations
    #:components
    #:component-ids
    #:coordinates-dict
    #:ext
    #:file-structure
    #:fm
    #:frame
    #:get-coordinates
    #:get-structure-name
    #:get-structure-string
    #:handle-resize
    #:hide-components
    #:id
    #:make-nglwidget
    #:n-components
    #:n-frames
    #:nglwidget
    #:params
    #:path
    #:pdbid
    #:pdb-id-structure
    #:pdb-id-trajectory
    #:picked
    #:pick-history
    #:remote-call-thread-queue
    #:remove-all-components
    #:remove-components
    #:remove-representation
    #:remove-representations-by-name
    #:send-binary
    #:set-coordinates
    #:set-representations
    #:set-visibility
    #:shape
    #:show-aggregate
    #:show-components
    #:show-file
    #:shown
    #:show-pdbid
    #:show-structure-file
    #:show-text
    #:show-url
    #:%step
    #:structure
    #:text
    #:text-structure
    #:trajectory
    #:url
    #:%view-height
    #:%view-width
    #:%wait-until-finished))

