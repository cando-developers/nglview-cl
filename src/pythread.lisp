(in-package :pythread)

(jupyter:inform :info nil "pythread.lisp")

(defparameter *remote-call-thread-queue* (clext.queue:make-queue 'remote-call-thread-queue))
(defparameter *remote-call-thread* nil)

(defclass remote-call-callback ()
  ((%callback :initarg :callback :accessor callback)
   (%description :initarg :description :initform nil :accessor description)
   (%ngl-msg :initarg :ngl-msg :accessor ngl-msg)
   (%widget :initarg :widget :accessor widget)
   (%method-name :initarg :method-name :accessor method-name)
   (%special-variables :initarg :special-variables :accessor special-variables)
   (%special-values :initarg :special-values :accessor special-values)))



(defun make-remote-call-callback (&rest args)
  (apply 'make-instance
         'remote-call-callback
       :special-variables nil ; FIXME: cl-jupyter:*special-variables*
       :special-values nil ; FIXME: (mapcar #'symbol-value cl-jupyter:*special-variables*)
       args))

(defmethod print-object ((object remote-call-callback) stream)
  (print-unreadable-object (object stream)
    (format stream "~a ~a ~a" (class-name (class-of object)) (method-name object) (description object))))

(defun fire-callback (callback &optional passed-widget)
  (when passed-widget
    (unless (eq passed-widget (widget callback))
      (error "passed-widget ~s does not match callback widget ~s" passed-widget (widget callback))))
  (progv (special-variables callback) (special-values callback)
    (funcall (callback callback) (widget callback))))

(defun remote-call-thread-run (registered-funcs)
  "Keep pulling callbacks out of the queue and evaluating them"
  (jupyter:inform :info nil "Starting remote-call-thread-run")
  (loop
    (jupyter:inform :info nil "About to dequeue from *remote-call-thread-queue*")
    (jupyter:inform :info nil "Queue contents: ~s " *remote-call-thread-queue*)
    (let ((callback (clext.queue:dequeue *remote-call-thread-queue* :timeout 10 :timeout-val :timeout))) ;; (nglv:remote-call-thread-queue widget())))
      ;; messages are sent within the dynamic environment of a specific *parent-msg*,*shell* and *kernel*
      (jupyter:inform :info nil "remote-call-thread-run callback: ~s" callback)
      (cond
        ((eq callback :shutdown)
         (return-from remote-call-thread-run nil))
        ((eq callback :timeout)
         (jupyter:inform :info nil "remote-call-thread-run timeout"))
        ((eq callback :status)
         (jupyter:inform :info nil "remote-call-thread-run is still alive")
         (format t "remote-call-thread-run ism still alive~%"))
        ((eq callback :ping)
         (format t "PONG~%"))
        ((typep callback 'remote-call-callback)
         (fire-callback callback)
         (when (member (method-name callback) registered-funcs :test #'string=)
           (jupyter:inform :info nil "method-name is one of ~s - waiting until callback is finished" registered-funcs)
           (nglview:%wait-until-finished (widget callback)))
         (jupyter:inform :info nil "Callback finished"))
        (t
         (format t "Handle remote-call-thread-run callback: ~a~%" callback)
         (jupyter:inform :info nil "Handle remote-call-thread-run callback: ~a" callback)))
      (jupyter:inform :info nil "remote-call-thread-run done handling callback: ~s" callback))))

(defun remote-call-add (message-or-callback)
  (jupyter:inform :info nil "remote-call-add ~s" message-or-callback)
  (clext.queue:enqueue *remote-call-thread-queue* message-or-callback))

(jupyter:inform :info nil "defclass event  pythread.lisp")

(defclass event ()
  ((%shared-mutex :initform (bordeaux-threads:make-lock "event")
		  :accessor shared-mutex)
   (%event-value  :initform nil
		  :accessor event-value)))

(jupyter:inform :info nil "defmethod is-set  pythread.lisp")

(defmethod is-set ((event event))
  (unwind-protect
       (progn
	 (bordeaux-threads:acquire-lock (shared-mutex event))
	 (event-value event))
    (bordeaux-threads:release-lock (shared-mutex event))))


(defmethod event-set ((event event))
  (unwind-protect
       (progn
	 (bordeaux-threads:acquire-lock (shared-mutex event))
	 (setf (event-value event) t))
    (bordeaux-threads:release-lock (shared-mutex event))))


(jupyter:inform :info nil "defmethod clear  pythread.lisp")

(defmethod clear ((event event))
  (unwind-protect
       (progn
	 (bordeaux-threads:acquire-lock (shared-mutex event))
	 (setf (event-value event) nil))
    (bordeaux-threads:release-lock (shared-mutex event))))

(jupyter:inform :info nil "done  pythread.lisp")


(defun kernel-start-callback ()
  "We need to create a new queue and thread to manage the queue
when the kernel starts up.   This is especially important for the fork-server."
  (jupyter:inform :info nil "Setting up the *remote-call-thread-queue* and *remote-call-thread*")
  (defparameter *remote-call-thread* (bordeaux-threads:make-thread 
                                      (lambda () (remote-call-thread-run
                                                  (list "loadFile" "replaceStructure")))
                                      :name "remote-call-thread")))

; FIXME: restore or replace
; (eval-when (:execute :load-toplevel)
;   (push #'kernel-start-callback cl-ipywidgets:*kernel-start-callbacks*))
  


