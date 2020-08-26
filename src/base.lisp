(in-package :nglview)

(defclass base-widget (jupyter-widgets:dom-widget)
  ((%msg-q ; _msg_q
     :accessor %msg-q
     :initform (clext.queue:make-queue (gensym)))
   (%msg-ar ; p:_msg_ar
     :accessor %msg-ar
     :initform nil
     :trait :list)
   (%ready ; p:_ready
     :accessor %ready
     :initform nil
     :trait :bool))
  (:metaclass jupyter-widgets:trait-metaclass))

; p:_js
(defun %js (instance code)
  (_call instance "executeCode" :args code))

; p:_call
(defun %call (instance method-name &key args kwargs)
  (with-slots (%msg-q %msg-ar %ready)
              instance
    (let ((msg (jupyter:json-new-obj
                 ("type" "callMethod")
                 ("methodName" method-name)
                 ("args" args)
                 ("kwargs" (cons :obj kwargs)))))
      (if %ready
        (jupyter-widgets:send-custom instance msg)
        (clext.queue:enqueue %msg-q msg))
      (setf %msg-ar (append %msg-ar (list msg))))))

(defmethod jupyter-widgets:on-trait-change (instance (name (eql :%ready)) type old-value new-value source)
  (declare (ignore name type old-value source))
  (when new-value
    (with-slots (%msg-q) instance
      (do ()
          ((clext.queue:queue-emptyp %msg-q))
        (jupyter-widgets:send-custom instance (clext.queue:dequeue %msg-q))))))
