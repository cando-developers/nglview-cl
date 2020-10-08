(in-package :nglview)

(defparameter *excluded-callback-after-firing*
  (list "setUnSyncCamera" "setSelector"
        "setDelay" "autoView" "_downloadImage" "_exportImage"
        "set_representation_from_backend"))

;; Save up to 8 previous picks
(defparameter *pick-history-depth* 16)

; player  - Replaced by new gui
; shape   - Replaced by direct inline methods
; stage   - Replaced by direct inline methods
; control - Replaced by direct inline methods
(defclass nglwidget (jupyter-widgets:dom-widget)
  ((%ngl-version ; p:_ngl_version
     :accessor ngl-version
     :initform ""
     :trait :unicode
     :documentation "NGL version in the frontend.")
   (%image-data ; p:_image_data
     :accessor %image-data
     :initform ""
     :trait :unicode
     :documentation "The current image data")
   (frame ; p:frame
     :accessor frame
     :initform 0
     :trait :float
     :documentation "The current frame number")
   (max-frame ; p:max_frame
     :accessor max-frame
     :initform 0
     :type Integer
     :trait :int
     :documentation "The total number of frames in the animation")
   (%step ; The isn't the python code b.c. they still have the remains of the old player.
     :accessor %step
     :initform 1
     :trait :float
     :documentation "Frame step amount. To use interpolation use a value smaller than one.")
   (background ; p:background
     :accessor background
     :initform "white"
     :trait :color
     :documentation "Background color of the widget.")
   (loaded ; p:loaded
     :accessor loaded
     :initform nil
     :type boolean
     :documentation "Whether loading the widget has been completed")
   (picked ; p:picked
     :accessor picked
     :initform nil
     :trait :json
     :documentation "Current picked atom.")
   (%pick-history
     :accessor pick-history
     :initform nil
     :type list
     :documentation "History of picked atoms.")
   (n-components ; p:n_components
     :accessor n-components
     :initform 0
     :type integer
     :trait :int
     :documentation "The number of components currently displayed.")
   (%view-width ; p:_view_width
     :accessor %view-width
     :initarg :width
     :initform ""
     :trait :unicode
     :documentation "The width of the current view.")
   (%view-height ; p:_view_height
     :accessor %view-height
     :initarg :height
     :initform ""
     :trait :unicode
     :documentation "The height of the current view.")
   (%scene-position ; p:_scene_position
     :accessor scene-position
     :trait :dict
     :initform nil
     :documentation "The position of the current scene.")
   (%scene-rotation ; p:_scene_rotation
     :accessor scene-rotation
     :initform nil
     :trait :dict
     :documentation "The rotation fo the current scene.")
   (parameters ; p:_parameters
     :accessor parameters
     :initform nil)
   (%ngl-full-stage-parameters ; p:_ngl_full_stage_parameters
     :accessor %ngl-full-stage-parameters
     :initform nil
     :trait :plist-camel-case)
   (%ngl-original-stage-parameters ; p:_ngl_original_stage_parameters
     :accessor %ngl-original-stage-parameters
     :initform nil
     :trait :plist-camel-case)
   (%coordinates-dict ; p:_coordinates_dict
     :accessor coordinates-dict
     :initform nil)
   (%camera-str ; p:_camera_str
     :accessor camera-str
     :initform "orthographic" ; probably need validate for following values "perspective" "orthographic"
     :trait :unicode
     :documentation "The type of camera view.")
   (%camera-orientation ; p:_camera_orientation
     :accessor camera-orientation
     :initform nil
     :type list
     :trait :list)
   (%synced-model-ids ; p:_synced_model_ids
     :accessor %synced-repr-ids
     :initform nil
     :trait :list)
   (%synced-repr-model-ids ; p:_synced_repr_model_ids
     :accessor %synced-repr-model-ids
     :initform nil
     :trait :list)
   (%ngl-view-id ; p:_ngl_view_id
     :accessor %ngl-view-id
     :initform nil
     :trait :list)
   (%ngl-repr-dict ; p:_ngl_repr_dict
     :accessor %ngl-repr-dict
     :initform nil
     :trait :json)
   (components ; This replaces p:_ngl_component_ids, p:_ngl_component_names and p:_trajlist
     :accessor components
     :initform nil)
   (%ngl-msg ; p:_ngl_msg
     :accessor ngl-msg
     :initform nil
     :type (or string null))
   (%send-binary ; p:_send_binary
     :accessor send-binary
     :initform t
     :type boolean)
   (%init-gui ; p:_init_gui
     :accessor init-gui
     :initarg :gui
     :initform nil
     :type boolean)
   (gui-style ; p:gui_style
     :accessor gui-style
     :initform nil
     :initarg :gui-style
     :trait :unicode)
   (%gui-theme ; p:_gui_theme
     :accessor %gui-theme
     :trait :unicode)
   (%widget-theme ; p:_widget_theme
     :accessor %widget-theme
     :initform nil
     :allocation :class)
   (%ngl-serialize ; p:_ngl_serialize
     :accessor %ngl-serialize
     :initform nil
     :type boolean
     :trait :bool)
   (%ngl-msg-archive ; p:_ngl_msg_archive
     :accessor %ngl-msg-archive
     :initform nil
     :type list
     :trait :list)
   (%ngl-coordinate-resource ; p:_ngl_coordinate_resource
     :accessor %ngl-coordinate-resource
     :trait :dict
     :initform nil)
   (representations ; p:_representations
     :accessor representations
     :initarg :representations
     :initform nil)
   (%ngl-color-dict ; p:_ngl_color_dict
     :accessor %ngl-color-dict
     :trait :dict
     :initform nil)
   (%ngl-player-dict ; p:_ngl_player_dict
     :accessor %ngl-player-dict
     :initform nil
     :trait :dict)
   (%iplayer ; p:_iplayer
     :accessor %iplayer
     :initform nil
     :trait :widget)
   (%igui ; p:_igui
     :accessor %igui
     :initform nil
     :trait :widget)
   (%ibtn-fullscreen ; p:_ibtn_fullscreen
     :accessor %ibtn-fullscreen
     :initform nil
     :trait :widget)
   (%gui ; p:_gui
     :accessor %gui
     :initform nil)
   (%theme ; p:_theme
     :accessor %theme
     :initarg :theme
     :initform "default")
   (%widget-image ; p:_widget_image
     :accessor widget-image
     :initform (make-instance 'jupyter-widgets:image :width 900))
   (%image-array ; p:_image_array
     :accessor image-array
     :initform #())
   (remote-call-queue
     :accessor remote-call-queue
     :initform (clext.queue:make-queue (jupyter:make-uuid)))
   (remote-call-thread
     :accessor remote-call-thread
     :initform nil)
   (%event ; p:_event
     :accessor event
     :initform (make-instance 'pythread:event))
   (%ngl-displayed-callbacks-before-loaded-reversed
     :accessor ngl-displayed-callbacks-before-loaded-reversed
     :initform nil)
   (%ngl-displayed-callbacks-after-loaded-reversed
     :accessor ngl-displayed-callbacks-after-loaded-reversed
     :initform nil)
   ;;; FIXME:  Would this be a Clasp mp:PROCESS??
   ;;;   (%handle-msg-thread :initarg :handle-msg-thread :accessor handle-msg-thread :initform :threading.thread)
   #|
   self._handle_msg_thread = threading.Thread(target=self.on_msg,
   args=(self._ngl_handle_msg,))
   # # register to get data from JS side
   self._handle_msg_thread.daemon = True
   self._handle_msg_thread.start()
   |#
   ;; Only one remote-call-thread in pythread:*remote-call-thread*
   #+(or)(%remote-call-thread
           :accessor remote-call-thread
           :allocation :class
           :initform pythread:*remote-call-thread*)
   ;; Only one remote-call-thread-queue in pythread:*remote-call-thread-queue*
   #+(or)(%remote-call-thread-queue
           :accessor remote-call-thread-queue
           :allocation :class
           :initform pythread:*remote-call-thread-queue*)
   (%handle-msg-thread ; p:_handle_msg_thread
     :accessor handle-msg-thread
     :initform nil)
   ;; keep track but making copy
   ;;; FIXME - fix this nonsense below
   #||
        self._set_unsync_camera()
        self.selector = str(uuid.uuid4()).replace('-', '')
        self._remote_call('setSelector', target='Widget', args=[self.selector,])
        self.selector = '.' + self.selector # for PlaceProxy
        self._place_proxy = PlaceProxy(child=None, selector=self.selector)
        self.player = trajectory-player(self)
        self._already_constructed = True
   ||#
   (%init-representations
     :accessor init-representations
     :initform nil))
  (:default-initargs
    :%view-name "NGLView"
    :%view-module +frontend-module+
    :%view-module-version +frontend-version+
    :%model-name "NGLModel"
    :%model-module +frontend-module+
    :%model-module-version +frontend-version+)
  (:metaclass jupyter-widgets:trait-metaclass))

(jupyter-widgets:register-widget nglwidget)

(defmethod initialize-instance :before ((instance nglwidget) &rest initargs &key &allow-other-keys)
  (declare (ignore initargs))
  (unless pythread:*remote-call-thread*
    (pythread::kernel-start-callback)))

(defmethod initialize-instance :after ((instance nglwidget) &rest initargs &key &allow-other-keys)
  (let ((kwargs (copy-list initargs))
        (structure (getf initargs :structure)))
    (cond
      ((getf initargs :representations)
        (setf (getf kwargs :default-representation)
              nil))
      ((getf initargs :default)
        (setf (getf kwargs :default-representation)
              (getf initargs :default))))

    (cond
      ((typep structure 'trajectory)
        (add-trajectory instance structure)); :name (apply #'get-name structure kwargs)))
      ((typep structure 'structure)
        (add-structure instance structure))
      ((listp structure)
        (dolist (trajectory structure)
          (add-trajectory instance trajectory :name (apply #'get-name trajectory kwargs))))
      (structure
        (apply #'add-structure instance structure kwargs))))

  (%sync-with-layout instance)
  (%create-player instance)
  (%create-ibtn-fullscreen instance)

  (unless (%widget-theme instance)
    (setf (%widget-theme instance) (make-instance 'theme-manager))
    (jupyter-widgets:display (%widget-theme instance)))

  (setf (%gui-theme instance) (%theme (%widget-theme instance)))
  (jupyter-widgets:link instance :%gui-theme (%widget-theme instance) :%theme))


(defun make-nglwidget (&rest initargs &key &allow-other-keys)
  "Create a nglwidget."
  (apply #'make-instance 'nglwidget initargs))

; p:_create_ibtn_fullscreen
(defun %create-ibtn-fullscreen (instance)
  (setf (%ibtn-fullscreen instance)
        (make-instance 'jupyter-widgets:button :icon "compress"
                       :layout (make-instance 'jupyter-widgets:layout :width "34px"))))

; p:_sync_with_layout
(defun %sync-with-layout (instance)
  (jupyter-widgets:observe (jupyter-widgets:widget-layout instance) :width
    (lambda (layout-instance name type old-value new-value source)
      (declare (ignore layout-instance name type old-value source))
      (%set-size instance new-value "")))
  (jupyter-widgets:observe (jupyter-widgets:widget-layout instance) :height
    (lambda (layout-instance name type old-value new-value source)
      (declare (ignore layout-instance name type old-value source))
      (%set-size instance "" new-value))))

; p:_create_player
(defun %create-player (instance)
  (let ((player (make-instance 'jupyter-widgets:play :max (max-frame instance)))
        (slider (make-instance 'jupyter-widgets:int-slider :max (max-frame instance) :continuous-update t)))
    (setf (%iplayer instance)
          (make-instance 'jupyter-widgets:h-box :children (list player slider)))
    (jupyter-widgets:link player :value slider :value)
    (jupyter-widgets:observe player :value
      (lambda (player name type old-value new-value source)
        (declare (ignore player name type old-value source))
        (setf (frame instance) new-value)))
    ;(jupyter-widgets:link player :value slider :value)
    (jupyter-widgets:observe instance :max-frame
      (lambda (instance name type old-value new-value source)
        (declare (ignore instance name type old-value source))
        (setf (jupyter-widgets:widget-max player) new-value)
        (setf (jupyter-widgets:widget-max slider) new-value)))
    (jupyter-widgets:observe instance :%step
      (lambda (instance name type old-value new-value source)
        (declare (ignore instance name type old-value source))
        (setf (jupyter-widgets:widget-step player) new-value)
        (setf (jupyter-widgets:widget-step slider) new-value)))))
    ;(jupyter-widgets:directional-link instance :max-frame player :max)))
    ;(jupyter-widgets:link slider :max instance :max-frame)))

; p:_trajlist
(defun %trajlist (instance)
  (remove-if-not (lambda (component)
                   (typep component 'trajectory))
                 (components instance)))

; p:_set_serialization
(defun %set-serialization (self &optional frame-range)
  (setf (%ngl-serialize self) t)
  (setf (ngl-msg-archive self)
         (mapcar (lambda (callback)
                   (ngl-msg callback))
                 (ngl-displayed-callbacks-after-loaded-reversed self)))
  (let ((resource (ngl-coordinate-resource self)))
    (when frame-range
      #|| ;; Finish set-serialization
      (loop for t-index from 0
      for traj in (%trajlist self)
      do (setf (elt resource t-index) (list))
      (loop for
      for f_index in range(*frame_range):
      if f_index < traj.n_frames:
      resource[t_index].append(encode_base64(traj.get_coordinates(f_index)))
      else:
      resource[t_index].append(encode_base64(
                            np.empty((0), dtype='f4')))
            resource['n_frames'] = len(resource[0])

        self._ngl_coordinate_resource = resource
        self._ngl_full_stage_parameters_embed = self._ngl_full_stage_parameters
      ||#
      )))

; p:_unset_serialization
(defun %unset-serialization (instance)
  (setf (%ngl-serialize self) nil
        (%ngl-coordinate-resource self) nil))

(defmethod (setf parameters) :after (new-value (instance nglwidget))
  (%remote-call instance "setParameters"
    :target "Widget"
    :args (list (cons :obj (dict-from-plist new-value)))))

; Fix this after enums added to common-lisp-jupyter
; p:camera
(defmethod camera ((widget nglwidget))
  (cond
    ((string= (camera-str widget) "orthographic")
     :orthographic)
    ((string= (camera-str widget) "perspective")
     :perspective)
    (t (error "Illegal value for %camera-str ~s - must be one of orthographic or perspective"))))

(defmethod (setf camera) (value (widget nglwidget))
  "Values:  :perspective or :orthographic"
  (checktype value (member :perspective :orthographic))
  (let ((camera-str (ecase value
                      (:perspective "perspective")
                      (:orthographic "orthographic"))))
    (setf (camera-str widget) camera-str)
    (%remote-call widget "setParameters"
                  :target "Stage"
                  :kwargs (dict-from-plist (list :camera-type camera-str)))))

; p:_set_camera_orientation
(defun %set-camera-orientation (instance arr)
  (%remote-call instance "set_camera_orientation"
                :target "Widget"
                :args (list arr)))

; p:_request_stage_parameters
(defun %request-stage-parameters (instance)
  (%remote-call instance
                "requestUpdateStageParameters"
                :target "Widget"))

(defmethod jupyter-widgets:on-trait-change ((self nglwidget) type (name (eql :picked)) old new source)
  (declare (ignore type name old source))
  (when (and new
             (jupyter:json-keyp new "atom")
             (slot-boundp self '%pick-history))
    (push new (pick-history self))
    (setf (pick-history self)
          (subseq (pick-history self) 0 (min *pick-history-depth* (length (pick-history self)))))))

(defmethod jupyter-widgets:on-trait-change ((object nglwidget) type (name (eql :background)) old new source)
  (declare (ignore type name old source))
  (setf (parameters object) (list :background-color new)))

(defun subseq-after (item seq)
  (let ((pos (position item seq)))
    (when pos
      (subseq seq (1+ pos)))))

; p:handle_resize
(defun handle-resize (instance)
  (%remote-call instance "handleResize"))

; p:_update_max_frame
(defun %update-max-frame (instance)
  (setf (max-frame instance)
        (apply #'max 0 (mapcar #'n-frames (components instance))))
  (values))

; p:_wait_until_finished
(defmethod %wait-until-finished ((widget nglwidget) &optional (timeout 1.0))
  (jupyter:inform :info nil "entered %wait-until-finished")
  (pythread:clear (event widget))
  (loop
    (sleep timeout)
    (when (pythread:is-set (event widget))
      (return-from %wait-until-finished))
    (jupyter:inform :info nil "woke %wait-until-finished after timeout ~a continuing to wait" timeout)))

; p:_run_on_another_thread
(defmethod %run-on-another-thread ((self nglwidget) func &rest args)
  (error "Finish %run-on-another-thread")
#|
      def _run_on_another_thread(self, func, *args):
        # use `event` to singal
        # func(*args)
        thread = threading.Thread(
            target=func,
            args=args, )
        thread.daemon = True
        thread.start()
        return thread
|#)

; p:on_loaded
(defmethod (setf loaded) :after (new (widget nglwidget))
  ;;;(setf (loaded widget) t)
  (jupyter:inform :info nil "entered on-loaded - firing before-loaded callbacks new -> ~a" new)
  (when new
    (when (slot-boundp widget '%ngl-displayed-callbacks-before-loaded-reversed)
      (%fire-callbacks widget (ngl-displayed-callbacks-before-loaded-reversed widget)))))

; p:_fire_callbacks
(defmethod %fire-callbacks ((widget nglwidget) callbacks)
  (jupyter:inform :info nil "%fire-callbacks entered in process ~s~%  callbacks: ~s" (bordeaux-threads:current-thread)
                   (loop for x in callbacks
                         collect (list (pythread:method-name x) (pythread:description x))))
  (flet ((_call ()
           (jupyter:inform :info nil "%fire-callbacks _call entered in process ~s" (bordeaux-threads:current-thread))
           (loop for callback in callbacks
                 do (progn
                      (jupyter:inform :info nil "      %fire-callback -> ~s in process ~s" (pythread:method-name callback) (bordeaux-threads:current-thread))
                      (pythread:fire-callback callback widget)
                      (when (string= (pythread:method-name callback) "loadFile")
                        (jupyter:inform :info nil "    Waiting until finished")
                        (%wait-until-finished widget))))))
    (bordeaux-threads:make-thread (lambda () (_call))
                             :initial-bindings nil ; FIXME: cl-jupyter:*default-special-bindings*
                             :name "fire-callbacks-thread"))
  (jupyter:inform :info nil "Done %fire-callbacks"))

; p:display
(defmethod jupyter-widgets:%display ((widget nglwidget) &rest args &key gui use-box &allow-other-keys)
  (declare (ignore args))
  (jupyter:inform :info widget "%display")
  (setf (%gui-theme widget) (when gui "ngl"))
  widget)

; p:_set_size
(defun %set-size (instance width height)
  (%remote-call instance
                "setSize"
                :target "Widget"
                :args (list width height)))

; p:_set_sync_repr
(defun %set-sync-repr (instance &rest other-views)
  (with-slots (%synced-repr-model-ids)
              instance
    (when other-views
      (setf %synced-repr-model-ids
            (union %synced-repr-model-ids (mapcar #'%model-id other-views) :test #'equal)))
    (%remote-call instance "setSyncRepr"
                  :target "Widget"
                  :args %synced-repr-model-ids)))

; p:_set_unsync_repr
(defun %set-unsync-repr (instance &rest other-views)
  (with-slots (%synced-repr-model-ids)
              instance
    (when other-views
      (setf %synced-repr-model-ids
            (set-difference %synced-repr-model-ids (mapcar #'%model-id other-views) :test #'equal)))
    (%remote-call instance "setSyncRepr"
                  :target "Widget"
                  :args %synced-repr-model-ids)))

; p:_set_sync_camera
(defun %set-sync-camera (instance &rest other-views)
  (with-slots (%synced-model-ids)
              instance
    (when other-views
      (setf %synced-model-ids
            (union %synced-model-ids (mapcar #'%model-id other-views) :test #'equal)))
    (%remote-call instance "setSyncCamera"
                  :target "Widget"
                  :args %synced-model-ids)))

; p:_set_unsync_camera
(defun %set-unsync-camera (instance &rest other-views)
  (with-slots (%synced-model-ids)
              instance
    (when other-views
      (setf %synced-model-ids
            (set-difference %synced-model-ids (mapcar #'%model-id other-views) :test #'equal)))
    (%remote-call instance "setSyncCamera"
                  :target "Widget"
                  :args %synced-model-ids)))

; p:_set_spin
(defun %set-spin (instance axis angle)
  (%remote-call instance "setSpin"
                :target "Stage"
                :args (list axis angle)))

; p:_set_selection
(defmethod %set-selection ((widget nglwidget) &key selection (component 0) (repr-index 0))
  (%remote-call widget "setSelection"
                :target "Representation"
                :args (list selection)
                :kwargs (list (cons "component_index" component)
                              (cons "repr_index" repr-index))))

; p:color_by
(defmethod color-by ((widget nglwidget) color-scheme &key (component 0))
  (let ((repr-names (get-repr-names-from-dict (%ngl-repr-dict widget) component))
        (index 0))
    (loop for _ in repr-names
       do
         (update-representation widget component index
                                :color-scheme color-scheme)
         (incf index)))
  (values))

;;; This performs the rest of the @representations.setter
(defmethod (setf representations) :after (value (instance nglwidget))
  (dotimes (index (length (components instance)))
    (set-representation instance value :component index)))

; p:update_representation
(defmethod update-representation ((widget nglwidget) &optional (component 0)
                                  (repr-index 0) &rest parameters)
  (%remote-call widget
                "setParameters"
                :target "Representation"
                :kwargs (append (list (cons "component_index" component)
                                      (cons "repr_index" repr-index))
                                (dict-from-plist parameters)))

  (%update-ngl-repr-dict widget)
  (values))

; p:_update_repr_dict
(defun %update-repr-dict (widget-instance)
  (%remote-call widget-instance "request_repr_dict" :target "Widget"))


; p:set_representations
(defmethod set-representations ((widget nglwidget) representations &key (component 0))
  (clear-representations widget :component component)
  (let ((kwargs ""))
    (loop for params in representations
       do
         (if t ; FIXME: (typep params 'cljupyter-widgets:dict)
             (progn
               (setf kwargs (aref params "params"))
               (warn "What to do about update kwargs")
               (%remote-call widget
                           "addRepresentations"
                           :target "compList"
                           :args (list (a params "type"))
                           :kwargs kwargs))
             (error "Params must be a dict"))))
  (values))

; p:_remove_representation
(defun %remove-representation (widget &key (component 0) (repr-index 0))
  (%remote-call widget
                "removeRepresentation"
                :target "Widget"
                :args (list component repr-index)))

; p:_remove_representations_by_name
(defmethod %remove-representations-by-name ((widget nglwidget) repr-name &key (component 0))
  (%remote-call widget
                "removeRepresentationsByName"
                :target "Widget"
                :args (list repr-name component))
  (values))

; p:_update_representations_by_name
(defmethod %update-representations-by-name ((widget nglwidget) repr-name &optional (component 0) &rest kwargs)
  (setf kwargs (%camelize-dict kwargs))
  (%remote-call widget
                "updateRepresentationsByName"
                :target "Widget"
                :args (list repr-name component)
                :kwargs kwargs)
  (values))

; p:_display_repr
(defmethod %display-repr ((widget nglwidget) &key (component 0) (repr-index 0) (name nil))
  (let ((c (format nil "c~A" component))
        (r (write-to-string repr-index)))
    (make-instance 'representation-control :%view widget
                   :component-index component
                   :repr-index repr-index
                   :name (jupyter:json-getf (jupyter:json-getf (jupyter:json-getf (%ngl-repr-dict widget) c) r) "type"))))

; p:_set_coordinates
(defun %set-coordinates (widget index &key movie-making render-params)
  "Update coordinates for all trajectories at index-th frame"
  (set-coordinates
    widget
    (do* ((components-tail (components widget) (cdr components-tail))
          (component (car components-tail) (car components-tail))
          (component-index 0 (1+ component-index))
          coordinates-dict)
         ((null components-tail) coordinates-dict)
      (when (typep component 'trajectory)
        (push (cons component-index (get-interpolated-coordinates component index))
              coordinates-dict)))
    :movie-making movie-making
    :render-params render-params))

; p:set_coordinates
(defmethod set-coordinates ((widget nglwidget) arr-dict &key movie-making render-params)
  (jupyter:inform :info nil  "In nglview set-coordinates")
  (let (buffers
        coordinates-meta)
    (setf (coordinates-dict widget) arr-dict)
    (loop for (index . arr) in (coordinates-dict widget)
          for byte-buffer = arr
          do (push byte-buffer buffers)
          do (push (cons (princ-to-string index) index) coordinates-meta))
    (let ((content (jupyter:json-new-obj ("type" "binary_single")
                                   ("data" (cons :obj coordinates-meta)))))
      (when movie-making
        (setf (jupyter:json-getf content "movie_making") t)
        (setf (jupyter:json-getf content "render_params") (cons :obs render-params)))
      (jupyter-widgets:send-custom widget content (nreverse buffers))))
  (values))

; p:on_frame_changed
(defmethod jupyter-widgets:on-trait-change ((object nglwidget) type (name (eql :frame)) old new source)
  (when (slot-boundp object 'frame)
    (%set-coordinates object (frame object))))

; p:clear_representations
(defmethod clear-representations ((widget nglwidget) &key (component 0))
  (%remote-call widget
                "removeAllRepresentations"
                :target "compList"
                :kwargs (list (cons "component_index" component)))
  (values))

; p:add_shape
(defmethod add-shape ((self nglwidget) shapes &key (name "shape"))
  "add shape objects

        Parameters
        ----------
        shapes : vector of vectors
        name : str, default 'shape'
            name of given shape

        Notes
        -----
        Supported shape: 'mesh', 'sphere', 'ellipsoid', 'cylinder', 'cone', 'arrow'.

        See also
        --------
        {ngl_url}

        Examples
        --------
        (asdf:load-system :nglview)
        (defparameter *v* (make-instance 'nglview:nglwidget))
        (defparameter *sphere* (vector \"sphere\" #(0 0 9) #(1 0 0) 1.5))
        (defparameter *arrow* (vector \"arrow\" #(1 2 7) #(30 3 3) #(1 0 1) 1.0))
        (nglview:add-shape *v* (vector *sphere* *arrow*) :name \"my_shape\")
        "
  (%remote-call self "addShape"
                :target "Widget"
                :args (list name shapes)))

; p:add_representation
(defun add-representation (self repr-type &rest kwargs &key (use-worker nil use-worker-p) (selection "all") &allow-other-keys)
  "Add structure representation (cartoon, licorice, ...) for given atom selection.

        Parameters
        ----------
        repr_type : str
            type of representation. Please see {ngl_url} for further info.
        selection : str or 1D array (atom indices) or any iterator that returns integer, default 'all'
            atom selection
        **kwargs: additional arguments for representation

        Example
        -------
        >>> import nglview as nv
        >>>
        >>> t = (pt.datafiles.load_dpdp()[:].supej = pt.load(membrane_pdb)
                trajrpose('@CA'))
        >>> w = nv.show_pytraj(t)
        >>> w.add_representation('cartoon', selection='protein', color='blue')
        >>> w.add_representation('licorice', selection=[3, 8, 9, 11], color='red')
        >>> w

        Notes
        -----
        User can also use shortcut

        >>> w.add_cartoon(selection) # w.add_representation('cartoon', selection)
        "
  (declare (ignore use-worker))
  (when (string= repr-type "surface")
    (unless use-worker-p
      (setf (getf kwargs :use-worker) nil)))

  ;; avoid space sensitivity
  (setf repr-type (string-trim " " repr-type))
  ;; overwrite selection
  (setf selection (seq-to-string (string-trim " " selection)))
  (let* ((kwargs2 (dict-from-plist kwargs))
         (comp-assoc (assoc :component kwargs2))
         (component (prog1
                        (getf kwargs2 :component 0)
                      (remf kwargs2 :component))))
    #|for k, v in kwargs2.items():
    try:
    kwargs2[k] = v.strip()
    except AttributeError:
    # e.g.: opacity=0.4
    kwargs2[k] = v
    |#
    (let* ((params (dict-from-plist kwargs :remove '(:selection))))
      (push (cons "sele" selection) params)
      (push (cons "component_index" component) params)
;;;      (format t "kwargs -> ~s~%" params)
      (%remote-call self "addRepresentation"
                    :target "compList"
                    :args (list repr-type)
                    :kwargs params))))


; p:center
(defun center (widget-instance &key (selection "*") (duration 0) (component 0))
  "center view for given atom selection

        Examples
        --------
        view.center(selection='1-4')
  "
  (%remote-call widget-instance "autoView"
                :target "compList"
                :args (list selection duration)
                :kwargs (list (cons "component_index" component))))

; p:_on_render_image
(defmethod jupyter-widgets:on-trait-change ((object nglwidget) type (name (eql :%image-data)) old new source)
  (declare (ignore type name old source))
  ;;;(setf (_b64value (widget-image object)) new)
  (when (and (slot-boundp object '%hold-image) (hold-image object))
    (setf (image-array object) (concatenate 'string (image-array object) new))))

; P:render_image
(defmethod render-image ((widget nglwidget) &key (frame nil) (factor 4) (antialias t) (trim nil) (transparent nil))
  (when frame
    (setf (frame widget) frame))
  (let ((params (list (cons "factor" factor)
                      (cons "antialias" antialias)
                      (cons "trim" trim)
                      (cons "transparent" transparent))))
    (%remote-call widget
                  "_exportImage"
                  :target "Widget"
                  :kwargs params))
  (values))

; p:download_image
(defmethod download-image ((widget nglwidget) &key (filename "screenshot.png")
                                                (factor 4)
                                                (antialias t)
                                                (trim nil)
                                                (transparent nil))
  (let ((params (list (cons "factor" factor)
                      (cons "antialias" antialias)
                      (cons "trim" trim)
                      (cons "transparent" transparent))))
    (%remote-call widget
                  "_downloadImage"
                  :target "Widget"
                  :args (list filename)
                  :kwargs params))
  (values))

; p:_ngl_handle_msg
(defmethod jupyter-widgets:on-custom-message ((widget nglwidget) content buffers)
  (jupyter:inform :info widget "Handling custom message ~A" (jupyter:json-getf content "type"))
  (setf (ngl-msg widget) content)
  (alexandria:switch ((jupyter:json-getf content "type") :test #'string=)
    ("updateIDs"
      (setf (%ngl-view-id widget)
            (jupyter:json-getf content "data")))
    ("request_loaded"
      (unless (loaded widget)
        (setf (loaded widget) nil))
      (setf (loaded widget) (jupyter:json-getf content "data")))
    ("request_repr_dict"
       (setf (%ngl-repr-dict widget) (jupyter:json-getf content "data")))
    ("stage_parameters"
      (let ((stage-parameters (jupyter:json-to-plist (jupyter:json-getf content "data") :symbol-case :camel)))
        (setf (%ngl-full-stage-parameters widget) stage-parameters)
        (unless (%ngl-original-stage-parameters widget)
          (setf (%ngl-original-stage-parameters widget) stage-parameters))))
    ("async_message"
      (when (string= (jupyter:json-getf content "data") "ok")
        (jupyter:inform :info widget "Setting event")
        (pythread:event-set (event widget))))
    (otherwise
      (jupyter:inform :warn "No handler for ~A" (jupyter:json-getf content "type")))))

; p:_request_repr_parameters
(defmethod %request-repr-parameters ((widget nglwidget) &key (component 0) (repr-index 0))
  (%remote-call widget
                "requestReprParameters"
                :target "Widget"
                :args (list component repr-index))
  (values))

; p:add_structure
(defun add-structure (self structure &rest kwargs)
  (jupyter:inform :info nil "In add-structure  (loaded self) -> ~a" (loaded self))
  (if (not (typep structure 'structure))
      (error "~s is not an instance of structure" structure))
  (apply '%load-data self structure kwargs)
  (setf (components self)
        (append (components self) (list structure)))
  (when (> (n-components self) 1)
    (center self :component (- (length (components self)) 1)))
  (id structure))

; p:add-trajectory
(defun add-trajectory (widget trajectory &rest kwargs)
  (setf (shown trajectory) t)
  (apply '%load-data widget trajectory kwargs)
  (setf (components widget)
        (append (components widget) (list trajectory)))
  (%update-max-frame widget)
  (id trajectory))

; p:add_pdbid
(defun add-pdbid (instance pdbid)
  (add-component instance (format nil "rcsb://~A.pdb" pdbid)))

; p:add_component
(defun add-component (instance filename &rest kwargs)
  (apply '%load-data instance filename kwargs))

; p:_load_data
(defun %load-data (widget obj &rest kwargs)
  (jupyter:inform :info nil "entered %load-data ~A" kwargs)
  (check-type kwargs list)
  (let* ((kwargs2 (dict-from-plist kwargs))
         (is-url (is-url (make-instance 'file-manager :src obj)))
         passing-buffer binary use-filename blob
         args blob-type)
    (unless (dict-entry "defaultRepresentation" kwargs2)
      (setf kwargs2 (dict-set-or-push "defaultRepresentation" kwargs2 t)))
    (if (null is-url)
        (let ((structure-string (get-structure-string obj)))
          (unless (dict-entry "name" kwargs2)
            (setf kwargs2 (dict-set-or-push "name" kwargs2 (name obj))))
          (if structure-string
              (setf blob structure-string
                    kwargs2 (dict-set-or-push "ext" kwargs2 (ext obj))
                    passing-buffer t
                    use-filename nil
                    binary nil)
              (error "Handle file-manager loads"))
          (if (and (eq binary t) (not use-filename))
              (error "Handle blob decoding of base64 files"))
          (setf blob-type (if passing-buffer "blob" "path"))
          (setf args (list (jupyter:json-new-obj
                       ("type" blob-type)
                                 ("data" blob)
                                 ("binary" (or binary :false))))))
        (setf blob-type "url"
              url obj
              args (list (jupyter:json-new-obj
              ("type" blob-type)
                               ("data" url)
                               ("binary" :false)))))
    ;(let ((name (get-name obj :dictargs kwargs2)))
      ;(vector-push-extend name (%ngl-component-names widget))
      (%remote-call widget "loadFile"
                    :target "Stage"
                    :args args
                    :kwargs kwargs2));)
  (jupyter:inform :info nil "leaving %load-data"))

(defun component-member-p (component index seq)
  (some (lambda (item)
          (or (and (typep item 'integer)
                   (= index item))
              (and (typep item 'string)
                   (string= (id component) item))
              (eq component item)))
        seq))

; p:remove_component
(defun remove-components (instance &rest args)
  (setf (components instance)
        (do* ((components-tail (components instance) (cdr components-tail))
              (component (car components-tail) (car components-tail))
              (index 0)
              remaining-components)
             ((null components-tail) (reverse remaining-components))
          (cond
            ((component-member-p component index args)
              (%remote-call instance
                            "removeComponent"
                            :target "Stage"
                            :args (list index)))
            (t
              (push component remaining-components)
              (setf index (1+ index))))))
  (values))

(defun remove-all-components (instance)
  (dotimes (index (length (components instance)))
    (declare (ignore index))
    (%remote-call instance
                  "removeComponent"
                  :target "Stage"
                  :args (list 0)))
  (setf (components instance) nil))

; p:_remote_call
(defun %remote-call (widget method-name &key (target "Widget") args kwargs)
  "call NGL's methods from Common Lisp

        Parameters
        ----------
        method_name : str
        target : str, (member \"Stage\" \"Viewer\" \"compList\" \"StructureComponent\")
        args : list
        kwargs : alist
            if target is \"compList\", \"component_index\" could be passed
            to specify which component will call the method.

        Examples
        --------
        (%remote-call view \"loadFile\" :args '(\"1L2Y.pdb\")
                          :target \"Stage\" :kwargs '((\"defaultRepresentation\" . t)))

        # perform centerView for 1-th component
        # component = Stage.compList[1];
        # component.centerView(true, \"1-12\");
        (%remote-call view \"centerView\"
                          :target \"component\"
                          :args (list t, \"1-12\" )
                          :kwargs '((\"component_index\" . 1)))
        "
  (check-type args list)
  (check-type kwargs list)              ; alist
  (jupyter:inform :info widget "entered %remote-call ~a" method-name)
  (let ((msg (jupyter:json-new-obj
               ("target" target)
               ("type" "call_method")
               ("methodName" method-name)
               ("args" args)))
        (component-index (assoc "component_index" kwargs :test #'string=))
        (repr-index (assoc "repr_index" kwargs :test #'string=)))
    (when component-index
      (setf (jupyter:json-getf msg "component_index") (cdr component-index))
      (setf kwargs (remove component-index kwargs)))
    (when repr-index
      (setf (jupyter:json-getf msg "repr_index") (cdr repr-index))
      (setf kwargs (remove repr-index kwargs)))
    (setf (jupyter:json-getf msg "kwargs") (cons :obj kwargs))
    (let ((callback-maker (lambda (description)
                            (jupyter:inform :info nil "About to make-remote-call-callback ~a" description)
                            (pythread:make-remote-call-callback
                             :widget widget
                             :callback (lambda (widget)
                                         (jupyter:inform :info nil "Start %remote-call method-name -> ~a" method-name)
                                         (jupyter:inform :info nil "      %remote-call widget -> ~s" widget)
                                         (jupyter:inform :info nil "      %remote-call msg -> ~s" msg)
                                         (prog1
                                             (jupyter-widgets:send-custom widget msg)
                                           (jupyter:inform :info nil "    Done %remote-call method-name -> ~s" method-name)))
                             :method-name method-name
                             :description description
                             :ngl-msg msg))))
      (if (and (slot-boundp widget 'loaded) (loaded widget))
          (let ((callback (funcall callback-maker "remote-call-add")))
            (jupyter:inform :info nil "enqueing remote-call ~a" callback)
            (pythread:remote-call-add callback))
          (let ((callback (funcall callback-maker "before-loaded")))
            (if (slot-boundp widget '%ngl-displayed-callbacks-before-loaded-reversed)
                (push callback (ngl-displayed-callbacks-before-loaded-reversed widget))
                (setf (ngl-displayed-callbacks-before-loaded-reversed widget) (list callback)))))
      (when (not (member method-name *excluded-callback-after-firing* :test #'string=))
        (let ((callback (funcall callback-maker "after-loaded")))
          (if (slot-boundp widget '%ngl-displayed-callbacks-after-loaded-reversed)
              (push callback (ngl-displayed-callbacks-after-loaded-reversed widget))
              (setf (ngl-displayed-callbacks-after-loaded-reversed widget) (list callback)))))))
  (jupyter:inform :info nil "leaving %remote-call ~a" method-name)
  t)

(defun set-visibility (instance visibility &rest args)
  "set visibility for given components (by their indicies or ids)"
  (do* ((components-tail (components instance) (cdr components-tail))
        (component (car components-tail) (car components-tail))
        (index 0 (1+ index)))
       ((null components-tail))
    (when (component-member-p component index args)
      (when (typep component 'trajectory)
        (setf (shown component) visibility))
      (%remote-call instance
                    "setVisibility"
                    :target "compList"
                    :args (list (if visibility :true :false))
                    :kwargs (list (cons "component_index" index))))))

; p:hide
(defun hide-components (instance &rest components)
  "Hide given components (by their indicies or ids)"
  (apply #'set-visibility instance nil components))

; p:show
(defun show-components (instance &rest components)
  "Show given components (by their indicies or ids)"
  (apply #'set-visibility instance t components))


; p:_js_console
(defmethod %js-console ((widget nglwidget))
  (jupyter-widgets:send-custom widget
                               (jupyter:json-new-obj ("type" "get")
                                               ("data" "any"))))

; p:_get_full_params
(defmethod %get-full-params ((widget nglwidget))
  (jupyter-widgets:send-custom widget
                               (jupyter:json-new-obj ("type" "get")
                                               ("data" "parameters"))))




;;; ----------------------------------------------------------------------------------------------------





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;Starting from the bottom down below. SCROLL!


;(defmethod %set-place-proxy ((widget nglwidget) widget)
;  (setf (child (%place-proxy widget)) widget)
;s  (values))

(defmacro pop-from-alist (key alist)
  (let ((k (gensym "KEY")) (a (gensym "ALIST"))
        (pair (gensym "PAIR")))
    `(let* ((,k ,key)
            (,a ,alist)
            (,pair (assoc ,k ,a)))
       (when ,pair
         (prog1 (cdr ,pair)
         (setf ,alist (remove ,pair ,a)))))))
(defmacro pop-from-hash-table (key table)
  (let ((k (gensym "KEY")) (tab (gensym "TABLE")))
    `(let ((,k ,key) (,tab ,table))
       (prog1 (gethash ,k ,tab)
         (remhash ,k ,tab)))))

(defmethod jupyter:on-comm-close :after ((widget nglwidget) data metadata buffers)
  (declare (ignore data metadata buffers))
  ;; (bordeaux-threads:destroy-thread (remote-call-thread widget))
  (when (handle-msg-thread widget)
    (bordeaux-threads:destroy-thread (handle-msg-thread widget)))
  ;;; FIXME: Kill handle-msg-thread
  )


(defmethod %update-ngl-repr-dict ((self nglwidget))
  "Send a request to the frontend to send representation parameters back"
  (jupyter:inform :info nil "Called %update-ngl-repr-dict")
  (%remote-call self
                "request_repr_dict"
                :target "Widget"))


; TWB: Appears unused
; (defmethod representations-setter ((widget nglwidget) reps)
;   (dolist (%ngl-component-ids widget)
;     (set-representations widget reps))
;   (values))

(defmethod camera-setter ((widget nglwidget) value)
  (setf (camera-str widget) value)
  (%remote-call widget
                "setParameters"
                :target "Stage"
                :kwargs (list (cons "cameraType "(camera-str widget))))
  (values))

(defun component-ids (widget)
  (mapcar #'id (components widget)))
