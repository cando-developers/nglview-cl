(in-package :nglview)


(defgeneric id (instance)
  (:documentation "Return the unique id of the instance")
  (:method ((value string))
    value))


(defclass component ()
  ((id
     :accessor id
     :initarg :id
     :initform (jupyter:make-uuid)
     :documentation "The unique id of the component.")
   (name
     :accessor name
     :initarg :name
     :initform ""
     :documentation "The name of the component.")
   (shown
     :accessor shown
     :initform t
     :type bool
     :documentation "Boolean indicated whether the component is currently visible."))
  (:documentation "Base component of structures and trajectories."))


; p:Structure
(defclass structure (component)
  ((ext
     :accessor ext
     :initarg :ext
     :initform "pdb"
     :documentation "File extension of structure.")
   (params
     :accessor params
     :initform nil
     :type list
     :documentation "Extra parameters associated with the structure."))
  (:documentation "Base class for structures."))

; p:get_structure_string
(defgeneric get-structure-string (instance)
  (:documentation "Get the structure string assocated with the instance"))


; p:Trajectory
(defclass trajectory (component)
  ()
  (:documentation "Base class for trajectories."))

; p:get_coordinates
(defgeneric get-coordinates (instance index)
  (:documentation "Get the coordinates assocated with the instance"))

; p:n_frames
(defgeneric n-frames (instance)
  (:documentation "Get the number of frames assocated with the instance")
  (:method (instance)
    (declare (ignore instance))
    1))

(defun get-interpolated-coordinates (instance index &key (method :linear))
  (multiple-value-bind (base-index parameter)
                       (floor index)
    (cond
      ((zerop parameter)
        (get-coordinates instance base-index))
      ((eql method :linear)
        (let* ((n-frames (n-frames instance))
               (f0 (get-coordinates instance (mod base-index n-frames)))
               (f1 (get-coordinates instance (mod (1+ base-index) n-frames))))
          (map (list 'simple-array 'single-float (list (max (length f0)
                                                            (length f1))))
               (lambda (a b)
                 (+ (* parameter b) (* (- 1 parameter) a)))
               f0 f1)))
      (t
        (error "Unknown interpolation method ~A" method)))))
