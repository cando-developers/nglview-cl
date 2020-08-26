(in-package :nglview)

;;; Register backend is something python does but we don't need to

; p:FileStructure
(defclass file-structure (structure)
  ((path
     :accessor path
     :initarg :path
     :initform nil
     :documentation "Path of the file structure.")
   (fm
     :accessor fm
     :initform nil
     :documentation "File Manager associated with the structure."))
  (:default-initargs
    :ext nil)
  (:documentation "A local file based structure."))

(defmethod initialize-instance :after ((instance file-structure) &rest initargs &key &allow-other-keys)
  (declare (ignore initargs))
  (with-slots (path fm ext) instance
    (unless ext
      (setf ext (pathname-type (pathname path))))
    (setf fm (make-instance 'file-manager :src path)))
  instance)

; p:get_structure_string
(defmethod get-structure-string ((instance file-structure))
  (read-file (fm instance)))


; p:TextStructure
(defclass text-structure (structure)
  ((text
     :accessor text
     :initarg :text
     :initform ""
     :documentation "The text representation of the structure."))
  (:documentation "A in-memory text based structure."))

; p:get_structure_string
(defmethod get-structure-string ((self text-structure))
  (text self))


; p:PdbIdStructure
(defclass pdb-id-structure (structure)
  ((pdbid
     :accessor pdbid
     :initarg :pdbid
     :initform nil
     :documentation "The PDB-ID of the structure.")
   (url
     :accessor url
     :initarg :url
     :initform "http://files.rcsb.org/view/~A.~A"
     :documentation "Base URL from which to retrieve the PDB structure."))
  (:default-initargs
    :ext "pdb")
  (:documentation "A structure referenced by PDB-ID."))

(defmethod initialize-instance :after ((instance pdb-id-structure) &rest initargs &key &allow-other-keys)
  (declare (ignore initargs))
  (when (string= (name instance) "")
    (setf (name instance) (pdbid instance)))
  instance)

; p:get-structure-string
(defmethod get-structure-string ((instance pdb-id-structure))
  (let ((drakma:*body-format-function* (lambda (headers external-format-in)
                                         (declare (ignore headers external-format-in))
                                         :utf-8)))
    (drakma:http-request (format nil (url instance) (pdbid instance) (ext instance)))))


(defun read-pdb-frames (&optional input-stream)
  (do* ((line (read-line input-stream nil nil)
              (read-line input-stream nil nil))
        (record-type (when line (subseq line 0 6))
                     (when line (subseq line 0 6)))
        (frame (make-array 30 :fill-pointer 0 :adjustable t :element-type 'single-float))
        frames index)
       ((null line) frames)
    (cond
      ((string= record-type "MODEL ")
        (setq index (1- (parse-integer (subseq line 10 14))))
        (setf (fill-pointer frame) 0))
      ((string= record-type "ENDMDL")
        (setq frames (acons index (coerce frame (list 'simple-array 'single-float (list (length frame)))) frames)))
      ((or (string= record-type "ATOM  ")
           (string= record-type "HETATM"))
        (vector-push-extend (parse-float:parse-float (subseq line 30 38)) frame)
        (vector-push-extend (parse-float:parse-float (subseq line 38 46)) frame)
        (vector-push-extend (parse-float:parse-float (subseq line 46 54)) frame)))))

(defclass pdb-id-trajectory (pdb-id-structure trajectory)
  ((frames
     :accessor frames
     :initform nil
     :documentation "The raw and computed frames of the PDB trajectory."))
  (:documentation "A trajectory based a PDB file."))

(defmethod initialize-instance :after ((instance pdb-id-trajectory) &rest initargs &key &allow-other-keys)
  (declare (ignore initargs))
  (with-input-from-string (input-stream (get-structure-string instance))
    (setf (frames instance) (read-pdb-frames input-stream)))
  instance)

(defmethod get-coordinates ((instance pdb-id-trajectory) index)
  (cdr (assoc index (frames instance))))

(defmethod n-frames ((instance pdb-id-trajectory))
  (length (frames instance)))



