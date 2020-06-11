(in-package :nglview)

(defun show-pdbid (pdbid &rest kwargs &key trajectory gui &allow-other-keys)
  (let ((structure (if trajectory
                     (make-instance 'pdb-id-trajectory :pdbid pdbid)
                     (make-instance 'pdb-id-structure :pdbid pdbid))))
    (values (apply #'make-nglwidget :structure structure :gui-style (when gui "ngl") kwargs)
            (id structure)
            structure)))

(defun show-url (url &rest kwargs &key gui &allow-other-keys)
  (let ((view (make-nglwidget :gui-style (when gui "ngl"))))
    (values view
            (apply #'add-component view url kwargs))))

(defun show-text (text &rest kwargs &key gui &allow-other-keys)
  (let ((structure (make-instance 'text-structure :text text)))
    (values (apply #'make-nglwidget :gui-style (when gui "ngl") :structure structure kwargs)
            (id structure)
            (structure))))

(defun show-structure-file (path &rest kwargs &key gui &allow-other-keys)
  (let ((structure (make-instance 'file-structure :path path)))
    (values (apply #'make-nglwidget :gui-style (when gui "ngl") :structure structure kwargs)
            (id structure)
            (structure))))

(defun show-file (path &rest kwargs &key gui &allow-other-keys)
  (let ((view (make-nglwidget :gui-style (when gui "ngl"))))
    (values view
            (apply #'add-component view path kwargs))))


