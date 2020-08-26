(in-package :nglview)


(defun show-pdbid (pdbid &rest kwargs &key trajectory gui &allow-other-keys)
  "Show a PDB file based on ID. To display the file as a trajectory use the trajectory
key. The gui key set to \"ngl\" will display graphical user interface that allows
changing the representation of the structure along with many other parameters. Extra
parameters can be passed as keys. The return values are the nglwidget, the stucture id,
and the structure instance."
  (let ((structure (if trajectory
                     (make-instance 'pdb-id-trajectory :pdbid pdbid)
                     (make-instance 'pdb-id-structure :pdbid pdbid))))
    (values (apply #'make-nglwidget :structure structure :gui-style (when gui "ngl") kwargs)
            (id structure)
            structure)))


(defun show-url (url &rest kwargs &key gui &allow-other-keys)
  "Show a component from a URL. The gui key set to \"ngl\" will display graphical user
interface that allows changing the presentation of the structure along with many other
parameters. Extra parameters can be passed as keys. The return values are the nglwidget
and the component instance."
  (let ((view (make-nglwidget :gui-style (when gui "ngl"))))
    (values view
            (apply #'add-component view url kwargs))))


(defun show-text (text &rest kwargs &key gui &allow-other-keys)
  "Show a structure from a text string. The gui key set to \"ngl\" will display graphical
user interface that allows changing the presentation of the structure along with many other
parameters. Extra parameters can be passed as keys. The return values are the nglwidget,
the stucture id, and the structure instance."
  (let ((structure (make-instance 'text-structure :text text)))
    (values (apply #'make-nglwidget :gui-style (when gui "ngl") :structure structure kwargs)
            (id structure)
            structure)))


(defun show-structure-file (path &rest kwargs &key gui &allow-other-keys)
  "Show a structure from a path. The gui key set to \"ngl\" will display graphical user
interface that allows changing the presentation of the structure along with many other
parameters. Extra parameters can be passed as keys. The return values are the nglwidget,
the stucture id, and the structure instance."
  (let ((structure (make-instance 'file-structure :path path)))
    (values (apply #'make-nglwidget :gui-style (when gui "ngl") :structure structure kwargs)
            (id structure)
            structure)))


(defun show-file (path &rest kwargs &key gui &allow-other-keys)
  "Show a component from a path. The gui key set to \"ngl\" will display graphical user
interface that allows changing the presentation of the structure along with many other
parameters. Extra parameters can be passed as keys. The return values are the nglwidget
and the component instance."
  (let ((view (make-nglwidget :gui-style (when gui "ngl"))))
    (values view
            (apply #'add-component view path kwargs))))


