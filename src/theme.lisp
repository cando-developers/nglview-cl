(in-package :nglview)

(defclass theme-manager (base-widget)
  ((%theme-css
     :accessor %theme-css
     :trait :unicode)
   (%theme
     :accessor %theme
     :trait :unicode))
  (:default-initargs
    :%view-name "ThemeManagerView"
    :%view-module +frontend-module+
    :%view-module-version +frontend-version+
    :%model-name "ThemeManagerModel"
    :%model-module +frontend-module+
    :%model-module-version +frontend-version+)
  (:metaclass jupyter-widgets:trait-metaclass))

(defmethod initialize-instance :after ((instance theme-manager) &rest initargs &key &allow-other-keys)
  (declare (ignore initargs))
  (setf (%theme instance) "light")
  (update-theme instance))

(defun update-theme (instance)
  (setf (%theme-css instance)
        (apply #'concatenate 'string
               (mapcar (lambda (name)
                         (let ((component (asdf:find-component :nglview-cl (list "res" name))))
                           (if component
                             (alexandria:read-file-into-string (asdf:component-pathname component))
                             "")))
                       (list (format nil "~A.css" (%theme instance))
                             "main.css")))))

; p:_on_theme_changed
(defmethod jw:on-trait-change ((instance theme-manager) (name (eql :%theme-css)) type old-value new-value source)
  (declare (ignore name type old-value source))
  (%call instance "handleThemeChanged"))

(defmethod jw:on-trait-change ((instance theme-manager) (name (eql :%theme)) type old-value new-value source)
  (declare (ignore name type old-value source))
  (setf (%theme-css instance)
        (apply #'concatenate 'string
               (mapcar (lambda (name)
                         (let ((component (asdf:find-component :nglview-cl (list "res" name))))
                           (if component
                             (alexandria:read-file-into-string (asdf:component-pathname component))
                             "")))
                       (list (format nil "~A.css" new-value)
                             "main.css")))))

