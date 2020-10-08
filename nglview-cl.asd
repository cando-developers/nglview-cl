
(asdf:defsystem #:nglview-cl
  :description "The nglview for common-lisp-jupyter"
  :version "2.7.7"
  :author ("Kevin Esslinger"
           "Alex Rose"
           "Christian Schafmeister"
           "Tarn W. Burton")
  :maintainer ("Christian Schafmeister"
               "Tarn W. Burton")
  :license "LGPL2. See LICENSE."
  :depends-on (:alexandria
                :common-lisp-jupyter
                :drakma
                :bordeaux-threads
                :parse-float
                :purl
                :trivial-garbage)
  :serial t
  :components ((:module "res"
                :serial t
                :components ((:static-file "main.css")
                             (:static-file "dark.css")
                             (:static-file "light.css")
                             (:static-file "oceans16.css")))
               (:module "src"
                :serial t
                :components ((:file "packages")
                             (:file "color")
                             (:file "utils")
                             (:file "default")
                             (:file "parameters")
                             (:file "queue")
                             (:file "base")
                             (:file "theme")
                             (:file "pythread")
                             (:file "base-adaptor")
                             (:file "adaptor")
                             (:file "widget")
                             (:file "show"))))
  . #-asdf3 () #+asdf3
  (:homepage "https://github.com/cando-developers/nglview-cl/"
   :bug-tracker "https://github.com/cando-developers/nglview-cl/issues"
   :source-control (:git "git://github.com/cando-developers/nglview-cl.git")))
