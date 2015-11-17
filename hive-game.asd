;;;; hive-game.asd

(asdf:defsystem #:hive-game
  :description "Describe hive-game here"
  :author "Your Name <your.name@example.com>"
  :license "Specify license here"
  :depends-on (#:lispbuilder-sdl #:cl-opengl #:cepl-default #:trivial-garbage)
  :serial t
  :components ((:file "package")
               (:file "hive-game")
               (:file "ocs")
               (:file "renderer")
               (:file "pathing")))

