;;;; hive-game.asd

(asdf:defsystem #:hive-game
  :description "Describe hive-game here"
  :author "Your Name <your.name@example.com>"
  :license "Specify license here"
  :depends-on (#:lispbuilder-sdl #:bordeaux-threads)
  :serial t
  :components ((:file "package")
               (:file "utils")
               (:file "hive-game")
               (:file "pathing")
               (:file "manager")))

