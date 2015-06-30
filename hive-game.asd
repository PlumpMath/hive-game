;;;; hive-game.asd

(asdf:defsystem #:hive-game
  :description "Describe hive-game here"
  :author "Your Name <your.name@example.com>"
  :license "Specify license here"
  :depends-on (#:lispbuilder-sdl)
  :serial t
  :components ((:file "package")
               (:file "hive-game")
               (:file "pathing")))

