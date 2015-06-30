;;;; package.lisp


(defpackage #:hive-game
  (:use #:cl)
  (:nicknames #:hive))

(defpackage #:hive-game.pathing
  (:use #:cl)
  (:use #:hive-game)
  (:nicknames #:hive.path))
