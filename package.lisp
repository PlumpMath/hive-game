;;;; package.lisp

(defpackage #:hive-game
  (:use #:cl)
  (:nicknames #:hive))

(defpackage #:hive-game.utilities
  (:use #:cl)
  (:nicknames #:hive.utils))

(defpackage #:hive-game.pathing
  (:use #:cl)
  (:use #:hive-game)
  (:nicknames #:hive.path))

(defpackage #:hive-game.manager
  (:use #:cl)
  (:use #:hive-game)
  (:nicknames #:hive.man))
