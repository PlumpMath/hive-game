;;;; package.lisp


(defpackage #:hive-game.utilities
  (:use #:cl)
  (:nicknames #:hive.utils)
  (:export :defmacro!
           :aif
           :alet))

(defpackage #:hive-game
  (:use #:cl)
  (:use #:hive-game.utilities)
  (:nicknames #:hive))

(defpackage #:hive-game.pathing
  (:use #:cl)
  (:use #:hive-game)
  (:use #:hive-game.utilities)
  (:nicknames #:hive.path))

(defpackage #:hive-game.manager
  (:use #:cl)
  (:use #:hive-game)
  (:use #:hive-game.utilities)
  (:nicknames #:hive.man))
