;;;; package.lisp


(defpackage #:hive-game.utilities
  (:use #:cl)
  (:nicknames #:hive.utils)
  (:export #:weak-push
           #:make-weak-list
           #:weak-car
           #:weak-cdr
           #:weak-find))

(defpackage #:hive-game
  (:use #:cl)
  (:nicknames #:hive))

(defpackage #:hive-game.pathing
  (:use #:cl)
  (:use #:hive-game)
  (:nicknames #:hive.path))

(defpackage #:hive-game.object-component-system
  (:use #:cl)
  (:use #:hive-game)
  (:nicknames #:hive.ocs))

(defpackage #:hive-game.renderer
  (:use :cl)
  (:use :hive-game)
  (:nicknames #:hive.renderer))
