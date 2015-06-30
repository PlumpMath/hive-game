(in-package #:hive-game.manager)

(defparameter *managers* nil)

(defmacro defmanager (name supers slots options)
  `(progn
     (defclass ,name ,supers ,slots ,options)
     (nconc *managers* (list ,name))))


