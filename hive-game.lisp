;;;; hive-game.lisp

(in-package #:hive-game)

;;; "hive-game" goes here. Hacks and glory await!

(defparameter *updates* nil)

(defgeneric update (item &key &accept-other-keys)
  (:documentation "Update the state of item."))

(defmacro! defupdate (name &body body)
  `(progn
     (defmethod update ((,g!self (eql ,name)) &key &accept-other-keys) ,@body)
     (if *updates*
         (nconc *updates* (list ,name))
         (setf *updates* (list ,name)))))
