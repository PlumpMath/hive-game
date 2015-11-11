(in-package #:hive-game.object-component-system)

(defclass object ()
  ((components :initarg :components
               :accessor components
               :type list)
   (instances :allocation :class
              :initform (hive.utils::make-weak-list)
              :reader objects)))
(defmethod initialize-instance :after ((instance object) &key &allow-other-keys)
  (hive.utils::weak-push instance (slot-value instance 'instances)))

(defclass component () ())

(defmethod add-component ((obj object) (comp component))
  (push comp (components obj)))

(defgeneric update (system)
  (:documentation "Called to run the system code on each applicable function."))
(defmethod update (system)
  (map-tick system))

(defmethod map-tick (sys &optional
                                    (objects (objects (class-prototype 'object))))
  (mapc #'(lambda (obj) (tick sys obj))
        objects))

(defmethod tick (sys (obj object))
  (error "No system by that name."))

(defmacro defsystem (name components &body code)
  (let ((components (loop for comp in components
                          if (typep comp 'cons) collect (cadr comp)
                            if (typep comp 'symbol) collect comp))
        (component-names (loop for comp in components
                               if (typep comp 'cons) collect (car comp)
                                 if (typep comp 'symbol) collect comp))
        (system (gensym "system"))
        (object (gensym "object")))
    `(progn
       (defmethod update ((,system (eql ',name)))
         (map-tick ,system (remove-if-not #'(lambda (obj) (loop for comp in ',components
                                                                always (hive.utils::weak-find comp (components obj) :key #'type-of)
                                                                finally (return t)))
                                          (objects (class-prototype (find-class 'object))))))
       (defmethod tick ((,system (eql ',name)) (,object object))
         (let (,@(loop for name in component-names
                       for comp in components
                       collect (list name `(hive.utils::weak-find ',comp (components ,object) :key #'type-of))))
           ,@code)))))


