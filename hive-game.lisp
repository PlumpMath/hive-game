;;;; hive-game.lisp

(in-package #:hive-game)

;;; "hive-game" goes here. Hacks and glory await!

(defparameter *window-width* 800)
(defparameter *window-height* 600)
(defparameter *scale* 20)

(defclass game-object ()
  ())
(defclass square ()
  ((x :initform 0
      :initarg :x
      :accessor x)
   (y :initform 0
      :initarg y
      :accessor y)
   (width :initform 2
          :initarg :width
          :accessor square-width)
   (color :initform '(0 0 0)
          :initarg :color
          :accessor color)))
(defclass worker (game-object square)
  ((inventory)
   (job)))
(defclass swarm (game-object square)
  ((wheat-count)
   (workers)
   ))

(defparameter *objects* `(,(make-instance 'square :color '(255 0 0))
                          ,(make-instance 'square :x 3 :width 1 :color '(0 255 0))))

(defgeneric update (object &key &allow-other-keys)
  (:documentation "Updates an object."))
(defgeneric draw (object &key &allow-other-keys)
  (:documentation "Draws an object on the screen."))

(defmethod update ((object (eql 'main)) &key &allow-other-keys)
  (declare (ignore object))
  (mapc #'update *objects*))
(defmethod draw ((object (eql 'main)) &key &allow-other-keys)
  (declare (ignore object))
  (mapc #'draw *objects*))

(defmethod update ((object square) &key &allow-other-keys))
(defmethod draw ((object square) &key &allow-other-keys)
  (sdl:draw-box (sdl:rectangle
                 :x (* *scale* (x object))
                 :y (* *scale* (y object))
                 :w (* *scale* (square-width object))
                 :h (* *scale* (square-width object)))
                :color (let* ((color (color object))
                              (r (first color))
                              (g (second color))
                              (b (third color)))
                         (sdl:color :r r :g g :b b))))


(defun main ()
  (sdl:with-init ()
    (sdl:window *window-width* *window-height* :title-caption "Main Game Window")
    (sdl:with-events ()
      (:quit-event () t)
      (:idle ()
             (sdl:clear-display sdl:*black*)
             (update 'main)
             (draw 'main)
             (sdl:update-display)))))
