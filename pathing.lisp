(in-package #:hive-game.pathing)

(defun path-map% (obstacles goals updates)
  (declare (type (array (mod 2)) obstacles goals updates))
  (destructuring-bind (sizex sizey) (array-dimensions obstacles)
    (let ((map (make-array (list sizex sizey) :element-type '(mod 255)))
          (path-min 0)
          (path-max 255)
          (update t))
      (labels ((get-neighbors (x y)
                 (let ((neighborsx (list (mod (+ x 1) sizex)
                                         (mod (- x 1) sizex)
                                         (mod x sizex)
                                         (mod x sizex)))
                       (neighborsy (list (mod y sizey)
                                         (mod y sizey)
                                         (mod (+ y 1) sizey)
                                         (mod (- y 1) sizey))))
                   (values neighborsx neighborsy)))
               (update-neighbors (neighborsx neighborsy)
                 (setf (aref updates (nth neighborsx 0) (nth neighborsy 0)) 1
                       (aref updates (nth neighborsx 1) (nth neighborsy 2)) 1
                       (aref updates (nth neighborsx 2) (nth neighborsy 2)) 1
                       (aref updates (nth neighborsx 3) (nth neighborsy 3)) 1))
               (update-square (i j)
                 (multiple-value-bind (neighborsx neighborsy) (get-neighbors i j)
                   (cond
                     ((and (= (aref obstacles i j) 1) (> (aref map i j) path-min))
                      (setf (aref map i j) path-min)
                      (update-neighbors neighborsx neighborsy))
                     ((and (= (aref goals i j) 1) (< (aref map i j) path-max))
                      (setf (aref map i j) path-max))
                     (t
                      (let ((max (max (aref map (nth neighborsx 0) (nth neighborsy 0))
                                      (aref map (nth neighborsx 1) (nth neighborsy 1))
                                      (aref map (nth neighborsx 2) (nth neighborsy 2))
                                      (aref map (nth neighborsx 3) (nth neighborsy 3)))))
                        (when (and (> max 0) (/= (- max 1) (aref map i j)))
                          (setf (aref map i j) (- max 1))
                          (update-neighbors neighborsx neighborsy))))))))
        (loop when update
                do (loop for i below sizex
                         do (loop for j below sizey
                                  do (when (= (aref updates i j) 1)
                                       (setf update t
                                             (aref updates i j) 0)
                                       (update-neighbors i j)))))))))
(defun make-coord (x y)
  (cons x y))
(defun get-x-y (coord)
  (values (car coord) (cdr coord)))
(defun lookup-coord (map coord)
  (multiple-value-bind (x y) (get-x-y coord)
    (aref map x y)))
(defun (setf lookup-coord) (value map coord)
  (multiple-value-bind (x y) (get-x-y coord)
    (setf (aref map x y) value)))
(defparameter *map-size* (make-coord 8 8))

(defclass path ()
  ((obstacles :initform (make-array *map-size* :element-type '(mod 2))
              :initarg :obstacles
              :type '(array (mod 2) 2))
   (goals :initform (make-array *map-size* :element-type '(mod 2))
          :initarg :goals
          :type '(array (mod 2) 2))
   (active :initform (make-array *map-size* :element-type '(mod 2))
           :type '(array (mod 2) 2))
   (map :initform (make-array *map-size* :element-type '(mod 256))
        :type '(array (mod 255) 2))
   (local-updates :initform (cons :loc nil)
                  :type 'cons)
   (global-updates :initform (cons :glob nil)
                   :type 'cons)))



(defun new-obstacle-p (path coord)
  (with-slots (obstacles map) path
    (and (= 1 (lookup-coord obstacles coord))
         (< 0 (lookup-coord obstacles coord)))))
(defun new-goal-p (path coord)
  (with-slots (goals map) path
    (and (= 1 (lookup-coord goals coord))
         (> 255 (lookup-coord map coord)))))

(defun get-neighbors (coord)
  (multiple-value-bind (x y) (get-x-y coord)
    (multiple-value-bind (i j) (get-x-y *map-size*)
      `(,(make-coord (mod (+ x 1) i) y)
        ,(make-coord (mod (- x 1) i) y)
        ,(make-coord x (mod (+ y 1) j))
        ,(make-coord x (mod (- y 1) j))))))

(defun path-map%% (path)
  (declare (type path path))
  (with-slots (obstacles goals active map local-updates global-updates) path
    (destructuring-bind (i j) *map-size*
      (if (cdr local-updates)
          (loop :for con = (cdr local-updates) :then (cdr con)
                :do (let ((coords (car con))
                         (x (caar con))
                         (y (cadar con)))
                     (cond
                       ((new-obstacle-p path coords)
                        (setf (aref map x y) 0)
                        (nconc con (get-neighbors coords)))
                       ((new-goal-p path coords)
                        (setf (aref map x y) 255)
                        (nconc con (get-neighbors coords)))
                       (t
                        (let ((new (- (apply #'max (mapcar (lambda (coords) (aref map
                                                                                  (car coords)
                                                                                  (cadr coords)))
                                                           (get-neighbors coords))) 1)))
                          (unless (or (= new (aref map x y))
                                      (= 1 (aref obstacles x y))
                                      (= 1 (aref goals x y)))
                            (setf (aref map x y) new)
                            (nconc con (get-neighbors coords))))))
                     (setf (aref active x y) 1))
                   (print map)
                :while (cdr con)
                :finally (setf local-updates con))))))

(defun print-array (array)
  (destructuring-bind (x y) (array-dimensions array)
    (loop :for i :below x :do
      (loop :for j :below y :do
        (format t "~3a " (lookup-coord array (make-coord i j))))
      (format t "~%"))))

(defun update-coord (path con coord)
  (with-slots (obstacles goals active map) path
    (cond
      ((new-obstacle-p path coord)
       (setf (lookup-coord map coord) 0)
       (nconc con (get-neighbors coord)))
      ((new-goal-p path coord)
       (setf (lookup-coord map coord) 255)
       (nconc con (get-neighbors coord)))
      (t
       (let ((new (- (apply #'max (mapcar (lambda (coord) (lookup-coord map coord))
                                          (get-neighbors coord)))
                     1)))
         (unless (or (= new (lookup-coord map coord))
                     (= 1 (lookup-coord obstacles coord))
                     (= 1 (lookup-coord goals coord))
                     (> 0 new))
           (setf (lookup-coord map coord) new)
           (nconc con (get-neighbors coord))))))))

(defun path-map%%% (path)
  (with-slots (active map local-updates global-updates) path
    (if (cdr global-updates)
        (loop :for con = (cdr global-updates) :then (cdr con)
              :if (= 1 (lookup-coord active (car con))) :do
                (update-coord path local-updates (car con))
              :while (cdr con)
              :finally (setf global-updates con)))
    (if (cdr local-updates)
        (loop :for con = (cdr local-updates) :then (cdr con)
              :do (update-coord path con (car con))
                  (setf (lookup-coord active (car con)) 1)
              :while (cdr con)
              :finally (setf local-updates con)))))

(defun add-obstacle (path coord)
  (with-slots (obstacles local-updates) path
    (setf (lookup-coord obstacles coord) 1)
    (nconc local-updates (list coord))))
(defun add-goal (path coord)
  (with-slots (goals local-updates) path
    (setf (lookup-coord goals map) 1)
    (nconc local-updates (list coord))))


(defun make-goals-by-targets (target-map)
  "Create map of outlines. Used to find squares adjacent to a given building, for example."
  (destructuring-bind (i j) (array-dimensions target-map)
    (let ((goals (make-array '(i j) :element-type 'bit)))
      (loop :for x :below i :do
        (loop :for y :below j
              :if (= 1 (aref target-map x y)) :do
                (loop :for (x1 y1) :in (get-neighbors '(x y))
                      :if (= 0 (aref target-map x1 y1)) :do
                        (setf (aref goals x1 y1) 1)))))))
