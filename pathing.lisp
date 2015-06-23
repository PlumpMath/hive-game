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
(defparameter *map-size* '(8 8))

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
   (local-updates :type 'cons)
   (global-updates :type 'cons)))

(defun new-obstacle-p (path coords)
  (with-slots (obstacles map) path
    (destructuring-bind (x y) coords
      (and (= 1 (aref obstacles x y))
           (< 0 (aref map x y))))))
(defun new-goal-p (path coords)
  (with-slots (goals map) path
    (destructuring-bind (x y) coords
      (and (= 1 (aref goals x y))
           (> 255 (aref map x y))))))

(defun get-neighbors (coords)
  (destructuring-bind (x y) coords
    (destructuring-bind (i j) *map-size*
      `((,(mod (+ x 1) i) ,y)
        (,(mod (- x 1) i) ,y)
        (,x ,(mod (+ y 1) j))
        (,x ,(mod (- y 1) j))))))

(defun path-map%% (path)
  (declare (type path path))
  (with-slots (obstacles goals active map local-updates global-updates) path
    (destructuring-bind (i j) *map-size*
      (if (cdr local-updates)
          (loop for con = (cdr local-updates) then (cdr con)
                do (let ((coords (car con))
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
                            (nconc con (get-neighbors coords)))))))
                   (print map)
                while (cdr con)
                finally (setf local-updates con))))))

