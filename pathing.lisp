(defun path-map (obstacles goals updates)
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

