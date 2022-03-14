;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Author: Abidán Brito Clavijo
;;; Subject: Artificial Intelligence
;;; Assignment: Parcel Picker Robot
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; -- GLOBAL VARIABLES --
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defglobal ?*grid_rows* = 5 
           ?*grid_columns* = 8
           ?*robot_pos_x* = 4
           ?*robot_pos_y* = 1
)


;; -- TEMPLATES --
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Static entities
(deftemplate destination (slot x) (slot y))
(deftemplate obstacle (slot x) (slot y))
(deftemplate parcel (slot x) (slot y))

;; Dynamic entities
(deftemplate robot (slot x) (slot y) (slot picked_up))

;; Misc 
(deftemplate grid (slot rows) (slot columns))


;; -- FACTS --
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(deffacts board 
                ;; Grid & status
                (grid (rows ?*grid_rows*) (columns ?*grid_columns*))

                ; Dynamic entities
                (robot (x ?*robot_pos_x*) (y ?*robot_pos_y*) (picked_up no))

                ; Static entities
                (destination (x 1) (y 3))
                (obstacle (x 1) (y 4))
                (obstacle (x 3) (y 1))
                (obstacle (x 3) (y 4))
                (obstacle (x 3) (y 5))
                (obstacle (x 3) (y 6))
                (obstacle (x 4) (y 4))
                (obstacle (x 5) (y 4))
                (parcel (x 4) (y 5))
)


;; -- RULES --
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defrule setup_strategy
    (declare (salience 3))
    =>
    (set-strategy breadth)
)

(defrule move_right
    (declare (salience 1))
    (robot (x ?rx) (y ?ry) (picked_up ?status))
    (grid (rows ?) (columns ?columns))

    ; Make sure there's not an obstacle to the right
    (not (obstacle (x ?rx) (y =(+ ?ry 1))))

    ; Check for right edge
    (test (< ?ry ?columns))
    =>
    (printout t "Move one right" crlf)
    (assert (robot (x ?rx) (y (+ ?ry 1)) (picked_up ?status)))
)

(defrule move_left
    (declare (salience 1))
    (robot (x ?rx) (y ?ry) (picked_up ?status))
    
    ; Make sure there's not an obstacle to the left
    (not (obstacle (x ?rx) (y =(- ?ry 1))))

    ; Check for left edge
    (test (> ?ry 1))
    =>
    (printout t "Move one left" crlf)
    (assert (robot (x ?rx) (y (- ?ry 1)) (picked_up ?status)))
)

(defrule move_up
    (declare (salience 1))
    (robot (x ?rx) (y ?ry) (picked_up ?status))
    
    ; Make sure there's not an obstacle above
    (not (obstacle (x =(- ?rx 1)) (y ?ry)))
    
    ; Check for top edge
    (test (> ?rx 1))
    =>
    (printout t "Move one up" crlf)
    (assert (robot (x (- ?rx 1)) (y ?ry) (picked_up ?status)))
)

(defrule move_down
    (declare (salience 1))
    (robot (x ?rx) (y ?ry) (picked_up ?status))
    (grid (rows ?rows) (columns ?))

    ; Make sure there's not an obstacle below
    (not (obstacle (x =(+ ?rx 1)) (y ?ry)))

    ; Check for bottom edge
    (test (< ?rx ?rows))
    =>
    (printout t "Move one down" crlf)
    (assert (robot (x (+ ?rx 1)) (y ?ry) (picked_up ?status)))
)

(defrule drop_off_parcel
    (declare (salience 2))
    (robot (x ?rx) (y ?ry) (picked_up ?status))
    (destination (x ?dx) (y ?dy))
    (test (and (= ?rx ?dx) (= ?ry ?dy) (eq ?status yes)))

    =>
    
    ;(assert (parcel (x ?rx) (y ?ry)))
    (printout t "Dropped off parcel" crlf)
    (halt)
)

(defrule pick_up_parcel
    (declare (salience 2))
    (robot (x ?rx) (y ?ry) (picked_up ?status))
    (parcel (x ?px) (y ?py))

    ; Check whether parcel has already been picked up
    (test (and (= ?rx ?px) (= ?ry ?py) (eq ?status no)))
    =>
    ; Update parcel status
    (assert (robot (x ?rx) (y ?ry) (picked_up yes)))
    (printout t "Picked up parcel" crlf)
)