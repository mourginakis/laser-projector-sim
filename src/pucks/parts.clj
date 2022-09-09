(ns pucks.parts
  (:use  [scad-clj.scad]
         [scad-clj.model :rename {import scadimport, use scaduse}]
        ;; [uncomplicate.neanderthal core native]
         )
  (:require [pucks.colors :refer :all]
            [pucks.scad-extended :refer :all]
            [clojure.core.matrix :as m]
            [clojure.math :refer [acos atan2 sqrt pow]]
            [pucks.raytracer :refer :all]
            ))



;; Protocols

(defprotocol MechanicalPart
  (build [_]))

(defprotocol Translate)



;; All Parts
(defrecord Sphere [radius V])

(defrecord Prism [normal])

(defrecord Mirror [V0 V1 V2 V3])







;; Mechanical parts


(defrecord Galvo
    [base-radius base-height
     trunk-radius trunk-height
     spindle-radius spindle-height
     mirror-width mirror-height mirror-thickness mirror-translation-z
     pcb-width pcb-length pcb-thickness pcb-translation-z
     total-height]
  MechanicalPart
  (build [_]
    (let [base    (->> (cylinder base-radius base-height :center false)
                       (with-fn 30)
                       (color black))
          trunk   (->> (cylinder trunk-radius trunk-height :center false)
                       (with-fn 30)
                       (color black))
          spindle (->> (cylinder spindle-radius spindle-height :center false)
                       (with-fn 20)
                       (color lightgray))
          mirror  (->> (cube mirror-width mirror-thickness mirror-height)
                       (color silver))
          pcb     (->> (cube pcb-length pcb-width pcb-thickness :center false)
                       (translate [0 (/ pcb-width -2) 0])
                       (color wheat))]
      (union base trunk spindle
             (->> pcb (translate [0 0 pcb-translation-z]))
             (->> mirror (translate [0 0 mirror-translation-z]))))))


(defrecord GalvoHolder [L-height L-width L-thickness height galvo-x galvo-y]
  MechanicalPart
  (build [_]
    (let [slot (->> (cube 2 height L-thickness :center false)
                    (rotate [half half 0])
                    (translate [0 0 25]))
          base (extrude-linear {:height height :center false :convexity 10}
                               (polygon [[0,0] [L-width 0] [L-width L-thickness]
                                         [L-thickness L-thickness]
                                         [L-thickness L-height] [0 L-height]]))
          move-x (fn [obj] (->> obj
                                (rotate [-quarter 0 0])
                                (translate [40 -25 30])))
          move-y (fn [obj] (->> obj
                                (rotate [0 quarter 0])
                                (rotate [-quarter 0 0])
                                (translate [-25 40 30])))]
      (union (difference (->> base (color lightslategray))
                         (->> galvo-x (scale [1.1 1.1 1.1]) move-x)
                         (->> slot move-x)
                         (->> galvo-y (scale [1.1 1.1 1.1]) move-y)
                         (->> slot move-y))
             (->> galvo-x move-x)
             (->> galvo-y move-y)))))


(defrecord OpticalTable [length width thickness hole-radius spacing]
  MechanicalPart
  (build [_]
    (color silver
           (difference
            (cube length width thickness :center false)
            (for [x (range spacing length spacing)
                  y (range spacing width spacing)]
              (->> (cylinder hole-radius (inc thickness) :center false)
                   (with-fn 12)
                   (translate [x y 0])))))))

(defrecord LaserDiode [base-radius base-height
                       head-radius head-height]
  MechanicalPart
  (build [_]
    (let [base  (cylinder base-radius base-height :center false)
          head  (cylinder head-radius head-height :center false)
          glass (cylinder 2 2 :center false) 

          prong (->> (cylinder 0.8 8 :center false)
                     (with-fn 10)
                     (mirror [0 0 1])
                     (translate [0 4 0]))]
      (union
       base
       (->> head (translate [0 0 base-height]) (color silver))
       (->> glass (translate [0 0 (+ base-height 6.1)]) (color blue))
       (->> prong (rotate [0 0 (* tau 1/3)]))
       (->> prong (rotate [0 0 (* tau 2/3)]))
       (->> prong (rotate [0 0 (* tau 3/3)]))))))



(defrecord LensParabola [a b]
  MechanicalPart
  (build [_]
    (let [parabola (fn [x] (+ (* a x x) (* b x)))
          xs       (range 0 11)
          ys       (map parabola xs)
          lasty    (last ys)]
      (->> (polygon (conj (m/transpose [xs ys]) [0 lasty]))
           (translate [0 (- lasty) 0])
           (extrude-rotate)
           (rotate [0 -quarter 0])
           (color (conj silver 0.3))))))



(defrecord LaserBeam [points radius beamcolor]
  MechanicalPart
  (build [_]
    (->> (with-fn 6 (circle (or radius 0.5)))
         (extrude-points points)
         (color (or beamcolor (conj blue 0.7))))))







(extend-type Mirror
  MechanicalPart
  (build [this]
    (let [{:keys [V0 V1 V2 V3]} this
          norm (m/dot -1 (m/normalise (normal this)))
          [V4 V5 V6 V7] (map (partial m/add norm) [V0 V1 V2 V3])]
      (color (conj silver 0.9)
             (polyhedron [V0 V1 V2 V3 V4 V5 V6 V7]
                         [[0 1 2 3] ;; bottom
                          [4 5 1 0] ;; front
                          [7 6 5 4] ;; top
                          [5 6 2 1] ;; right
                          [6 7 3 2] ;; back
                          [7 4 0 3] ;; left
                          ])))))


(extend-type Prism
  MechanicalPart
  (build [this]
    (let [points [[0 0] [0 10] [10 0]]
          normal  [1 1 0]]
      (->> (polygon points)
           (extrude-linear {:height 10})
           (color (conj silver 0.3))))))


(extend-type Sphere
  MechanicalPart
  (build [this]
    (->> (with-fn 32 (sphere (:radius this)))
         (translate (:V this))
         (color (conj silver 0.9)))))



















(comment "FOR ALGEBRAIC CONSTRAINTS"

;; Phrozen sonic 4k:  120 x 68 x 130 mm

(let [m {:base-radius 20 :base-height 20
         :trunk-radius 10 :trunk-height 20
         :spindle-radius 5 :spindle-height 5
         :mirror-width 20 :mirror-height 20 :mirror-thickness 0.5
         :pcb-height 10 :pbc-width 20 :pcb-thickness 0.5
         :total-height 61}]
  (logic/run* [q]
    (logic/== (:pcb-width q) (:base-radius m))
    ;(logic/== {:hello 3 :goodbye 2 :nest 0} q)
    ))

[:base-radius :base-height
 :trunk-radius :trunk-height
 :spindle-radius :spindle-height
 :mirror-width :mirror-height :mirror-thickness
 :pcb-height :pbc-width :pcb-thickness
 :total-height]

;; base-height + trunk-height + spindle-height + mirror-height = total-height
;; pcb-width = base-radius


;; Probably not going to use 
(defmacro defpart [] nil)
 )
