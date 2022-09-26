(ns pucks.parts
  (:require [pucks.colors :refer :all]
            [scad-clj.model :refer :all]
            [pucks.scad-extended :refer :all]
            [clojure.core.matrix :as m]
            [clojure.math :refer [acos atan2 sqrt pow cos sin]]
            [pucks.raytracer :refer :all]
            [same.core :refer [zeroish? not-zeroish?]]
            [clojure.spec.alpha :as s]))


;; Protocols

(defprotocol MechanicalPart
  (build [this] "output a scad-clj data structure")
  (update-optics [this] [this rot]
    "transform the optics to match the scad transforms"))


(defprotocol Transform
  (affinetransform [this M])
  (scale- [this [sx sy sz]])
  (translate- [this [dx dy dz]])
  (rotatex [this theta])
  (rotatey [this theta])
  (rotatez [this theta])
  (rotate- [this [rx ry rz]]))




;; All Parts
(defrecord Sphere [radius V])

(defrecord Prism [normal])

(defrecord Mirror [V0 V1 V2 V3])





;; Translations

(extend-type Sphere
  Transform
  (translate- [this V1]
    (->Sphere (:radius this) (m/add (:V this) V1))))


(extend-type Mirror
  Transform
  (affinetransform [this M]
    ;; affine transform   
    ;; |x'|      |1 0 0 tx||x|  
    ;; |y'|   =  |0 1 0 ty||y|
    ;; |z'|      |0 0 1 tz||z|
    ;; |1 |      |0 0 0 1 ||1|
    (apply ->Mirror
           (vec (for [[x y z] (vals this)
                      :let [[x' y' z' _] (m/mmul M [x y z 1])]]
                  [x' y' z']))))
  (scale- [this [sx sy sz]]
    (affinetransform this [[sx 0  0  0]
                           [0  sy 0  0]
                           [0  0  sz 0]
                           [0  0  0  1]]))
  (translate- [this [dx dy dz]]
    (affinetransform this [[1 0 0 dx]
                           [0 1 0 dy]
                           [0 0 1 dz]
                           [0 0 0 1 ]]))
  (rotatex [this theta]
    (affinetransform this [[1  0              0           0]
                           [0 (cos theta)     (- (sin theta)) 0]
                           [0 (sin theta) (cos theta) 0]
                           [0  0              0           1]]))
  (rotatey [this theta]
    (affinetransform this [[(cos theta) 0  (sin theta) 0]
                           [0           1  0               0]
                           [(- (sin theta)) 0  (cos theta)     0]
                           [0           0  0               1]]))
  (rotatez [this theta]
    (affinetransform this [[(cos theta) (- (sin theta)) 0          0]
                           [(sin theta) (cos theta)     0          0]
                           [0           0               1          0]
                           [0           0               0          1]]))
  (rotate- [this [rx ry rz]]
    (-> this (rotatex rx) (rotatey ry) (rotatez rz))))







;; Constructors

(defn make-mirror
  ([length-y width-z] (make-mirror length-y width-z {:center true}))
  ([length-y width-z & [{:keys [center]} options]]
   (let [max-y (/ length-y 2) max-z (/ width-z 2)
         m0 (->Mirror [0 -0.5 -0.5] [0 -0.5 0.5] [0 0.5 0.5] [0 0.5 -0.5])]
     (if (true? center) (scale- m0 [0 length-y width-z])
         (translate- (make-mirror length-y width-z {:center true})
                     [0 (/ length-y 2) (/ width-z 2) ])))))




;; Mechanical parts


(defrecord Galvo
    [base-radius base-height
     trunk-radius trunk-height
     spindle-radius spindle-height
     mirror-width mirror-height mirror-thickness mirror-translation-z
     pcb-width pcb-length pcb-thickness pcb-translation-z
     total-height]
  MechanicalPart
  (build [this]
    (let [base    (->> (cylinder base-radius base-height :center false)
                       (with-fn 30)
                       (color black))
          trunk   (->> (cylinder trunk-radius trunk-height :center false)
                       (with-fn 30)
                       (color black))
          spindle (->> (cylinder spindle-radius spindle-height :center false)
                       (with-fn 20)
                       (color lightgray))
          ;; mirror  (make-mirror mirror-width mirror-height)
          pcb     (->> (cube pcb-length pcb-width pcb-thickness :center false)
                       (translate [0 (/ pcb-width -2) 0])
                       (color wheat))]
      (union base trunk spindle
             (->> pcb (translate [0 0 pcb-translation-z])))))
  (update-optics [this rot]
    (-> (make-mirror mirror-width mirror-height)
        (rotate- [0 0 rot])
        (translate- [0 0 mirror-translation-z]))))




(defrecord GalvoHolder [L-height L-width L-thickness
                        height-z
                        galvo-x galvo-y
                        mirror-x mirror-y
                        galvo-x-height galvo-y-height]
  MechanicalPart
  (build [this]
    (let [slot (->> (cube 2 height-z L-thickness :center false)
                    (rotate [half half 0])
                    (translate [0 0 25]))
          base (extrude-linear {:height height-z :center false :convexity 10}
                               (polygon [[0,0] [L-width 0] [L-width L-thickness]
                                         [L-thickness L-thickness]
                                         [L-thickness L-height] [0 L-height]]))
          move-x (fn [obj] (->> obj
                                (rotate [-quarter 0 0])
                                (translate [40 -25 galvo-x-height])))
          move-y (fn [obj] (->> obj
                                (rotate [0 quarter 0])
                                (rotate [-quarter 0 0])
                                (translate [-25 40 galvo-y-height])))]

      (union (difference (->> base (color lightslategray))
                         (->> galvo-x build (scale [1.1 1.1 1.1]) move-x)
                         (->> slot move-x)
                         (->> galvo-y build (scale [1.1 1.1 1.1]) move-y)
                         (->> slot move-y))
             (->> galvo-x build move-x)
             (->> galvo-y build move-y))))
  (update-optics [this]
    [(-> mirror-x (rotate- [-quarter 0 0]) (translate- [40 -25 galvo-x-height]))
     (-> mirror-y (rotate- [0 quarter 0]) (rotate- [-quarter 0 0])
         (translate- [-25 40 galvo-y-height]))]))


(defrecord OpticalTable [length width thickness hole-radius spacing]
  MechanicalPart
  (build [this]
    (color silver
           (difference
            (cube length width thickness :center false)
            (for [x (range spacing length spacing)
                  y (range spacing width spacing)]
              (->> (cylinder hole-radius (inc thickness) :center false)
                   (with-fn 12)
                   (translate [x y 0])))))))


(defrecord LaserDiode [base-radius base-height
                       head-radius head-height
                       beamcolor]
  MechanicalPart
  (build [this]
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
       (->> glass (translate [0 0 (+ base-height 6.1)]) (color beamcolor))
       (->> prong (rotate [0 0 (* tau 1/3)]))
       (->> prong (rotate [0 0 (* tau 2/3)]))
       (->> prong (rotate [0 0 (* tau 3/3)]))))))



(defrecord LensParabola [a b]
  MechanicalPart
  (build [this]
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
          anti-norm       (m/mul -1 (m/normalise (normal this)))
          [V4 V5 V6 V7]   (map (partial m/add anti-norm) [V0 V1 V2 V3])
          mirror          (polyhedron [V0 V1 V2 V3 V4 V5 V6 V7]
                                      [[0 1 2 3] ;; bottom
                                       [4 5 1 0] ;; front
                                       [7 6 5 4] ;; top
                                       [5 6 2 1] ;; right
                                       [6 7 3 2] ;; back
                                       [7 4 0 3] ;; left
                                       ])]
      (union (->> mirror (color (conj silver 0.9)))
             (->> mirror (translate anti-norm) (color black))))))



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







;;;;;;;;;;;;;;;;;;;;;;;
;; Optical Parts


;; sphere equation
;; (x - Xc)^2 + (y-Yc)^2 + (z-Zc)^2 - R^2 = 0
(extend-type pucks.parts.Sphere
  OpticalPart
  (distance [this ray]
    (let [sphere-to-ray (m/sub (:rayorigin ray) (:V this))
          a 1
          b (* 2 (m/dot (:raynormal ray) sphere-to-ray))
          c (- (m/dot sphere-to-ray sphere-to-ray) (pow (:radius this) 2))
          discriminant (- (pow b 2) (* 4 a c))
          distance (/ (- (- b) (sqrt discriminant)) (* 2 a)) 
          distance2 (solve-fast-quad a b c)]
      (cond (NaN? distance)  ##Inf
            (neg? distance)  ##Inf
            (zero? distance) ##Inf
            :else            distance)))
  (point-on? [this point] "not implemented")
  (normal [this point] (m/normalise (m/sub point (:V this)))))



(extend-type pucks.parts.Mirror
  OpticalPart
  (distance [this ray]
    (let [{:keys [V0 V1 V2 V3]} this
          Q V0, E (:rayorigin ray), D (:raynormal ray), N (normal this)]
      (if (zeroish? (m/dot N D)) ##Inf ;; parallel case, no reflection
          (let [t (m/div (m/dot N (m/sub Q E)) (m/dot N D))
                P0 (m/add (m/dot t D) E)]
            (if (and (not-zeroish? t) (point-on? this P0)) t ##Inf)))))
  (point-on? [this P0]
    (let [{:keys [V0 V1 V2 V3]} this]
      (let [U (m/sub V1 V0) W (m/sub V3 V0)]
        (and (<= (m/dot U V0) (m/dot U P0) (m/dot U V1))
             (<= (m/dot W V0) (m/dot W P0) (m/dot W V3))))))
  (normal
    ([this]
     (let [{:keys [V0 V1 V2]} this]
       (m/normalise (m/cross (m/sub V2 V1) (m/sub V0 V1)))))
    ([this point] (normal this))))



















(comment "FOR ALEBRAIC CONSTRAINTS"

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
