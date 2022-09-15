(ns pucks.raytracer
  (:use  [scad-clj.scad]
         [scad-clj.model :rename {import scadimport, use scaduse}]
         [numeric.expresso.core])
  (:require
   [pucks.colors :refer :all]
   [clojure.core.matrix :as m]
   [pucks.scad-extended :refer :all]
   [clojure.math :refer [acos atan2 sqrt pow]]
   [same.core :refer [zeroish? not-zeroish?]]))


;; Ray tracing primer
;; https://www.cl.cam.ac.uk/teaching/1999/AGraphHCI/SMAG/node2.html

;; Conventions
;; E - origin point
;; D - offset vector
;; P(t) = E + tD, where t>=0

;; V0, V1, V2, V3... vectors are always zero indexed




;;;;;;;;;;;;;;;;;;;;;;;
;; Protocols

(defprotocol OpticalPart
  (distance [this ray]
    "calculate the distance between [this-obj] and a ray")
  (point-on? [this point]
    "test if this point lies on the surface of [this-obj]")
  (normal [this] [this point]
    "calculate the normal vector of [this-obj] at a point"))


(defprotocol RayTrace
  (trace [this elements]
    "returns a lazy seq of all points along a reflective path")
  (get-closest [this elements]
    "returns the closest element, and its distance")
  (rayproject [this distance]
    "project a ray by a certain distance, return two points"))




;;;;;;;;;;;;;;;;;;;;;;;
;; Math

(defn reflect
  "vector reflection. r = d - 2(d . n) n
  d = incoming normal; n = surface normal"
  [d n]
  (m/sub d (m/mul 2 (m/dot d n) n)))


(defn solve-quad
  "solves roots for quadratics
  determinant = b^2 - 4ac
  + = 2 solutions, 0 = one solution, - = complex solutions
  "
  [a b c]
  (-> "a*x**2 + b*x + c = 0"
      parse-expression
      (substitute {'a a 'b b 'c c})
      (as-> expr (solve 'x expr))))

(def solve-fast-quad
  (->> "a*x**2 + b*x + c = 0"
       parse-expression
       (solve 'x)
       first ;; potentially problematic
       (compile-expr [a b c])))





;;;;;;;;;;;;;;;;;;;;;;;
;; Raytracing


(defrecord Ray [rayorigin raynormal]
  RayTrace
  (get-closest [this elements]
    (->> elements
         (map (fn [element] [element (distance element this)]))
         (sort-by second) first))
  (rayproject [this distance]
    [rayorigin (m/add rayorigin (m/dot raynormal distance))])
  (trace [this elements]
    (lazy-seq
     (cons rayorigin
           (let [[closest distance-to] (get-closest this elements)]
             (if (infinite? distance-to) [(second (rayproject this 400))]
                 (let [[V0 V1] (rayproject this distance-to)]
                   (trace (->Ray V1 (reflect raynormal (normal closest V1)))
                          elements))))))))







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
          Q V0, E (:rayorigin ray), D (:raynormal ray), N (normal this)
          t (m/div (m/dot N (m/sub Q E)) (m/dot N D))
          P0 (m/add (m/dot t D) E)]
      (if (and (not-zeroish? t) (point-on? this P0)) t ##Inf)))
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




