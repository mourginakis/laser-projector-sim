(ns pucks.scad-extended
  (:use  [scad-clj.scad]
         [scad-clj.model :rename {import scadimport, use scaduse}])
  (:require
   [clojure.core.matrix :as m]
   [clojure.math.combinatorics :as combo]
   [clojure.math :refer [acos atan2 sqrt pow]]

   ;; cad
   [pucks.colors :refer :all]))


(def quarter (deg->rad 90))
(def -quarter (- (deg->rad 90)))
(def half (deg->rad 180))



(defn rotate-vector
  "Rotate a part from [0 0 1] to align perfectly with a vector"
  [[x y z] part]
  (let [magnitude (m/length [x y z])]
    (rotate [0 (acos (/ z magnitude)) (atan2 y x)] part)))



(defn extrude-vector
  "extrudes a 2d shape into a vector"
  ([[x y z]] (extrude-vector [x y z] (circle 0.5)))
  ([[x y z] shape]
   (when-not (m/zero-matrix? [x y z])
     (let [magnitude (m/length [x y z])]
       (->> shape
            (extrude-linear {:height magnitude :center false})
            (rotate-vector [x y z]))))))



(def extrude-vector-example
  (map #(color (m/div (m/add % 10) 20) (extrude-vector %))
       (combo/permuted-combinations
        [-10 -10 -10 0 0 0 10 10 10] 3)))



(defn extrude-points
  "extrudes a shape through a collection of points in 3d"
  ([points] (extrude-points points (circle 0.5)))
  ([points startshape]
   (let [pairs (partition 2 1 points)]
     (for [[v0 v1] pairs
           :let [v' (m/sub v1 v0)]]
       (->> (extrude-vector v' startshape) (translate v0))))))



(defn extrude-segments
  "extrudes a shape through a collection of segments in 3d"
  ([points] (extrude-points points (circle 0.5)))
  ([points startshape]
   (for [[V0 V1] points
         :let [V' (m/sub V1 V0)]]
     (->> (extrude-vector V' startshape) (translate V0)))))



