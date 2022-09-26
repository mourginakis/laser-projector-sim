(ns pucks.core
  (:require
   ;; openscad stuff
   [scad-clj.scad :refer :all]
   [scad-clj.model :refer :all :rename {import scadimport, use scaduse}]
   
   ;; math
   [clojure.core.logic :as logic]
   [clojure.core.matrix :as m]
   [clojure.math.combinatorics :as combo]
   [clojure.math :refer [acos atan2 sqrt pow]]

   ;; traversal
   [clojure.walk :refer [postwalk]]

   ;; extended cad
   [pucks.colors :refer :all]
   [pucks.scad-extended :refer :all]
   [pucks.parts :refer :all]
   [pucks.raytracer :refer :all :reload :all]))




;;UNITS mm


(defn maprange [[a1 a2] [b1 b2] x]
  (+ b1 (/ (* (- x a1) (- b2 b1)) (- a2 a1))))

(defn maprange-1
  "maprange with a closure"
  [[a1 a2] [b1 b2]]
  (fn [x]
    (+ b1 (/ (* (- x a1) (- b2 b1)) (- a2 a1)))))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Parts

(def build-plate (square 120 68 :center false))
(def build-volume (->> (cube 120 68 130 :center false)
                       (color [1 0 0 0.2])))

(def puck (cube 3 20 20 :center false))

(def table (map->OpticalTable {:length 500 :width 250 :thickness 10
                               :hole-radius 4 :spacing 50}))


(def galvo-1 (map->Galvo
              {:base-radius 20 :base-height 20
               :trunk-radius 10 :trunk-height 50
               :spindle-radius 2 :spindle-height 55
               :mirror-width 10 :mirror-height 20 :mirror-thickness 0.5
               :pcb-width 40 :pcb-length 50 :pcb-thickness 1
               :mirror-translation-z 63 :pcb-translation-z 15
               :total-height 61}))

(def mirror-x (update-optics galvo-1 (+ pi 1)))
(def mirror-y (update-optics galvo-1 1))

(def galvoholder (map->GalvoHolder
                  {:L-height 80 :L-width 80 :L-thickness 15
                   :height-z 70 :galvo-x galvo-1 :galvo-y galvo-1
                   :mirror-x mirror-x :mirror-y mirror-y
                   :galvo-x-height 40 :galvo-y-height 30}))

(def bluelaserdiode (map->LaserDiode
                     {:base-radius 10 :base-height 3
                      :head-radius 6 :head-height 8
                      :beamcolor (conj blue 0.7)}))

(def redlaserdiode (map->LaserDiode
                    {:base-radius 10 :base-height 3
                     :head-radius 6 :head-height 8
                     :beamcolor (conj red 0.7)}))


(def lensparabola (build (map->LensParabola
                          {:a 1/60 :b 1/20 :focal-length 20})))

;; (def prism (build (map->Prism {})))

(def bluebeam
  (map->LaserBeam {:radius 0.5 :points [[0 0 0] [0 0 200]]
                   :beamcolor (conj blue 0.7)}))

(def redbeam
  (map->LaserBeam {:radius 0.5 :points [[0 0 0] [0 0 200]]
                   :beamcolor (conj red 0.7)}))


(def optical-mirror (map->Mirror {:length 20 :width 10
                                  :thickness 0.2 :normal [1 1 1]}))






;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Assembly


(def MainAssembly
  (union
   ;; hardware
   (->> galvoholder (rotate [0 0 quarter]) (translate [400 60 0]))
   (->> table (translate [0 0 -10]))

   ;; lasers
   (->> bluelaserdiode (rotate [0 quarter 0]) (translate [100 100 20]))
   (->> redlaserdiode (rotate [0 quarter -quarter]) (translate [155 200 20]))


   (->> bluebeam build (rotate [0 quarter 0]) (translate [100 100 20]))
   (->> redbeam build (rotate [0 quarter -quarter]) (translate [155 200 20]))


   ;; optics
   (->> lensparabola (rotate [0 0 0]) (translate [130 100 20]))
   ;; (->> prism (rotate [0 0 0]) (translate [150 100 20]))
   )

  )



(defn BeamTest [xrot yrot]

  (let [galvoholder-t (assoc galvoholder
                             :mirror-x (update-optics galvo-1 (+ pi xrot))
                             :mirror-y (update-optics galvo-1 (+ yrot)))
        
        move-mirrors #(-> % (rotate- [0 0 quarter]) (translate- [150 -40 0]))
        [mirror-x mirror-y] (map move-mirrors (update-optics galvoholder-t))

        plate (-> (make-mirror 120 120)
                  (rotate- [0 0 quarter])
                  (translate- [100 100 60]))
        
        ray0 (->Ray [0 0 30] (m/normalise [1 0 0]))
        
        points [[0 0 0] [200 0 0]]
        points (drop-last (trace ray0 [mirror-x mirror-y plate]))
        hitpoint (last points)
        laserbeam (map->LaserBeam {:points points})]

    (spit "pucks.scad"
          (write-scad

           (union
            (->> bluelaserdiode build
                 (rotate [0 quarter 0])
                 (translate [0 0 30]))
            (build laserbeam)
            
            (->> galvoholder build
                 (rotate [0 0 quarter])
                 (translate [150 -40 0]))

            (build mirror-x)
            (build mirror-y)
            (->> plate build (color aqua))
            (->> table build (translate [-90 -90 -10])))))
    hitpoint))



(comment
  (doseq [x (range 0.52 0.92 0.08)
          y (range 0.52 0.92 0.08)]
    (do
      (Thread/sleep 150)
      (BeamTest x y)
      (println [x y])))

  
  {:ymin 0.52
   :ymax 0.92
   :xmax 0.52
   :xmin 0.92}
  )

;; (BeamTest 0.6 0.6)





(def raytraceexample
  (let [ray0 (->Ray [0 0 0] (m/normalise [1 0 0]))
        
        mirror0 (map->Sphere {:radius 20 :V [25 13 10]})
        mirror1 (map->Sphere {:radius 10 :V [27 -13 -10]})
        mirror2 (map->Sphere {:radius 20 :V [-50 -20 -5]})
        mirror3 (map->Sphere {:radius 15 :V [-50 -50 -65]})

        mirrorp0 (-> (make-mirror 15 5)
                     (rotate- [0 0.5 1.6])
                     (translate- [20 30 -22]))

        points (trace ray0 [mirror0 mirror1 mirror2 mirror3 mirrorp0])
        
        laserbeam (map->LaserBeam {:points points})]
    (union
     (map build [laserbeam mirror0 mirror1 mirror2 mirror3 mirrorp0]))))






;; Compile
(comment
  (spit "pucks.scad"
        (write-scad build-plate
                    (-% build-volume) puck)))
(comment
  (spit "pucks.scad"
        (write-scad
         
         ;; extrude-along-path-example

          raytraceexample
         ;;  MainAssembly
         ;; galvoholder
         

         
         ;; (build (->MainAssembly))
         ;;(build (->BeamTestAssembly))
         )))





