(ns pucks.core
  (:use  [scad-clj.scad]
         [scad-clj.model :rename {import scadimport, use scaduse}]
        ;; [uncomplicate.neanderthal core native]
        )
  (:require
   ;; math
   [clojure.core.logic :as logic]
   [clojure.core.matrix :as m]
   [clojure.math.combinatorics :as combo]
   [clojure.math :refer [acos atan2 sqrt pow]]

   ;; traversal
   [clojure.walk :refer [postwalk]]

   ;; cad
   [pucks.colors :refer :all]
   [pucks.scad-extended :refer :all]
   [pucks.parts :refer :all]
   [pucks.raytracer :refer :all :reload :all]))


;;UNITS mm




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Parts

(def build-plate (square 120 68 :center false))
(def build-volume (->> (cube 120 68 130 :center false)
                       (color [1 0 0 0.2])))

(def puck (cube 3 20 20 :center false))

(def table (build (map->OpticalTable {:length 500 :width 250 :thickness 10
                                      :hole-radius 4 :spacing 50})))


(def galvo-1 (build (map->Galvo
                     {:base-radius 20 :base-height 20
                      :trunk-radius 10 :trunk-height 50
                      :spindle-radius 2 :spindle-height 55
                      :mirror-width 10 :mirror-height 20 :mirror-thickness 0.5
                      :pcb-width 40 :pcb-length 50 :pcb-thickness 1
                      :mirror-translation-z 63 :pcb-translation-z 15
                      :total-height 61})))

(def galvoholder (build (map->GalvoHolder
                         {:L-height 80 :L-width 80 :L-thickness 15
                          :height 70 :galvo-x galvo-1 :galvo-y galvo-1})))

(def bluelaserdiode (build (map->LaserDiode
                        {:base-radius 10 :base-height 3
                         :head-radius 6 :head-height 8})))

(def redlaserdiode (build (map->LaserDiode
                        {:base-radius 10 :base-height 3
                         :head-radius 6 :head-height 8})))


(def lensparabola (build (map->LensParabola
                          {:a 1/60 :b 1/20 :focal-length 20})))

;; (def prism (build (map->Prism {})))

(def bluebeam
  (map->LaserBeam {:radius 10 :ray (->Ray [0 0 0] [0 0 1]) :divergence 20
                   :beamcolor (conj blue 0.7)}))

(def redbeam
  (map->LaserBeam {:radius 10 :ray (->Ray [0 0 0] [0 0 1]) :divergence 20
                   :beamcolor (conj red 0.7)}))


(def optical-mirror (map->Mirror {:length 20 :width 10
                                  :thickness 0.2 :normal [1 1 1]}))






;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Assembly


(defrecord MainAssembly []
  MechanicalPart
  (build [_]
    (union
     ;; hardware
     (->> galvoholder (rotate [0 0 half]) (translate [400 150 0]))
     (->> table (translate [0 0 -10]))

     ;; lasers
     (->> bluelaserdiode (rotate [0 quarter 0]) (translate [100 100 20]))
     (->> redlaserdiode (rotate [0 quarter -quarter]) (translate [155 200 20]))

     ;; (->> bluebeam (rotate [0 quarter 0]) (translate [100 100 20]))
     ;; (->> redbeam (rotate [0 quarter -quarter]) (translate [155 200 20]))

     ;; optics
     (->> lensparabola (rotate [0 0 0]) (translate [130 100 20]))
     ;; (->> prism (rotate [0 0 0]) (translate [150 100 20]))
     )))



(defrecord BeamTestAssembly []
  MechanicalPart
  (build [_]
    (union
     ;; lasers
     (->> bluelaserdiode (rotate [0 quarter 0]))
     (->> redlaserdiode (rotate [0 quarter -quarter]))

     (->> bluebeam (rotate [0 quarter 0]))
     (->> redbeam (rotate [0 quarter -quarter]))

     ;; optics
     (build optical-mirror)
     ;; (->> lensparabola (rotate [0 0 0]) (translate [130 100 20]))
     ;; (->> prism (rotate [0 0 0]) (translate [150 100 20]))
     )))


(def raytraceexample
  (let [ray0 (->Ray [0 0 0] (m/normalise [1 0 0]))
        
        mirror0 (map->Sphere {:radius 20 :V [25 13 10]})
        mirror1 (map->Sphere {:radius 10 :V [27 -13 -10]})
        mirror2 (map->Sphere {:radius 20 :V [-50 -20 -5]})
        mirror3 (map->Sphere {:radius 15 :V [-50 -50 -65]})

        ;; mirrorp0 (->Mirror [5 1 1] [5 10 1] [5 10 10] [5 1 10])
        ;; mirrorp1 (->Mirror [10 1 1] [10 8 1] [10 8 2] [10 1 8])
        ;; mirrorp1 (->Mirror [13 -5 -3] [13 -5 3] [10 5 3] [10 5 -3])

        
        segments (trace ray0 [mirror0 mirror1 mirror2 mirror3])
        laserbeam (map->LaserBeam {:segments segments})
        ]
    (union (map build [laserbeam
                       mirror0
                       mirror1 mirror2 mirror3]))
    )
  )








;; Compile
(comment
  (spit "pucks.scad"
        (write-scad build-plate
                    (-% build-volume) puck)))

(spit "pucks.scad"
      (write-scad
       


       raytraceexample
       ;; (build (->MainAssembly))
       ;;(build (->BeamTestAssembly))
       ))





