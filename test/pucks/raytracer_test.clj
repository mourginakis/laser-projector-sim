(ns pucks.raytracer-test
  (:require [clojure.test :refer :all]
            [pucks.parts :refer :all]
            [pucks.raytracer :refer :all]
            [clojure.core.matrix :as m]
            [same.core :refer [ish? zeroish?]]))


(deftest reflect-test
  (testing "vector reflection"
    (is (ish? [-1 0 0] (reflect [1 0 0] [-1 0 0])))))


(deftest mirrorsphere-normal
  (let [mirrorsphere0 (map->Sphere {:radius 5 :V [0 0 0]})]
    (testing "normals for X, Y, and Z"
      (is (ish? [1 0 0] (normal mirrorsphere0 [5 0 0])))
      (is (ish? [0 1 0] (normal mirrorsphere0 [0 5 0])))
      (is (ish? [0 0 1] (normal mirrorsphere0 [0 0 5]))))))



(deftest mirrorsphere-distance
  (testing "distance along x axis"
    (let [mirrorsphere0 (map->Sphere {:radius 5 :V [15 0 0]})
          ray0          (->Ray [0 0 0] [1 0 0])]
      (is (ish? 10 (distance mirrorsphere0 ray0)))))

  (testing "distance along x axis starting from 1"
    (let [mirrorsphere0 (map->Sphere {:radius 5 :V [15 0 0]})
          ray0          (->Ray [1 0 0] [1 0 0])]
      (is (ish? 9 (distance mirrorsphere0 ray0)))))

  (testing "infinite distance along x axis"
    (let [mirrorsphere0 (map->Sphere {:radius 5 :V [0 15 0]})
          ray0          (->Ray [0 0 0] [-1 0 0])]
      (is (ish? ##Inf (distance mirrorsphere0 ray0)))))
  
  (testing "distance along -x axis"
    (let [mirrorsphere0 (map->Sphere {:radius 5 :V [-15 0 0]})
          ray0          (->Ray [0 0 0] [-1 0 0])]
      (is (ish? 10 (distance mirrorsphere0 ray0)))))

  (testing "infinite distance along -x axis"
    (let [mirrorsphere0 (map->Sphere {:radius 5 :V [15 0 0]})
          ray0          (->Ray [0 0 0] [-1 0 0])]
      (is (ish? ##Inf (distance mirrorsphere0 ray0)))))
  
  (testing "zero distance is infinity"
    (let [mirrorsphere0 (map->Sphere {:radius 5 :V [15 0 0]})
          ray0          (->Ray [10 0 0] [-1 0 0])]
      (is (ish? ##Inf (distance mirrorsphere0 ray0))))))





(deftest mirror-normal
  (testing "normal in positive z direction"
    (let [mirror0 (->Mirror [0 0 0] [1 0 0] [1 1 0] [0 1 0])]
      (is (ish? [0 0 1] (normal mirror0))))))

(deftest mirror-distance
  (testing "distance test in +x direction"
    (let [mirror0 (->Mirror [10 0 0] [10 1 0] [10 1 1] [10 0 1])
          ray0    (->Ray [0 0 0] [1 0 0])]
      (is (ish? 10 (distance mirror0 ray0)))))
  (testing "infinite distance test in +x direction"
    (let [mirror0 (->Mirror [10 1 1] [10 2 1] [10 2 2] [10 1 2])
          ray0    (->Ray [0 0 0] [1 0 0])]
      (is (ish? ##Inf (distance mirror0 ray0))))))
