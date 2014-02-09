(ns genartis.agent
  (:require [quil.core :refer (red green blue)])
  (:use [genartis.geometry :only [make-triangle]]))

(def TRI-COUNT 50)

(def TRI-MUTATION-RATE 0.05)
(def ATTR-MUTATION-RATE 0.10)

;; Generating random paintings
(defn rand-painting []
  (repeatedly TRI-COUNT make-triangle))

;; Mutating paintings, triangles, and attributes
(defn mutate-tri [tri]
  tri)

(defn mutate-painting [p]
  (map (fn [tri] (if (< (rand) TRI-MUTATION-RATE)) (mutate-tri tri) tri) p))

(defn abs [x]
  (if (pos? x) x (- x)))

;; Determining painting fitness
(defn painting-fitness [goal p]
  (reduce +
    (map 
      (fn [o n]
        (let [[o-r o-g o-b] [(red o) (green o) (blue o)]
              [n-r n-g n-b] [(red n) (green n) (blue n)]
              r-r           (- 255 (abs (- o-r n-r)))
              r-g           (- 255 (abs (- o-g n-g)))
              r-b           (- 255 (abs (- o-b n-b)))
              final         (+ r-r r-g r-b)]
          final))
      goal p)))
