(ns genartis.agent
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

;; Determining painting fitness
(defn painting-fitness [p]
  (rand))
