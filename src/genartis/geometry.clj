(ns genartis.geometry
  (:use genartis.util))

(def WIDTH  512)
(def HEIGHT 512)

(defn rand-pos []
  [(rand-int WIDTH) (rand-int HEIGHT)])

(defn make-triangle []
  {:p1  (rand-pos)
   :p2  (rand-pos)
   :p3  (rand-pos)
   :col (rand-color-a)})
