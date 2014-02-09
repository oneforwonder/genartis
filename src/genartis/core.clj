(ns genartis.core
  (:require [genartis.geometry :refer (WIDTH HEIGHT)]
            [genartis.agent :refer (rand-painting)]
            [quil.core :refer :all])) 

(def FPS 1)
(def BG-COLOR [0 0 0])
(def AGENT-COUNT 100)

(def paintings (atom []))
(def triangles (take TRI-COUNT (repeatedly make-triangle)))

(def TEST-IMAGE "img/Lenna.png")

(defn setup []
  ;establish 

  (reset! paintings (repeatedly AGENT-COUNT rand-painting))
  (smooth)
  (frame-rate FPS) 
  (no-stroke)
  (apply background BG-COLOR))

(defn draw-triangle! [{:keys [p1 p2 p3 col]}] 
  (let [[[x1 y1] [x2 y2] [x3 y3]] [p1 p2 p3]]
    (apply fill col) 
    (triangle x1 y1 x2 y2 x3 y3)))

(defn draw-painting! [painting]
  (doseq [tri painting]
    (draw-triangle! tri)))

(defn update []
  (doseq [p @paintings]
    (draw-painting! p))) 

(defn -main []
  (defsketch genartis
    :title "Genartis"
    :setup setup
    :draw update 
    :size [WIDTH HEIGHT]))
