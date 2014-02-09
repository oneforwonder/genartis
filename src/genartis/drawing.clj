(ns genartis.drawing
  (:use [genartis.geometry :only [WIDTH HEIGHT]]
        [genartis.agent :only [rand-painting]]
        quil.core)) 

(def FPS 100)
(def BG-COLOR [0 0 0])
(def AGENT-COUNT 100)

(def paintings (atom []))
(def frame-num (atom 0))

(defn setup []
  (reset! paintings (repeatedly rand-painting))
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

(defn draw-bg! []
  (no-stroke)
  (apply fill BG-COLOR)
  (rect 0 0 WIDTH HEIGHT))

(defn update []
  (draw-bg!)
  (draw-painting! (nth @paintings @frame-num))
  (swap! frame-num inc))

(defn -main []
  (defsketch genartis
    :title "Genartis"
    :setup setup
    :draw update 
    :size [WIDTH HEIGHT]))
