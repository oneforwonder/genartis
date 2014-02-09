(ns genartis.core
  (:use [genga.ga])
  (:import java.util.Arrays)
  (:require [genartis.agent :refer [rand-painting mutate-painting painting-fitness WIDTH HEIGHT]]
            [quil.core :refer :all])) 

(def FPS 60)
(def BG-COLOR [0 0 0])
(def POPULATION 40)

(def GOAL-IMAGE "img/Lenna.png")

(def bests (atom []))
(def current-paintings (atom []))
(def current-scores (atom []))

;(def ^:dynamic goal-pixels [])

;; Genetic algorithm
(defn run-ga-gen []
  (binding [*fitness-fn*            painting-fitness
            *agent-mutation-fn*     mutate-painting
            *mutation-type*         :agent
            *crossover-type*        :point
            *selection-type*        :tournament
            *agent-mutation-chance* 1.0]
    (let [best-p   (->> (apply max @current-scores)
                        (.indexOf @current-scores)
                        (nth @current-paintings))
          next-gen (map (comp mutate-painting crossover) 
                        (select-mates @current-paintings @current-scores))]
      (println "GENERATION " (count @bests))
      (println "Avg score: " (int  (quot (/ (reduce + @current-scores) (count @current-scores)) 1000000)))
      (println "Max score: " (int (quot (apply max @current-scores) 1000000)))
      (println)
      (swap! bests #(concat % [best-p]))
      (reset! current-paintings (conj (drop 1 next-gen) best-p) )
      (reset! current-scores []))))

;; Quil
(defn setup []
  ; Load in goal image
  (image (load-image GOAL-IMAGE) 0 0)
  (def goal-pixels (Arrays/copyOf (pixels) (count (pixels))))

  ; Create starting population
  (reset! current-paintings (repeatedly POPULATION rand-painting))

  ; Set Quil settings
  (smooth)
  (frame-rate FPS) 
  (no-stroke)
  (apply background BG-COLOR))

(defn draw-triangle! [{:keys [p1 p2 p3 col]}] 
  (let [[[x1 y1] [x2 y2] [x3 y3] [r g b a]] [p1 p2 p3 col]]
    (fill r g b (+ 30 (* 0.15 a)))  ; Alpha restricted to 30-68
    (triangle x1 y1 x2 y2 x3 y3)))

(defn draw-painting! [painting]
  (doseq [tri painting]
    (draw-triangle! tri)))

(defn draw-bg! []
  (no-stroke)
  (apply fill BG-COLOR)
  (rect 0 0 WIDTH HEIGHT))

(defn draw-and-score! [p]
  (draw-bg!)
  (draw-painting! p)
  (let [score (painting-fitness goal-pixels (pixels))]
    (swap! current-scores #(concat % [score]))))

(defn update []
  (if (= (count @current-paintings) (count @current-scores))
    (do 
      (run-ga-gen)
      (when (= 0 (mod (dec (count @bests)) 100))
        (draw-painting! (last @bests))
        (save (str "saves/" (count @bests) ".png"))))
    (draw-and-score! (nth @current-paintings (count @current-scores)))))

(defn -main []
  (defsketch genartis
    :title "Genartis"
    :setup setup
    :draw update 
    :size [WIDTH HEIGHT]
    :renderer :p2d))
