(ns genartis.agent
  (:require [quil.core :refer (red green blue)]))

(def TRI-COUNT 50)

(def TRI-MUTATION-RATE 0.10)
(def ATTR-MUTATION-RATE 0.20)
(def NEW-TRIANGLE-RATE 0.20)
(def RAND-ATTR-RATE 0.20)
(def ALTER-ATTR-RATE 0.60)

(def WIDTH  256)
(def HEIGHT 256)


;; Generating random paintings
(defn rand-pos []
  [(rand-int WIDTH) (rand-int HEIGHT)])

(defn rand-color-a []
  (repeatedly 4 #(rand-int 256)))

(defn make-triangle []
  {:p1  (rand-pos)
   :p2  (rand-pos)
   :p3  (rand-pos)
   :col (rand-color-a)})

(defn rand-painting []
  (repeatedly TRI-COUNT make-triangle))


;; Mutating paintings, triangles, and attributes
(defn +- 
  ([]  (rand-nth [+ -]))
  ([x] ((+-) x)))


(defn alter-x [x]
  (-> x (+ (+- (rand-int 6))) (max 0) (min WIDTH)))

(defn alter-y [y]
  (-> y (+ (+- (rand-int 6))) (max 0) (min HEIGHT)))

(defn alter-col-comp [cc] 
  (-> cc (+ (+- (rand-int 6))) (max 0) (min 255)))


(defn rand-x []
  (rand-int WIDTH))

(defn rand-y []
  (rand-int HEIGHT))

(defn rand-col-comp [] 
  (rand-int 255))


(defn mutate-attr? []
  (< (rand) ATTR-MUTATION-RATE))

(defn mutate-tri? []
  (< (rand) TRI-MUTATION-RATE))


(defn rand-mutate-tri [tri]
  (let [{:keys [p1 p2 p3 col]} tri
        [x1 y1]   p1
        [x2 y2]   p2
        [x3 y3]   p3
        [r g b a] col]
    {:p1  [(if (mutate-attr?) (rand-x) x1)
           (if (mutate-attr?) (rand-y) y1)]
     :p2  [(if (mutate-attr?) (rand-x) x2)
           (if (mutate-attr?) (rand-y) y2)]
     :p3  [(if (mutate-attr?) (rand-x) x3)
           (if (mutate-attr?) (rand-y) y3)]
     :col [(if (mutate-attr?) (rand-col-comp) r)
           (if (mutate-attr?) (rand-col-comp) g)
           (if (mutate-attr?) (rand-col-comp) b)
           (if (mutate-attr?) (rand-col-comp) a)]}))

(defn alter-mutate-tri [tri]
  (let [{:keys [p1 p2 p3 col]} tri
        [x1 y1]   p1
        [x2 y2]   p2
        [x3 y3]   p3
        [r g b a] col]
    {:p1  [(if (mutate-attr?) (alter-x x1) x1)
           (if (mutate-attr?) (alter-y y1) y1)]
     :p2  [(if (mutate-attr?) (alter-x x2) x2)
           (if (mutate-attr?) (alter-y y2) y2)]
     :p3  [(if (mutate-attr?) (alter-x x3) x3)
           (if (mutate-attr?) (alter-y y3) y3)]
     :col [(if (mutate-attr?) (alter-col-comp r) r)
           (if (mutate-attr?) (alter-col-comp g) g)
           (if (mutate-attr?) (alter-col-comp b) b)
           (if (mutate-attr?) (alter-col-comp a) a)]}))

(defn new-mutate-tri [tri]
  (make-triangle))

(defn mutate-tri-fn []
  (let [r (rand)]
    (if (< r NEW-TRIANGLE-RATE)
      new-mutate-tri
      (if (< r (+ NEW-TRIANGLE-RATE RAND-ATTR-RATE))
        rand-mutate-tri
        alter-mutate-tri))))

(defn mutate-painting [p]
  (map (fn [tri] (if (mutate-tri?) ((mutate-tri-fn) tri) tri)) p))

(defn abs [x]
  (if (pos? x) x (- x)))

;; Determining painting fitness
(defn painting-fitness [goal p]
  (reduce +
          (map 
            (fn [o n]
              (let [[o-r o-g o-b] [(red o) (green o) (blue o)]
                    [n-r n-g n-b] [(red n) (green n) (blue n)]
                    t-r           (- 255 (abs (- o-r n-r)))
                    t-g           (- 255 (abs (- o-g n-g)))
                    t-b           (- 255 (abs (- o-b n-b)))
                    r-r           (* t-r t-r)
                    r-g           (* t-g t-g)
                    r-b           (* t-b t-b)
                    final         (+ r-r r-g r-b)]
                final))
            goal p)))
