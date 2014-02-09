(ns genartis.agent)

(def TRI-COUNT 3)

(def TRI-MUTATION-RATE 0.10)
(def ATTR-MUTATION-RATE 0.20)

(def WIDTH  512)
(def HEIGHT 512)


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


(defn mutate-tri [tri]
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

(defn mutate-tri* [tri]
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

(defn mutate-painting [p]
  (map (fn [tri] (if (mutate-tri?) (mutate-tri tri) tri)) p))


;; Determining painting fitness
(defn painting-fitness [goal current]
  (rand))
