; https://adventofcode.com/2019/day/3 in Clojure
; Provide input as two lines on STDIN. Try it online: https://tio.run/#clojure

; (parse-direction "R") ==> [1 0]
(defn parse-direction [direction-string]
  (case direction-string
    "R" [+1 +0]
    "U" [+0 -1]
    "D" [+0 +1]
    "L" [-1 +0]))

; (parse-leg "R3") ==> [[1 0] 3]
; (parse-leg "U7") ==> [[0 -1] 7]
(defn parse-leg [leg-string]
  [(parse-direction (subs leg-string 0 1))
   (Integer.        (subs leg-string 1))])

; (parse-path "R3,U4") ==> [[[1 0] 3] [[0 -1] 4]]
(defn parse-path [path-string]
  (map parse-leg (clojure.string/split path-string #",")))

; (expand-leg [[1 0] 3]) ==> (seq [1 0] [1 0] [1 0])
(defn expand-leg [[direction n]]
  (repeat n direction))

; (point-add [5 5] [0 -1]) ==> [5 4]
(defn point-add [[x1 y1] [x2 y2]]
  [(+ x1 x2) (+ y1 y2)])

; (plot-path [[[1 0] 3] [[0 -1] 4]]) ==> '([1 0] [2 0] [3 0] [3 -1] [3 -2] ...)
(defn plot-path [path]
  (->> path
    (mapcat expand-leg)
    (reductions point-add)))

; (first-occurrence-map '(:a :b :c :a :b :d)) ==> {:a 1, :b 2, :c 3, :d 6}
; Note the "1-indexed" result.
(defn first-occurrence-map [xs]
  (reduce
    (fn [m [x i]] (if (nil? (get m x)) (assoc m x i) m))
    {}
    (map vector xs (iterate inc 1))))

; Return a sequence of [[x y] sum-of-delays], for all intersections.
; (path-intersections (parse-path "R3,U1") (parse-path "U4,R3,D3")) ==> '([[3 -1] 14])
(defn path-intersections [path1 path2]
  (let [p1 (plot-path path1)
        p2 (plot-path path2)
        m1 (first-occurrence-map p1)
        m2 (first-occurrence-map p2)]
    ; Filter m2 down to points that also occur in m1, and sum their delays.
    (keep
      (fn [[point i]]
        (let [j (get m1 point)]
          (if (some? j) [point (+ i j)])))
      m2)))

; (manhattan-origin-distance [3 -5]) ==> 8
(defn manhattan-origin-distance [[x y]]
  (+ (Math/abs x) (Math/abs y)))

; The * problem.
(defn shortest-intersection-distance [path1 path2]
  (->> (path-intersections path1 path2)
    (map first)
    (map manhattan-origin-distance)
    (apply min)))

; The ** problem.
(defn minimal-intersection-delay [path1 path2]
  (->> (path-intersections path1 path2)
    (map second)
    (apply min)))

(def path1 (parse-path (read-line)))
(def path2 (parse-path (read-line)))
(printf "*  shortest intersection distance = %d%n" (shortest-intersection-distance path1 path2))
(printf "** minimal intersection delay     = %d%n" (minimal-intersection-delay path1 path2))
