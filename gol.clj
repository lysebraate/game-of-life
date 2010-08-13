(load-file "./patterns.clj")

(defn print-symbol [cell-value]
	(if (= 1 cell-value) "#" " "))

(defn print-row [cells]
	(print "\r") 
	(doseq [cell cells]
		(print (print-symbol cell)))
		(println))

(defn print-grid [grid]
	(doseq [row grid]
		(print-row row)))

(defn get-cell [x y grid]
	(nth (nth grid x []) y 0))

(defn has-life [[x y] grid]
	(= 1 (get-cell x y grid)))

(defn has-life-cells [cells grid]
	(for [cell cells]
		(has-life cell grid)))

(defn get-neighbors-cord [x y grid]
	(list [(- x 1)(- y 1)], 
				[x (- y 1)], 
				[(+ x 1) (- y 1)], 
				[(- x 1) y], 
				[(+ x 1) y], 
				[(- x 1) (+ y 1)], 
				[x (+ y 1)], 
				[(+ x 1) (+ y 1)]))

(defn get-neighbors-values [x y grid]
	(map (fn [[x1 y1]] (get-cell x1 y1 grid)) (get-neighbors-cord x y grid)))

(defn has-num-alive-neighbors [neighbors & nums]
	(contains? (set nums) (reduce #(+ %1 %2) neighbors)))

(defn evolve-single-cell [x y old-grid]
	(let [neighbors-values (get-neighbors-values x y old-grid)]
	(if (has-life [x y] old-grid) (if (has-num-alive-neighbors neighbors-values 2 3) 1 0)
		(if (has-num-alive-neighbors neighbors-values 3) 1 0))))

(defn evolve [grid]
	(map-indexed (fn [i row] 
		(map-indexed (fn [j cell]
			(evolve-single-cell i j grid)) 
			row))
		grid))

(defn start-ticker [grid]
	(Thread/sleep 100)
	(print-grid grid)
	(recur (evolve grid)))

(defn init [pattern] 
	(println "Initializing game of life...")
	(agent (start-ticker pattern)))
