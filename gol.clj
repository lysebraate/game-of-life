(load-file "./patterns.clj")

(defn to-bool [grid]
	(vec (for [rows grid]
		(vec (for [cell rows]
			(if (= cell 1) true false))))))	

(defn print-symbol [cell]
	(if (true? cell) "#" " "))

(defn print-row [cells]
	(print "\r") 
	(doseq [cell cells]
		(print (print-symbol cell)))
		(println))

(defn print-grid [grid]
	(doseq [row grid]
		(print-row row)))

(defn get-cell [x y grid]
	(get (get grid x) y))

(defn has-life [[x y] grid]
	(true? (get-cell x y grid)))

(defn has-life-cells [cells grid]
	(for [cell cells]
		(has-life cell grid)))

(defn get-neighbors [x y grid]
	(list [(- x 1)(- y 1)], 
				[x (- y 1)], 
				[(+ x 1) (- y 1)], 
				[(- x 1) y], 
				[(+ x 1) y], 
				[(- x 1) (+ y 1)], 
				[x (+ y 1)], 
				[(+ x 1) (+ y 1)]))

(defn has-num-alive-neighbors [neighbors num]
	(= (count (filter true? neighbors)) num))

(defn has-exactly-three-living-neighbors [x y grid]
	(let [neighbors (has-life-cells (get-neighbors x y grid) grid)]
		(has-num-alive-neighbors neighbors 3)))

(defn has-two-or-three-neighbors [x y grid]
	(let [neighbors (has-life-cells (get-neighbors x y grid) grid)]
		(or (has-num-alive-neighbors neighbors 2) (has-num-alive-neighbors neighbors 3))))

(defn evolve-single-cell [[x y] old-grid]
	(cond
		(has-life [x y] old-grid) (has-two-or-three-neighbors x y old-grid)
		:default (has-exactly-three-living-neighbors x y old-grid)))

(defn evolve [grid]
	(vec (take (count grid) (for [i (iterate inc 0)] 
		(vec (take (count (get grid 0)) (for [j (iterate inc 0)] 
			(evolve-single-cell [i j] grid))))))))

(defn start-ticker [grid]
	(Thread/sleep 100)
	(print-grid grid)
	(recur (evolve grid)))

(defn initialize [pattern] 
	(println "Initializing game of life...")
	(agent (start-ticker (to-bool pattern))))
