(ns bird-watcher)

(def last-week 
  [0 2 5 3 7 8 4]
  )

(def birds-per-day
  )

(defn today [birds]
  (if (empty? (rest birds))
    (first birds)
    (today (rest birds)))
  )

(defn inc-bird [birds]
  (conj (pop birds) (+ (today birds) 1))
  )

(defn day-without-birds? [birds]
  (if (empty? birds)
    false
    (if (= (first birds) 0)
      true
      (day-without-birds? (rest birds))))
  )

(defn n-days-count [birds n]
  (reduce + (take n birds))
  )

(defn busy-days [birds]
  (count (filter #(> % 4) birds))
  )

(defn odd-week? [birds]
  (= birds [1 0 1 0 1 0 1])
  )
