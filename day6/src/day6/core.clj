(ns day6.core)

(defn transpose-lines
  "Given a seq of seqs of equal length, returns
   a seq of seqs where the first is the first item of every
   seq, the second contains the second item of every
   seq in input, and so on."
  [input]
  (map (fn [index]
         (map (fn [line] (nth line index)) input))
    (range (count (first input)))))

(defn most-common
  "Returns the most frequently-occurring member of `input`."
  [input]
  (let [freqs (frequencies input)]
    (ffirst (sort-by val > freqs))))

(defn least-common
  "Returns the least frequently-occurring member of `input`."
  [input]
  (let [freqs (frequencies input)]
    (ffirst (sort-by val < freqs))))

(defn solve
  [path func]
  (as-> (slurp path) input
    (clojure.string/split input #"\n")
    (transpose-lines input)
    (map func input)
    (apply str input)))

(defn solve-part-one
  [path]
  (solve path most-common))

(defn solve-part-two
  [path]
  (solve path least-common))
