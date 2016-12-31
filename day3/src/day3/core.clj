(ns day3.core
  (:require [clojure.math.combinatorics :as c]
            [clojure.string :as s]))

(defn get-partitions
  "Given a triangle represented by a sequence of three integers
   representing sides a, b, and c, returns all of the possible 
   partitions of (a, b), (c)."
  [sides]
  (map #(sort-by count %) (c/partitions sides :min 2 :max 2)))

(defn possible?
  "Determines whether an individual partition of triangle sides
   (a, and then b + c) results in a valid triangle."
  [sides]
  (let [a (ffirst sides)
        bc (apply + (second sides))]
    (> bc a)))

(defn valid-triangle?
  "A triangle is considered valid if every combination
   of two sides added together is greater than the remaining
   side."
  [sides]
  (let [combos (get-partitions sides)]
    (every? possible? combos)))

(defn count-valid-triangles
  "Counts the number of valid triangles in a sequence of 
   triangles."
  [triangles]
  (count
    (filter valid-triangle? triangles)))

(defn lines-to-triangles
  "Given sequence of strings, each of which is a set of 
   three numbers separated by whitespace, transforms each element
   into a sequence of three integers."
  [lines]
  (map (fn [line]
         (map #(Integer/parseInt %)
           (remove empty? (s/split line #" "))))
    lines))

(defn columns-to-triangles
  "Gathers the input numbers into groups of three column-wise,
   instead of row-wise."
  [lines]
  (let [col1 (map first lines)
        col2 (map second lines) ()
        col3 (map last lines)]
    (partition 3 (concat col1 col2 col3))))

(defn parse-input-file
  [path]
  (-> path
    slurp
    (s/split #"\n")
    lines-to-triangles))
