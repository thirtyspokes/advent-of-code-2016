(ns day2.core
  (:require [clojure.string :as s]))

(def grid
  [[1 2 3]
   [4 5 6]
   [7 8 9]])

(def grid-two
  [["-" "-"  1  "-" "-"]
   ["-"  2   3   4  "-"]
   [ 5   6   7   8   9 ]
   ["-" "A" "B" "C" "-"]
   ["-" "-" "D" "-" "-"]])

(defn to-coordinates
  "Transforms a single letter representing a directional
   move into a vector. Somewhat counter-intuitively, up
   and down are - and + on the y-axis respectively, because of
   the way the grid is represented (the \"top\" of the grid is 
   the first item in the vector of vectors)."
  [direction]
  (case direction
    "U" [0 -1]
    "D" [0 1]
    "L" [-1 0]
    "R" [1 0]))

(defn to-value
  "Given a coordinate pair and a grid, returns the value
   at that location in the grid."
  [grid coordinates]
  (let [[x y] coordinates]
    (nth (nth grid y) x)))

(defn within-bounds?
  "Determines `new-pos` is a valid location in
   `grid`.  To be valid, it must not be out of the
   boundaries of the grid and must not contain an
   \"illegal value\" (in this case -)."
  [grid new-pos]
  (let [x-bound (count (first grid))
        y-bound (count grid)
        [new-x new-y] new-pos]
    (and
      (and (>= new-x 0) (< new-x x-bound))
      (and (>= new-y 0) (< new-y y-bound))
      (not= (to-value grid new-pos) "-"))))

(defn move-on-keypad
  "Adds the vector represented in `step` to the 
   location represented by `current` on `grid`.  If the
   result is a valid move, returns the result.  Otherwise,
   returns the current location."
  [grid current step]
  (let [new-pos (map + current step)]
    (if (within-bounds? grid new-pos)
      (vec new-pos)
      current)))

(defn walk-keypad
  "Given a grid representation of the keypad, a starting point,
   and a series of instructions, returns the final stopping point on
   the keypad resulting from following all the directions from `start`."
  [pad start instructions]
  (reduce (partial move-on-keypad pad)
    start
    instructions))

(defn walk-instructions
  "Given a keypad grid, a starting point, and a collection of instruction
   sequences, walks the instructions with `walk-keypad`, and uses the result
   as the starting point for the next line, until all lines have been
   traversed."
  [pad start lines]
  (loop [res []
         instr lines
         coords start]
    (if (empty? instr)
      res
      (let [new-loc (walk-keypad pad coords (first instr))]
        (recur (conj res new-loc) (rest instr) new-loc)))))

(defn make-instructions
  [lines]
  (map #(map to-coordinates (s/split % #"")) lines))

(defn read-input
  [path]
  (-> path
    slurp
    (s/split #"\n")
    make-instructions))
