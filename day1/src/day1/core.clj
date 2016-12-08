(ns day1.core
  (:require [clojure.string :as s]))

(defn new-heading
  "Given a current heading (N,S,E, or W) and a direction
   to turn (left or right), returns the new heading that would
   be faced by making a 90-degree turn in the specificed direction."
  [current direction]
  (let [headings {:north [:west :east]
                  :south [:east :west]
                  :east  [:north :south]
                  :west  [:south :north]}
        [left right] (current headings)]
    (if (= direction :left)
      left
      right)))

(defn forward
  "Given a heading, a start location, and an amount of blocks to move,
   returns the new location arrived at by treating N,S,E,W as the axes
   of a cartesian grid, where N is +y, S is -y, E is +x, and W is -x."
  [heading start amount]
  (let [[x y] start]
    (cond
      (= :north heading) [x (+ y amount)]
      (= :south heading) [x (- y amount)]
      (= :east heading)  [(+ x amount) y]
      (= :west heading)  [(- x amount) y]
      :otherwise (throw "This is fine."))))

(defn move
  "Given a current location (a vector of a heading followed by
   the current cartesian coordinates) and an instruction (a vector
   of a direction to turn followed by a distance in blocks to move),
   returns the new location arrived at by turning and moving per the
   instruction."
  [location instr]
  (let [[facing coords] location
        [turn dist] instr
        heading (new-heading facing turn)]
    [heading (forward heading coords dist)]))

(defn move-visits
  "Like move, except tracks which blocks have been previously visited in the process
   of following an instruction.  If a block has been visited previously, then returns
   that location."
  [location instr]
  (let [[facing coords visited] location
        [turn dist] instr
        heading (new-heading facing turn)]
    (loop [n 1 v visited loc (forward heading coords 1)]
      (cond
        (v loc) (reduced [heading loc v])
        (= n dist) [heading loc v]
        :else (let [new (forward heading loc 1)]
                (recur (inc n) (conj v loc) new))))))

(defn to-instruction
  "An instruction is a direction to turn (either :left or :right) and a
   number of blocks to move after the turn, as an int.  Give a step like
   R3 or L10, creates an instruction."
  [step]
  (let [directions {"L" :left "R" :right}]
    [(get directions (str (first step)))
     (Integer/parseInt (apply str (drop 1 step)))]))

(defn parse-input-file
  "Turns an input file of (a CSV of raw steps) into a seq of steps."
  [path]
  (let [raw-input (slurp path)
        make-instr (comp to-instruction s/trim)]
    (map make-instr (s/split raw-input #","))))

(defn solution-part-one
  [path-to-input]
  (let [steps (parse-input-file path-to-input)
        final-loc (reduce move [:north [0 0]] steps)
        [_ [x y]] final-loc]
    (+ (Math/abs x) (Math/abs y))))

(defn solution-part-two
  [path-to-input]
  (let [steps (parse-input-file path-to-input)
        final-loc (reduce move-visits [:north [0 0] #{}] steps)
        [_ [x y] _] final-loc]
    (+ (Math/abs x) (Math/abs y))))
