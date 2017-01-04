(ns day4.core
  (:require [clojure.core.reducers :as r]
            [tesser.core :as t]))

(defn clean-input
  "Removes dashes from the room name."
  [raw-string]
  (apply str (remove #(= \- %) raw-string)))

(defn extract-name
  "Extracts the room's name from the raw string."
  [input]
  (first (clojure.string/split (clean-input input) #"[0-9]")))

(defn extract-checksum
  "Extracts the room checksum from the raw string."
  [input]
  (last (re-find #"\[(.*?)\]" input)))

(defn extract-room-code
  "Extracts the sector ID from the raw string."
  [input]
  (Integer/parseInt (last (re-find #"\-([0-9]*?)\[" input))))

(defn sorted-freqs
  "Computes the checksum for a room's :name.  A checksum is
   computed by taking the top five most common letters in the
   name, ordered by frequency, with ties resolved by alphabetical
   ordering from A to Z."
  [input]
  (into
    (sorted-map-by
      (fn [x y]
        (let [freqs (frequencies input)
              c (compare (get freqs x) (get freqs y))]
          (if (= 0 c)
            (compare x y)
            (- c)))))
    (frequencies input)))

(defn valid-checksum?
  "Validates whether the listed checksum for a room indicates
   the room is real."
  [name orig-checksum]
  (let [checksum (sorted-freqs name)]
    (= orig-checksum (apply str (take 5 (keys checksum))))))

(defn parse-encrypted-room
  "Given a raw input string from part one, parses the individual
   components and returns a map representing the room."
  [input]
  (let [name (extract-name input)
        checksum (extract-checksum input)]
    {:room-code (extract-room-code input)
     :checksum checksum
     :name name
     :real? (valid-checksum? name checksum)}))

(defn read-input
  "Reads the input file and splits it on newlines."
  [path]
  (-> path
    slurp
    (clojure.string/split #"\n")))

(defn shift-letter
  "Shifts a letter (a char) by the number indicated in
   amount.  Assumes the input is the set of capital letters,
   chars 65 through 90."
  [amount input]
  (let [int-value (int input)
        mod-amount (mod amount 26)
        new-val (+ int-value mod-amount)]
    (if (< 90 new-val)
      (char (+ 64 (- new-val 90)))
      (char new-val))))

(defn shift-decode
  "Given a parsed room, performs a shift cipher by shifting each letter
   in :name a number of times equal to :room-code (the sector ID)."
  [room]
  (let [name (:name room)
        amount (:room-code room)]
    (assoc room :decoded (apply str (map (partial shift-letter amount) (clojure.string/upper-case name))))))

(defn write-output
  "Write the translated rooms for part II to a file."
  [rooms path]
  (let [translated (map shift-decode rooms)
        output (map #(str (:decoded %) " - " (:room-code %)) translated)]
    (spit path (clojure.string/join "\n" output))))

(defn solve
  "Solve part one."
  [lines]
  (reduce + (map :room-code (filter :real? (map parse-encrypted-room lines)))))

(defn solve-reducer
  "Solve part one with reducers."
  [lines]
  (r/fold + (r/map :room-code (r/filter :real? (r/map parse-encrypted-room lines)))))

(defn solve-transducer
  "Solve part one using transducers."
  [lines]
  (let [xf (comp
             (map parse-encrypted-room)
             (filter :real?)
             (map :room-code))]
    (transduce xf + lines)))

(defn solve-tesser
  "Solve part one using the tesser library."
  [lines]
  (->> (t/map parse-encrypted-room)
    (t/filter :real?)
    (t/map :room-code)
    (t/fold +)
    (t/tesser (t/chunk 2048 lines))))
