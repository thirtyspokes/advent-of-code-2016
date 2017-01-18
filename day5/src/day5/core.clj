(ns day5.core
  (:require [digest :as d]))

(defn valid?
  [hash]
  (= '(\0 \0 \0 \0 \0) (take 5 hash)))

(defn md5
  [id index]
  (d/md5 (str id index)))

(defn make-password
  [input-map]
  (apply str (map second (sort input-map))))

(defn add-new-password-char
  [chars hash]
  (let [position (nth hash 5)
        character (nth hash 6)
        digit (Character/digit position 10)]
    (if (and (not= -1 digit) (< digit 8))
      (if (get chars digit)
        chars
        (assoc chars digit character))
      chars)))

(defn find-password-1
  [password]
  (loop [idx 0 chars []]
    (if (= 8 (count chars))
      (apply str (reverse chars))
      (let [hash (md5 password idx)]
        (if (valid? hash)
          (recur (inc idx) (cons (nth hash 5) chars))
          (recur (inc idx) chars))))))

(defn find-password-2
  [password]
  (loop [idx 0 chars {}]
    (if (= 8 (count chars))
      (make-password chars)
      (let [hash (md5 password idx)]
        (if (valid? hash)
          (recur (inc idx) (add-new-password-char chars hash))
          (recur (inc idx) chars))))))
