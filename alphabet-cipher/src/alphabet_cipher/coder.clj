(ns alphabet-cipher.coder
  (:require [clojure.string :as str]))

(def alphabet "abcdefghijklmnopqrstuvwxyz")

(defn char->index [char]
  (str/index-of alphabet char))

(defn extend-keyword [keyword message]
  (take (count message) (cycle keyword)))

(defn shrink-keyword [extended-keyword index]
  (if (= (extend-keyword (take index extended-keyword) extended-keyword) extended-keyword)
    (take index extended-keyword)
    (shrink-keyword extended-keyword (inc index))))

(defn rotate-alphabet [steps]
  (drop steps (cycle alphabet)))

(defn encode-char [char-index keyword-char-index]
  (nth (rotate-alphabet char-index) keyword-char-index))

(defn decode-char [char-index keyword-char-index]
  (first (rotate-alphabet (- (count alphabet) (- keyword-char-index char-index)))))

(defn decipher-char [char-index cipher-char-index]
  (first (rotate-alphabet (+ (count alphabet) (- cipher-char-index char-index)))))

(defn encode [keyword message]
  (let [message-indices (map char->index message)
        extended-keyword-indices (map char->index (extend-keyword keyword message))]
    (str/join (map encode-char message-indices extended-keyword-indices))))

(defn decode [keyword message]
  (let [message-indices (map char->index message)
        extended-keyword-indices (map char->index (extend-keyword keyword message))]
    (str/join (map decode-char message-indices extended-keyword-indices))))

(defn decipher [cipher message]
  (let [message-indices (map char->index message)
        cipher-indices (map char->index cipher)]
    (str/join (shrink-keyword (map decipher-char message-indices cipher-indices) 1))))

