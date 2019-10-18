(ns rpn.core
  (:require [clojure.string :as str])
  (:use [clojure.tools.logging :as log])
  (:gen-class))

(defn is-op [x]
  (re-matches #"^(\+|-|\*|/)$" x))
(defn is-num [x]
  (re-matches #"^-?\d+(\.\d+)?" x))
(defn crash [err exp]
  (throw (Exception. (str err " Expression '" exp "' is not parsable."))))

(defn apply-op
  ""
  [a b c]
  (if (= a "+") (+ b c)
      (if (= a "-") (- b c)
          (if (= a "*") (* b c) (/ b c)))))

(defn parse-tokens
  ""
  [exp]
  (map (fn [token]
         (if (is-op token) token
             (if (is-num token)
               (Float/parseFloat token)
               (crash exp))))
       (str/split exp #" ")))

(defn compute
  ""
  [list]
  (if (>= (count list) 3)
    (let [[a b c & rest] list]
      (log/debug "...treat args " a ", " b ", and " c)
      (if (and (string? a) (number? b) (number? c))
        (do
          (log/debug "-> We can compute " b a c)
          (let [result (apply-op a b c)
                return (conj rest result)]
            (log/debug "Result of '" b a c "' = " result)
            (log/debug "Return with rest:" return)
            return))
        (if (and (string? a) (string? c))
          (do
            (log/debug "-> We cannot compute " b a c ", recurse at C:" c)
            (let [result (compute (conj rest c))]
              (recur (concat [a b] result))))
          (if (and (string? a) (string? b))
            (do
              (log/debug "-> We cannot compute " b a c ", recurse at B: " b)
              (let [result (compute (concat [b c] rest))]
                (recur (conj result a))))))))

    (do
      (log/debug "List is no longer treatable, return as-is")
      list)))

(defn calc
  "Coming soon"
  [exp]
  (log/debug "Start parsing expression " (reverse (parse-tokens exp)))
  (let [list (parse-tokens exp)
        result (compute (reverse list))]
    (if (= (count result) 1)
      (let [n (first result)]
        (if (number? n)
          n
          (crash (str "Result '" n "' is not a number.") exp)))
      (crash (str "Result '" result "' doesn't contain exactly 1 value.") exp))))

(defn -main
  "I don't do a whole lot ... yet."
  [exp]
  (println (calc exp)))