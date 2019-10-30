(ns rpn.core
  (:require [clojure.string :as str])
  (:gen-class))

(defn- is-op
  "Helper function to verify the token is a valid operator."
  [x]
  (re-matches #"^(\+|-|\*|/)$" x))

(defn- is-num
  "Helper function to verify the token is a valid number.
  Negative and floating point numbers are allowed."
  [x]
  (re-matches #"^-?\d+(\.\d+)?" x))

(defn- crash
  "Helper function to kill the program."
  [err exp]
  (throw (Exception. (str err " Expression '" exp "' is not parsable."))))

(defn- apply-op
  "Helper function to apply an operator to 2 numbers."
  [a b c]
  (if (= a "+")
    (+ c b)
    (if (= a "-")
      (- c b)
      (if (= a "*")
        (* c b)
        (if (= a "/")
          (/ c b)
          (throw (Exception. (str "Unkown operator '" a "'"))))))))

(defn- parse-tokens
  "Helper function for parsing an expression to a list of tokens.
  Take a space-separated string of values, and return a list of tokens. Each
  token is either a Float, or a String. If a value cannot be parsed (because
  it's neither a number, nor a valid operator), and exception is thrown."
  [exp]
  (map (fn [token]
         (if (is-op token) token
             (if (is-num token)
               (Float/parseFloat token)
               (crash (str "Couldn't validate token '" token "' as either a number or a valid operator.") exp))))
       (str/split exp #" ")))

(defn- compute
  "Perform the computation on a list of tokens.
  Take a list of tokens, and reduce it to -- hopefully -- a single Float. If the
  result is a list containing anything else than a single Float, the computation
  has failed, and the expression wasn't computable."
  [list exp]
  (if (>= (count list) 3)
    (let [[a b c & rest] list]
      (if (and (string? a) (number? b) (number? c))
        (let [result (apply-op a b c)
              return (conj rest result)]
          return)
        (if (and (string? a) (string? c) (>= (count rest) 2))
          (let [result (compute (conj rest c) exp)]
            (recur (concat [a b] result) exp))
          (if (and (string? a) (string? b) (>= (count rest) 1))
            (let [result (compute (concat [b c] rest) exp)]
              (recur (conj result a) exp))
            (crash (str "We don't know how to handle the remaining stack:" a ", " b ", " c) exp)))))
    list))

(defn calc
  "Interpet a Reverse Polish Notation mathematical expression.
  Reverse Polish Notation is a notation for mathematical expressions, that uses
  the concept of a \"stack\". Whenever an operator (+, -, *, or /) is encountered,
  the 2 next elements of the \"stack\" are retrieved, and the result is pushed
  back onto the stack. If one of those 2 previous elements is also an operator,
  we recursively treat it starting from there, all the way until we have the
  condition with an operator and 2 values.
  Example: '1 2 3 + /' = (/ (+ 3 2) 1) = 5"
  [exp]
  (let [list (parse-tokens exp)
        result (compute (reverse list) exp)]
    (if (= (count result) 1)
      (let [n (first result)]
        (if (number? n)
          n
          (crash (str "Result '" n "' is not a number.") exp)))
      (crash (str "Result '" result "' doesn't contain exactly 1 value.") exp))))

(defn -main
  [exp]
  (println (calc exp)))