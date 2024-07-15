(ns project.util
  (:require [clojure.math :as math]))

(defn factorial [n]
  "Calculate factorial of passed argument"
  (reduce * (range 1 (inc n))))

(defn round2
  "Round a double to the given precision (number of significant digits)"
  [precision d]
  (let [factor (Math/pow 10 precision)]
    (/ (Math/round (* d factor)) factor)))

(defn poisson-distribution
  "Apply Poisson's distribution with passed arguments for expected goals and amount of goals"
  [expected-goals k]
  (* (/ (Math/pow expected-goals k) (factorial k)) (Math/pow math/E (- expected-goals))))