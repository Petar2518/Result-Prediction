(ns project.util_test
  (:require [midje.sweet :refer :all]
            [project.util :as util]))


(fact "Testing factorial function"
      (util/factorial 3) => 6)
(fact "Testing factorial function"
      (util/factorial 4) => 24)
(fact "Testing factorial function"
      (util/factorial 5) => 120)

(fact "Testing rounding function"
      (util/round2 2 10.555) => 10.56)
(fact "Testing rounding function"
      (util/round2 0 10.555) => 11.0)
(fact "Testing rounding function"
      (util/round2 2 10.551) => 10.55)

(fact "Testing Poisson distribution"
      (util/poisson-distribution 0 0) => 1.0)
(fact "Testing Poisson distribution"
      (util/round2 2 (util/poisson-distribution 1 0)) => 0.37)
(fact "Testing Poisson distribution"
      (util/round2 2 (util/poisson-distribution 1 1)) => 0.37)
(fact "Testing Poisson distribution"
      (util/round2 2 (util/poisson-distribution 1 2)) => 0.18)
(fact "Testing Poisson distribution"
      (util/round2 2 (util/poisson-distribution 1 3)) => 0.06)
