(ns project.file_operations)

(defn parse
  "Convert csv file into rows of columns"
  [file]
  (map #(clojure.string/split % #",")
       (clojure.string/split (slurp file) #"\r\n")))


(def game-keys [:homeTeam :homeGoals :awayGoals :awayTeam :date])

(defn string->int
  "Transform string to integer"
  [value]
  (Integer. value))

(def conversions {:homeTeam  identity
                  :homeGoals string->int
                  :awayGoals string->int
                  :awayTeam  identity
                  :date      identity})

(defn convert [game-key value]
  ((get conversions game-key) value))

(defn add-labels
  "Add keys to values"
  [row]
  (map (fn [row-to-map]
         (reduce (fn [row-map [game-key value]] (assoc row-map game-key (convert game-key value)))
                 {}
                 (map vector game-keys row-to-map)))
       row))
(def prepared-data (add-labels (parse "data.csv")))