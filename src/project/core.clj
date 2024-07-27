(ns project.core
  (:require [project.calculate-result :as result]
            [project.file_operations :as data]))

(defn menu
  "Start menu"
  []
  (println "")
  (println "Welcome!")
  (println "Choose an option:")
  (println "1. Predict winner of the game!")
  (println "2. Show result probabilities for game")
  (println "0. Exit")
  (print "Enter your choice: ")
  (flush))

(defn get-choice
  "Read user choice"
  []
  (read-line))

(defn prompt-user
  "Displays the given prompt to the user and reads input from the console."
  [prompt]
  (println prompt)
  (read-line))


(defn show-win-probabilities
  [data]
  (let [home-team (prompt-user "Enter home team: ")
        away-team (prompt-user "Enter away team: ")]
    (println (result/calculate-win-probabilities home-team away-team data))))

(defn show-results-probabilities
  [data]
  (let [home-team (prompt-user "Enter home team: ")
        away-team (prompt-user "Enter away team: ")]
    (run! println (result/show-result-probabilities home-team away-team data))))
(def data data/prepared-data)
(defn process-user-choice
  "Process users choice."
  [choice]
  (cond
    (= choice "1") (do
                     (show-win-probabilities data)
                     (menu)
                     (process-user-choice (get-choice)))
    (= choice "2") (do
                     (show-results-probabilities data)
                     (menu)
                     (process-user-choice (get-choice)))
    (= choice "0") (do
                     (println "Exiting...")
                     (System/exit 0))
    :else (do
            (println "Invalid choice. Please enter a valid option.")
            (menu)
            (process-user-choice (get-choice)))
    ))

(defn -main
  "Main function that allows usage of software"
  []
  (menu)
  (process-user-choice (get-choice)))