(ns euler-project-solutions.core
  (:gen-class))

(defn multiples-of-three-or-five
  [num]
  num)

(defn prompt-menu
  "Print the menu of available solutions to run."
  [solutions]
  (println "Please select a solution to run.")
  (loop [to-print solutions]
    (let [[option-id option-desc] (first to-print)
          remaining (rest to-print)]
      (println (str "  [" option-id "] - " option-desc))
      (when (seq remaining)
        (recur remaining)))))

(defn select-problem
  "Prompt the user for the solution to run."
  [options]
  (prompt-menu (map (fn [opt] [(:problem-id opt) (:title opt)]) options))
  (print " >> ")
  (let [selection (Integer/parseInt (read-line))
        selected (first (filter (fn [opt] (= (:problem-id opt) selection)) options))]
    (apply (:execute selected) (:input selected))))

(defn -main
  "I don't do a whole lot ... yet."
  [& _]
  (let [menu-options [{:problem-id 1
                       :title "Multiples of 3 or 5"
                       :input [1000]
                       :execute multiples-of-three-or-five}]]
    (select-problem menu-options)))
