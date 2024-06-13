(ns euler-project-solutions.core
  (:gen-class))

(defn multiples-of-three-or-five
  "Find the sum of all multiples of three or five below the target."
  [target]
  (let [mult-of-num (fn [num multiple] (= (mod num multiple) 0))]
    (loop [current-num 0
           sum 0]
      (if (>= current-num target) 
        sum
        (recur (inc current-num)
               (if (or (mult-of-num current-num 3) (mult-of-num current-num 5))
                 (+ sum current-num)
                 sum))))))

(defn prompt-menu
  "Print the menu of available solutions to run."
  [solutions]
  (println)
  (println "Please select a solution to run.")
  (loop [to-print solutions]
    (let [[option-id option-desc] (first to-print)
          remaining (rest to-print)]
      (println (str "  [" option-id "] - " option-desc))
      (when (seq remaining)
        (recur remaining))))
  (println))

(defn select-problem
  "Prompt the user for the solution to run."
  [options]
  (prompt-menu (map (fn [opt] [(:problem-id opt) (:title opt)]) options))
  (let [selection (Integer/parseInt (read-line))
        selected (first (filter (fn [opt] (= (:problem-id opt) selection)) options))]
    (apply (:execute selected) (:input selected))))

(def menu-options [{:problem-id 1
                    :title "Multiples of 3 or 5"
                    :input [1000]
                    :execute multiples-of-three-or-five}])

(defn -main
  "I don't do a whole lot ... yet."
  [& _]
    (try
      (let [solution (select-problem menu-options)]
        (println (str "Solution: " solution)))
      (catch NullPointerException _ (println "Not a valid selection."))
      (catch NumberFormatException _ (println "Please make a selection by entering a valid number."))))
