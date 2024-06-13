(ns euler-project-solutions.core
  (:gen-class))

(defn prompt-menu
  "Prompt the user for a Project Euler solution to run."
  [solutions]
  (println "Please select a solution to run.")
  (loop [to-print solutions]
    (let [[option-id option-desc] (first to-print)
          remaining (rest to-print)]
      (println (str "  [" option-id "] - " option-desc))
      (when (seq remaining)
        (recur remaining)))))

(comment
  (prompt-menu [[1 "Test Test"] [2 "Test 2"] [3 "Test 3"]]))

(defn -main
  "I don't do a whole lot ... yet."
  [& _]
  (let [menu-options [{:problem-id 1
                       :title "Multiples of 3 or 5"
                       :input 1000
                       :execute nil}]]
    menu-options))
