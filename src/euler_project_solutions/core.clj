(ns euler-project-solutions.core
  (:gen-class))

;; ? UTILS ----------------------------------------------------------------------------------------
(defn divisors
  [num]
  (filter #(= 0 (mod num %)) (range 2 num)))

(defn is-prime?
  [num]
  (= [1 num] (vec (divisors num))))

;; credit: example from https://clojuredocs.org/clojure.core/lazy-seq
(defn fib
    ([]
     (fib 1 1))
    ([a b]
     (lazy-seq (cons a (fib b (+ a b))))))

;; comments for testing utils via REPL
(comment
  (divisors 13195))

(comment
  (take 10 (fib))
  (take-while (partial > 1000) (fib)))

;; *** PROJECT EULER SOLUTIONS *** ----------------------------------------------------------------
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

(defn even-fibonacci
  "Find the sum of the even-valued terms in the Fibonacci sequence below the target."
  [target]
  (->> (fib)
       (take-while (partial > target))
       (filter even?)
       (reduce +)))

(defn largest-prime-factor
  "Find the largest prime factor of the given number."
  [num]
  (loop [to-process (divisors num)
         primes #{}]
    (let [new-primes (into primes (filter is-prime? to-process))
          new-to-process (->> to-process
                              (filter #(not (is-prime? %)))
                              (map divisors)
                              flatten)]
      (println [new-primes new-to-process])
      (if (seq new-to-process)
        (recur new-to-process new-primes)
        (apply max new-primes)))))

;; taking too long to run... more efficient solution?
(comment (largest-prime-factor 600851475143))

;; ! ----------------------------------------------------------------------------------------------
;; solve the selected problem
(defn solve-problem
  "Execute the function related to the given problem with any required input values."
  [solution]
  (let [solved (apply (:execute solution) (:input solution))
        problem-title (:title solution)
        input-vals (:input solution)]
  (println (str "\nProblem: " problem-title))
  (println (str "Input: " input-vals))
  (println (str "Solution: " solved))))

;; prompt all options available for the user to select
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

;; select and run a problem's solution
(defn select-problem
  "Prompt the user for the solution to run."
  [options]
  (prompt-menu (map (fn [opt] [(:problem-id opt) (:title opt)]) options))
  (let [selection (Integer/parseInt (read-line))]
    (first (filter (fn [opt] (= (:problem-id opt) selection)) options))))

;; menu select options
(def menu-options [{:problem-id 1
                    :title "Multiples of 3 or 5"
                    :input [1000]
                    :execute multiples-of-three-or-five}
                   {:problem-id 2
                    :title "Even Fibonacci Numbers"
                    :input [4000000]
                    :execute even-fibonacci}
                   {:problem-id 3
                    :title "Largest Prime Factor"
                    :input [600851475143]
                    :execute largest-prime-factor}])

;; ...Engage!
(defn -main
  "I don't do a whole lot ... yet."
  [& _]
    (try
      (let [selected (select-problem menu-options)]
        (solve-problem selected))
      (catch NullPointerException _ (println "Not a valid selection."))
      (catch NumberFormatException _ (println "Please make a selection by entering a valid number."))))
