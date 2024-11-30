(ns euler-project-solutions.core
  (:gen-class) 
  (:require
   [clojure.set :as set]))

;; ? UTILS ----------------------------------------------------------------------------------------
(defn divisors
  [num]
  (filter #(= 0 (mod num %)) (range 2 num)))

(defn is-prime?
  [num]
  (= [1 num] (vec (divisors num))))

(defn is-divisor?
  [a b]
  (zero? (mod a b)))

;; credit: example from https://clojuredocs.org/clojure.core/lazy-seq
(defn fib
    ([]
     (fib 1 1))
    ([a b]
     (lazy-seq (cons a (fib b (+ a b))))))

(defn greatest-common-divisor
  [a b]
  (let [a-divisors (->> a divisors (into #{}))
        b-divisors (->> b divisors (into #{}))
        common-divisors (set/intersection a-divisors b-divisors)]
    (->> common-divisors
         (into [])
         (apply max))))

;; comments for testing utils via REPL
(comment
  (divisors 13195))

(comment
  (greatest-common-divisor 21 6))

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
  (letfn [(smallest-factor [n]
            (loop [i 2]
              (cond
                (> (* i i) n) n ; If no smaller factor found, n is prime
                (zero? (mod n i)) i
                :else (recur (inc i)))))]
    (loop [n num
           largest 0]
      (let [factor (smallest-factor n)]
        (if (= factor n)
          (max largest n) ; If n is prime, it's the largest factor
          (recur (/ n factor) (max largest factor)))))))

(defn largest-palindrome-product
  "Find the largest palindrome made from the product of two 3-digit numbers."
  []
  (let [products (for [x (range 100 1000) y (range 100 1000)]
                   (* x y))]
    (->> products
         (filter #(= (str %) (apply str (reverse (str %)))))
         (apply max))))

(defn smallest-multiple
  "Find the smallest number divisible by all numbers in the given range."
  [start end]
  ;; Brute-force solution, will revisit later
  (let [to-check (range start (inc end))]
    (loop [current end]
      (if-not (= (count to-check) (count (filter (partial is-divisor? current) to-check)))
        (recur (inc current))
        current))))

(comment
  (smallest-multiple 1 10))

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
                    :execute largest-prime-factor}
                   {:problem-id 4
                    :title "Largest Palindrome Product"
                    :input []
                    :execute largest-palindrome-product}
                   {:problem-id 5
                    :title "Smallest Multiple"
                    :input [1 20]
                    :execute smallest-multiple}])

;; ...Engage!
(defn -main
  "I don't do a whole lot ... yet."
  [& _]
    (try
      (let [selected (select-problem menu-options)]
        (solve-problem selected))
      (catch NullPointerException _ (println "Not a valid selection."))
      (catch NumberFormatException _ (println "Please make a selection by entering a valid number."))))
