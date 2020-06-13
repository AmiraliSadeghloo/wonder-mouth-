(ns wonder-mouth.core
  (:gen-class))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))
;;/////////////////////////////////
(defn primeMod
  [x]
  (loop [n x counted 0]
    (if (< n 1)
      counted
      (if (= 0 (mod x n))
        (recur (dec n) (inc counted))
        (recur (dec n) counted)))))
;;-------
(defn prime?                                                ;;true?
  [x]
  (true? (= 2 (primeMod x))
         ))

;;////////////////////////////////
(def fibLazy                                                ;;returns lazy seq of all fibo numbers
  ((fn fib [a b]
     (lazy-seq (cons a (fib b (+ a b)))))
   0 1))
;;-------
(def fiboNums (into [] (take 25 fibLazy)))                  ;; vector of first 25 fibo numbers
;;-------
(defn fibo?                                                 ;; check if fibo in first 25
  [x]
  (loop [i 0]
    (if (= x (get fiboNums i))
      true
      (if (= 25 i)
        false
        (recur (inc i))))))

;;//////////////////////////////
(defn chenMod                                               ;;returns [[modList] modCount] of x+2
  [x]
  (loop [n (+ x 2) mods [] countMod 0]
    (if (> 1 n)
      [mods countMod]
      (if (= 0 (mod (+ 2 x) n))
        (recur (dec n) (conj mods n) (inc countMod))
        (recur (dec n) mods countMod)))))

;;--------------
(defn chen?                                                 ;; Checks if chen (p prime, p+2 prime or semi-prime)
  [x]
  (true? (or (and (prime? x) (prime? (+ x 2)))
             (and (prime? x)
                  (or (and (prime? (nth (first (chenMod x)) 1))
                           (= 3 (last (chenMod x))))
                      (and (prime? (nth (first (chenMod x)) 1))
                           (prime? (nth (first (chenMod x)) 2))
                           (= 4 (last (chenMod x)))))))))

;;/////////////////////////////////
(defn wonder_mouth                                          ;;creates output string
  [input]
  (loop [n input output ""]
    (if (= 0 n)                                             ;;reduce
      output
      (if (and (fibo? n) (chen? n))
        (recur (dec n) (str "OO" output))
        (if (and (fibo? n) (prime? n))
          (recur (dec n) (str "Oo" output))
          (if (prime? n)
            (recur (dec n) (str "o" output))
            (if (fibo? n)
              (recur (dec n) (str "O" output))
              (recur (dec n) output))))))))

(wonder_mouth 10)

