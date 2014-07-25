(ns looping-is-recursion)

(defn power [base exp]
  (let [helper (fn [res exp]
                 (if  (zero? exp)
                   res
                   (recur (* res base) (dec exp))))]
    (helper 1 exp)))

(defn last-element [a-seq]
  (let [rest-a-seq (rest a-seq)]
    (cond
     (empty? a-seq) nil
     (empty? rest-a-seq) (first a-seq)
     :else (recur rest-a-seq))))

(defn seq= [seq1 seq2]
  (cond
   (and (empty? seq1) (empty? seq2)) true
   (or (empty? seq1) (empty? seq2)) false
   (= (first seq1) (first seq2)) (recur (rest seq1) (rest seq2))
   :else false))

(defn find-first-index [pred a-seq]
  (loop [seq a-seq
         n 0]
    (if (empty? seq)
      nil
      (if (pred (first seq))
        n
        (recur (rest seq) (inc n))))))

(defn avg [a-seq]
  (loop [seq a-seq
         sum 0
         count 0]
    (cond
     (and (empty? seq) (zero? count)) nil
     (empty? seq) (/ sum count)
     :else (recur (rest seq) (+ sum (first seq)) (inc count)))))

(defn parity [a-seq]
  (let [toggle (fn [set item]
                 (if (contains? set item)
                   (disj set item)
                   (conj set item)))]
    (loop [seq a-seq
           odd-elems #{}]
      (cond
       (empty? seq) odd-elems
       :else (recur (rest seq) (toggle odd-elems (first seq)))))))

(defn fast-fibo [n]
  (loop [n-1 1
         n-2 0
         counter n]
    (if (zero? counter)
      n-2
      (recur n-2 (+ n-1 n-2) (dec counter)))))

(defn cut-at-repetition [a-seq]
  (loop [seen #{}
         seq a-seq
         so-far []]
    (if (or (empty? seq) (contains? seen (first seq)))
      so-far
      (recur (conj seen (first seq))
             (rest seq)
             (conj so-far (first seq))))))

