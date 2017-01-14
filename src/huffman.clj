(ns huffman)

(defn- freq
  [[key val]]
  val)

(defn- sym
  [[key val]]
  key)

(defn- merge-nodes
  [l r]
  (vector `(~(sym l) ~(sym r)) (+ (freq l) (freq r)))
  )

(defn- freq-sort
  [freqs]
  (sort #(< (freq %1) (freq %2)) freqs))

(defn- make-tree
  "Make a huffman encoding tree from a given sequence"
  [sequence]
  (loop [freqs (freq-sort (frequencies sequence))]
    (if (= 1 (count freqs))
      (sym (first freqs))
      (recur
       (freq-sort ;; better to use a priority queue / heap
        (cons
         (merge-nodes (first freqs) (second freqs))
         (drop 2 freqs)))))))

(defn- encoding-map
  [[l r :as b-tree] prefix]
  (merge
   (if (seq? l)
     (encoding-map l (str prefix 0))
     {l (str prefix 0)})
   (if (seq? r)
     (encoding-map r (str prefix 1))
     {r (str prefix 1)})))

(defn huffman-encode
  [sequence]
  (let [encoding (encoding-map (make-tree sequence) "")]
    {:encoded (pmap encoding sequence),
     :code encoding}))





