(ns infix-postfix-prefix.util)

(defn every?*
  "Like `every?` but taking multiple collections. If some coll has different
  length, returns `false`."
  [pred & colls]
  (and (apply = (map count colls))
       (every? #(apply pred %) (apply map list colls))))

(defn map-partial=
  "Compares keys only in `want-map`."
  [want-map got-map]
  (every? (fn [[k v]] (= v (get got-map k))) want-map))

(defn zip [& colls] (apply map list colls))
(defn unzip [colls] (if (seq colls) (apply zip colls) '()))
;; > (zip [1 2 3] [:a :b :c] [10 20])
;; ((1 :a 10) (2 :b 20))
;; > (unzip [[1 :a 10] [2 :b 20] [3 :c]])
;; ((1 2 3) (:a :b :c))

(defn with-index [coll] (map-indexed list coll))
;; > (with-index [:a :b :c])
;; ((0 :a) (1 :b) (2 :c))

;; using core.match may be better?
(defmacro map-result [[bind result] ok error]
  `(let [~bind ~result]
     (case (first ~bind)
       :ok    ~ok
       :error ~error)))

(defmacro map-result-ok [[bind result] ok]
  `(map-result [~bind ~result] ~ok ~bind))

(defmacro map-result-error [[bind result] error]
  `(map-result [~bind ~result] ~bind ~error))

(defn put-new
  ([m k v] (if (contains? m k) m (assoc m k v)))
  ([m k v & kvs]
   (let [m (put-new m k v)] (apply put-new m kvs))))
;; > (util/put-new {:c -1} :a 11 :b 27 :c 6 :a 21)
;; {:c -1, :a 11, :b 27}

(defmacro if-first [[x xs] then else]
  `(let [coll# ~xs]
     (if (seq coll#)
       (let [~x (first coll#)] ~then)
       ~else)))
