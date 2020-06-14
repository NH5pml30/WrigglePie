;; Review + delay

; Solution stub

; Scalars
(def s+ +)
(def s- -)
(def s* *)

(defn- mapv-bind-last [mapv-arg op & op-args]
  (mapv (fn [op-arg1] (apply op (concat [op-arg1] op-args))) mapv-arg))
(defn- mapv-bind-first [mapv-arg op & op-args]
  (mapv (fn [op-arg1] (apply op (concat op-args [op-arg1]))) mapv-arg))
(defn- v-all-eq [v]
  (let [x (first v)]
    (every? (fn [vi] (= vi x)) v)))
(defn- v-count-eq [& v]
  (v-all-eq (mapv count v)))

(defn- count-dims [depth t]
  {:pre [(if (vector? t) (or (> depth 0) (< depth -1)) (and (number? t) (or (= 0 depth) (< depth -1))))]}
  (if (vector? t)
    (conj (count-dims (dec depth) (first t)) (count t))
    []))

(defn- test-dims [dims args]
  (if (> (count dims) 0) (and (every? vector? args) (v-all-eq (conj (mapv count args) (last dims)))) (every? number? args)))

(defn- elwise-op [op dims args]
  {:pre [(test-dims dims args)]}
  (if (every? vector? args)
    (apply mapv (fn [& argsi] (elwise-op op (butlast dims) argsi)) args)
    (apply op args)))

(defn- verify [depth]
  (fn [& t]
    (elwise-op (fn [& _] _) (count-dims depth (first t)) t)
    true))

(defn- ify [depth]
  (fn [func]
    (fn [& args]
      (apply func (conj args depth)))))

(def ^:private v-ify (ify 1))
(def ^:private m-ify (ify 2))
(def ^:private t-ify (ify -2))

(def ^:private v-verify ((v-ify verify)))
(def ^:private m-verify ((m-ify verify)))
(def ^:private t-verify ((t-ify verify)))

(def ^:private v-count-dims (v-ify count-dims))
(def ^:private m-count-dims (m-ify count-dims))
(def ^:private t-count-dims (t-ify count-dims))

(defn- spec-elwise-op [depth op]
  (fn [& m] (elwise-op op (count-dims depth (first m)) m)))

(def ^:private v-elwise-op (v-ify spec-elwise-op))
(def ^:private m-elwise-op (m-ify spec-elwise-op))
(def ^:private t-elwise-op (t-ify spec-elwise-op))

(def v+ (v-elwise-op +))
(def v- (v-elwise-op -))
(def v* (v-elwise-op *))
(def m+ (m-elwise-op +))
(def m- (m-elwise-op -))
(def m* (m-elwise-op *))
(def t+ (t-elwise-op +))
(def t- (t-elwise-op -))
(def t* (t-elwise-op *))

(defn- contract-helper [t t-dims v v-dims]
  {:pre [(or (v-count-eq t-dims v-dims) (vector? t))]}
  (if (= (count t-dims) (count v-dims))
    (if (vector? v)
      (reduce + (t* t v))
      (* t v))
    (mapv-bind-last t contract-helper (butlast t-dims) v v-dims)))
(defn- contract [& args]
  (reduce (fn [t v] (contract-helper t (t-count-dims t) v (t-count-dims v))) args))

(defn scalar [v1 v2]
  (v-verify v1 v2)
  (contract v1 v2))
(let [
  vect-dirs [[1 2] [2 0] [0 1]]
  vect-comp (fn [v1, v2]
    (fn [[i1 i2]] (- (* (nth v1 i1) (nth v2 i2)) (* (nth v1 i2) (nth v2 i1)))))
  ]
  (defn- vect' [v1 v2]
    {:pre [(= (count v1) 3)]}
    (v-verify v1 v2)
    (mapv (vect-comp v1 v2) vect-dirs))
  (defn vect [& v] (reduce vect' v)))

(defn- elwise-scalar [ifier]
  (fn [t & s]
    {:pre [(every? number? s)]}
    (((ifier verify)) t)
    (apply contract t s)))

(def v*s (elwise-scalar v-ify))
(def m*s (elwise-scalar m-ify))

(defn m*v [m v]
  (m-verify m)
  (v-verify v)
  (contract m v))

(defn transpose [m]
  (m-verify m)
  (apply mapv vector m))

(defn m*m [m & tail]
  (m-verify m)
  (every? m-verify tail)
  (reduce
    (fn [m1 m2]
      (mapv-bind-first m1 mapv-bind-last (transpose m2) scalar))
    m tail))
