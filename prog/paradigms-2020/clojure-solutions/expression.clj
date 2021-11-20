;; 11: Review
;; 12: Review

; hw 10
(require '[clojure.string :as string])
(comment ":NOTE: explicit pass of `v` that can be removed")
(comment ":NOTE: parsing with regular expression with weak groups is not good")
(def constant constantly)
(defn variable [name] (fn [var-map]
  (if (contains? var-map name) (get var-map name)
    (throw (Exception. (str "No such variable with name " name))))))
(defn- operator [op]
  (fn [& args]
    (fn [var-map] (apply op (mapv #(% var-map) args)))))
(defn- divide-workaround [lhs & args] (/ (double lhs) (apply * args)))
(def add (operator +))
(def subtract (operator -))
(def negate (operator -))
(def multiply (operator *))
(def divide (operator divide-workaround))
(defn- get-args-with-info [& args] (conj args (count args)))
(defn- operator-with-info [op] (operator (comp #(apply op %) get-args-with-info)))
(def avg (operator-with-info #(/ (apply + %&) %1)))
(def med (operator-with-info #(nth (sort %&) (quot %1 2))))
(defn parse-node [id-map node]
  (cond
    (list? node) (apply (get id-map (name (first node))) (mapv (partial parse-node id-map) (rest node)))
    (number? node) ((get id-map :const) node)
    :else ((get id-map :variable) (name node))))
(defn parse-expression [id-map input]
  (parse-node id-map (read-string input)))
(def ^:private functional-identifier-map {:variable variable :const constant "+" add "-" subtract "negate" negate "*" multiply "/" divide "med" med "avg" avg})
(def parseFunction (partial parse-expression functional-identifier-map))

; hw 11
(defprotocol ^:private Expression
  (to-expression [this])
  (evaluate [this vars])
  (diff [this name])
  (toStringImpl [this is-postfix])
  (toStringInfix [this]))
(defn toStringPrefix [this] (toStringImpl this false))
(defn toStringSuffix [this] (toStringImpl this true))
(defn toString [this] (.toString this))

(defn- get-factory [name] (eval (symbol (str "->" name))))
(defn- gentype [name fields]
  (eval `(deftype ~name ~fields ~'Object ~'(toString [this] (toStringPrefix this)))))
(defn- implement-protocol [clazz protocol override]
  (extend clazz protocol override))
(defn- inherit-from [clazz & protocols]
  (mapv (fn [[protocol base & override]] (implement-protocol clazz protocol (merge base (first override)))) protocols))
(defn- inherit-expression [clazz base-behaviour override & protocols]
  (apply inherit-from clazz [Expression base-behaviour override] protocols))
(defn- create-expression
  ([name fields base override & protocols]
    (let [clazz (gentype name fields)]
      (apply inherit-expression clazz base override (mapv #(conj % {}) protocols))
      (get-factory name)))
  ([name fields override] (create-expression name fields override {:to-expression identity :toStringInfix #(toStringPrefix %)})))

(declare CONST-ZERO)
(declare CONST-ONE)
(def Constant (create-expression "CConstant" '[value]
  {:evaluate (fn [this vars] (.value this))
   :diff (fn [this name] CONST-ZERO)
   :toStringImpl (fn [this is-postfix] (format "%.1f" (double (.value this))))}))
(def Variable (create-expression "CVariable" '[nam]
  {:evaluate (fn [this vars] (get vars (.nam this)))
   :diff (fn [this name] (if (= name (.nam this)) CONST-ONE CONST-ZERO))
   :toStringImpl (fn [this is-postfix] (.nam this))}))
(def ^:private CONST-ZERO (Constant 0))
(def ^:private CONST-ONE (Constant 1))

(defprotocol ^:private Operable
  (to-operation [this])
  (map-args [this mapper])
  (apply-map-args [this applier mapper])
  (get-symb [this])
  (get-f [this])
  (get-args [this])
  (count-diff [this name diff-args]))
(defn count-args [this] (count (get-args this)))
(defn print-expression [begin args end] (str begin (string/join " " args) end))
(def ^:private base-operation {
  :evaluate (fn [this vars] (apply-map-args this (get-f this) #(evaluate % vars)))
  :toStringImpl (fn [this is-postfix]
    (let [begin (if is-postfix \( (str \( (get-symb this) " "))
          end (if is-postfix (str " " (get-symb this) \)) \))]
      (print-expression begin (map-args this #(toStringImpl % is-postfix)) end)))
  :toStringInfix (fn [this]
    {:pre [(or (= 1 (count-args this)) (= 2 (count-args this)))]}
    (let [args (get-args this) left (first args)]
      (if (= 1 (count-args this)) (str (get-symb this) \( (toStringInfix left) \))
        (let [args (get-args this) left (first args) right (second args)]
          (str \( (toStringInfix left) \space (get-symb this) \space (toStringInfix right) \))))))
  :diff (fn [this nam] (count-diff this nam (map-args this #(diff % nam))))
})
(def ^:private base-operable {
    :to-operation identity
    :get-symb #(.symb %)
    :get-f #(.f %)
    :get-args #(.args %)
    :map-args #(mapv %2 (get-args %1))
    :apply-map-args #(apply %2 (map-args %1 %3))
  })
(def Operation (create-expression "COperation" '[symb f args] base-operation
  {} [Operable base-operable]))
(defn- inherit-methods [base-methods to-base]
   (into {} (mapv (fn [[x y]] [x #(apply y (to-base %1) %&)]) base-methods)))
(def ^:private override-operation (inherit-methods base-operable #(.op %)))
(defn- construct-operation [factory symb f] (fn [& args] (factory (Operation symb f args))))
(defn- create-operation [name symb f override]
  (let [factory (create-expression name ['op] base-operation {:to-expression #(:op %)} [Operable (merge override-operation override)])]
    (construct-operation factory symb f)))

(declare Add)
(declare Subtract)
(comment ":NOTE: how extraction to separate function solves the problem of copy-pasted `diff` call on operands?")
(def ^:private add-behaviour {:count-diff (fn [this name args-diff] (apply Add args-diff))})
(def ^:private subtract-behaviour {:count-diff (fn [this name args-diff] (apply Subtract args-diff))})
(def Add (create-operation "CAdd" \+ + add-behaviour))
(def Subtract (create-operation "CSubtract" \- - subtract-behaviour))
(def Negate (create-operation "CNegate" "negate" - subtract-behaviour))
(declare Multiply)
(defn- count-diff-mult [name args args-diff] (if (= 1 (count args)) (first args-diff)
  (let [coll (mapv #(vector %1 %2) args args-diff)]
    (reduce (fn [res [r rd]] (Add (Multiply (diff res name) r) (Multiply res rd))) (first args) (rest coll)))))
(def Multiply (create-operation "CMultiply" \* * {
  :count-diff (fn [this name args-diff] (count-diff-mult name (get-args this) args-diff))
}))
(def Divide (create-operation "CDivide" \/ divide-workaround {:count-diff (fn [this name args-diff]
  (let [args (get-args this) l (first args) ld (first args-diff) rest-args (rest args)
        r (apply Multiply rest-args) rd (count-diff-mult name rest-args (rest args-diff))]
    (Divide (Subtract (Multiply ld r) (Multiply l rd)) (Multiply r r))))
}))
(def Sum (create-operation "CSum" "sum" + add-behaviour))
(def Avg (create-operation "CAvg" "avg" #(/ (apply + %&) (count %&)) {
  :count-diff (fn [this name args-diff] (Divide ((get add-behaviour :count-diff) this name args-diff) (Constant (count (get-args this)))))
}))
(defn right-assoc-reduce [op] #(op %1 (if (= 1 (count %&)) (first %&) (apply (right-assoc-reduce op) %&))))
(def Pow (create-operation "CPow" "**" (right-assoc-reduce #(Math/pow %1 %2)) {}))
(def Log (create-operation "CLog" "//" (right-assoc-reduce #(/ (Math/log (Math/abs %2)) (Math/log (Math/abs %1)))) {}))

(def ^:private object-identifier-map {:variable Variable :const Constant "+" Add "-" Subtract "negate" Negate "*" Multiply "/" Divide "sum" Sum "avg" Avg "**" Pow "//" Log})
(def parseObject (partial parse-expression object-identifier-map))

; hw 12
(defn -return [value tail] {:value value :tail tail})
(def -valid? boolean)
(def -value :value)
(def -tail :tail)
(defn _show [result]
  (if (-valid? result) (str "-> " (pr-str (-value result)) " | " (pr-str (apply str (-tail result))))
    "!"))
(defn tabulate [parser inputs]
  (run! (fn [input] (printf "    %-10s %s\n" (pr-str input) (_show (parser input)))) inputs))

(defn _empty [value] (partial -return value))
(defn _char [p]
  (fn [[c & cs]]
    (if (and c (p c)) (-return c cs))))
(defn _map [f result]
  (if (-valid? result)
    (-return (f (-value result)) (-tail result))))

(defn _combine [f a b]
  (fn [str]
    (let [ar ((force a) str)]
      (if (-valid? ar)
        (_map (partial f (-value ar))
        ((force b) (-tail ar)))))))
(defn _either [a b]
  (fn [str]
    (let [ar ((force a) str)]
      (if (-valid? ar) ar ((force b) str)))))

(defn _parser [p]
  (fn [input]
    (-value ((_combine (fn [v _] v) p (_char #{\u0000})) (str input \u0000)))))

(defn _check [checker p]
  (fn [str]
    (let [ar ((force p) str)]
      (if (and (-valid? ar) (checker (-value ar)))
        ar))))

(defn +char [chars] (_char (set chars)))
(defn +char-not [chars] (_char (comp not (set chars))))
(defn +map [f parser] (comp (partial _map f) parser))
(def +parser _parser)
(def +check _check)

(def +ignore (partial +map (constantly 'ignore)))
(defn iconj [coll value]
  (if (= value 'ignore) coll (conj coll value)))
(defn +seq [& ps]
  (reduce (partial _combine iconj) (_empty []) ps))
(defn +seqf [f & ps] (+map (partial apply f) (apply +seq ps)))
(defn +seqn [n & ps] (apply +seqf (fn [& vs] (nth vs n)) ps))

(defn +or [p & ps]
  (reduce _either p ps))
(defn +opt [p]
  (+or p (_empty nil)))
(defn +star [p]
  (letfn [(rec [] (+or (+seqf cons p (delay (rec))) (_empty ())))] (rec)))
(defn +plus [p] (+seqf cons p (+star p)))
(defn +str [p] (+map (partial apply str) p))
(def +seq-str (comp +str +seq))

(defn +string [string]
  (apply +seq-str (mapv (comp +char hash-set) string)))
(defn +strings [& strs]
  (if (empty? strs) (constantly nil) (apply +or (mapv +string strs))))
(def +ignore-char (comp +ignore +char))

(def is-id-function? (partial contains? object-identifier-map))
(def ^:private prior-to-name [
  ["negate"]
  {:right ["//" "**"]}
  {:left ["*" "/"]}
  {:left ["+" "-"]}
])
(def ^:private prior-to-parser
  (letfn [(map-assoc [x] (if x (+map (partial get object-identifier-map) (apply +strings x))))
      (getter-wrap [getter] #(if (map? %) (getter %) %))
      (getter-left [x] ((getter-wrap #(:left %)) x))
      (getter-right [x] ((getter-wrap #(:right %)) x))]
    (mapv #(hash-map :left (map-assoc (getter-left %)), :right (map-assoc (getter-right %))) prior-to-name)))

(defn +collect [defs]
  (cond
    (empty? defs) ()
    (seq? (first defs)) (let [[[key args body] & tail] defs]
                          (cons
                            {:key key :args args :body body}
                            (+collect tail)))
    :else (let [[key body & tail] defs]
            (cons
              {:key key :args [] :synth true :body body}
              (+collect tail)))))
(comment ":NOTE: good job, but limit of code for parser 30 lines")
(defmacro grammar [& defs]
  (let [collected (+collect defs)
        keys (set (map :key (filter :synth collected)))]
    (letfn [(rule [r] `(~(:key r) ~(:args r) ~(convert (:body r))))
            (convert [value]
              (cond
                (seq? value) (map convert value)
                (vector? value) (vec (map convert value))
                (char? value) `(+char ~(str value))
                (keys value) `(~value)
                :else value))]
      `(letfn ~(mapv rule collected) (+parser (~(:key (last collected))))))))
    
(def parseObjectInfix
  (grammar
    *digits (+str (+plus (+char "0123456789")))
    *number (+map (:const object-identifier-map) (+map read-string (+seq-str (+opt (+char "+-")) *digits (+opt (+seq-str (+char ".") *digits)))))
    *all-chars (mapv char (range 0 128))
    *id-letter (+char (str (apply str (filter #(Character/isLetter %) *all-chars)) "@#$_0123456789"))
    *identifier (+or (+str (+seqf cons *id-letter (+star *id-letter))) (+char "+-*/"))
    *function (+map (partial get object-identifier-map) (+check is-id-function? *identifier))
    *variable (+map (:variable object-identifier-map) (+check (comp not is-id-function?) *identifier))
    *ws (+ignore (+star (+char (apply str (filter #(Character/isWhitespace %) *all-chars)))))
    *seq-ast (fn [p sep] (+map first (+seq *ws (+opt (+seqf cons *ws p (+star (+seq *ws sep *ws p)) *ws)))))
    *infix-atom (+or *number *variable
      (+map (fn [[op arg]] (op arg)) (+seq (:left (first prior-to-parser)) *ws (delay *infix-atom)))
      (+seqn 1 (+char "(") *ws (delay *infix-expr) *ws (+char ")")))
    *infix-level-left (memoize (fn [level] (if (some? (:left (prior-to-parser level)))
      (+map #(reduce (fn [left [op right]] (op left right)) %) (*seq-ast (delay (*infix-level (dec level))) (:left (prior-to-parser level)))))))
    *infix-level-right (memoize (fn [level] (if (some? (:right (prior-to-parser level)))
      (+map
        (fn [[left & [[op right]]]] (if op (op left right) left))
        (+seq *ws (*infix-level (dec level)) *ws (+opt (+seq (:right (prior-to-parser level)) *ws (delay (*infix-level level)))))))))
    *infix-level (memoize (fn [level] (if (= level 0) *infix-atom
      (let [r (*infix-level-right level) l (*infix-level-left level)]
        (if l (if r (+or r l) l) (if r r (constantly nil)))))))
    *infix-expr (*infix-level (dec (count prior-to-parser)))
    *infix-parser (+seqn 0 *ws *infix-expr *ws)))
