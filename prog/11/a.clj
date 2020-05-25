;; 10: Review
;; 11: Review

(require '[clojure.string :as string])

; hw 10
(comment ":NOTE: use `constantly` instead")
(defn constant [v] (constantly v))
(defn variable [name] (fn [var-map]
  (if (contains? var-map name) (get var-map name)
    (throw (Exception. (string/join ["No such variable with name " name]))))))

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

(defprotocol IParser
  (parse-const [this s])
  (parse-identifier [this s])
  (parse-expr [this s])
  (parse-token [this s])
  (parse-tokens [this s])
  (parse-program [this s]))

(comment ":NOTE: too many code for parser even for 11 HW")
(defn- string-first
  ([s len] (subs s 0 len))
  ([s] (first s)))
(defn- string-rest
  ([s len] (string/triml (subs s len)))
  ([s] (string-rest s 1)))
(defn- parse-regex [s regex]
  (let [token (re-find regex s) tail (string-rest s (count token))]
    [token tail]))
(defn- fail [s] [nil s])
(defn- success? [res] (some? (first res)))
(defn- create-function [func id-map] (vector (get id-map func)))
(defn- get-function [packed-func]
  (if (vector? packed-func) (first packed-func)
    (throw (Exception. "Cannot call variable/constant"))))

(deftype Parser [id-map]
  IParser
    (parse-const [this s]
      (let [[str-val tail] (parse-regex s #"^[\+\-]?\d+(?:\.\d+)?(?=(?:\)|\(|\s|$))")]
        (if (string/blank? str-val) (fail s)
          [((:const id-map) (read-string str-val)) tail])))
    (parse-identifier [this s]
      (let [[token tail] (parse-regex s #"^[^\s0-9\(\)][^\s\(\)]*")]
      (if (string/blank? token) (fail s)
        [(if (contains? id-map token) (create-function token id-map) ((:variable id-map) token)) tail])))
    (parse-expr [this s]
      (if (not= \( (string-first s)) (fail s)
        (let [[parsed tail] (parse-tokens this (string-rest s))]
          (if (empty? parsed)
            (throw (Exception. "Empty parentheses"))
            (if (not= \) (string-first tail))
              (throw (Exception. "No matching closing parenthesis found"))
              (let [operation (get-function (first parsed)) args (rest parsed)]
                [(apply operation args) (string-rest tail)]))))))
    (parse-token [this s]
      (first (filter #(some? (first %)) (map #(% this s) (vector parse-const parse-identifier parse-expr)))))
    (parse-tokens [this s]
      (let [res (parse-token this s)]
      (if (success? res)
        (let [tail (parse-tokens this (second res))]
          [(conj (first tail) (first res)) (second tail)])
          [(list) s])))
    (parse-program [this s]
      (let [[expr tail] (parse-token this (string/triml s))]
        (if (string/blank? tail) (if (some? expr) expr
            (throw (Exception. "Can not read the value/expression (no match in known tokens)")))
          (throw (Exception. "Can only read one object"))))))

(def ^:private functional-identifier-map {:variable variable :const constant "+" add "-" subtract "negate" negate "*" multiply "/" divide "med" med "avg" avg})

(defn parseFunction [s]
  (let [parser (Parser. functional-identifier-map)]
    (parse-program parser s)))

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
  (extend clazz
    protocol
      override))
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
  (get-args [this]))
(defn count-args [this] (count (get-args this)))
(defn print-expression [begin args end]
  (str begin (string/join " " args) end))
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
})
(def ^:private base-operable {
    :to-operation identity
    :get-symb #(.symb %)
    :get-f #(.f %)
    :get-args #(.args %)
    :map-args #(mapv %2 (get-args %1))
    :apply-map-args #(apply %2 (map-args %1 %3))
  })
(def Operation (create-expression "COperation" '[symb f args] base-operation {} [Operable base-operable]))

(defn- inherit-methods [base-methods to-base]
   (into {} (mapv (fn [[x y]] [x #(apply y (to-base %1) %&)]) base-methods)))
(def ^:private override-operation (inherit-methods base-operable #(.op %)))
(defn- construct-operation [factory symb f]
  (fn [& args] (factory (Operation symb f args))))
(defn- create-operation [name symb f override]
  (let [factory (create-expression name ['op] base-operation (merge override {:to-expression #(:op %)}) [Operable override-operation])]
    (construct-operation factory symb f)))

(declare Add)
(declare Subtract)
(def ^:private add-behaviour {
  :diff (fn [this name] (apply-map-args this Add #(diff % name)))
})
(def ^:private subtract-behaviour {
  :diff (fn [this name] (apply-map-args this Subtract #(diff % name)))
})
(def Add (create-operation "CAdd" \+ + add-behaviour))
(def Subtract (create-operation "CSubtract" \- - subtract-behaviour))
(def Negate (create-operation "CNegate" "negate" - subtract-behaviour))
(def Multiply (create-operation "CMultiply" \* * {
  :diff (fn [this name] (reduce #(Add (Multiply (diff %1 name) %2) (Multiply %1 (diff %2 name))) (get-args this)))
}))
(def Divide (create-operation "CDivide" \/ divide-workaround {
  :diff (fn [this name] (reduce #(Divide (Subtract (Multiply (diff %1 name) %2) (Multiply %1 (diff %2 name))) (Multiply %2 %2)) (get-args this)))
}))
(def Sum (create-operation "CSum" "sum" + add-behaviour))
(def Avg (create-operation "CAvg" "avg" #(/ (apply + %&) (count %&)) {
  :diff (fn [this name] (Divide ((get add-behaviour :diff) this name) (Constant (count (get-args this)))))
}))

(defn right-assoc-reduce [op] #(op %1 (if (= 1 (count %&)) (first %&) (apply (right-assoc-reduce op) %&))))
(def Pow (create-operation "CPow" "**" (right-assoc-reduce #(Math/pow %1 %2)) {}))
(def Log (create-operation "CLog" "//" (right-assoc-reduce #(/ (Math/log (Math/abs %2)) (Math/log (Math/abs %1)))) {}))

(def ^:private object-identifier-map {:variable Variable :const Constant "+" Add "-" Subtract "negate" Negate "*" Multiply "/" Divide "sum" Sum "avg" Avg "**" Pow "//" Log})

(defn parseObject [s]
  (let [parser (Parser. object-identifier-map)]
    (parse-program parser s)))

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
(def *digit (+char "0123456789"))
(def *digits (+str (+plus *digit)))
(def *number (+map (:const object-identifier-map) (+map read-string (+seq-str (+opt (+char "+-")) *digits (+opt (+seq-str (+char ".") *digits))))))
(def *all-chars (mapv char (range 0 128)))
(def *id-letter (+char (str (apply str (filter #(Character/isLetter %) *all-chars)) "@#$_")))
(def *identifier (+or (+str (+seqf cons *id-letter (+star (+or *id-letter *digit)))) (+char "+-*/")))
(def *function (+map (partial get object-identifier-map) (+check is-id-function? *identifier)))
(def *variable (+map (:variable object-identifier-map) (+check (comp not is-id-function?) *identifier)))
(def *space (+char (apply str (filter #(Character/isWhitespace %) *all-chars))))
(def *ws (+ignore (+star *space)))
(defn *seq [begin p sep sep-ignore end]
  (+seq begin (+opt (+seqf cons *ws p (+star (if sep-ignore (+seqn 0 *ws sep *ws p) (+seq *ws sep *ws p))))) *ws end))
(defn *seq-ws [begin p end]
  (*seq begin p *ws true end))
(defn *seq-ast [p sep]
  (+map first (*seq *ws p sep false *ws)))
(declare *infix-level)
(declare *infix-expr)
(def *infix-atom (+or
  *number
  (+map (fn [[op arg]] (op arg)) (+seq (:left (first prior-to-parser)) *ws (delay *infix-atom)))
  *variable
  (+seqn 1 (+char "(") *ws (delay *infix-expr) *ws (+char ")"))))
(def *infix-level-left (memoize (fn [level] (if (some? (:left (prior-to-parser level)))
  (+map #(reduce (fn [left [op right]] (op left right)) %) (*seq-ast (delay (*infix-level (dec level))) (:left (prior-to-parser level))))))))
(def *infix-level-right (memoize (fn [level] (if (some? (:right (prior-to-parser level)))
  (+map
    (fn [[left & [[op right]]]] (if op (op left right) left))
    (+seq *ws (*infix-level (dec level)) *ws (+opt (+seq (:right (prior-to-parser level)) *ws (delay (*infix-level level))))))))))
(def *infix-level (memoize (fn [level] (if (= level 0) *infix-atom
  (let [r (*infix-level-right level) l (*infix-level-left level)]
    (if l (if r (+or r l) l) (if r r (constantly nil))))))))
(def *infix-expr (*infix-level (dec (count prior-to-parser))))
(def *infix-parser (+seqn 0 *ws *infix-expr *ws))
(def parseObjectInfix (+parser *infix-parser))