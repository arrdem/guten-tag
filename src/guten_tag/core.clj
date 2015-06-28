(ns guten-tag.core
  (:refer-clojure :exclude [val])
  (:require [clojure.set :as set]))

(defprotocol ITaggedVal
  (-tag [_])
  (-val [_]))

(deftype ATaggedVal [t v guards]
  ITaggedVal
  (-tag [self]
    t)
  (-val [self]
    v)

  clojure.lang.Indexed
  (nth [self i]
    (nth self i nil))
  (nth [self i o]
    (case i
      (0) t
      (1) v
      o))

  clojure.lang.Sequential
  clojure.lang.ISeq
  (next  [this]
    (seq this))
  (first [this]
    t)
  (more  [this]
    (.more (seq this)))
  (count [this]
    2)
  (equiv [this obj]
    (= (seq this) obj))
  (seq [self]
    (cons t (cons v nil)))

  clojure.lang.Associative
  (entryAt [self key]
    (.entryAt v key))
  (assoc [_ sk sv]
    (let [v'    (.assoc v sk sv)
          preds (get guards sk)]
      (doseq [p preds]
        (assert (p v')))
      (ATaggedVal. t v' guards)))

  clojure.lang.ILookup
  (valAt [self k]
    (.valAt v k))
  (valAt [self k o]
    (.valAt v k o))

  clojure.lang.IPersistentMap
  (assocEx [_ sk sv]
    (let [v'    (.assocEx v sk sv)
          preds (get guards sk)]
      (doseq [p preds]
        (assert (p v')))
      (ATaggedVal. t v' guards)))
  (without [_ sk]
    (let [v'    (.without v sk)
          preds (get guards sk)]
      (doseq [p preds]
        (assert (p v')))
      (ATaggedVal. t v' guards))))

(def ^:dynamic *concise-printing*
  true)

(defmethod print-method ATaggedVal [v ^java.io.Writer w]
  (->> (format
        (if *concise-printing*
          "#g/t [%s %s]"
          "#guten/tag [%s %s]")
        (-tag v)
        (-val v))
       (.write w)))

(defn tagged?
  "Predicate indicating whether the argument value is a tagged value or
  not. Returns true if and only if the argument is all of #{ITaggedVal, Indexed,
  ISeq}."
  [x]
  (and (instance? guten_tag.core.ITaggedVal x)
       (instance? clojure.lang.Indexed x)
       (instance? clojure.lang.ISeq x)
       (instance? clojure.lang.Associative x)))

(defn tag
  "Returns the tag on a tagged value, returning nil if the value is not tagged."
  [x]
  (when (tagged? x)
    (-tag x)))

(defn val
  "Returns the value of a tagged value, returning nil if the value is not
  tagged."
  [x]
  (when (tagged? x)
    (-val x)))

(defn- take-when
  [f empty col]
  {:pre [(fn? f)]}
  (let [[x & rest] col]
    (if (f x) [x rest] [empty col])))

(defmacro deftag
  "Generates a tagged value smart constructor `->T` where T is the name of the
  tag. The constructor accepts an arglist identical to the list of members and
  returns an instance of ATaggedVal with the defined tag and a map constructed
  from keywordized member names to given values. Preconditions on members may be
  specified by the pre-map as for clojure.core/defn.

  Also generates a predicate `T?` where T is the name of the tag. The predicate
  will return true if and only if the argument value is `tagged?`, an instance
  of the given tag type, and satisfies the `:pre` conditions in the tag
  constructor if any.

  Ex.
  user> (deftag test \"A demo variant\" [a b]
  {:pre [(number? a) (vector? b)]})
  nil
  user> (->test 1 [1])
  〈:user/test {:a 1 :b [1]}〉
  user> (test? (->test 1 [1]))
  true
  user> (tagged? (->test 1 [1]))
  true
  user> (tag (->test 1 [1]))
  :user/test
  user> (val (->test 1 [1]))
  {:a 1 b [1]}"
  {:arglists '([name doc-string? attr-map? members pre-map?])}
  [vname & args]
  (let [;; Parse direct args
        [?docstring args] (take-when string? "" args)
        [?attr-map  args] (take-when map? {} args)
        [members    args] (take-when vector? nil args)
        [?pre-map   args] (take-when map? {} args)

        ;; FIXME inline guards are a bad habit of mine
        _                 (assert (vector? members)
                                  "Members is not a vector!")
        _                 (assert (every? symbol? members)
                                  "Members may contain only symbols!")

        ;; Build used datastructures
        kw-members        (mapv (comp keyword name) members)
        kw-tag            (keyword (or (namespace vname)
                                       (name (ns-name *ns*)))
                                   (name vname))
        ?attr-map         (merge {}
                                 ?attr-map
                                 ?pre-map
                                 (when ?docstring
                                   {:doc ?docstring}))
        ->guard           (fn [form]
                            (eval
                             `(fn [{:keys [~@members]}]
                                ~form)))
        guards            (->> (for [m members]
                                 [(keyword (name m))
                                  (for [g     (:pre ?pre-map)
                                        :when (-> (flatten g)
                                                  set
                                                  (contains? m))]
                                    (->guard g))])
                               (into {}))]
    `(do (defn ~(with-meta
                  (symbol (str "->" (name vname)))
                  (select-keys ?pre-map [:private]))
           ~(str "Generated constructor for the " vname " type.")
           ~members
           ~?pre-map
           (->ATaggedVal
            ~kw-tag
            (hash-map ~@(interleave kw-members members))
            ~guards))
         (def ~(with-meta (symbol (name vname))
                 (select-keys ?pre-map [:private]))
           ~?docstring
           (guten-tag.core/->TagDescriptor
            ,,(quote ~?attr-map)
            ,,(quote ~kw-members)
            ,,(quote ~kw-tag)))
         (defn ~(with-meta
                  (symbol (str (name vname) "?"))
                  (select-keys ?pre-map [:private]))
           ~(str "Generated predicate for the " vname " type.")
           ([x#]
            (and (tagged? x#)
                 (= ~kw-tag (tag x#))
                 (or (map? (val x#))
                     (nil? (val x#)))
                 (let [[_ {:keys ~members}] x#]
                   (and ~@(:pre ?pre-map))))))
         nil)))

(deftag TagDescriptor
  "Tag describing the tag of other things! so this is the tag tag :-P"
  [attrs members tag])

(defn read-tagged-val
  "Wrapper function around ->ATaggedVal which serves as the tagged value ctor."
  [[tag val :as a]]
  {:pre [(vector? a)
         (keyword? tag)
         (map? val)
         (every? keyword? (keys val))]}
  (let [[tag val] (eval a)]
    (->ATaggedVal tag val nil)))
