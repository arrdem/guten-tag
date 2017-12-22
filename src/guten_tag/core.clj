(ns guten-tag.core
  (:refer-clojure :exclude [val])
  )

(defprotocol ITaggedVal
  (-tag [_])
  (-val [_]))

(deftype ATaggedVal [t v]
  ITaggedVal
  (-tag [self]
    t)
  (-val [self]
    v)

  clojure.lang.Indexed
  (nth [self i] (nth self i nil))
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

  java.lang.Iterable
  (iterator [this]
    (clojure.lang.SeqIterator.
     (seq this)))

  clojure.lang.Associative
  (entryAt [self key]
    (.entryAt v key))
  (assoc [_ sk sv]
    (ATaggedVal. t (.assoc v sk sv)))

  clojure.lang.ILookup
  (valAt [self k]
    (.valAt v k))
  (valAt [self k o]
    (.valAt v k o))

  clojure.lang.IPersistentMap
  (assocEx [_ sk sv]
    (ATaggedVal. t (.assocEx v sk sv)))
  (without [_ sk]
    (ATaggedVal. t (.without v sk))))

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
  (deftag test \"A demo variant\" [a b]
    {:pre [(number? a) (vector? b)]})
  ;; => nil
  (->test 1 [1])
  ;; => #g/t [:user/test {:a 1 :b [1]}]
  (test? (->test 1 [1]))
  ;; => true
  (tagged? (->test 1 [1]))
  ;; => true
  (tag (->test 1 [1]))
  ;; => :user/test
  (val (->test 1 [1]))
  ;; => {:a 1 b [1]}"
  {:arglists '([name doc-string? attr-map? members pre-map?])}
  [vname & args]
  (let [;; Parse direct args
        [?docstring args] (take-when string? "" args)
        [?attr-map args]  (take-when map? {} args)
        [members args]    (take-when vector? nil args)
        [?pre-map args]   (take-when map? {} args)

        _ (when (:post ?pre-map)
            (->> (for [e (:post ?pre-map)]
                   (str "........ " e "\n"))
                 (list* "Warning: deftag ignores :post, the following forms are ignored:\n")
                 (apply str)
                 print))

        _ (dissoc ?pre-map :post)

        ;; FIXME inline guards are a bad habit of mine
        _ (assert (vector? members) "Members is not a vector!")
        _ (assert (every? symbol? members) "Members may contain only symbols!")
        _ (assert (every? (complement namespace) members) "Members may not be namespaced!")

        ;; Build used datastructures
        kw-members (mapv (comp keyword name) members)
        kw-tag     (keyword (or (namespace vname)
                                (name (ns-name *ns*)))
                            (name vname))
        ?attr-map  (merge {}
                          ?attr-map
                          ?pre-map
                          (when ?docstring
                            {:doc ?docstring}))]
    `[(defn ~(with-meta
               (symbol (str "->" (name vname)))
               (select-keys ?pre-map [:private]))
        ~(str "Generated constructor for the " vname " type.")
        ~members
        ~?pre-map
        (->ATaggedVal ~kw-tag (hash-map ~@(interleave kw-members members))))
      (defn ~(with-meta
               (symbol (str (name vname) "?"))
               (select-keys ?pre-map [:private]))
        ~(str "Generated predicate for the " vname " type.")
        ([x#]
         (and (tagged? x#)
              (= ~kw-tag (tag x#))
              (or (map? (val x#))
                  (nil? (val x#)))
              (let [[_# {:keys ~members}] x#]
                (and ~@(:pre ?pre-map))))))]))

(defn- general-read-tagged-val
  "Wrapper function around ->ATaggedVal which serves as the tagged value ctor.

  If the second argument is :do-eval, calls clojure.core/eval on its first
  argument. Otherwise doesn't."
  [[tag val :as a] eval?]
  {:pre [(vector? a)
         (keyword? tag)
         (map? val)
         (every? keyword? (keys val))]}
  (let [[tag val] (if (= eval? :do-eval)
                    (eval a)
                    a)]
    (->ATaggedVal tag val)))

(defn read-tagged-val
  "Reader function for tagged guten-tag literals. Doesn't evaluate anything it
  reads, so it's safe for use clojure.edn/read(-string) on data from not so much
  trusted sources.

  Example:

  (require '[guten-tag.core :as t]
           '[clojure.edn :as edn])
  ;; => nil
  (edn/read-string
    {:readers {'g/t t/read-tagged-val}}
    \"#g/t [:geom.simple/oblong {:length 3 :width 4}]\")
  ;; => #g/t [:geom.simple/oblong {:length 3, :width 4}]"
  [a]
  (general-read-tagged-val a :do-not-eval))

(defn unsafe-read-tagged-val
  "Reader function for tagged guten-tag literals. IMPORTANT: This function calls
  eval on the data it reads. Use it only with data from trusted sources.

  Example:

  (require '[guten-tag.core :as t])
  ;; => nil
  (binding [*data-readers* (assoc *data-readers* 'g/t t/unsafe-read-tagged-val)]
    (read-string \"#g/t [:geom.simple/square {:length 3 :area (* 3 3)}]\"))
  ;; => #g/t [:geom.simple/square {:length 3, :area 9}]"
  [a]
  (general-read-tagged-val a :do-eval))



(defmacro defenum
  "Generates a tagged value predicate T? and a number of defs corresponding to the symbolic values
  of the enumeration type T.

  Ex.
  (defenum foo [bar baz qux])
  ;; => nil
  "
  {:arglists '([name doc-string? attr-map? members])}
  [ename & args]
  (let [[?docstring args] (take-when string? "" args)
        [?attr-map args]  (take-when map? {} args)
        [members args]    (take-when vector? nil args)

        ;; FIXME inline guards are a bad habit of mine
        _ (assert (vector? members) "Members is not a vector!")
        _ (assert (every? symbol? members) "Members may contain only symbols!")

        ;; Build used datastructures
        kw-tag    (keyword (or (namespace ename)
                               (name (ns-name *ns*)))
                           (name ename))
        ?attr-map (merge {}
                         ?attr-map
                         (when ?docstring
                           {:doc ?docstring}))
        pred      (symbol (str (name ename) "?"))
        members   (map #(keyword (namespace %) (name %)) members)]
    `[~@(mapcat (fn [member]
                  `[(def ~(symbol (name member)) (->ATaggedVal ~kw-tag ~member))
                    (defn ~(symbol (str (name member) "?")) [x#]
                      (boolean
                       (and (tagged? x#)
                            (= ~kw-tag (tag x#))
                            (= ~member (val x#)))))])
                members)
      (defn ~pred [x#]
        (boolean
         (and (tagged? x#)
              (= ~kw-tag (tag x#))
              (~(set (map (comp keyword name) members)) (val x#)))))]))
