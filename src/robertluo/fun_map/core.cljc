(ns ^:no-doc robertluo.fun-map.core
  "Where the fun starts."
  (:import #?(:clj [clojure.lang
                    IMapEntry
                    IPersistentMap
                    ITransientMap])))

#?(:cljr
   (do

     (defprotocol IObjectIterable
       (hasNext [_] )
       (next [_] ))

     (defprotocol IObjectInCljs
       (toString [_])
       (equiv [this other])
       (keys [_])
       (entries [this])
       (vals [this])
       (has [this k])
       (get [this k not-found])
       (forEach [this f]))

     (defprotocol ICounted
       "Protocol for adding the ability to count a collection in constant time."
       (^number -count [coll]
         "Calculates the count of coll in constant time. Used by cljs.core/count."))

     (defprotocol IEmptyableCollection
       "Protocol for creating an empty collection."
       (-empty [coll]
         "Returns an empty collection of the same category as coll. Used
          by cljs.core/empty."))

     (defprotocol IIterable
       "Protocol for iterating over a collection."
       (-iterator [coll]
         "Returns an iterator for coll."))

     (defprotocol IPrintWithWriter
       "The old IPrintable protocol's implementation consisted of building a giant
        list of strings to concatenate.  This involved lots of concat calls,
        intermediate vectors, and lazy-seqs, and was very slow in some older JS
        engines.  IPrintWithWriter implements printing via the IWriter protocol, so it
        be implemented efficiently in terms of e.g. a StringBuffer append."
       (-pr-writer [o writer opts]))

     (defprotocol IWriter
       "Protocol for writing. Currently only implemented by StringBufferWriter."
       (-write [writer s]
         "Writes s with writer and returns the result.")
       (-flush [writer]
         "Flush writer."))

     (defprotocol IMeta
       "Protocol for accessing the metadata of an object."
       (^clj-or-nil -meta [o]
         "Returns the metadata of object o."))

     (defprotocol IWithMeta
       "Protocol for adding metadata to an object."
       (^clj -with-meta [o meta]
         "Returns a new object with value of o and metadata meta added to it."))

     (defprotocol IEditableCollection
       "Protocol for collections which can transformed to transients."
       (^clj -as-transient [coll]
         "Returns a new, transient version of the collection, in constant time."))

     (defprotocol IEquiv
       "Protocol for adding value comparison functionality to a type."
       (^boolean -equiv [o other]
         "Returns true if o and other are equal, false otherwise."))

     (defprotocol ISeqable
       "Protocol for adding the ability to a type to be transformed into a sequence."
       (^clj-or-nil -seq [o]
         "Returns a seq of o, or nil if o is empty."))

     (defprotocol ICollection
       "Protocol for adding to a collection."
       (^clj -conj [coll o]
         "Returns a new collection of coll with o added to it. The new item
          should be added to the most efficient place, e.g.
          (conj [1 2 3 4] 5) => [1 2 3 4 5]
          (conj '(2 3 4 5) 1) => '(1 2 3 4 5)"))

     (defprotocol ILookup
       "Protocol for looking up a value in a data structure."
       (-lookup [o k] [o k not-found]
         "Use k to look up a value in o. If not-found is supplied and k is not
          a valid value that can be used for look up, not-found is returned."))

     (defprotocol IAssociative
       "Protocol for adding associativity to collections."
       (^boolean -contains-key? [coll k]
         "Returns true if k is a key in coll.")
       #_(-entry-at [coll k])
       (^clj -assoc [coll k v]
         "Returns a new collection of coll with a mapping from key k to
          value v added to it."))

     (defprotocol IFind
       "Protocol for implementing entry finding in collections."
       (-find [coll k] "Returns the map entry for key, or nil if key not present."))

     (defprotocol IMap
       "Protocol for adding mapping functionality to collections."
       #_(-assoc-ex [coll k v])
       (^clj -dissoc [coll k]
         "Returns a new collection of coll without the mapping for key k."))





     (defprotocol ITransientCollection
       "Protocol for adding basic functionality to transient collections."
       (^clj -conj! [tcoll val]
         "Adds value val to tcoll and returns tcoll.")
       (^clj -persistent! [tcoll]
         "Creates a persistent data structure from tcoll and returns it."))

     (defprotocol ITransientAssociative
       "Protocol for adding associativity to transient collections."
       (^clj -assoc! [tcoll key val]
         "Returns a new transient collection of tcoll with a mapping from key to
          val added to it."))

     (defprotocol ITransientMapInCljs
       "Protocol for adding mapping functionality to transient collections."
       (^clj -dissoc! [tcoll key]
         "Returns a new transient collection of tcoll without the mapping for key."))

     (defprotocol IObjectEquiv
       (equiv [this other]))

     (deftype NeverEquiv []
       IObjectEquiv ; Object
       (equiv [this other]
         (-equiv this other))
       IEquiv
       (-equiv [o other] false))

     (def ^:private never-equiv (NeverEquiv.))

     (defprotocol IKVReduce
       "Protocol for associative types that can reduce themselves
       via a function of key and val. Called by cljs.core/reduce-kv."
       (-kv-reduce [coll f init]
         "Reduces an associative collection and returns the result. f should be
          a function that takes three arguments."))

     (defn equiv-map
       "Test map equivalence. Returns true if x equals y, otherwise returns false."
       [x y]
       (boolean
        (when (and (map? y) (not (record? y)))
          ; assume all maps are counted
          (when (== (count x) (count y))
            (if (satisfies? IKVReduce x)
              (reduce-kv
               (fn [_ k v]
                 (if (= (get y k never-equiv) v)
                   true
                   (reduced false)))
               true x)
              (every?
               (fn [xkv]
                 (= (get y (first xkv) never-equiv) (second xkv)))
               x))))))
     ))

#?(:clj
;;Marker iterface for a funmap
   (definterface IFunMap
     (rawSeq []))
   :cljr
   (defprotocol  IFunMap
     (-raw-seq [m])))

(declare ->DelegatedMap)

;;Support transient
#?(:clj
   (deftype TransientDelegatedMap [^ITransientMap tm fn-entry]
     ITransientMap
     (conj [_ v] (TransientDelegatedMap. (.conj tm v) fn-entry))
     (persistent [_] (->DelegatedMap (persistent! tm) fn-entry))
     ;;ITransientAssociative
     (assoc [_ k v] (TransientDelegatedMap. (.assoc tm k v) fn-entry))
     ;;ILookup
     (valAt [this k] (.valAt this k nil))
     (valAt
       [this k not-found]
       (if-let #?(:clj [^clojure.lang.IMapEntry entry (.entryAt this k)]
                  :cljr [entry (.entryAt this k)])
         (.val entry)
         not-found))

     (without [_ k] (TransientDelegatedMap. (.without tm k) fn-entry))
     (count [_] (.count tm))

     clojure.lang.ITransientAssociative2
     (containsKey [_ k]
       (.containsKey tm k))
     (entryAt [this k]
       (fn-entry this (.entryAt tm k))))
   :cljr
   (deftype TransientDelegatedMap [tm fn-entry]
     ITransientMapInCljs
     (-dissoc!
      [_ k]
      (TransientDelegatedMap. (-dissoc! tm k) fn-entry))

     ITransientAssociative
     (-assoc!
      [_ k v]
      (TransientDelegatedMap. (-assoc! tm k v) fn-entry))

     ITransientCollection
     (-persistent!
      [_]
      (->DelegatedMap (-persistent! tm) fn-entry))
     (-conj!
      [_ pair]
      (TransientDelegatedMap. (-conj! tm pair) fn-entry))))

#?(:clj
;; DelegatedMap takes a map `m` and delegates most feature to it.
;; The magic happens on function `fn-entry`, which takes the delegated map
;; itself and a pair of kv as arguments. Returns a pair of kv.
   (deftype DelegatedMap [^IPersistentMap m fn-entry]
     java.io.Closeable
     (close
      [this]
      (when-let [close-fn (some-> this meta ::close-fn)]
        (close-fn this)))
     IFunMap
     (rawSeq [_]
       (.seq m))
     clojure.lang.MapEquivalence
     clojure.lang.IHashEq
     (hasheq [_]
       (.hasheq ^clojure.lang.IHashEq m))
     (hashCode [_]
       (.hashCode m))
     (equals [this other]
       (clojure.lang.APersistentMap/mapEquals this other))
     clojure.lang.IObj
     (meta [_]
       (.meta ^clojure.lang.IObj m))
     (withMeta [_ mdata]
       (DelegatedMap. (with-meta m mdata) fn-entry))
     clojure.lang.ILookup
     (valAt [this k]
       (some-> ^IMapEntry (.entryAt this k) (.val)))
     (valAt [this k not-found]
       (if (.containsKey this k)
         (.valAt this k)
         not-found))
     clojure.lang.IPersistentMap
     (count [_]
       (.count m))
     (empty [_]
       (DelegatedMap. (.empty m) fn-entry))
     (cons [_ o]
       (DelegatedMap.
        (.cons m (if (instance? IFunMap o) (.rawSeq ^IFunMap o) o))
        fn-entry))
     (equiv [this other]
       (.equals this other))
     (containsKey [_ k]
       (.containsKey m k))
     (entryAt [this k]
       (when (.containsKey m k)
         (fn-entry this (.entryAt m k))))
     (seq [this]
       (clojure.lang.IteratorSeq/create (.iterator this)))
     (iterator [this]
       (let [ite (.iterator m)]
         (reify java.util.Iterator
           (hasNext [_]
             (.hasNext ite))
           (next [_]
             (fn-entry this (.next ite))))))
     (assoc [_ k v]
       (DelegatedMap. (.assoc m k v) fn-entry))
     (assocEx [_ k v]
       (DelegatedMap. (.assocEx m k v) fn-entry))
     (without [_ k]
       (DelegatedMap. (.without m k) fn-entry))
     java.util.Map
     (size [this]
       (.count this))
     (isEmpty [this]
       (zero? (.count this)))
     (containsValue [this v]
       (boolean (some #{v} (vals this))))
     (get [this k]
       (.valAt this k))
     (keySet [this]
       (set (keys this)))
     (values [this]
       (vals this))
     (entrySet [this]
       (set this))
     (put [_ _ _] (throw (UnsupportedOperationException.)))
     (remove [_ _] (throw (UnsupportedOperationException.)))
     (putAll [_ _] (throw (UnsupportedOperationException.)))
     (clear [_] (throw (UnsupportedOperationException.)))

     clojure.lang.IEditableCollection
     (asTransient [_]
       (TransientDelegatedMap. (transient m) fn-entry)))
   :cljr
   (deftype DelegatedMap [m fn-entry]
     IFunMap
     (-raw-seq [_] (-seq m))
     IObjectInCljs
     (toString [_] (.toString m))
     (equiv
       [this other]
       (-equiv this other))
     (keys [_] (keys m))
     (entries [this] (-seq this))
     (vals [this] (map second (-seq this)))
     (has [this k] (-contains-key? this k))
     (get
       [this k not-found]
       (-lookup this k not-found))
     (forEach
      [this f]
      (doseq [[k v] (-seq this)]
        (f v k)))

     IFind
     (-find
      [this k]
      (when (-contains-key? m k)
        (fn-entry this (-find m k))))

     ILookup
     (-lookup
       [this k]
       (-lookup this k nil))
     (-lookup
       [this k not-found]
       (or (some-> ^IMapEntry (-find this k) (val)) not-found))
     
     IMap
     (-dissoc
      [_ k]
      (DelegatedMap. (-dissoc m k) fn-entry))

     ICollection
     (-conj
       [_ o]
      (if (satisfies? IFunMap o)
        (DelegatedMap. (-conj m (-raw-seq o)) fn-entry)
        (DelegatedMap. (-conj m o) fn-entry)))

     IAssociative
     (-assoc [_ k v] (DelegatedMap. (-assoc m k v) fn-entry))
     (-contains-key? [_ k] (-contains-key? m k))

     IEquiv
     (-equiv
       [this other]
       (equiv-map this other))

     ICounted
     (-count
       [_]
       (-count m))
     
     IEmptyableCollection
     (-empty
      [_]
      (DelegatedMap. (-empty m) fn-entry))
     
     IIterable
     (-iterator
      [this]
      (let [ite (-iterator m)]
        (reify IObjectIterable
          (hasNext [_]
            (.hasNext ite))
          (next [_]
            (fn-entry this (.next ite))))))

     ISeqable
     (-seq
      [this]
      (some->> (-seq m)
               (map #(fn-entry this %))))

     IPrintWithWriter
     (-pr-writer
       [_ wtr opts]
       (-write wtr "#fun-map")
       (-pr-writer m wtr opts))
     
     IWithMeta
     (-with-meta
      [_ meta]
      (DelegatedMap. (-with-meta m meta) fn-entry))
     
     IMeta
     (-meta [_] (-meta m))

     IEditableCollection
     (-as-transient
       [_]
       (TransientDelegatedMap. (-as-transient m) fn-entry))))

(defn- fn-entry-adapter
  "turns function `fn-entry` to entry in/out function"
  [fn-entry]
  (fn [m ^IMapEntry entry]
    (when-let [[k v] (fn-entry m entry)]
      #?(:clj (clojure.lang.MapEntry/create k v)
         :cljr (clojure.lang.MapEntry/create k v)
         :cljs (cljs.core/MapEntry. k v nil)))))

(defn delegate-map
  "Return a delegated map"
  [m fn-entry]
  (->DelegatedMap m (fn-entry-adapter fn-entry)))

(defn fun-map?
  [o]
  #?(:clj (instance? IFunMap o)
     :cljr (satisfies? IFunMap o)))

#?(:clj
   (do
     (defmethod print-method IFunMap [^IFunMap o ^java.io.Writer wtr]
       (let [raw-entries (.rawSeq o)]
         (print-method (into {} raw-entries) wtr)))

     (prefer-method print-method IFunMap clojure.lang.IPersistentMap)
     (prefer-method print-method IFunMap java.util.Map)))
