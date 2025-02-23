(ns ^:no-doc robertluo.fun-map.core
  "Where the fun starts."
  (:import #?(:clj [clojure.lang
                    IMapEntry
                    IPersistentMap
                    ITransientMap])))

#?(:clj
;;Marker iterface for a funmap
   (definterface IFunMap
     (rawSeq []))
   :cljs
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
                  :cljs [entry (.entryAt this k)])
         (.val entry)
         not-found))

     (without [_ k] (TransientDelegatedMap. (.without tm k) fn-entry))
     (count [_] (.count tm))

     clojure.lang.ITransientAssociative2
     (containsKey [_ k]
       (.containsKey tm k))
     (entryAt [this k]
       (fn-entry this (.entryAt tm k))))
   :cljs
   (deftype TransientDelegatedMap [tm fn-entry]
     ITransientMap
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
   :cljs
   (deftype DelegatedMap [m fn-entry]
     IFunMap
     (-raw-seq [_] (-seq m))
     Object
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
        (reify Object
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
         :cljs (cljs.core/MapEntry. k v nil)))))

(defn delegate-map
  "Return a delegated map"
  [m fn-entry]
  (->DelegatedMap m (fn-entry-adapter fn-entry)))

(defn fun-map?
  [o]
  #?(:clj (instance? IFunMap o)
     :cljs (satisfies? IFunMap o)))

#?(:clj
   (do
     (defmethod print-method IFunMap [^IFunMap o ^java.io.Writer wtr]
       (let [raw-entries (.rawSeq o)]
         (print-method (into {} raw-entries) wtr)))

     (prefer-method print-method IFunMap clojure.lang.IPersistentMap)
     (prefer-method print-method IFunMap java.util.Map)))
