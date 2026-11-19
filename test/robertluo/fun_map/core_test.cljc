(ns robertluo.fun-map.core-test
  (:require
   [robertluo.fun-map.core :as sut]
   [clojure.test :refer [deftest is testing]]))

(deftest delegate-map
  (testing "methods implementation"
    (is (= (sut/delegate-map {} second) {})))
  (testing "delegate-map with entry simple pass is a normal map"
    (let [m (sut/delegate-map {:a 1 :b 2} (fn [_ [k v]] [k (* 2 v)]))]
      (is (= 4 (get m :b)))
      (is (= 2 (count m)))
      (is (= {:a 2 :b 4} m))
      (is (= m {:a 2 :b 4})))))

(deftest delegate-map-lookup-falsy-values
  (testing "键存在时返回它的值（包括 false 和 nil），键不存在才返回 not-found"
    (let [m (sut/delegate-map {:a false :b nil :c 1} (fn [_ entry] entry))]
      (is (false? (get m :a)))
      (is (false? (get m :a :not-found)))
      (is (nil? (get m :b)))
      (is (nil? (get m :b :not-found)))
      (is (= 1 (get m :c :not-found)))
      (is (nil? (get m :d)))
      (is (= :not-found (get m :d :not-found))))))

(deftest transient-test
  (letfn [(wrap-m [m f] (-> m (sut/delegate-map (fn [_ [k v]] [k (* 2 v)])) transient f persistent!))]
    (is (= {} (wrap-m {} identity)))
    (is (= {:a 2} (wrap-m {} #(assoc! % :a 1))))
    (is (= {:a 2} (wrap-m {} #(conj! % [:a 1]))))
    (is (= {:b 4} (wrap-m {} #(-> % (conj! [:a 1]) (conj! [:b 2]) (dissoc! :a)))))))

#?(:cljs
   (deftest map-compatibility
     (testing "If it compatible to map's expected behavior"
       (let [m (sut/delegate-map {:a 1 :b 2 :c 3} (fn [_ [k v]] [k (* 2 v)]))]
         (is (= [:a :b :c] (keys m)))
         (is (= [2 4 6] (vals m)))
         (is (= [[:a 2] [:b 4] [:c 6]] (.entries m)))))))
