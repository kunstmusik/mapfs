(ns mapfs.core-test
  (:require [clojure.test :refer :all]
            [mapfs.core :refer :all]))

(deftest cat-test
  (testing "cat works relative to any path"
    (mount! {:a {:b {:c "test"}}}) 
    (cd [:a :b])
    (is (= [:a :b] (pwd)))
    (is (= "test" (cat :c)))
    (is (= {:b {:c "test"}} (cat :..)))
    (is (= "test" (cat [:.. :.. :a :b :c])))
    ))

