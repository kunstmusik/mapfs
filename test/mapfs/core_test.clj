(ns mapfs.core-test
  (:require [clojure.test :refer :all]
            [mapfs.core :refer :all]))

(deftest keypath-exists?-test
  (testing "check keypath-exists? functions correctly"
    (let [m {:a {:b {:c "test"}}}]
      (is (keypath-exists? m [:a])) 
      (is (keypath-exists? m [:a :b])) 
      (is (keypath-exists? m [:a :b :c])) 
      (is (not (keypath-exists? m [:a :c]))) 
      (is (not (keypath-exists? m [:a :b :d]))) 
      (is (not (keypath-exists? m [:b :d]))))
    ))

(deftest cat-test
  (testing "cat works relative to any path"
    (mount! {:a {:b {:c "test"}}}) 
    (cd [:a :b])
    (is (= [:a :b] (pwd)))
    (is (= "test" (cat :c)))
    (is (= {:b {:c "test"}} (cat :..)))
    (is (= "test" (cat [:.. :.. :a :b :c])))
    ))

(deftest cd-test
  (testing "cd works relative to any path"
    (mount! {:a {:b {:c "test"}}}) 
    (cd [:a :b])
    (is (= [:a :b] (pwd)))
    (is (thrown-with-msg? Exception #"Error: Keypath not found:.*" 
                          (cd :d))) 
    (is (thrown-with-msg? Exception #"Error: Keypath is not a directory:.*" 
                          (cd :c))) 
    (cd [:.. :..])
    (is (= [] (pwd)))
    ))
