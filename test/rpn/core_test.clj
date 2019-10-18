(ns rpn.core-test
  (:require [clojure.test :refer :all]
            [rpn.core :refer :all]))

(deftest testSimple
  (testing "should correctly do simple arithmetic"
    (is (= (rpn.core/calc "1") 1.0))
    (is (= (rpn.core/calc "1 2 +") 3.0))
    (is (= (rpn.core/calc "2 1 -") 1.0))
    (is (= (rpn.core/calc "2 1 *") 2.0))
    (is (= (rpn.core/calc "4 2 /") 2.0))
    (is (= (rpn.core/calc "2.5 1.5 +") 4.0))
    (is (= (rpn.core/calc "3 2 /") 1.5))))

(deftest testAdvanced
  (testing "should correctly do more advanced arithmetic"
    (is (= (rpn.core/calc "1 2 3 + *") 5.0))
    (is (= (rpn.core/calc "1 2 3 * +") 7.0))
    (is (= (rpn.core/calc "2 2 3 4 * - +") -8.0))
    (is (= (rpn.core/calc "2 2 * 4 + 2 /") 4.0))))

(deftest testExceptions
  (testing "should correctly throw exceptions for impossible expressions"
    (is (thrown? Exception (rpn.core/calc "1 2 3 +")))
    (is (thrown? Exception (rpn.core/calc "1 +")))
    (is (thrown? Exception (rpn.core/calc "1 2 + *")))
    (is (thrown? Exception (rpn.core/calc "12+*")))))
