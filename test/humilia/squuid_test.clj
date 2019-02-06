;; Copyright 2017-2018 Workiva Inc.

(ns humilia.squuid-test
  (:require [humilia.squuid :refer :all]
            [clojure.test :refer :all]))

(deftest squuid-construction-and-time-extraction
  (let [msec (System/currentTimeMillis)
        uuid (squuid msec)
        expected-ms (* 1000 (quot msec 1000))]
    (is (= expected-ms (squuid-time-millis uuid)))

    (let [s1 (squuid)
          _ (Thread/sleep 1000)
          s2 (squuid)]

      (is (= -1 (compare s1 s2)))
      (is (= 1 (compare s2 s1))))))
