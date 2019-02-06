;; Copyright 2014 Timothy Dean
;; Copyright 2017-2018 Workiva Inc.

(ns humilia.control
  "Control forms")

(defmacro ?->>
  "Like cond->>, but threads the argument through the conditions as well.
  (?->> 4 even? (assoc {} :four))"
  [expr & clauses]
  (assert (even? (count clauses)))
  (let [g (gensym)
        pstep (fn [[test step]] `(if (->> ~g ~test) (->> ~g ~step) ~g))]
    `(let [~g ~expr
           ~@(interleave (repeat g) (map pstep (partition 2 clauses)))]
       ~g)))

(defmacro ?->
  "Like cond->, but threads the argument through the conditions as well.
  (?-> 4 even? inc odd? inc neg? inc)"
  [expr & clauses]
  (assert (even? (count clauses)))
  (let [g (gensym)
        pstep (fn [[test step]] `(if (-> ~g ~test) (-> ~g ~step) ~g))]
    `(let [~g ~expr
           ~@(interleave (repeat g) (map pstep (partition 2 clauses)))]
       ~g)))

(defmacro ->>?->
  "Like ?->, but the conditions are threaded with ->> and the results are
  threaded with ->.
  (->>?-> 4 (> 2) inc (> 6) dec)"
  [expr & clauses]
  (assert (even? (count clauses)))
  (let [g (gensym)
        pstep (fn [[test step]] `(if (->> ~g ~test) (-> ~g ~step) ~g))]
    `(let [~g ~expr
           ~@(interleave (repeat g) (map pstep (partition 2 clauses)))]
       ~g)))

(defmacro ->?->>
  "Like cond->, but threads the argument through the conditions as well.
  (->?->> 4 (> 2) inc (> 6) dec)"
  [expr & clauses]
  (assert (even? (count clauses)))
  (let [g (gensym)
        pstep (fn [[test step]] `(if (-> ~g ~test) (->> ~g ~step) ~g))]
    `(let [~g ~expr
           ~@(interleave (repeat g) (map pstep (partition 2 clauses)))]
       ~g)))
