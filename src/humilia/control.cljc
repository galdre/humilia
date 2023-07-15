;; Copyright 2014 Timothy Dean
;; Copyright 2017-2018 Workiva Inc.

(ns humilia.control
  "Control forms"
  #?(:cljs (:require-macros humilia.control)))

#?(:clj
   (defmacro ?->>
     "Like cond->>, but threads the argument through the conditions as well.
  (?->> 4 even? (assoc {} :four))"
     [expr & clauses]
     (assert (even? (count clauses)))
     (let [g (gensym)
           pstep (fn [[test step]] `(if (->> ~g ~test) (->> ~g ~step) ~g))]
       `(let [~g ~expr
              ~@(interleave (repeat g) (map pstep (partition 2 clauses)))]
          ~g))))

#?(:clj
   (defmacro ?->
     "Like cond->, but threads the argument through the conditions as well.
  (?-> 4 even? inc odd? inc neg? inc)"
     [expr & clauses]
     (assert (even? (count clauses)))
     (let [g (gensym)
           pstep (fn [[test step]] `(if (-> ~g ~test) (-> ~g ~step) ~g))]
       `(let [~g ~expr
              ~@(interleave (repeat g) (map pstep (partition 2 clauses)))]
          ~g))))

#?(:clj
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
          ~g))))

#?(:clj
   (defmacro ->?->>
     "Like cond->, but threads the argument through the conditions as well.
  (->?->> 4 (> 2) inc (> 6) dec)"
     [expr & clauses]
     (assert (even? (count clauses)))
     (let [g (gensym)
           pstep (fn [[test step]] `(if (-> ~g ~test) (->> ~g ~step) ~g))]
       `(let [~g ~expr
              ~@(interleave (repeat g) (map pstep (partition 2 clauses)))]
          ~g))))

#?(:clj
   (defmacro guarded-let
     "Binds symbols as in let. If an exception is thrown within the
  block of bindings, all symbols already successfully bound will
  have guard-fn called on them, in reverse order, for side-effects."
     [guard-fn bindings & body]
     (let [stack (gensym)
           transformation (fn [[sym init]]
                            (conj (if (= sym '_)
                                    ()
                                    (list '_ `(vswap! ~stack conj ~sym)))
                                  init sym))
           bindings (sequence (comp (partition-all 2)
                                    (mapcat transformation))
                              bindings)
           body (cons `(vswap! ~stack empty) body)]
       `(let [~stack (volatile! ())
              guard-fn# ~guard-fn]
          (try
            (let ~(vec bindings)
              ~@body)
            (finally
              (doseq [item# @~stack]
                (guard-fn# item#))))))))
