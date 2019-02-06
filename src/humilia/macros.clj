;; Copyright 2017-2018 Workiva Inc.

(ns humilia.macros
  (:require [backtick :refer [resolve-symbol]]))

(defn- class-sym*
  [sym]
  (try
    (-> sym resolve-symbol name Class/forName)
    (catch Throwable e)))

(defn class-sym?
  [sym]
  (boolean (class-sym* sym)))

(defn sym->class
  [sym]
  (if-let [class (class-sym* sym)]
    class
    (throw (IllegalArgumentException. (format "Unable to resolve class-name: %s" (pr-str sym))))))

(defmacro when-class
  "When the class can be found, this expands to the body; when not, it suppresses it."
  [class & body]
  (when (class-sym? class)
    `(do ~@body)))

(defmacro if-class
  "When the class can be found, this expands to the form in the `then` clause;
  when not, it expands to the form in the `else` clause."
  [class then else]
  (if (class-sym? class) then else))
