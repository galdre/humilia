;; Copyright 2017-2018 Workiva Inc.

(ns humilia.squuid
  (:import (java.util UUID)))

(def ^:const +ub32-mask+ 0x00000000ffffffff)

(defn ^UUID squuid
  ([] (squuid (System/currentTimeMillis)))
  ([msec]
   (let [uuid (UUID/randomUUID)
         sec (quot msec 1000)
         lsb (.getLeastSignificantBits uuid)
         msb (.getMostSignificantBits uuid)
         timed-msb (bit-or (bit-shift-left sec 32)
                           (bit-and +ub32-mask+ msb))]
     (UUID. timed-msb lsb))))

(defn squuid-time-millis [^UUID uuid]
  (let [msb (.getMostSignificantBits uuid)
        sec (bit-shift-right msb 32)]
    (* (long sec) 1000)))
