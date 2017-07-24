(ns midi-explorer.axe-fx.ascii)

(defn string-to-ascii [xs]
  #?(:cljs (map #(.charCodeAt % 0) xs)
     :clj (map (comp byte int) xs)))
