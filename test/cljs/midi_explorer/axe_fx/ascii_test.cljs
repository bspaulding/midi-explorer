(ns midi-explorer.axe-fx.ascii-test
  (:require-macros [cljs.test :refer [deftest testing is]])
  (:require [cljs.test :as t]
            [midi-explorer.axe-fx.ascii :refer [string-to-ascii]]))

(deftest test-string-to-ascii []
  (is (= '(97 115 99 105 105) (string-to-ascii "ascii"))))
