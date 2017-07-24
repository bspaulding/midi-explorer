(ns midi-explorer.axe-fx.parameters-test
  (:require-macros [cljs.test :refer [deftest testing is]])
  (:require [cljs.test :as t]
            [midi-explorer.axe-fx.parameters :refer [parameter-name]]))

(deftest test-parameter-name []
  (is (= (parameter-name 106 0) "Effect Type"))
  (is (= (parameter-name 106 1) "Input Drive"))
  (is (= (parameter-name 106 2) "Bass"))
  (is (= (parameter-name 106 3) "Middle"))
  (is (= (parameter-name 106 4) "Treble"))
  (is (= (parameter-name 106 5) "Master Volume"))
  (is (= (parameter-name 106 6) "Preamp Low Cut"))
  (is (= (parameter-name 106 7) "High Cut Freq"))
  (is (= (parameter-name 106 8) "Tone Freq"))
  (is (= (parameter-name 106 9) "XFormer Grind"))
  (is (= (parameter-name 106 10) "Bright Cap"))
  (is (= (parameter-name 106 12) "XFormer Low Freq"))
  (is (= (parameter-name 106 13) "XFormer Hi Freq"))
  (is (= (parameter-name 106 14) "Tone Location"))
  (is (= (parameter-name 106 15) "Input Select"))
  (is (= (parameter-name 106 16) "Depth"))
  (is (= (parameter-name 106 19) "Supply Sag"))
  (is (= (parameter-name 106 66) "Triode 2 Plate Freq"))
  (is (= (parameter-name 106 84) "Dynamic Presence"))
  (is (= (parameter-name 106 91) "Preamp CF Ratio"))
  (is (= (parameter-name 106 96) "Preamp Sag"))
  (is (= (parameter-name 106 104) "Presence Shift")))

