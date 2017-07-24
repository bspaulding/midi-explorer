(ns midi-explorer.axe-fx-test
  (:require-macros [cljs.test :refer [deftest testing is]])
  (:require [cljs.test :as t]
            [midi-explorer.axe-fx :as axe-fx]))

(deftest test-checksum []
  (is (= 0x09 (axe-fx/checksum [0xF0 0x00 0x01 0x74 0x03 0x0F 0xF7]))))

(deftest test-with-checksum []
  (is (= [0xF0 0x00 0x01 0x74 0x03 0x0F 0x09 0xF7] (axe-fx/with-checksum [0xF0 0x00 0x01 0x74 0x03 0x0F 0xF7])))
  (is (= [0xF0 0x00 0x01 0x74 0x03 0x14 18 0xF7] (axe-fx/with-checksum [0xF0 0x00 0x01 0x74 0x03 0x14 0xF7]))))

(deftest test-get-preset-number []
  (let [model (:ii axe-fx/models)]
    (is (= [0xF0 0x00 0x01 0x74 model 0x14 18 0xF7] (axe-fx/get-preset-number model)))))

(deftest test-set-preset-number []
  (let [model (:ii axe-fx/models)]
    (is (= [0xF0 0x00 0x01 0x74 model 0x3C 127 0 69 0xF7]
           (axe-fx/set-preset-number model 127)))
    (is (= [0xF0 0x00 0x01 0x74 model 0x3C 0 1 59 0xF7]
           (axe-fx/set-preset-number model 128)))))

(deftest test-parse-message-preset-number []
  (is (= {:type :get-preset-number :value 235} (axe-fx/parse-message [240 0 1 116 3 20 1 107 120 247])))
  (is (= {:type :get-preset-number :value 236} (axe-fx/parse-message [240 0 1 116 3 20 1 108 121 247]))))

(deftest test-get-preset-name []
  (let [model (:ii axe-fx/models)]
    (is (= [0xF0 0x00 0x01 0x74 model 0x0F 9 0xF7]
           (axe-fx/get-preset-name model)))))

(deftest test-set-preset-name []
  (let [model (:ii axe-fx/models)
        msg [0xF0 0x00 0x01 0x74 model 0x09 97 115 99 105 105 0xF7]]
    (is (= (axe-fx/with-checksum msg)
           (axe-fx/set-preset-name model "ascii")))))

(deftest test-parse-get-preset-name []
  (let [msg [240 0 1 116 3 15 66 83 32 65 67 50 48 32 66 97 115 101 32 32 32 32 32 32 32 32 32 32 32 32 32 32 32 32 32 32 32 0 13 247]]
    (is (= {:type :get-preset-name :value "BS AC20 Base"}
           (axe-fx/parse-message msg)))))

(deftest test-get-firmware-version []
  (let [model (:ii axe-fx/models)]
    (is (= [0xF0 0x00 0x01 0x74 model 0x08 14 0xF7] (axe-fx/get-firmware-version model)))))

(deftest test-parse-message-firmware-version []
  (is (= {:type :get-firmware-version :value "8.2"} (axe-fx/parse-message [240 0 1 116 3 8 0x08 0x02 0 0 0 0 "checksum" 247]))))

(deftest test-disconnect-from-controller []
  (let [model (:ii axe-fx/models)]
    (is (= [0xF0 0x00 0x01 0x74 model 0x42 68 0xF7] (axe-fx/disconnect-from-controller model)))))

(deftest test-parse-front-panel-change-detected []
  (is (= {:type :front-panel-change-detected} (axe-fx/parse-message [240 0 1 116 3 0x21 "checksum" 0xF7]))))

(deftest test-multipurpose-response []
  (is (= {:type :multipurpose-response
          :response-function-id 0x42
          :response-function-type :disconnect-from-controller
          :response-code 0}
         (axe-fx/parse-message [240 0 1 116 3 0x64 0x42 0x00 "checksum" 0xF7]))))

(deftest test-parse-midi-tempo-beat []
  (is (= {:type :midi-tempo-beat} (axe-fx/parse-message [240 0 1 116 3 0x10 0xF7]))))

(deftest test-get-midi-channel []
  (let [model (:ii axe-fx/models)]
    (is (= [240 0 1 116 3 0x17 17 0xF7] (axe-fx/get-midi-channel model)))))

(deftest test-parse-get-midi-channel []
  (is (= {:type :get-midi-channel :value 10} (axe-fx/parse-message [240 0 1 116 3 0x17 9 "checksum" 0xF7]))))

(deftest test-parse-tuner-info []
  (is (= {:type :tuner-info
          :note 1
          :string-number 2
          :tuner-data 63}
         (axe-fx/parse-message [240 0 1 116 3 0x0D 1 2 63 0xF7]))))

(deftest test-tuner-toggle []
  (is (= [176 15 0] (axe-fx/tuner-toggle 1 false)))
  (is (= [177 15 0] (axe-fx/tuner-toggle 2 false)))
  (is (= [176 15 127] (axe-fx/tuner-toggle 1 true))))

(deftest test-metronome-toggle []
  (is (= [176 122 0] (axe-fx/metronome-toggle 1 false)))
  (is (= [177 122 0] (axe-fx/metronome-toggle 2 false)))
  (is (= [176 122 127] (axe-fx/metronome-toggle 1 true))))

(deftest test-get-preset-blocks-flags []
  (is (= [240 0 1 116 3 0x0E 8 0xF7] (axe-fx/get-preset-blocks-flags (:ii axe-fx/models)))))

(deftest test-parse-get-preset-blocks-flags []
  (let [msg [240 0 1 116 3 14 3 74 16 83 6 3 78 24 99 6 2 86 124 39 6 3 94 40 3 7 2 98 48 43 120 2 100 52 51 120 3 102 124 63 120 2 10 125 23 7 3 38 81 115 6 2 52 125 7 120 3 58 125 127 7 247]]
    (is (= {:type :get-preset-blocks-flags
            :blocks-flags [{:is-bypassed false, :xy :x, :cc 37, :effect-id 106, :effect-name "Amp 1"}
                           {:is-bypassed false, :xy :x, :cc 39, :effect-id 108, :effect-name "Cab 1"}
                           {:is-bypassed true, :xy :x, :cc 43, :effect-id 100, :effect-name "Compressor 1"}
                           {:is-bypassed false, :xy :x, :cc 47, :effect-id 112, :effect-name "Delay 1"}
                           {:is-bypassed true, :xy :x, :cc 49, :effect-id 133, :effect-name "Drive 1"}
                           {:is-bypassed true, :xy :x, :cc 50, :effect-id 134, :effect-name "Drive 2"}
                           {:is-bypassed false, :xy :x, :cc 51, :effect-id 135, :effect-name "Enhancer"}
                           {:is-bypassed true, :xy :x, :cc 69, :effect-id 114, :effect-name "Multi Delay 1"}
                           {:is-bypassed false, :xy :x, :cc 83, :effect-id 110, :effect-name "Reverb 1"}
                           {:is-bypassed true, :xy :x, :cc 90, :effect-id 128, :effect-name "Tremolo/Panner 1"}
                           {:is-bypassed false, :xy :x, :cc 93, :effect-id 127, :effect-name "Volume/Pan 1"}]}
           (axe-fx/parse-message msg)))))

(deftest test-set-scene-number []
  (is (= [240 0 1 116 3 41 0 47 247] (axe-fx/set-scene-number (:ii axe-fx/models) 0))))

(deftest test-parse-set-scene-number []
  (is (= {:type :set-scene-number :value 0} (axe-fx/parse-message [240 0 1 116 3 41 0 47 247])))
  (is (= {:type :set-scene-number :value 1} (axe-fx/parse-message [240 0 1 116 3 41 1 47 247])))
  (is (= {:type :set-scene-number :value 7} (axe-fx/parse-message [240 0 1 116 3 41 7 47 247]))))

(deftest test-get-grid-layout-and-routing []
  (is (= [240 0 1 116 3 0x20 38 0xF7] (axe-fx/get-grid-layout-and-routing (:ii axe-fx/models)))))

(deftest test-parse-get-grid-layout-and-routing []
  (let [msg [240 0 1 116 3 32 0 0 0 0 127 0 2 0 0 0 0 0 0 0 0 0 0 0 0 0 100 0 2 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 2 0 0 0 0 0 0 0 0 0 0 0 0 0 5 1 2 0 0 0 0 0 0 0 0 0 0 0 0 0 6 1 2 0 0 0 0 0 0 0 0 0 0 0 0 0 106 0 2 0 0 0 0 0 0 0 0 0 0 0 0 0 108 0 2 0 0 0 0 0 0 0 0 0 0 0 0 0 79 1 2 0 0 0 0 0 0 0 0 0 0 0 0 0 112 0 2 0 0 0 0 0 0 0 0 0 0 0 0 0 114 0 2 0 0 0 0 0 0 0 0 0 0 0 0 0 110 0 2 0 0 0 0 0 0 0 0 0 0 0 0 0 7 1 2 0 0 0 0 0 0 0 0 0 29 247]
        parsed (axe-fx/parse-message msg)
        blocks (:blocks parsed)]
    (is (= :get-grid-layout-and-routing (:type parsed)))
    (is (= 48 (count blocks)))
    (is (= {:effect-id 127
            :effect-name "Volume/Pan 1"
            :connect-row-1 false
            :connect-row-2 true
            :connect-row-3 false
            :connect-row-4 false}
           (second blocks)))
    (is (= {:effect-id 100
            :effect-name "Compressor 1"
            :connect-row-1 false
            :connect-row-2 true
            :connect-row-3 false
            :connect-row-4 false}
           (first (drop 5 blocks))))
    (is (= {:effect-id 128
            :effect-name "Tremolo/Panner 1"
            :connect-row-1 false
            :connect-row-2 true
            :connect-row-3 false
            :connect-row-4 false}
           (first (drop 9 blocks))))
    (is (= {:effect-id 133
            :effect-name "Drive 1"
            :connect-row-1 false
            :connect-row-2 true
            :connect-row-3 false
            :connect-row-4 false}
           (first (drop 13 blocks))))
    (is (= {:effect-id 134
            :effect-name "Drive 2"
            :connect-row-1 false
            :connect-row-2 true
            :connect-row-3 false
            :connect-row-4 false}
           (first (drop 17 blocks))))
    (is (= {:effect-id 106
            :effect-name "Amp 1"
            :connect-row-1 false
            :connect-row-2 true
            :connect-row-3 false
            :connect-row-4 false}
           (first (drop 21 blocks))))
    (is (= {:effect-id 108
            :effect-name "Cab 1"
            :connect-row-1 false
            :connect-row-2 true
            :connect-row-3 false
            :connect-row-4 false}
           (first (drop 25 blocks))))
    (is (= {:effect-id 207
            :effect-name "Shunt"
            :connect-row-1 false
            :connect-row-2 true
            :connect-row-3 false
            :connect-row-4 false}
           (first (drop 29 blocks))))
    (is (= {:effect-id 112
            :effect-name "Delay 1"
            :connect-row-1 false
            :connect-row-2 true
            :connect-row-3 false
            :connect-row-4 false}
           (first (drop 33 blocks))))
    (is (= {:effect-id 114
            :effect-name "Multi Delay 1"
            :connect-row-1 false
            :connect-row-2 true
            :connect-row-3 false
            :connect-row-4 false}
           (first (drop 37 blocks))))
    (is (= {:effect-id 110
            :effect-name "Reverb 1"
            :connect-row-1 false
            :connect-row-2 true
            :connect-row-3 false
            :connect-row-4 false}
           (first (drop 41 blocks))))
    (is (= {:effect-id 135
            :effect-name "Enhancer"
            :connect-row-1 false
            :connect-row-2 true
            :connect-row-3 false
            :connect-row-4 false}
           (first (drop 45 blocks))))))

(deftest test-get-block-parameters-list []
  (let [model (:ii axe-fx/models)]
    (is (= [240 0 1 116 3 0x01 0 1 6 0xF7]
           (axe-fx/get-block-parameters-list model 128)))
    (is (= [240 0 1 116 3 0x01 127 0 120 0xF7]
           (axe-fx/get-block-parameters-list model 127)))
    (is (= [240 0 1 116 3 0x01 1 1 7 0xF7]
           (axe-fx/get-block-parameters-list model 129)))))

(deftest test-parse-get-block-parameters-list []
  (let [msg [240 0 1 116 3 1 106 0 0 0 109 1 0 116 0 0 2 2 65 67 45 50 48 32 49 50 65 88 55 32 66 0 55 247]]
    (is (= {:type :get-block-parameters-list
            :effect-id 106
            :effect-name "Amp 1"
            :parameter-id 0
            :parameter-name "Effect Type"
            :value 237}
           (axe-fx/parse-message msg))))
  (let [msg [240 0 1 116 3 1 106 0 1 0 75 104 0 25 0 0 0 0 50 46 48 52 0 78 247]]
    (is (= {:type :get-block-parameters-list
            :effect-id 106
            :effect-name "Amp 1"
            :parameter-id 1
            :parameter-name "Input Drive"
            :value 13387}
           (axe-fx/parse-message msg))))
  (let [msg [240 0 1 116 3 1 106 0 2 0 110 70 1 49 0 0 0 0 51 46 56 56 0 106 247]]
    (is (= {:type :get-block-parameters-list
            :effect-id 106
            :effect-name "Amp 1"
            :parameter-id 2
            :parameter-name "Bass"
            :value 25454}
           (axe-fx/parse-message msg))))
  (let [msg [240 0 1 116 3 1 106 0 3 0 25 29 3 102 0 0 0 0 56 46 48 55 0 30 247]]
    (is (= {:type :get-block-parameters-list
            :effect-id 106
            :effect-name "Amp 1"
            :parameter-id 3
            :parameter-name "Middle"
            :value 52889}
           (axe-fx/parse-message msg))))
  (let [msg [240 0 1 116 3 1 106 0 4 0 24 51 2 76 0 0 0 0 54 46 48 48 0 20 247]]
    (is (= {:type :get-block-parameters-list
            :effect-id 106
            :effect-name "Amp 1"
            :parameter-id 4
            :parameter-name "Treble"
            :value 39320}
           (axe-fx/parse-message msg))))
  (let [msg [240 0 1 116 3 1 106 0 5 0 126 127 3 127 0 0 0 0 49 48 46 48 48 0 58 247]]
    (is (= {:type :get-block-parameters-list
            :effect-id 106
            :effect-name "Amp 1"
            :parameter-id 5
            :parameter-name "Master Volume"
            :value 65534}
           (axe-fx/parse-message msg))))
  (let [msg [240 0 1 116 3 1 106 0 6 0 0 0 0 0 0 0 0 0 49 48 46 48 32 72 122 0 102 247]]
    (is (= {:type :get-block-parameters-list
            :effect-id 106
            :effect-name "Amp 1"
            :parameter-id 6
            :parameter-name "Preamp Low Cut"
            :value 0}
           (axe-fx/parse-message msg))))
  (let [msg [240 0 1 116 3 1 106 0 7 0 118 50 3 107 0 0 0 0 50 48 48 48 48 32 72 122 0 102 247]]
    (is (= {:type :get-block-parameters-list
            :effect-id 106
            :effect-name "Amp 1"
            :parameter-id 7
            :parameter-name "High Cut Freq"
            :value 55670}
           (axe-fx/parse-message msg))))
  (let [msg [240 0 1 116 3 1 106 0 8 0 71 22 2 69 0 0 0 0 55 48 48 46 48 32 72 122 0 72 247]]
    (is (= {:type :get-block-parameters-list
            :effect-id 106
            :effect-name "Amp 1"
            :parameter-id 8
            :parameter-name "Tone Freq"
            :value 35655}
           (axe-fx/parse-message msg))))
  (let [msg [240 0 1 116 3 1 106 0 9 0 25 51 0 12 0 0 0 0 49 46 48 48 0 93 247]]
    (is (= {:type :get-block-parameters-list
            :effect-id 106
            :effect-name "Amp 1"
            :parameter-id 9
            :parameter-name "XFormer Grind"
            :value 6553}
           (axe-fx/parse-message msg))))
  (let [msg [240 0 1 116 3 1 106 0 10 0 127 105 1 58 0 0 0 0 50 51 53 46 48 32 112 70 0 118 247]]
    (is (= {:type :get-block-parameters-list
            :effect-id 106
            :effect-name "Amp 1"
            :parameter-id 10
            :parameter-name "Bright Cap"
            :value 29951}
           (axe-fx/parse-message msg))))
  (let [msg [240 0 1 116 3 1 106 0 11 0 126 127 3 127 0 0 0 0 52 48 48 48 48 32 72 122 0 61 247]]
    (is (= {:type :get-block-parameters-list
            :effect-id 106
            :effect-name "Amp 1"
            :parameter-id 11
            :parameter-name nil ; "XFormer Low Freq"
            :value 65534}
           (axe-fx/parse-message msg))))
  (let [msg [240 0 1 116 3 1 106 0 12 0 84 87 0 21 0 0 0 0 50 50 46 48 32 72 122 0 123 247]]
    (is (= {:type :get-block-parameters-list
            :effect-id 106
            :effect-name "Amp 1"
            :parameter-id 12
            :parameter-name "XFormer Low Freq"
            :value 11220}
           (axe-fx/parse-message msg))))
  (let [msg [240 0 1 116 3 1 106 0 13 0 51 14 3 98 0 0 0 0 50 52 48 48 48 32 72 122 0 24 247]]
    (is (= {:type :get-block-parameters-list
            :effect-id 106
            :effect-name "Amp 1"
            :parameter-id 13
            :parameter-name "XFormer Hi Freq"
            :value 50995}
           (axe-fx/parse-message msg))))
  (let [msg [240 0 1 116 3 1 106 0 14 0 1 0 0 42 0 0 3 0 80 79 83 84 0 83 247]]
    (is (= {:type :get-block-parameters-list
            :effect-id 106
            :effect-name "Amp 1"
            :parameter-id 14
            :parameter-name "Tone Location"
            :value 1}
           (axe-fx/parse-message msg))))
  (let [msg [240 0 1 116 3 1 106 0 15 0 2 0 0 127 0 0 2 0 83 85 77 32 76 43 82 0 67 247]]
    (is (= {:type :get-block-parameters-list
            :effect-id 106
            :effect-name "Amp 1"
            :parameter-id 15
            :parameter-name "Input Select"
            :value 2}
           (axe-fx/parse-message msg)))))


(deftest test-batch-list-request-start []
  (let [msg [240 0 1 116 3 50 1 117 0 64 247]]
    (is (= {:type :batch-list-request-start}
           (axe-fx/parse-message msg)))))

(deftest test-batch-list-request-complete []
  (let [msg [240 0 1 116 3 51 1 52 247]]
    (is (= {:type :batch-list-request-complete}
           (axe-fx/parse-message msg)))))

(deftest test-get-block-parameter-value []
  (let [msg [240 0 1 116 3 0x02 127 0 60 0 0 0 0 0 71 0xF7]
        model (:ii axe-fx/models)]
    (is (= msg (axe-fx/get-block-parameter-value model 127 60)))))

(deftest test-parse-get-block-parameter-value []
  (let [msg (axe-fx/with-checksum [240 0 1 116 3 0x02 106 0 60 0 127 0 0 0xF7])]
    (is (= {:type :get-set-block-parameter-value
            :effect-id 106
            :effect-name "Amp 1"
            :parameter-id 60
            :parameter-name "GEQ Band 6"
            :value 127}
           (axe-fx/parse-message msg)))))

(deftest test-set-block-parameter-value []
  (let [msg (axe-fx/with-checksum [240 0 1 116 3 0x02 127 0 60 0 0 1 0 1 0xF7])
        model (:ii axe-fx/models)]
    (is (= msg (axe-fx/set-block-parameter-value model 127 60 128)))))

(deftest test-set-tempo []
  (let [model (:ii axe-fx/models)]
    (is (= (axe-fx/set-block-parameter-value model 141 32 132)
           (axe-fx/set-tempo model 132)))))

(deftest test-set-typed-block-parameter-value []
  (let [raw [240 0 1 116 3 0x2E 127 0 60 0 0 0 0 0 0 0xF7]
        msg (axe-fx/with-checksum raw)
        model (:ii axe-fx/models)]
    (is (= msg (axe-fx/set-typed-block-parameter-value model 127 60 0)))))

(deftest test-get-modifier-value []
  (let [raw [240 0 1 116 3 0x07 127 0 10 0 0x1 0 0 0 0 0 0xF7]
        msg (axe-fx/with-checksum raw)
        model (:ii axe-fx/models)]
    (is (= msg (axe-fx/get-modifier-value model 127 10 0x1)))))

(deftest test-parse-get-modifier-value []
  (let [raw [240 0 1 116 3 0x07 127 0 2 0 0x1 0 127 0 0 0 0xF7]
        msg (axe-fx/with-checksum raw)]
    (is (= {:type :get-set-modifier-value
            :effect-id 127
            :effect-name "Volume/Pan 1"
            :parameter-id 2
            :parameter-name "Volume Taper"
            :modifier-selector-id 0x1
            :modifier-selector :min
            :value 127}
           (axe-fx/parse-message msg)))))

(deftest test-set-modifier-value []
  (let [raw [240 0 1 116 3 0x07 127 0 10 0 0x1 0 30 0 0 1 0xF7]
        msg (axe-fx/with-checksum raw)
        model (:ii axe-fx/models)]
    (is (= msg (axe-fx/set-modifier-value model 127 10 0x1 30)))))

(deftest test-midi-looper-status-enable []
  (let [msg (axe-fx/with-checksum [240 0 1 116 3 0x23 1 0xF7])]
    (is (= msg (axe-fx/midi-looper-status-enable (:ii axe-fx/models))))))

(deftest test-midi-looper-status-disable []
  (let [msg (axe-fx/with-checksum [240 0 1 116 3 0x23 0 0xF7])]
    (is (= msg (axe-fx/midi-looper-status-disable (:ii axe-fx/models))))))

(deftest test-parse-midi-looper-status []
  (let [msg (axe-fx/wrap [3 0x23 0 25])]
    (is (= {:type :midi-looper-status
            :record false
            :play false
            :once false
            :overdub false
            :reverse false
            :half false
            :undo false
            :position 25}
           (axe-fx/parse-message msg))))
  (let [msg (axe-fx/wrap [3 0x23 1 0])
        parsed (axe-fx/parse-message msg)]
    (is (= true (get parsed :record))))
  (let [msg (axe-fx/wrap [3 0x23 2 0])
        parsed (axe-fx/parse-message msg)]
    (is (= true (get parsed :play))))
  (let [msg (axe-fx/wrap [3 0x23 4 0])
        parsed (axe-fx/parse-message msg)]
    (is (= true (get parsed :once))))
  (let [msg (axe-fx/wrap [3 0x23 8 0])
        parsed (axe-fx/parse-message msg)]
    (is (= true (get parsed :overdub))))
  (let [msg (axe-fx/wrap [3 0x23 16 0])
        parsed (axe-fx/parse-message msg)]
    (is (= true (get parsed :reverse))))
  (let [msg (axe-fx/wrap [3 0x23 32 0])
        parsed (axe-fx/parse-message msg)]
    (is (= true (get parsed :half))))
  (let [msg (axe-fx/wrap [3 0x23 64 0])
        parsed (axe-fx/parse-message msg)]
    (is (= true (get parsed :undo)))))

(deftest test-get-block-xy []
  (let [msg (axe-fx/wrap [3 0x11 106 0 0 0])]
    (is (= msg (axe-fx/get-block-xy 3 106)))))

(deftest test-set-block-x []
  (let [msg (axe-fx/wrap [3 0x11 106 0 0 1])]
    (is (= msg (axe-fx/set-block-x 3 106)))))

(deftest test-set-block-y []
  (let [msg (axe-fx/wrap [3 0x11 106 0 1 1])]
    (is (= msg (axe-fx/set-block-y 3 106)))))

(deftest test-parse-get-block-xy []
  (let [msg (axe-fx/wrap [3 0x11 106 0 0])]
    (is (= {:type :get-block-xy
            :effect-id 106
            :effect-name "Amp 1"
            :value :x}
           (axe-fx/parse-message msg))))
  (let [msg (axe-fx/wrap [3 0x11 106 0 1])]
    (is (= {:type :get-block-xy
            :effect-id 106
            :effect-name "Amp 1"
            :value :y}
           (axe-fx/parse-message msg)))))

(deftest test-set-block-bypass []
  (let [model (:ii axe-fx/models)
        effect-id 106]
    (is (= (axe-fx/set-block-parameter-value model effect-id 255 1)
           (axe-fx/set-block-bypass model effect-id true)))
    (is (= (axe-fx/set-block-parameter-value model effect-id 255 0)
           (axe-fx/set-block-bypass model effect-id false)))))

(deftest test-get-cpu-usage []
  (is (= (axe-fx/with-checksum [240 0 1 116 3 0x13 0xF7])
         (axe-fx/get-cpu-usage 3))))

(deftest test-parse-get-cpu-usage []
  (let [msg (axe-fx/wrap [3 0x13 0x63])]
    (is (= {:type :get-cpu-usage
            :value 99}
           (axe-fx/parse-message msg)))))
