(ns midi-explorer.axe-fx
  (:require [clojure.string :refer [trimr]]
            [midi-explorer.axe-fx.parameters :refer [parameter-name]]
            [midi-explorer.axe-fx.modifiers :refer [modifier-parameter]]
            [midi-explorer.axe-fx.ascii :refer [string-to-ascii]]
            [midi-explorer.axe-fx.encoders :refer [encode-effect-id encode-parameter-value encode-typed-parameter-value]]
            [midi-explorer.axe-fx.models :refer [models]]
            [midi-explorer.axe-fx.blocks :refer [name-for-effect-id]]))

(def header [0xF0 0x00 0x01 0x74])

(defn checksum [msg]
  (bit-and 0x7F (apply bit-xor (take (- (count msg) 1) msg))))

(defn with-checksum [msg]
  (let [msg-without-term (take (- (count msg) 1) msg)
        term (first (drop (- (count msg) 1) msg))]
    (into [] (concat msg-without-term [(checksum msg) term]))))

(defn wrap
  "Adds header, checksum and term to msg"
  [msg]
  (with-checksum (concat header msg [0xF7])))

(defn get-preset-number [model]
  (wrap [model 0x14]))

(defn set-preset-number [model n]
  (let [[a b] (encode-effect-id n)]
    (wrap [model 0x3C a b])))

(defn get-preset-name [model]
  (wrap [model 0x0F]))

(defn set-preset-name [model name]
  (wrap (concat [model 0x09] (string-to-ascii name))))

(defn get-firmware-version [model]
  (wrap [model 0x08]))

(defn disconnect-from-controller [model]
  (wrap [model 0x42]))

(defn get-midi-channel [model]
  (wrap [model 0x17]))

(defn tuner-toggle [mchan on]
  [(+ 176 (- mchan 1)) 15 (if on 127 0)])

(defn metronome-toggle [mchan on]
  [(+ 176 (- mchan 1)) 122 (if on 127 0)])

(defn get-preset-blocks-flags [model]
  (wrap [model 0x0E]))

(defn set-scene-number [model scene]
  (wrap [model 0x29 scene]))

(defn get-grid-layout-and-routing [model]
  (wrap [model 0x20]))

(defn get-block-parameters-list [model block-id]
  (let [[a b] (encode-effect-id block-id)]
    (wrap [model 0x01 a b])))

(defn get-block-parameter-value [model effect-id parameter-id]
  (let [[a b] (encode-effect-id effect-id)
        [c d] (encode-effect-id parameter-id)]
    (wrap [model 0x02 a b c d 0 0 0 0])))

(defn set-block-parameter-value [model effect-id parameter-id value]
  (let [[a b] (encode-effect-id effect-id)
        [c d] (encode-effect-id parameter-id)
        [e f g] (encode-parameter-value value)]
    (wrap [model 0x02 a b c d e f g 1])))

(defn set-tempo [model bpm]
  (set-block-parameter-value model 141 32 bpm))

(defn set-typed-block-parameter-value
  [model effect-id parameter-id value]
  (let [[a b] (encode-effect-id effect-id)
        [c d] (encode-effect-id parameter-id)
        [e f g h i] (encode-typed-parameter-value value)]
    (wrap [model 0x2E a b c d e f g h i])))

(defn get-modifier-value
  [model effect-id parameter-id selector-id]
  (let [[a b] (encode-effect-id effect-id)
        [c d] (encode-effect-id parameter-id)]
    (wrap [model 0x07 a b c d selector-id 0 0 0 0 0])))

(defn set-modifier-value
  [model effect-id parameter-id selector-id value]
  (let [[a b] (encode-effect-id effect-id)
        [c d] (encode-effect-id parameter-id)
        [e f g] (encode-parameter-value value)]
    (wrap [model 0x07 a b c d selector-id 0 e f g 1])))

(defn midi-looper-status-enable [model]
  (wrap [model 0x23 1]))

(defn midi-looper-status-disable [model]
  (wrap [model 0x23 0]))

(defn get-block-xy [model effect-id]
  (let [[a b] (encode-effect-id effect-id)]
    (wrap [model 0x11 a b 0 0])))

(defn set-block-x [model effect-id]
  (let [[a b] (encode-effect-id effect-id)]
    (wrap [model 0x11 a b 0 1])))

(defn set-block-y [model effect-id]
  (let [[a b] (encode-effect-id effect-id)]
    (wrap [model 0x11 a b 1 1])))

(def symbols-by-function-id
  {0x01 :get-block-parameters-list
   0x02 :get-set-block-parameter-value
   0x07 :get-set-modifier-value
   8 :get-firmware-version
   20 :get-preset-number
   0x0D :tuner-info
   0x0E :get-preset-blocks-flags
   0x0F :get-preset-name
   0x10 :midi-tempo-beat
   0x11 :get-block-xy
   0x13 :get-cpu-usage
   0x17 :get-midi-channel
   0x20 :get-grid-layout-and-routing
   0x21 :front-panel-change-detected
   0x23 :midi-looper-status
   0x29 :set-scene-number
   0x2A :get-preset-edited-status
   0x32 :batch-list-request-start
   0x33 :batch-list-request-complete
   0x42 :disconnect-from-controller
   0x64 :multipurpose-response})

(defn symbol-for-function-id [function-id]
  (get symbols-by-function-id function-id :unknown))

(defn decode-preset-number
  "(lsb & 0x7F) << 7 | rsb"
  [lsb rsb]
  (bit-or (bit-shift-left (bit-and lsb 0x7F) 7) rsb))

(defn decode-preset-name [bytes]
  (let [chars (take-while #(not (= 0 %)) bytes)]
    (trimr (apply str (map char chars)))))

(defn decode-firmware-version [major minor]
  (str major "." minor))

(defn decode-preset-blocks-flags-chunk [a b c d e]
  (let [is-bypassed (not (or (= a 3) (= a 1)))
        is-x (or (= a 3) (= a 2))
        cc (+ (bit-shift-right (bit-and b 0x7E) 1) (bit-shift-left (bit-and c 3) 6))
        effect-id (+ (bit-shift-right (bit-and d 0x78) 3) (bit-shift-left (bit-and e 0x0F) 4))]
    {:is-bypassed is-bypassed
     :xy (if is-x :x :y)
     :cc cc
     :effect-id effect-id
     :effect-name (name-for-effect-id effect-id)}))

(defn decode-preset-blocks-flags [msg]
  (let [chunks (partition 5 msg)]
    {:blocks-flags (into [] (map #(apply decode-preset-blocks-flags-chunk %) chunks))}))

(defn decode-effect-id [a b]
  (bit-or (bit-and a 0x7F) (bit-shift-left (bit-and b 0x7F) 7)))

(defn decode-grid-layout-and-routing-chunk [a b c d]
  (let [effect-id (decode-effect-id a b)]
    {:effect-id effect-id
     :effect-name (name-for-effect-id effect-id)
     :connect-row-1 (not (= 0 (bit-and c 1)))
     :connect-row-2 (not (= 0 (bit-and c 2)))
     :connect-row-3 (not (= 0 (bit-and c 4)))
     :connect-row-4 (not (= 0 (bit-and c 8)))}))

(defn decode-grid-layout-and-routing [msg]
  (let [chunks (partition 4 msg)]
    {:blocks (into [] (map #(apply decode-grid-layout-and-routing-chunk %) chunks))}))

(defn decode-parameter-value [a b c]
  (bit-or
    (bit-and a 0x7F)
    (bit-shift-left (bit-and b 0x7F) 7)
    (bit-shift-left (bit-and c 0x7F) 14)))

(defn decode-block-parameters-list [msg]
  (let [effect-id (apply decode-effect-id (take 2 msg))
        parameter-id (apply decode-effect-id (take 2 (drop 2 msg)))
        value (apply decode-parameter-value (take 3 (drop 4 msg)))]
    {:effect-id effect-id
     :effect-name (name-for-effect-id effect-id)
     :parameter-id parameter-id
     :parameter-name (parameter-name effect-id parameter-id)
     :value value}))

(defn decode-block-parameter-value [a b c d e f g]
  (let [effect-id (decode-effect-id a b)
        parameter-id (decode-effect-id c d)
        value (decode-parameter-value e f g)]
    {:effect-id effect-id
     :effect-name (name-for-effect-id effect-id)
     :parameter-id parameter-id
     :parameter-name (parameter-name effect-id parameter-id)
     :value value}))

(defn decode-get-set-modifier-value [a b c d e _ f g h]
  (let [effect-id (decode-effect-id a b)
        parameter-id (decode-effect-id c d)
        selector-id e
        value (decode-parameter-value f g h)]
    {:effect-id effect-id
     :effect-name (name-for-effect-id effect-id)
     :parameter-id parameter-id
     :parameter-name (parameter-name effect-id parameter-id)
     :modifier-selector-id selector-id
     :modifier-selector (modifier-parameter selector-id)
     :value value}))

(defn decode-midi-looper-status [status position]
  (let [record (= 1 (bit-and status 1))
        play (= 2 (bit-and status 2))
        once (= 4 (bit-and status 4))
        overdub (= 8 (bit-and status 8))
        reverse (= 16 (bit-and status 16))
        half (= 32 (bit-and status 32))
        undo (= 64 (bit-and status 64))]
    {:position position
     :record record
     :play play
     :once once
     :overdub overdub
     :reverse reverse
     :half half
     :undo undo}))

(defn decode-get-block-xy [a b c]
  (let [effect-id (decode-effect-id a b)
        value (if (= 1 c) :y :x)]
    {:effect-id effect-id
     :effect-name (name-for-effect-id effect-id)
     :value value}))

(defn set-block-bypass [model effect-id bypass]
  (let [value (if bypass 1 0)]
    (set-block-parameter-value model effect-id 255 value)))

(defn get-cpu-usage [model]
  (wrap [model 0x13]))

(defn get-preset-edited-status [model]
  (wrap [model 0x2A]))

(defn set-target-block [model effect-id]
  (let [[a b] (encode-effect-id effect-id)]
    (wrap [model 0x37 a b])))

(defn payload-for-msg [type msg]
  (case type
    :get-preset-number {:value (apply decode-preset-number (take 2 (drop 6 msg)))}
    :get-preset-name {:value (decode-preset-name (drop 6 msg))}
    :get-firmware-version {:value (apply decode-firmware-version (take 2 (drop 6 msg)))}
    :front-panel-change-detected {}
    :multipurpose-response {:response-function-id (first (drop 6 msg))
                            :response-function-type (symbol-for-function-id (first (drop 6 msg)))
                            :response-code (first (drop 7 msg))}
    :midi-tempo-beat {}
    :get-midi-channel {:value (+ 1 (first (drop 6 msg)))}
    :tuner-info {:note (first (drop 6 msg))
                 :string-number (first (drop 7 msg))
                 :tuner-data (first (drop 8 msg))}
    :set-scene-number {:value (first (drop 6 msg))}
    :get-preset-blocks-flags (decode-preset-blocks-flags (drop 6 msg))
    :get-grid-layout-and-routing (decode-grid-layout-and-routing (drop 6 msg))
    :batch-list-request-start {}
    :batch-list-request-complete {}
    :get-block-parameters-list (decode-block-parameters-list (drop 6 msg))
    :get-set-block-parameter-value (apply decode-block-parameter-value (take 7 (drop 6 msg)))
    :get-set-modifier-value (apply decode-get-set-modifier-value (drop 6 msg))
    :midi-looper-status (apply decode-midi-looper-status (take 2 (drop 6 msg)))
    :get-block-xy (apply decode-get-block-xy (take 3 (drop 6 msg)))
    :get-cpu-usage {:value (first (drop 6 msg))}
    :get-preset-edited-status {:value (= 1 (first (drop 6 msg)))}
    {:msg msg}))

(defn parse-message
  "Takes a vector of bytes as a message. Returns a map of message info."
  [msg]
  (let [function-id (first (drop 5 msg))
        type (symbol-for-function-id function-id)]
    (conj {:type type} (payload-for-msg type msg))))
