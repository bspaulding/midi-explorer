(ns midi-explorer.axe-fx.encoders)

(defn encode-effect-id [block-id]
  (let [a (bit-and block-id 0x7F)
        b (bit-and (bit-shift-right block-id 7) 0x7F)]
    [a b]))

(defn encode-parameter-value [x]
  (let [a (bit-and x 0x7F)
        b (bit-and (bit-shift-right x 7) 0x7F)
        c (bit-and (bit-shift-right x 14) 0x7F)]
    [a b c]))

(defn encode-typed-parameter-value [x]
  (let [a (bit-and x 0x7F)
        b (bit-and (bit-shift-right x 7) 0x7F)
        c (bit-and (bit-shift-right x 14) 0x7F)
        d (bit-and (bit-shift-right x 21) 0x7F)
        e (bit-and (bit-shift-right x 28) 0x7F)]
    [a b c d e]))
