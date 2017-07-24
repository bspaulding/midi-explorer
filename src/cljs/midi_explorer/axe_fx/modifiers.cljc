(ns midi-explorer.axe-fx.modifiers)

(def parameters-by-id
  {0x0 :modifier-source
   0x1 :min
   0x2 :max
   0x3 :start
   0x4 :mid
   0x5 :end
   0x6 :slope
   0x7 :damping
   0xA :auto-engage
   0xB :pc-reset
   0xC :off-val
   0xD :scale
   0xE :offset})

(defn modifier-parameter [parameter-id]
  (get parameters-by-id parameter-id))
