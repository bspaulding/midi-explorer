(ns midi-explorer.app
    (:require [rum.core :as rum]
              [axe-fx-midi.core :as axe-fx]
              [axe-fx-midi.models :refer [models]]
              [nightlight.repl-server]))

(defonce state (atom {}))
(add-watch state :log-state (fn [_ _ _ new-state] (.log js/console new-state)))

(defn selected-input-device [state]
  (let [selected-id (get-in state [:midi :selected-input-id])
        devices (get-in state [:midi :inputs])]
    (first (filter #(= (.-id %) selected-id) devices))))

(defn selected-output-device [state]
  (let [selected-id (get-in state [:midi :selected-output-id])
        outputs (get-in state [:midi :outputs])]
    (first (filter #(= (.-id %) selected-id) outputs))))

(defn send-midi-message [fm]
  (fn []
    (let [device (selected-output-device @state)
          model (:ii models)]
      (.send device (clj->js (fm model))))))

(defn onMidiMessage [e]
  (let [msg (es6-iterator-seq (.values (.-data e)))
        parsed-message (axe-fx/parse-message msg)]
    (swap! state assoc (:type parsed-message) parsed-message)
    (.log js/console (.toString (.-data e)))
    (.log js/console parsed-message)))

(defn updateSelectedInputDevice [deviceId]
  (swap! state assoc-in [:midi :selected-input-id] deviceId))

(defn updateSelectedOutputDevice [deviceId]
  (swap! state assoc-in [:midi :selected-output-id] deviceId))

(defn requestMIDIAccessSuccess [midi]
  (let [ins (es6-iterator-seq (.values (.-inputs midi)))
        outs (es6-iterator-seq (.values (.-outputs midi)))]
    (swap! state assoc-in [:midi :inputs] ins)
    (swap! state assoc-in [:midi :outputs] outs)
    (if (nil? (selected-input-device @state))
      (updateSelectedInputDevice (.-id (first ins))))
    (if (nil? (selected-output-device @state))
      (updateSelectedOutputDevice (.-id (first outs))))))

(defn loadMidiDevices []
  (if (exists? js/navigator)
    (.then (.requestMIDIAccess js/navigator #js{"sysex" true})
      requestMIDIAccessSuccess
      #(.log js/console %))))

(add-watch state :reconnect-midi
  (fn [_ _ old-state new-state]
    (let [new-device (selected-input-device new-state)
          old-device (selected-input-device old-state)]
      (if (and
           (and
             (not (nil? new-device))
             (nil? (.-onmidimessage new-device)))
           (not (= new-device old-device)))
        (set! (.-onmidimessage new-device) onMidiMessage)))))

(rum/defc option < { :key-fn (fn [device] (.-id device)) } [device]
  [:option {:value (.-id device)} (str (.-manufacturer device) " - " (.-name device))])

(rum/defc send-button [f label]
  [:button {:style {:width "100%"} :on-click (send-midi-message f)} label])

(rum/defc blocks-grid-item
  [block]
  [:div
    {:style {:flex "0 0 25%"
             :min-height "50px"
             :border-color "black"
             :border-width "1px"
             :border-style "solid"}}
    (str (:effect-name block))])

(rum/defc blocks-grid-column [blocks]
  [:div
    {:style {:display "flex"
             :flex-direction "column"
             :flex-wrap "wrap"
             :justify-content "space-between"}}
    (map-indexed #(rum/with-key (blocks-grid-item %2) %1) blocks)])

(rum/defc blocks-grid [blocks]
  [:div
    {:style {:display "flex"
             :flex-direction "row"}}
    (map-indexed #(rum/with-key (blocks-grid-column %2) %1) (partition 4 blocks))])

(rum/defc app [state]
  [:div
    [:div
     [:button {:on-click loadMidiDevices} "Refresh MIDI"]
     [:select
      {:on-change (fn [e] (updateSelectedInputDevice (.. e -target -value)))}
      (map option (get-in state [:midi :inputs]))]
     [:select
      {:on-change (fn [e] (updateSelectedOutputDevice (.. e -target -value)))}
      (map option (get-in state [:midi :outputs]))]]
   [:div
    {:style {:display "flex" :flex-direction "row"}}
    [:div
     {:style {:flex 1 :flex-direction "column"}}
     [:div "Send Event:"]
     (send-button axe-fx/get-preset-number "Get Preset Number")
     (send-button axe-fx/get-preset-name "Get Preset Name")
     (send-button axe-fx/get-firmware-version "Get Firmware Version")
     (send-button axe-fx/disconnect-from-controller "Disconnect From Controller")
     (send-button axe-fx/get-midi-channel "Get MIDI Channel")
     (send-button (fn [] (axe-fx/tuner-toggle 1 true)) "Tuner Toggle On")
     (send-button (fn [] (axe-fx/tuner-toggle 1 false)) "Tuner Toggle Off")
     (send-button (fn [] (axe-fx/metronome-toggle 1 true)) "Metronome Toggle On")
     (send-button (fn [] (axe-fx/metronome-toggle 1 false)) "Metronome Toggle Off")
     (send-button axe-fx/get-preset-blocks-flags "Get Preset Blocks Flags")
     (send-button axe-fx/get-grid-layout-and-routing "Get Grid Layout And Routing")
     (send-button (fn [model] (axe-fx/get-block-parameters-list model 106)) "Get Amp 1 Block Parameters List")
     (send-button (fn [model] (axe-fx/set-scene-number model 0)) "Set Scene Number 1")
     (send-button (fn [model] (axe-fx/set-scene-number model 1)) "Set Scene Number 2")
     (send-button (fn [model] (axe-fx/set-scene-number model 2)) "Set Scene Number 3")
     (send-button (fn [model] (axe-fx/set-scene-number model 3)) "Set Scene Number 4")]
    [:div
     {:style {:flex 1}}
     "TODO MIDI Log"]]
   [:div (str (get-in state [:get-preset-number :value]) " - " (get-in state [:get-preset-name :value]))]
   [:div (blocks-grid (get-in state [:get-grid-layout-and-routing :blocks] []))]])

(defn render [state]
  (rum/mount (app state) (. js/document (getElementById "container"))))
(add-watch state :render (fn [_ _ _ new-state] (render new-state)))

(defn init []
  (loadMidiDevices)
  (render @state))
