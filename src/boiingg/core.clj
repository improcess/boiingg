(ns boiingg.core
  (:use
   overtone.live
   [clojure.contrib.seq-utils :only [indexed]])

  (:require [polynome.core :as poly])
  (:require [overtone-contrib.core :as contrib]))

(refer-ugens)
(contrib/boot-and-wait)

(def m (poly/init "/dev/tty.usbserial-m64-0790"))

(defsynth beep [freq-mul 1]
  (* (sin-osc (* 110 (+ 1 freq-mul))) (env-gen (perc 0.05 0.05) 1 1 0 1 :free)))

(def metro (metronome 880))

(def state (atom [nil nil nil nil nil nil nil nil]))

(defn schedule-notes
  "If any of the state's columns have reached the 'ground' (i.e. max-y) then trigger the synth with the column id as a param to play at the supplied time"
  [time]
  (let [s @state]
    (doall (for [col (indexed s)]
      (if (= (poly/max-y m) (:current (nth col 1))) (at time (beep (nth col 0))))))))

(defn bounce [col-state]
  (let [max (:max col-state)
        cur (:current col-state)
        falling (:falling col-state)]

    (cond (= (poly/max-y m) cur)    (assoc col-state :current (dec cur) :falling false)
          (= max cur)  (assoc col-state :current (inc cur) :falling true)
          falling      (assoc col-state :current (inc cur) :falling true)
          :else        (assoc col-state :current (dec cur) :falling false))))

(defn update-state []
  (reset! state (vec (map #(if % (bounce %)) @state))))

(defn draw-col [time idx info]
  (poly/led-on-at m time idx (:current info) ))

(defn draw-state [time]
  (let [s (indexed @state)]
  (doall (map #(if (nth % 1) (draw-col time (nth % 0) (nth % 1))) s))))

(defn go [beat]
  (let [time (metro beat)]

    (update-state)

    (schedule-notes time)
    (apply-at poly/clear (- time 5) m)
    (draw-state time)

    (contrib/apply-before #'go (metro (inc beat)) (inc beat))))

(go (metro))

(defn start-falling [col row]
  (if (= (poly/max-y m) row)
    (swap! state assoc col nil)
    (swap! state assoc col {:max row :current row :falling true})))

(poly/on-press m (fn [x y] (start-falling x y)))

;;;;;;;;;;;;;;
;;
;;(defn go [beat] nil)
;;
;;(reset! state  [nil nil nil nil nil nil nil nil])
;;
;;(reset! state [{:max 3 :current 0 :falling true} nil {:max 5 :current 0 :falling true} nil {:max 4 :current 0 :falling true} nil {:max 6 :current 0 :falling true} nil])
;;
;;(reset! state [{:max 6 :current 6 :falling true}  {:max 6 :current 6 :falling true} {:max 6 :current 6 :falling true} {:max 6 :current 6 :falling true} {:max 6 :current 6 :falling true} {:max 6 :current 6 :falling true} {:max 6 :current 6 :falling true} {:max 6 :current 6 :falling true} ])

