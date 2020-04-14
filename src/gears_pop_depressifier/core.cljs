(ns gears-pop-depressifier.core
    (:require [reagent.core :as r]
              [reagent.dom :as rdom]
              [goog.string :as gstring]
              [goog.string.format]))

(enable-console-print!)

(defrecord pin [name rarity level dupes])
(defrecord cost [dupes coins xp])

(def costs
  {:common [(cost. 10 50 4)
            (cost. 15 120 5)
            (cost. 23 220 7)
            (cost. 36 350 9)
            (cost. 57 500 12)
            (cost. 90 850 16)
            (cost. 140 1400 22)
            (cost. 230 2400 30)
            (cost. 360 4000 42)
            (cost. 560 7000 58)
            (cost. 890 12000 81)
            (cost. 1400 20000 110)
            (cost. 2200 34000 150)
            (cost. 3600 58000 220)
            (cost. 5600 99000 300)
            (cost. 8900 170000 430)
            (cost. 14100 290000 600)
            (cost. 22400 490000 840)
            (cost. 35500 830000 1180)]
   :rare [(cost. 6 500 6)
          (cost. 10 850 8)
          (cost. 18 1400 11)
          (cost. 30 2400 16)
          (cost. 51 4000 23)
          (cost. 87 7000 34)
          (cost. 150 12000 50)
          (cost. 250 20000 74)
          (cost. 430 34000 110)
          (cost. 740 58000 160)
          (cost. 1300 99000 240)
          (cost. 2200 170000 360)
          (cost. 3700 290000 550)
          (cost. 6300 490000 820)
          (cost. 10800 830000 1230)]
   :epic [(cost. 4 4000 25)
          (cost. 7 7000 37)
          (cost. 13 12000 54)
          (cost. 24 20000 80)
          (cost. 44 34000 110)
          (cost. 81 58000 170)
          (cost. 150 99000 250)
          (cost. 280 170000 380)
          (cost. 520 290000 560)
          (cost. 960 490000 830)
          (cost. 1800 830000 1230)]
   :legendary [(cost. 2 50000 250)
               (cost. 4 80000 300)
               (cost. 10 130000 360)
               (cost. 20 210000 430)
               (cost. 40 330000 510)
               (cost. 90 520000 620)
               (cost. 190 830000 740)]})
(def xps [20 30 50 80 120 180 300 450 700 1100 1700 2600 4100 6400 10000 16000 25000 38000 60000])
(def max-xp (reduce + xps))

(defonce pins (r/atom [(pin. "spotters" :common 13 942)
                       (pin. "longshot gear" :common 14 2908)
                       (pin. "scion" :common 1 578)
                       (pin. "decoy" :common 14 1311)
                       (pin. "stun tracker" :common 14 1933)
                       (pin. "frag grenade" :common 14 4012)
                       (pin. "locust drone" :common 14 716)
                       (pin. "shepherds" :common 14 1144)
                       (pin. "ink grenade" :common 11 5688)
                       (pin. "ticker" :common 2 8977)
                       (pin. "onyx guards" :common 14 436)
                       (pin. "shock barrier" :common 14 949)
                       (pin. "stun grenade" :common 7 7956)
                       (pin. "savage granadier" :common 14 3582)
                       (pin. "juvies" :rare 1 1288)
                       (pin. "sentry" :rare 7 1088)
                       (pin. "power genertor" :rare 10 170)
                       (pin. "clayton carmine" :rare 6 2064)
                       (pin. "augustus cole" :rare 8 1323)
                       (pin. "boomer" :rare 10 94)
                       (pin. "wretches" :rare 10 610)
                       (pin. "bernadette mataki" :rare 10 423)
                       (pin. "drone division" :rare 7 1629)
                       (pin. "lancer crew" :rare 10 330)
                       (pin. "gnasher gang" :rare 9 1205)
                       (pin. "del walker" :rare 9 604)
                       (pin. "deadeye" :rare 10 268)
                       (pin. "sam byrne" :epic 7 52)
                       (pin. "marcus fenix" :epic 2 292)
                       (pin. "dominic santiago" :epic 5 181)
                       (pin. "grinder" :epic 6 20)
                       (pin. "hammer of dawn" :epic 6 29)
                       (pin. "drop pod" :epic 1 211)
                       (pin. "fahz chutani" :epic 1 89)
                       (pin. "dr-1" :epic 6 319)
                       (pin. "damon baird" :epic 6 39)
                       (pin. "butcher" :epic 6 119)
                       (pin. "jack" :epic 6 63)
                       (pin. "kait diaz" :epic 7 118)
                       (pin. "anya stroud" :legendary 3 1)
                       (pin. "victor hoffman" :legendary 1 8)
                       (pin. "mina jinn" :legendary 1 0)
                       (pin. "myrrah" :legendary 3 0)
                       (pin. "old man marcus" :legendary 3 8)
                       (pin. "skorge" :legendary 1 6)
                       (pin. "general raam" :legendary 1 7)
                       (pin. "winter kait" :legendary 3 4)
                       (pin. "lancer gear" :common 13 2163)
                       (pin. "shock grenade" :common 14 1342)
                       (pin. "nemacysts" :common 14 793)
                       (pin. "snub soldiers" :common 15 703)
                       (pin. "kantus" :common 13 1844)
                       (pin. "sentinel" :rare 11 208)
                       (pin. "reyna diaz" :epic 6 71)
                       (pin. "jd fenix" :epic 6 48)
                       (pin. "seeder" :epic 6 40)
                       (pin. "emergency hole" :epic 7 80)
                       (pin. "windflare" :epic 1 24)]))

#_(defn -main []
    (let [result (for [p pins]
                   (let [costs ((:rarity p) costs)
                         current-pins (+ (reduce + (take (- (:level p) 1) (map :dupes costs))) (:dupes p) 1)
                         xp-from-pin (reduce + (take (- (:level p) 1) (map :xp costs)))
                         max-pins (reduce + (map :dupes costs))]
                     [(:name p) current-pins max-pins xp-from-pin]))
          current-xp (reduce + (map last result))]
      (println (format "%.3f" (double (/ current-xp max-xp))))))

(defn- calc-percentage [p]
  (let [costs ((:rarity p) costs)
        current-pins (+ (reduce + (take (- (:level p) 1) (map :dupes costs))) (:dupes p) 1)
        ;xp-from-pin (reduce + (take (- (:level p) 1) (map :xp costs)))
        max-pins (reduce + (map :dupes costs))]
    (gstring/format "%.2f" (* 100 (double (/ current-pins max-pins))))))

(defn pin-inputs []
  [:div.pin-inputs
   (doall
    (for [[index pin] (map-indexed vector @pins)]
      ^{:key (:name pin)}
      [:div.pin-input
       [:p.pin-name (:name pin)]
       [:label {:for "level"} "Level:"]
       [:input {:type "number"
                :id "level"
                :min 1
                :max 20
                :value (:level pin)
                :on-change #(swap! pins assoc-in [index :level] (-> % .-target .-value))}]
       [:label {:for "dupes"} "Pins:"]
       [:input {:type "number"
                :id "dupes"
                :value (:dupes pin)
                :on-change #(swap! pins assoc-in [index :dupes] (-> % .-target .-value))}]
       [:p (calc-percentage pin)]
       [:progress {:id "pin" :value (calc-percentage pin) :max "100"}]]))])

(defn root []
  [:div
   [pin-inputs]])

(defn ^:export run []
  (rdom/render [root] (js/document.getElementById "app")))
