(ns gears-pop-depressifier.core
  (:require [reagent.core :as r]
            [reagent.dom :as rdom]
            [goog.string :as gstring]
            [goog.string.format]
            [alandipert.storage-atom :refer [local-storage]]
            [clojure.string :refer [blank?]]
            [tick.alpha.api :as t]))

(enable-console-print!)

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
(def total-xps (take-last 5 (reductions + xps)))

(defonce all-pins [{:name "spotters" :rarity :common :level 1 :dupes 0}
                   {:name "longshot gear" :rarity :common :level 1 :dupes 0}
                   {:name "scion" :rarity :common :level 1 :dupes 0}
                   {:name "decoy" :rarity :common :level 1 :dupes 0}
                   {:name "stun tracker" :rarity :common :level 1 :dupes 0}
                   {:name "frag grenade" :rarity :common :level 1 :dupes 0}
                   {:name "locust drone" :rarity :common :level 1 :dupes 0}
                   {:name "shepherds" :rarity :common :level 1 :dupes 0}
                   {:name "ink grenade" :rarity :common :level 1 :dupes 0}
                   {:name "ticker" :rarity :common :level 1 :dupes 0}
                   {:name "onyx guards" :rarity :common :level 1 :dupes 0}
                   {:name "shock barrier" :rarity :common :level 1 :dupes 0}
                   {:name "stun grenade" :rarity :common :level 1 :dupes 0}
                   {:name "savage granadier" :rarity :common :level 1 :dupes 0}
                   {:name "juvies" :rarity :rare :level 1 :dupes 0}
                   {:name "sentry" :rarity :rare :level 1 :dupes 0}
                   {:name "power genertor" :rarity :rare :level 1 :dupes 0}
                   {:name "clayton carmine" :rarity :rare :level 1 :dupes 0}
                   {:name "augustus cole" :rarity :rare :level 1 :dupes 0}
                   {:name "boomer" :rarity :rare :level 1 :dupes 0}
                   {:name "wretches" :rarity :rare :level 1 :dupes 0}
                   {:name "bernadette mataki" :rarity :rare :level 1 :dupes 0}
                   {:name "drone division" :rarity :rare :level 1 :dupes 0}
                   {:name "lancer crew" :rarity :rare :level 1 :dupes 0}
                   {:name "gnasher gang" :rarity :rare :level 1 :dupes 0}
                   {:name "del walker" :rarity :rare :level 1 :dupes 0}
                   {:name "deadeye" :rarity :rare :level 1 :dupes 0}
                   {:name "sam byrne" :rarity :epic :level 1 :dupes 0}
                   {:name "marcus fenix" :rarity :epic :level 1 :dupes 0}
                   {:name "dominic santiago" :rarity :epic :level 1 :dupes 0}
                   {:name "grinder" :rarity :epic :level 1 :dupes 0}
                   {:name "hammer of dawn" :rarity :epic :level 1 :dupes 0}
                   {:name "drop pod" :rarity :epic :level 1 :dupes 0}
                   {:name "fahz chutani" :rarity :epic :level 1 :dupes 0}
                   {:name "dr-1" :rarity :epic :level 1 :dupes 0}
                   {:name "damon baird" :rarity :epic :level 1 :dupes 0}
                   {:name "butcher" :rarity :epic :level 1 :dupes 0}
                   {:name "jack" :rarity :epic :level 1 :dupes 0}
                   {:name "kait diaz" :rarity :epic :level 1 :dupes 0}
                   {:name "anya stroud" :rarity :legendary :level 1 :dupes 0}
                   {:name "victor hoffman" :rarity :legendary :level 1 :dupes 0}
                   {:name "mina jinn" :rarity :legendary :level 1 :dupes 0}
                   {:name "myrrah" :rarity :legendary :level 1 :dupes 0}
                   {:name "old man marcus" :rarity :legendary :level 1 :dupes 0}
                   {:name "skorge" :rarity :legendary :level 1 :dupes 0}
                   {:name "general raam" :rarity :legendary :level 1 :dupes 0}
                   {:name "winter kait" :rarity :legendary :level 1 :dupes 0}
                   {:name "lancer gear" :rarity :common :level 1 :dupes 0}
                   {:name "shock grenade" :rarity :common :level 1 :dupes 0}
                   {:name "nemacysts" :rarity :common :level 1 :dupes 0}
                   {:name "snub soldiers" :rarity :common :level 1 :dupes 0}
                   {:name "kantus" :rarity :common :level 1 :dupes 0}
                   {:name "sentinel" :rarity :rare :level 1 :dupes 0}
                   {:name "reyna diaz" :rarity :epic :level 1 :dupes 0}
                   {:name "jd fenix" :rarity :epic :level 1 :dupes 0}
                   {:name "seeder" :rarity :epic :level 1 :dupes 0}
                   {:name "emergency hole" :rarity :epic :level 1 :dupes 0}
                   {:name "windflare" :rarity :epic :level 1 :dupes 0}])
(defonce pins (local-storage (r/atom all-pins) :pins))
(defonce start-date (local-storage (r/atom "") :start-date))
(defonce coins (local-storage (r/atom 0) :coins))

(defn- format-percentage [percentage formatter]
  (str (gstring/format formatter (* 100 percentage)) "%"))

(defn- calc-percentage [p]
  (let [costs ((:rarity p) costs)
        current-pins (+ (reduce + (take (dec (:level p)) (map :dupes costs))) (:dupes p) 1)
        max-pins (reduce + (map :dupes costs))]
    (format-percentage (double (/ current-pins max-pins)) "%.2f")))

(defn- upgradeable [p]
  (let [costs ((:rarity p) costs)]
    (loop [dupes (:dupes p)
           level (:level p)
           coins 0
           xp 0
           c (get costs (dec level))]
      (if (or (< dupes (:dupes c)) (> level (count costs)))
        (if (pos? coins)
          (str "Upgradeable to level " level " with " coins " coins for that sweet " (format-percentage (/ xp (last total-xps)) "%.2f") " extra total progress")
          "")
        (recur (- dupes (:dupes c))
               (inc level)
               (+ coins (:coins c))
               (+ xp (:xp c))
               (get costs level))))))

(defn- current-xp []
  (reduce + (map (fn [p] (reduce + (take (dec (:level p)) (map :xp ((:rarity p) costs))))) @pins)))

(defn- current-coins-used []
  (reduce + (map (fn [p] (reduce + (take (dec (:level p)) (map :coins ((:rarity p) costs))))) @pins)))

(defn- completion-date [progress]
  (let [start-date-time (t/instant (str @start-date "T00:00:00"))
        duration (t/duration
                  {:tick/beginning start-date-time
                   :tick/end (t/instant)})
        total-days (int (/ 1 (/ progress (t/days duration))))]
    (t/format :iso-local-date (t/date (t/+ start-date-time (t/new-duration total-days :days))))))

(defn- next-upgrades [p]
  (let [costs ((:rarity p) costs)]
    (for [l (range (-> p :level dec) (count costs))]
      (let [cost (get costs l)]
        (assoc cost :cx (/ (:coins cost) (:xp cost)))))))

(defn- last-level-estimates [current-xp current-coins]
  (let [r (sort-by :cx (mapcat next-upgrades @pins))
        r2 (reductions (fn [a c] (merge-with + a c)) r)]
    (for [[index xp] (map-indexed vector total-xps)]
      (when (< current-xp xp)
        (let [pin-progress (double (/ current-xp xp))
              coin-progress (double (/ current-coins (:coins (first (filter #(> (:xp %) (- xp current-xp)) r2)))))]
          {:level (+ index 16)
           :pin-progress pin-progress
           :coin-progress coin-progress
           :date (when-not (or (blank? @start-date) (not (pos? pin-progress)) (not (pos? coin-progress)))
                   (completion-date (min pin-progress coin-progress)))})))))

(defn- total-progress []
  (let [current-xp (current-xp)
        current-coins (+ @coins (current-coins-used))
        estimates (last-level-estimates current-xp current-coins)]
    [:div.total-progress
     [:span (str "Pin progress: " (gstring/format "%.2f" (* 100 (-> estimates last :pin-progress))) "%")]
     [:span (str "Coin progress: " (gstring/format "%.2f" (* 100 (-> estimates last :coin-progress))) "%")]
     (when-not (blank? @start-date)
       [:div.estimates
        (for [{:keys [level date] :as est} estimates]
          (when est
            [:span (str "Level " level ": " date)]))])]))

(defn- pin-inputs []
  [:div.pin-inputs
   (doall
    (for [[index pin] (map-indexed vector @pins)]
      (let [progress (calc-percentage pin)
            update (fn [what event] (swap! pins assoc-in [index what] (-> event .-target .-value int)))]
        ^{:key (:name pin)}
        [:div.pin-input
         [:p.pin-name (:name pin)]
         [:label {:for "level"} "Level:"]
         [:input {:type "number"
                  :id "level"
                  :min 1
                  :max 20
                  :value (:level pin)
                  :on-change (partial update :level)}]
         [:label {:for "dupes"} "Pins:"]
         [:input {:type "number"
                  :id "dupes"
                  :min 0
                  :value (:dupes pin)
                  :on-change (partial update :dupes)}]
         [:p progress]
         [:p.upgradeable (upgradeable pin)]])))])

(defn- date-picker []
  [:span.start-date
   [:label {:for "start-date"} "Start date:"]
   [:input {:type "date"
            :id "start-date"
            :value @start-date
            :on-change #(reset! start-date (-> % .-target .-value))}]])

(defn- coin-input []
  [:span.coins
   [:label {:for "coins"} "Coins:"]
   [:input {:type "number"
            :id "coins"
            :value @coins
            :on-change #(reset! coins (-> % .-target .-value int))}]])

(defn- root []
  [:div
   [total-progress]
   [pin-inputs]
   [:div.other-inputs
    [date-picker]
    [coin-input]]])

(defn ^:export run []
  (reset! pins (vec (sort-by
                     (juxt #(condp = (:rarity %) :common 0 :rare 1 :epic 2 :legendary 3) :name)
                     (mapv (comp first second) (group-by :name (concat @pins all-pins))))))
  (rdom/render [root] (js/document.getElementById "app")))