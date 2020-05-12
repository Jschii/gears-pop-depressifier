(ns gears-pop-depressifier.core
  (:require [reagent.core :as r]
            [reagent.dom :as rdom]
            [goog.string :as gstring]
            [goog.string.format]
            [alandipert.storage-atom :refer [local-storage]]
            [clojure.string :refer [blank? upper-case]]
            [clojure.set :refer [rename-keys]]
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
(def total-xps (reductions + xps))
(defn- total-pins [rarity level]
  (if (< level 2)
    level
    (let [dupes (map :dupes (rarity costs))]
      (if (> (dec level) (count dupes))
        0
        (inc (nth (reductions + dupes) (- level 2)))))))

(defonce all-pins [{:name "spotters" :rarity :common}
                   {:name "longshot gear" :rarity :common}
                   {:name "scion" :rarity :common}
                   {:name "decoy" :rarity :common}
                   {:name "stun tracker" :rarity :common}
                   {:name "frag grenade" :rarity :common}
                   {:name "locust drone" :rarity :common}
                   {:name "shepherds" :rarity :common}
                   {:name "ink grenade" :rarity :common}
                   {:name "ticker" :rarity :common}
                   {:name "onyx guards" :rarity :common}
                   {:name "shock barrier" :rarity :common}
                   {:name "stun grenade" :rarity :common}
                   {:name "savage grenadier" :rarity :common}
                   {:name "juvies" :rarity :rare}
                   {:name "sentry" :rarity :rare}
                   {:name "power genertor" :rarity :rare}
                   {:name "clayton carmine" :rarity :rare}
                   {:name "augustus cole" :rarity :rare}
                   {:name "boomer" :rarity :rare}
                   {:name "wretches" :rarity :rare}
                   {:name "bernadette mataki" :rarity :rare}
                   {:name "drone division" :rarity :rare}
                   {:name "lancer crew" :rarity :rare}
                   {:name "gnasher gang" :rarity :rare}
                   {:name "del walker" :rarity :rare}
                   {:name "deadeye" :rarity :rare}
                   {:name "sam byrne" :rarity :epic}
                   {:name "marcus fenix" :rarity :epic}
                   {:name "dominic santiago" :rarity :epic}
                   {:name "grinder" :rarity :epic}
                   {:name "hammer of dawn" :rarity :epic}
                   {:name "drop pod" :rarity :epic}
                   {:name "fahz chutani" :rarity :epic}
                   {:name "dr-1" :rarity :epic}
                   {:name "damon baird" :rarity :epic}
                   {:name "butcher" :rarity :epic}
                   {:name "jack" :rarity :epic}
                   {:name "kait diaz" :rarity :epic}
                   {:name "anya stroud" :rarity :legendary}
                   {:name "victor hoffman" :rarity :legendary}
                   {:name "mina jinn" :rarity :legendary}
                   {:name "myrrah" :rarity :legendary}
                   {:name "old man marcus" :rarity :legendary}
                   {:name "skorge" :rarity :legendary}
                   {:name "general raam" :rarity :legendary}
                   {:name "winter kait" :rarity :legendary}
                   {:name "lancer gear" :rarity :common}
                   {:name "shock grenade" :rarity :common}
                   {:name "nemacysts" :rarity :common}
                   {:name "snub soldiers" :rarity :common}
                   {:name "kantus" :rarity :common}
                   {:name "sentinel" :rarity :rare}
                   {:name "reyna diaz" :rarity :epic}
                   {:name "jd fenix" :rarity :epic}
                   {:name "seeder" :rarity :epic}
                   {:name "emergence hole" :rarity :epic}
                   {:name "windflare" :rarity :epic}
                   {:name "gabe diaz" :rarity :legendary}])
(defonce pins-with-ids (for [[index pin] (map-indexed vector all-pins)]
                         (assoc pin :id index :level 0 :dupes 0)))
(defonce pins (local-storage (r/atom pins-with-ids) :pins-v2))
(defonce start-date (local-storage (r/atom "") :start-date))
(defonce coins (local-storage (r/atom 0) :coins))
(defonce target-level (r/atom 20))

(defn- pins-of-rarity [rarity]
  (count (filter #(= (:rarity %) rarity) all-pins)))

(defn- format-percentage [percentage formatter]
  (str (gstring/format formatter (* 100 percentage)) "%"))

(defn- calc-percentage [{:keys [rarity level dupes]}]
  (let [costs (rarity costs)
        current-pins (+ (total-pins rarity level) dupes)
        max-pins (inc (reduce + (map :dupes costs)))]
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
          (str (- (:dupes c) dupes) " more needed for next level"))
        (recur (- dupes (:dupes c))
               (inc level)
               (+ coins (:coins c))
               (+ xp (:xp c))
               (get costs level))))))

(defn- current-xp []
  (reduce + (map (fn [p] (reduce + (take (dec (:level p)) (map :xp ((:rarity p) costs))))) @pins)))

(defn- current-coins-used []
  (reduce + (map (fn [p] (reduce + (take (dec (:level p)) (map :coins ((:rarity p) costs))))) @pins)))

(defn- days-played []
  (when-not (blank? @start-date)
    (let [start-date-time (t/instant (str @start-date "T00:00:00"))
          duration (t/duration
                    {:tick/beginning start-date-time
                     :tick/end (t/instant)})]
      (t/days duration))))

(defn- completion-date [days]
  (t/format :iso-local-date (t/date (t/+ (t/instant) (t/new-duration days :days)))))

(defn- next-upgrades [[commons-per-day rares-per-day epics-per-day legendaries-per-day coins-per-day] p]
  (let [costs ((:rarity p) costs)
        owned (+ (reduce + (map :dupes (take (max (-> p :level dec) 0) costs)))
                 (:dupes p))
        needed-for-level (fn [level]
                           (reduce + (map :dupes (take (inc level) costs))))
        cpd (/ commons-per-day (pins-of-rarity :common))
        rpd (/ rares-per-day (pins-of-rarity :rare))
        epd (/ epics-per-day (pins-of-rarity :epic))
        lpd (/ legendaries-per-day (pins-of-rarity :legendary))]
    (loop [dupes-used 0
           level-range (range (max (-> p :level dec) 0) (count costs))
           result []]
      (if-let [cost (get costs (first level-range))]
        (let [missing (max (- (needed-for-level (first level-range)) owned) 0)]
          (recur (+ dupes-used (:dupes cost))
                 (next level-range)
                 (conj result (assoc cost
                                     :cx (/ (/ (:coins cost) coins-per-day) (:xp cost))
                                     :px (/ (/ missing
                                               (condp = (:rarity p)
                                                 :common cpd
                                                 :rare rpd
                                                 :epic epd
                                                 :legendary lpd))
                                            (:xp cost))
                                     :pin-name (:name p)
                                     :pin-level (+ (first level-range) 2)
                                     :missing missing
                                     :rarity (:rarity p)
                                     :id (:id p)))))
        result))))

(defn- current-pins [rarity]
  (reduce + (map (fn [{:keys [rarity level dupes]}]
                   (+ (total-pins rarity level) dupes))
                 (filter #(= rarity (:rarity %)) @pins))))

(defn- per-day [current-coins]
  (if-let [days-played (days-played)]
    [(/ (current-pins :common) days-played)
     (/ (current-pins :rare) days-played)
     (/ (current-pins :epic) days-played)
     (/ (current-pins :legendary) days-played)
     (/ current-coins days-played)]
    [nil nil nil nil nil]))

(defn- missing [r]
  (reduce + (map :missing r)))

(defn- pin-progress [r [commons-per-day rares-per-day epics-per-day legendaries-per-day coins-per-day]]
  (let [most-missing-of-rarity (fn [rarity]
                                 (apply max (map :missing (rarity r))))
        rarity-progress (fn [rarity per-day]
                          (/ (most-missing-of-rarity rarity) (/ per-day (pins-of-rarity rarity))))]
    (max (rarity-progress :common commons-per-day)
         (rarity-progress :rare rares-per-day)
         (rarity-progress :epic epics-per-day)
         (rarity-progress :legendary legendaries-per-day))))

(defn- sort-upgrades [next-upgrades]
  (loop [result []
         remaining next-upgrades
         coins @coins
         coin 0
         pin 0]
    (if (empty? remaining)
      result
      (let [[fst & rst] (sort-by :sort-value < (map (fn [p]
                                                      (assoc p :sort-value (max (:px p) 
                                                                                pin
                                                                                (+ coin (:cx p))))) remaining))
            coins-remaining (- coins (:coins fst))]
        (recur (conj result (assoc fst :coins-missing coins-remaining))
               rst
               coins-remaining
               (if (pos? coins-remaining) coin (+ coin (:cx fst)))
               (max (:px fst) pin))))))

(defn- last-level-estimates [current-xp current-coins]
  (let [per-day (per-day current-coins)
        [commons-per-day rares-per-day epics-per-day legendaries-per-day coins-per-day] per-day
        sorted-upgrades (sort-upgrades (mapcat (partial next-upgrades per-day) @pins))
        requirements (reductions + (map :xp sorted-upgrades))
        required (fn [xp-required] (ffirst (filter (fn [[_ xp]] (> xp xp-required)) (map-indexed vector requirements))))
        xp (if (and (> @target-level 1) (<= @target-level 20))
             (nth total-xps (- @target-level 2))
             0)
        xp-progress (double (/ current-xp (last total-xps)))]
    (if (< current-xp xp)
      (let [xp-required (- xp current-xp)
            needed-for-level (take (required xp-required) sorted-upgrades)
            coins-missing (max (-> needed-for-level last :coins-missing -) 0)
            rarity-groups (into {} (map (fn [[rarity ps]]
                                          {rarity
                                           (map (fn [[pin-name pins-for-id]]
                                                  {:pin-name pin-name
                                                   :missing (apply max (map :missing pins-for-id))
                                                   :level (apply max (map :pin-level pins-for-id))
                                                   :cost (reduce + (map :coins (filter #(pos? (:missing %)) pins-for-id)))})
                                                (group-by :pin-name ps))})
                                        (group-by :rarity needed-for-level)))
            path (flatten (flatten (vals rarity-groups)))
            commons-missing (missing (:common rarity-groups))
            rares-missing (missing (:rare rarity-groups))
            epics-missing (missing (:epic rarity-groups))
            legendaries-missing (missing (:legendary rarity-groups))
            progress (int (max (/ coins-missing coins-per-day)
                               (pin-progress rarity-groups per-day)))]
        {:xp-progress xp-progress
         :path path
         :coins-required coins-missing
         :commons-required commons-missing
         :rares-required rares-missing
         :epics-required epics-missing
         :legendaries-required legendaries-missing
         :commons-per-day commons-per-day
         :rares-per-day rares-per-day
         :epics-per-day epics-per-day
         :legendaries-per-day legendaries-per-day
         :coins-per-day coins-per-day
         :date (when (pos? progress)
                 (completion-date progress))})
      {:xp-progress xp-progress
       :commons-per-day commons-per-day
       :rares-per-day rares-per-day
       :epics-per-day epics-per-day
       :legendaries-per-day legendaries-per-day
       :coins-per-day coins-per-day})))

(defn- total-progress [{xp :xp-progress commons :commons-per-day rares :rares-per-day epics :epics-per-day legendaries :legendaries-per-day coins :coins-per-day}]
  [:div.total-progress
   [:span (str "XP progress: " (gstring/format "%.2f" (* 100 xp)) "%")]
   (when (and commons rares epics legendaries coins)
     [:span
      [:span (str (gstring/format "%.2f" commons) " commons, ")]
      [:span (str (gstring/format "%.2f" rares) " rares, ")]
      [:span (str (gstring/format "%.2f" epics) " epics, ")]
      [:span (str (gstring/format "%.2f" legendaries) " legendaries and ")]
      [:span (str (gstring/format "%.2f" coins) " coins per day")]])])

(defn- path [{:keys [date path coins-required commons-required rares-required epics-required legendaries-required] :as est}]
  [:div.estimates
   [:label {:for "level"} "Target level:"]
   [:input {:type "number"
            :id "level"
            :min 2
            :max 20
            :value @target-level
            :on-change #(reset! target-level (-> % .-target .-value int))}]
   (when-not (or (blank? @start-date) (nil? est))
     [:span
      [:p.path-header (str "ETA: " date)]
      [:p.path-row (str coins-required " coins missing")]
      [:p.path-row (str commons-required " commons missing")]
      [:p.path-row (str rares-required " rares missing")]
      [:p.path-row (str epics-required " epics missing")]
      [:p.path-row (str legendaries-required " legendaries missing")]
      (for [{:keys [pin-name level missing]} path]
        ^{:key (str pin-name level)} [:p.path-row (str (upper-case pin-name) " to level " level " (" missing " missing)")])])])

(defn- pin-inputs []
  [:div.pin-inputs
   (doall
    (for [[index pin] (map-indexed vector @pins)]
      (let [progress (calc-percentage pin)
            update (fn [what event] (swap! pins assoc-in [index what] (-> event .-target .-value int)))]
        ^{:key (:name pin)}
        [:div
         [:div.pin-input
          [:span.pin-name (:name pin)]
          [:label {:for "level"} "Level:"]
          [:input {:type "number"
                   :id "level"
                   :min 0
                   :max (inc (count ((:rarity pin) costs)))
                   :value (:level pin)
                   :on-change (partial update :level)}]
          [:label {:for "dupes"} "Pins:"]
          [:input {:type "number"
                   :id "dupes"
                   :min 0
                   :value (:dupes pin)
                   :on-change (partial update :dupes)}]]
         [:div.pin-progress
          [:span.pin-progress progress]
          [:span.upgradeable (upgradeable pin)]]])))])

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
  (let [current-xp (current-xp)
        current-coins (+ @coins (current-coins-used))
        estimates (last-level-estimates current-xp current-coins)]
    [:div.container
     [total-progress estimates]
     [:div.other-inputs
      [date-picker]
      [coin-input]]
     [pin-inputs]
     [path estimates]]))

(defn- find-pin [id]
  (first (filter #(= (:id %) id) @pins)))

(defn ^:export run []
  (reset! pins (vec (sort-by
                     (juxt #(condp = (:rarity %) :common 0 :rare 1 :epic 2 :legendary 3) :name)
                     (map (fn [p]
                            (-> p
                                (assoc :level (:level (find-pin (:id p))))
                                (assoc :dupes (:dupes (find-pin (:id p)))))) pins-with-ids))))
  (rdom/render [root] (js/document.getElementById "app")))