(ns gears-pop-depressifier.core
  (:require [reagent.core :as r]
            [reagent.dom :as rdom]
            [goog.string :as gstring]
            [goog.string.format]
            [alandipert.storage-atom :refer [local-storage]]
            [clojure.string :refer [upper-case join]]
            [gears-pop-depressifier.data :refer [pins-with-ids costs total-xps hordes]]))

(enable-console-print!)

(defonce pins (local-storage (r/atom pins-with-ids) :pins-v2))
(defonce coins (local-storage (r/atom 0) :coins))
(defonce menu-item (r/atom 0))
(defonce horde (r/atom (first hordes)))
(defonce hide (r/atom false))

(defn- total-pins [rarity level]
  (if (< level 2)
    level
    (let [dupes (map :dupes (rarity costs))]
      (if (> (dec level) (count dupes))
        0
        (inc (nth (reductions + dupes) (- level 2)))))))

(defn- pins-of-rarity [rarity]
  (->> pins-with-ids
       (filter #(= (:rarity %) rarity))
       (remove #(boolean ((:exclude @horde) (:id %))))
       count))

(defn- format-percentage [percentage formatter]
  (str (gstring/format formatter (* 100 percentage)) "%"))

(defn- calc-percentage [{:keys [rarity level dupes]}]
  (let [costs (rarity costs)
        current-pins (+ (total-pins rarity level) dupes)
        max-pins (inc (reduce + (map :dupes costs)))]
    (double (/ current-pins max-pins))))

(defn- upgradeable [p]
  (let [costs ((:rarity p) costs)]
    (loop [dupes (:dupes p)
           level (:level p)
           c (get costs (dec level))]
      (if (or (< dupes (:dupes c)) (> level (count costs)))
        (if (> level (:level p))
          [:span
           [:span.upgradeable.green
            (str "Upgradeable to level " level)]
           [:span.upgradeable.red
            (str " (" (- (:dupes c) dupes) " more needed for level " (inc level) ")")]]
          [:span.upgradeable.red (str (- (:dupes c) dupes) " more needed for next level")])
        (recur (- dupes (:dupes c))
               (inc level)
               (get costs level))))))

(defn- current-xp []
  (reduce + (map (fn [p] (reduce + (take (dec (:level p)) (map :xp ((:rarity p) costs))))) @pins)))

(defn- extract [pins field rarity]
  (map field (filter #(= (:rarity %) rarity) pins)))

(defn- missing [state rarity]
  (reduce + (extract (:pins state) :missing rarity)))

(defn- doo [p adds]
  (let [unreachable-count (count (if ((:exclude @horde) (:id p))
                                   (filter pos? (:missing p))
                                   (drop-while #(>= adds %) (:missing p))))
        m (drop-last unreachable-count (:missing p))
        x (drop-last unreachable-count (:xp p))
        c (drop-last unreachable-count (:coin p))
        l (take-last (count c) (range (- (:level p) (- unreachable-count 1))))]
    (map (fn [m x c l]
           {:pin-name (:pin-name p)
            :rarity (:rarity p)
            :missing m
            :cost c
            :xp x
            :xp-cost (/ c x)
            :level l}) m x c l)))

(defn- can-be-removed [pins xp-required]
  (->> pins
       (filter #(> (- (reduce + (map :xp pins)) xp-required) (:xp %)))
       (sort-by :xp-cost)
       reverse
       (filter (fn [{:keys [pin-name level]}]
                 (empty? (filter #(and (= (:pin-name %) pin-name)
                                       (> (:level %) level))
                                 pins))))
       first))

(defn- do-add [pins [commons rares epics legendaries]]
  (let [xp-required (- (last total-xps) (current-xp))
        pins2 (sort-by (juxt :xp-cost :missing) (mapcat (fn [p]
                                                          (condp = (:rarity p)
                                                            :common (doo p commons)
                                                            :rare (doo p rares)
                                                            :epic (doo p epics)
                                                            :legendary (doo p legendaries)))
                                                        pins))
        requirements (reductions + (map :xp pins2))
        how-many (inc (count (take-while #(> xp-required %) requirements)))]
    (loop [p (take how-many pins2)]
      (if-let [r (can-be-removed p xp-required)]
        (recur (remove #(= % r) p))
        {:pins (->> (vals (group-by :pin-name p))
                    (map #(sort-by :level %))
                    (map last))
         :coins (reduce + (map :cost p))}))))

(defn- progress [{:keys [pins xp-required]}]
  (let [p (remove (fn [p] ((:exclude @horde) (:id p))) pins)
        coins-needed (atom -1)
        runs-to-max-a-pin [(/ (apply min (extract p :missing-from-max :common)) (/ (:commons @horde) (pins-of-rarity :common)))
                           (/ (apply min (extract p :missing-from-max :rare)) (/ (:rares @horde) (pins-of-rarity :rare)))
                           (/ (apply min (extract p :missing-from-max :epic)) (/ (:epics @horde) (pins-of-rarity :epic)))
                           (/ (apply min (extract p :missing-from-max :legendary)) (/ (:legendaries @horde) (pins-of-rarity :legendary)))]
        days-remaining (/ (- (.getTime (js/Date. "2021-04-26T00:00:00")) (.now js/Date)) 86400000)
        foo (fn [added-pins rarity]
              (map (fn [[m x c]]
                     (if (>= added-pins m)
                       {:xp x :coin c}
                       {:xp 0 :coin 0}))
                   (map vector
                        (flatten (extract p :missing rarity))
                        (flatten (extract p :xp rarity))
                        (flatten (extract p :coin rarity)))))]
    (loop [runs 0
           step 200]
      (let [add-commons (* runs (/ (:commons @horde) (pins-of-rarity :common)))
            add-rares (* runs (/ (:rares @horde) (pins-of-rarity :rare)))
            add-epics (* runs (/ (:epics @horde) (pins-of-rarity :epic)))
            add-legendaries (if (zero? (:legendaries @horde))
                              0
                              (* runs (/ (:legendaries @horde) (pins-of-rarity :legendary))))
            coins-from-dupes (+ (reduce + (map #(* % 5) (filter pos? (map #(- (- % add-commons)) (extract p :missing-from-max :common)))))
                                (reduce + (map #(* % 50) (filter pos? (map #(- (- % add-rares)) (extract p :missing-from-max :rare)))))
                                (reduce + (map #(* % 1000) (filter pos? (map #(- (- % add-epics)) (extract p :missing-from-max :epic)))))
                                (reduce + (map #(* % 20000) (filter pos? (map #(- (- % add-legendaries)) (extract p :missing-from-max :legendary))))))
            add-coins (+ (* runs (:coins @horde))
                         (* days-remaining 28928.57)
                         coins-from-dupes
                         @coins)
            addz (apply merge-with +
                        (concat (foo add-commons :common)
                                (foo add-rares :rare)
                                (foo add-epics :epic)
                                (foo add-legendaries :legendary)))]
        (when (and (>= (:xp addz) xp-required)
                   (neg? @coins-needed))
          (reset! coins-needed (:coin addz)))
        (if (and (>= (:xp addz) xp-required)
                 (pos? @coins-needed)
                 (>= add-coins @coins-needed))
          (if-not (= step 1)
            (recur (- runs (dec step)) 1)
            (let [pc (do-add pins [add-commons add-rares add-epics add-legendaries])
                  coin-discount (- @coins-needed (:coins pc))
                  run-discount (int (/ coin-discount (/ @coins-needed runs)))]
              {:runs (- runs run-discount)
               :pin-runs runs-to-max-a-pin
               :days days-remaining
               :pins (:pins pc)
               :coins-missing (max (- (:coins pc) @coins) 0)}))
          (recur (+ runs step) step))))))

(defn- next-upgrades [p]
  (let [costs ((:rarity p) costs)
        owned (+ (reduce + (map :dupes (take (max (-> p :level dec) 0) costs)))
                 (:dupes p))
        needed-for-level (fn [level]
                           (reduce + (map :dupes (take (inc level) costs))))]
    (for [l (range (max (-> p :level dec) 1) (count costs))]
      (let [cost (get costs l)
            missing (max (- (needed-for-level l) owned) 0)]
        (assoc cost
               :cx (/ (:coins cost) (:xp cost))
               :pin-name (:name p)
               :rarity (:rarity p)
               :pin-level (+ l 2)
               :id (:id p)
               :missing missing)))))

(defn- make-state []
  (let [xp-required (- (last total-xps) (current-xp))
        sorted-upgrades (sort-by :cx (mapcat next-upgrades @pins))
        pins (map (fn [[pin-name pins-for-id]]
                    {:pin-name pin-name
                     :id (-> pins-for-id first :id)
                     :rarity (-> pins-for-id first :rarity)
                     :level (apply max (map :pin-level pins-for-id))
                     :missing (map :missing pins-for-id)
                     :xp (map :xp pins-for-id)
                     :missing-from-max (apply max (map :missing (filter #(= (:pin-name %) pin-name) sorted-upgrades)))
                     :coin (map :coins pins-for-id)})
                  (group-by :pin-name sorted-upgrades))]
    {:xp-required xp-required
     :pins pins}))

(defn- last-level-estimates []
  (let [xp-progress (double (/ (current-xp) (last total-xps)))
        state (progress (make-state))
        commons-missing (missing state :common)
        rares-missing (missing state :rare)
        epics-missing (missing state :epic)
        legendaries-missing (missing state :legendary)]
    {:xp-progress xp-progress
     :path (:pins state)
     :runs (concat [(:runs state) (/ (:runs state) (:days state))] (:pin-runs state))
     :coins-required (:coins-missing state)
     :commons-required commons-missing
     :rares-required rares-missing
     :epics-required epics-missing
     :legendaries-required legendaries-missing}))

(defn- do-input [id label-text type step atom]
  (let [on-change-fn (fn [event]
                       (let [v (-> event .-target .-value)]
                         (condp = type
                           "number" (int v)
                           :default v)))
        input-map {:type type
                   :id id
                   :step step
                   :value (if (= step "1") @atom (gstring/format "%.2f" @atom))
                   :on-change #(reset! atom (on-change-fn %))}]
    [:span {:class id}
     [:label {:for id} label-text]
     [:input (if (= type "number")
               (assoc input-map :min 0)
               input-map)]]))

(defn- path [{:keys [path runs coins-required commons-required rares-required epics-required legendaries-required xp-progress]}]
  [:div.stats
   [:select {:value (:name @horde)
             :size (count hordes)
             :on-change #(reset! horde (first (filter (fn [h]
                                                        (= (:name h) (.. % -target -value))) hordes)))}
    (for [h hordes]
      ^{:key (:name h)}
      [:option {:value (:name h)} (:name h)])]
   [:div.estimates
    [:span
     [:p.path-header (str "XP-progress : " (format-percentage xp-progress "%.2f"))]
     [:p.path-header (str "Runs to level 20 : " (nth runs 0) " (per day: " (Math/ceil (nth runs 1)) ")")]
     [:p.path-header (str "Runs to max pin  : " (join ", " (map Math/ceil (drop 2 runs))))]
     [:div.missing
      [:div.overall
       [:p.path-row {:class (if (zero? coins-required) "green" "red")}
        (str coins-required " coins missing")]
       [:p.path-row {:class (if (zero? commons-required) "green" "red")}
        (str commons-required " commons missing")]
       [:p.path-row {:class (if (zero? rares-required) "green" "red")}
        (str rares-required " rares missing")]
       [:p.path-row {:class (if (zero? epics-required) "green" "red")}
        (str epics-required " epics missing")]
       [:p.path-row {:class (if (zero? legendaries-required) "green" "red")}
        (str legendaries-required " legendaries missing")]]
      [:div.pins.commons
       (for [{:keys [pin-name level missing]} (->> path (filter #(= (:rarity %) :common)))]
         ^{:key (str pin-name level)}
         [:p.path-row {:class (if (zero? missing) "green" "red")}
          (str (upper-case pin-name) " to level " level " (" missing " missing)")])]
      [:div.pins.rares
       (for [{:keys [pin-name level missing]} (->> path (filter #(= (:rarity %) :rare)))]
         ^{:key (str pin-name level)}
         [:p.path-row {:class (if (zero? missing) "green" "red")}
          (str (upper-case pin-name) " to level " level " (" missing " missing)")])]
      [:div.pins.epics
       (for [{:keys [pin-name level missing]} (->> path (filter #(= (:rarity %) :epic)))]
         ^{:key (str pin-name level)}
         [:p.path-row {:class (if (zero? missing) "green" "red")}
          (str (upper-case pin-name) " to level " level " (" missing " missing)")])]
      [:div.pins.legendaries
       (for [{:keys [pin-name level missing]} (->> path (filter #(= (:rarity %) :legendary)))]
         ^{:key (str pin-name level)}
         [:p.path-row {:class (if (zero? missing) "green" "red")}
          (str (upper-case pin-name) " to level " level " (" missing " missing)")])]]]]])

(defn- pin-inputs []
  [:div.pin-inputs
   (doall
    (for [[index pin] (map-indexed vector @pins)]
      (let [perc (calc-percentage pin)
            progress (format-percentage perc "%.2f")
            update (fn [what event] (swap! pins assoc-in [index what] (-> event .-target .-value int)))]
        (when-not (and @hide (>= perc 1))
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
            (upgradeable pin)]]))))])

(defn- menu []
  [:nav.menu
   [:ul
    [:li {:on-click #(reset! menu-item 0) :class (when (= 0 @menu-item) "selected")} "PINS"]
    [:li {:on-click #(reset! menu-item 1) :class (when (= 1 @menu-item) "selected")} "STATS"]]])

(defn- root []
  [:div
   [menu]
   [:div.container
    (if (zero? @menu-item)
      [:div
       [:div.other-inputs
        (do-input "coins" "Coins:" "number" "1" coins)
        [:button {:type "button"
                  :class "button"
                  :on-click #(swap! hide not)}
         (if @hide
           "Show maxed pins"
           "Hide maxed pins")]]
       [pin-inputs]]
      [path (last-level-estimates)])]])

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