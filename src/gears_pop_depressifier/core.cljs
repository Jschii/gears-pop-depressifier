(ns gears-pop-depressifier.core
  (:require [reagent.core :as r]
            [reagent.dom :as rdom]
            [goog.string :as gstring]
            [goog.string.format]
            [alandipert.storage-atom :refer [local-storage]]
            [clojure.string :refer [upper-case]]
            [gears-pop-depressifier.data :refer [pins-with-ids costs all-pins total-xps]]))

(enable-console-print!)

(defonce pins (local-storage (r/atom pins-with-ids) :pins-v2))
(defonce coins (local-storage (r/atom 0) :coins))
(defonce menu-item (r/atom 0))

(defn- total-pins [rarity level]
  (if (< level 2)
    level
    (let [dupes (map :dupes (rarity costs))]
      (if (> (dec level) (count dupes))
        0
        (inc (nth (reductions + dupes) (- level 2)))))))

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
          [:span.upgradeable.green
           (str "Upgradeable to level " level " with " coins " coins for that sweet "
                (format-percentage (/ xp (last total-xps)) "%.2f") " extra total progress")]
          [:span.upgradeable.red (str (- (:dupes c) dupes) " more needed for next level")])
        (recur (- dupes (:dupes c))
               (inc level)
               (+ coins (:coins c))
               (+ xp (:xp c))
               (get costs level))))))

(defn- current-xp []
  (reduce + (map (fn [p] (reduce + (take (dec (:level p)) (map :xp ((:rarity p) costs))))) @pins)))

(defn- extract [pins field rarity]
  (map field (filter #(= (:rarity %) rarity) (vals pins))))

(defn- missing [state rarity]
  (reduce + (extract (:pins state) :missing rarity)))

(defn- progress [{:keys [pins coins-missing]}]
  (let [most-missing-of-rarity (fn [rarity]
                                 (apply max (extract pins :missing rarity)))
        runs-to-max-a-pin (min (/ (apply min (extract pins :missing :common)) (/ 294 (pins-of-rarity :common)))
                               (/ (apply min (extract pins :missing :rare)) (/ 27 (pins-of-rarity :rare)))
                               (/ (apply min (extract pins :missing :epic)) (/ 4.15 (pins-of-rarity :epic)))
                               (/ (apply min (extract pins :missing :legendary)) (/ 0.145 (pins-of-rarity :legendary))))
        _ (println runs-to-max-a-pin)
        rarity-progress (fn [rarity per-run]
                          (/ (most-missing-of-rarity rarity) (/ per-run (pins-of-rarity rarity))))
        min-brumaks-needed (min (rarity-progress :common 294)
                                (rarity-progress :rare 27)
                                (rarity-progress :epic 4.15)
                                (rarity-progress :legendary 0.0145))
        days-remaining (/ (- (.getTime (js/Date. "2021-04-26T00:00:00")) (.now js/Date)) 86400000)]
        (loop [runs min-brumaks-needed]
          (let [add-commons (* runs (/ 294 (pins-of-rarity :common)))
        add-rares (* runs (/ 27 (pins-of-rarity :rare)))
        add-epics (* runs (/ 4.15 (pins-of-rarity :epic)))
        add-legendaries (* runs (/ 0.0145 (pins-of-rarity :legendary)))
        add-coins (* runs 2304)
        coins-from-dupes (+ (reduce + (map #(* % 5) (filter pos? (map #(- (- % add-commons)) (extract pins :missing-from-max :common)))))
                            (reduce + (map #(* % 50) (filter pos? (map #(- (- % add-rares)) (extract pins :missing-from-max :rare)))))
                            (reduce + (map #(* % 1000) (filter pos? (map #(- (- % add-epics)) (extract pins :missing-from-max :epic)))))
                            (reduce + (map #(* % 20000) (filter pos? (map #(- (- % add-legendaries)) (extract pins :missing-from-max :legendary))))))]
          (if (< (- coins-missing (* days-remaining 22500)) (+ add-coins coins-from-dupes))
            [(/ runs days-remaining) runs-to-max-a-pin]
            (recur (inc runs)))))))

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
               :px (/ (/ missing
                         (condp = (:rarity p)
                           :common 294
                           :rare 27
                           :epic 4.15
                           :legendary 0.0145))
                      (:xp cost))
               :pin-name (:name p) 
               :rarity (:rarity p)
               :pin-level (+ l 2) 
               :id (:id p)
               :missing missing)))))

(defn- make-state [xp-required]
  (let [sorted-upgrades (sort-by (juxt :cx :px) (mapcat next-upgrades @pins))
        requirements (reductions + (map :xp sorted-upgrades))
        required (fn [xp-required] (ffirst (filter (fn [[_ xp]] (> xp xp-required)) (map-indexed vector requirements))))
        needed-for-level (take (required xp-required) sorted-upgrades)]
    {:coins-missing (reduce + (map :coins needed-for-level))
     :pins (into {} (map (fn [[pin-name pins-for-id]]
                                              {(keyword (str "id" (-> pins-for-id first :id)))
                                                {:pin-name pin-name
                                                 :rarity (-> pins-for-id first :rarity)
                                                :missing (apply max (map :missing pins-for-id))
                                                :missing-from-max (apply max (map :missing (filter #(= (:pin-name %) pin-name) sorted-upgrades)))
                                                :level (apply max (map :pin-level pins-for-id))
                                                :cost (reduce + (map :coins (filter #(pos? (:missing %)) pins-for-id)))}})
                                            (group-by :pin-name needed-for-level)))}))
          
(defn- last-level-estimates []
  (let [current-xp (current-xp)
        xp-progress (double (/ current-xp (last total-xps)))
        state (make-state (- (last total-xps) current-xp))
        commons-missing (missing state :common)
        rares-missing (missing state :rare)
        epics-missing (missing state :epic)
        legendaries-missing (missing state :legendary)]
    {:xp-progress xp-progress
     :path (:pins state)
     :brumaks (progress state)
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

(defn- path [{:keys [path brumaks coins-required commons-required rares-required epics-required legendaries-required xp-progress best-pin]}]
  [:div.stats
   [:div.estimates
      [:span
       [:p.path-header (str "Brumaks per day: " (-> brumaks first Math/ceil))]
       [:p.path-header (str "Brumaks to max a pin: " (-> brumaks last Math/ceil))]
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
       (for [{:keys [pin-name level missing]} (->> path vals (filter #(= (:rarity %) :common)))]
         ^{:key (str pin-name level)}
         [:p.path-row {:class (if (zero? missing) "green" "red")}
          (str (upper-case pin-name) " to level " level " (" missing " missing)")])]
       [:div.pins.rares
        (for [{:keys [pin-name level missing]} (->> path vals (filter #(= (:rarity %) :rare)))]
          ^{:key (str pin-name level)}
          [:p.path-row {:class (if (zero? missing) "green" "red")}
           (str (upper-case pin-name) " to level " level " (" missing " missing)")])]
       [:div.pins.epics
        (for [{:keys [pin-name level missing]} (->> path vals (filter #(= (:rarity %) :epic)))]
          ^{:key (str pin-name level)}
          [:p.path-row {:class (if (zero? missing) "green" "red")}
           (str (upper-case pin-name) " to level " level " (" missing " missing)")])]
       [:div.pins.legendaries
        (for [{:keys [pin-name level missing]} (->> path vals (filter #(= (:rarity %) :legendary)))]
          ^{:key (str pin-name level)}
          [:p.path-row {:class (if (zero? missing) "green" "red")}
           (str (upper-case pin-name) " to level " level " (" missing " missing)")])]]]]])

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
          (upgradeable pin)]])))])

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
        (do-input "coins" "Coins:" "number" "1" coins)]
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