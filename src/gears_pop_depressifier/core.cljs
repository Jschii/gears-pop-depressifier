(ns gears-pop-depressifier.core
  (:require [reagent.core :as r]
            [reagent.dom :as rdom]
            [goog.string :as gstring]
            [goog.string.format]
            [alandipert.storage-atom :refer [local-storage]]
            [clojure.string :refer [blank? upper-case]]
            [tick.alpha.api :as t]
            [gears-pop-depressifier.data :refer [pins-with-ids costs all-pins total-xps bundles]]))

(enable-console-print!)

(defonce pins (local-storage (r/atom pins-with-ids) :pins-v2))
(defonce start-date (local-storage (r/atom "") :start-date))
(defonce coins (local-storage (r/atom 0) :coins))
(defonce target-level (r/atom 20))
(defonce extra-commons (r/atom 0))
(defonce extra-rares (r/atom 0))
(defonce extra-epics (r/atom 0))
(defonce extra-legendaries (r/atom 0))

(defn- total-pins [rarity level]
  (if (< level 2)
    level
    (let [dupes (map :dupes (rarity costs))]
      (if (> (dec level) (count dupes))
        0
        (inc (nth (reductions + dupes) (- level 2)))))))

(defn- pins-of-rarity [rarity]
  (+ (count (filter #(= (:rarity %) rarity) all-pins))
     (condp = rarity
       :common @extra-commons
       :rare @extra-rares
       :epic @extra-epics
       :legendary @extra-legendaries)))

(defn- format-percentage [percentage formatter]
  (str (gstring/format formatter (* 100 percentage)) "%"))

(defn- calc-percentage [{:keys [rarity level dupes]}]
  (let [costs (rarity costs)
        current-pins (+ (total-pins rarity level) dupes)
        max-pins (inc (reduce + (map :dupes costs)))]
    (format-percentage (double (/ current-pins max-pins)) "%.2f")))

(defn- best-pin [[commons-per-day rares-per-day epics-per-day legendaries-per-day _]]
  (first
   (sort-by :days <
            (for [{:keys [name rarity level dupes]} @pins]
              (let [costs (rarity costs)
                    current-pins (+ (total-pins rarity level) dupes)
                    max-pins (inc (reduce + (map :dupes costs)))
                    missing (- max-pins current-pins)
                    per-day (/ (condp = rarity
                                 :common commons-per-day
                                 :rare rares-per-day
                                 :epic epics-per-day
                                 :legendary legendaries-per-day)
                               (pins-of-rarity rarity))]
                {:name name :days (/ missing per-day)})))))

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
                                     :rarity (:rarity p)))))
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
     (/ current-coins days-played)
     days-played]
    [nil nil nil nil nil nil]))

(defn- missing [r]
  (reduce + (map :missing r)))

(defn- pin-progress [r [commons-per-day rares-per-day epics-per-day legendaries-per-day coins-per-day days-played] [bundle-commons bundle-rares bundle-epics bundle-legendaries]]
  (let [most-missing-of-rarity (fn [rarity bundle-pins]
                                 (- (apply max (map :missing (rarity r))) bundle-pins))
        rarity-progress (fn [rarity per-day bundle-pins]
                          (/ (most-missing-of-rarity rarity bundle-pins) (/ per-day (pins-of-rarity rarity))))]
    (max (rarity-progress :common (+ commons-per-day (/ bundle-commons days-played)) bundle-commons)
         (rarity-progress :rare (+ rares-per-day (/ bundle-rares days-played)) bundle-rares)
         (rarity-progress :epic (+ epics-per-day (/ bundle-epics days-played)) bundle-epics)
         (rarity-progress :legendary (+ legendaries-per-day (/ bundle-legendaries days-played)) bundle-legendaries))))

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
        [_ _ _ _ coins-per-day days-played] per-day
        make-extra (fn [rarity id]
                     {:name (str "extra-" (name rarity) "-" (inc id)) :rarity rarity :level 0 :dupes 0})
        pins-with-extras (concat @pins
                                 (mapv (partial make-extra :common) (range @extra-commons))
                                 (mapv (partial make-extra :rare) (range @extra-rares))
                                 (mapv (partial make-extra :epic) (range @extra-epics))
                                 (mapv (partial make-extra :legendary) (range @extra-legendaries)))
        sorted-upgrades (sort-upgrades (mapcat (partial next-upgrades per-day) pins-with-extras))

        requirements (reductions + (map :xp sorted-upgrades))
        required (fn [xp-required] (ffirst (filter (fn [[_ xp]] (> xp xp-required)) (map-indexed vector requirements))))
        best-pin (best-pin per-day)
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
                               (pin-progress rarity-groups per-day [0 0 0 0])))
            best-bundle (last (sort-by :value (for [bundle bundles]
                                                (let [bundle-progress (int (max (/ (- coins-missing (:coins bundle)) (+ coins-per-day (/ (:coins bundle) days-played)))
                                                                                (pin-progress rarity-groups per-day
                                                                                              [(/ (:commons bundle) (pins-of-rarity :common))
                                                                                               (/ (:rares bundle) (pins-of-rarity :rare))
                                                                                               (/ (:epics bundle) (pins-of-rarity :epic))
                                                                                               (/ (:legendaries bundle) (pins-of-rarity :legendary))])))]
                                                  {:name (:name bundle) :value (/ (- progress bundle-progress) (:crystals bundle))}))))]
        {:xp-progress xp-progress
         :path path
         :coins-required coins-missing
         :commons-required commons-missing
         :rares-required rares-missing
         :epics-required epics-missing
         :legendaries-required legendaries-missing
         :best-bundle (when (pos? (:value best-bundle))
                        (:name best-bundle))
         :date (when (pos? progress)
                 (completion-date progress))
         :best-pin best-pin})
      {:xp-progress xp-progress
       :best-pin best-pin})))

(defn- total-progress [{xp :xp-progress best-pin :best-pin best-bundle :best-bundle}]
  [:div.total-progress
   [:span (str "XP progress: " (gstring/format "%.2f" (* 100 xp)) "%")]
   [:span (str "Best pin: " (-> best-pin :name upper-case) " (maxed on " (-> best-pin :days int completion-date) ")")]
   (when best-bundle
     [:span (str "Best bundle: " (upper-case best-bundle))])])

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

(defn- extra-inputs []
  [:span
   [:p "What if they were to add new pins?"]
   [:label {:for "extra-commons"} "Commons:"]
   [:input {:type "number"
            :id "extra-commons"
            :min 0
            :value @extra-commons
            :on-change #(reset! extra-commons (-> % .-target .-value int))}]
   [:label {:for "extra-rares"} "Rares:"]
   [:input {:type "number"
            :id "extra-rares"
            :min 0
            :value @extra-rares
            :on-change #(reset! extra-rares (-> % .-target .-value int))}]
   [:label {:for "extra-epics"} "Epics:"]
   [:input {:type "number"
            :id "extra-epics"
            :min 0
            :value @extra-epics
            :on-change #(reset! extra-epics (-> % .-target .-value int))}]
   [:label {:for "extra-legendaries"} "Legendaries:"]
   [:input {:type "number"
            :id "extra-legendaries"
            :min 0
            :value @extra-legendaries
            :on-change #(reset! extra-legendaries (-> % .-target .-value int))}]])

(defn- root []
  (let [current-xp (current-xp)
        current-coins (+ @coins (current-coins-used))
        estimates (last-level-estimates current-xp current-coins)]
    [:div.container
     [total-progress estimates]
     [:div.other-inputs
      [date-picker]
      [coin-input]]
     [:div.other-inputs
      [extra-inputs]]
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