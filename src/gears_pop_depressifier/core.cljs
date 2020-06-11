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
(defonce crystals (local-storage (r/atom 0) :crystals))
(defonce target-level (r/atom 20))
(defonce extra-commons (r/atom 0))
(defonce extra-rares (r/atom 0))
(defonce extra-epics (r/atom 0))
(defonce extra-legendaries (r/atom 0))
(defonce menu-item (r/atom 0))

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

(defn- best-pin [{:keys [commons-per-day rares-per-day epics-per-day legendaries-per-day]}]
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

(defn- next-upgrades [{:keys [commons-per-day rares-per-day epics-per-day legendaries-per-day coins-per-day]} p]
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

(defn- per-day []
  (let [current-coins (+ @coins (current-coins-used))]
    (if-let [days-played (days-played)]
      {:commons-per-day (/ (current-pins :common) days-played)
       :rares-per-day (/ (current-pins :rare) days-played)
       :epics-per-day (/ (current-pins :epic) days-played)
       :legendaries-per-day (/ (current-pins :legendary) days-played)
       :coins-per-day (/ current-coins days-played)
       :days-played days-played}
      {})))

(defn- missing [r]
  (reduce + (map :missing (vals r))))

(defn- pin-progress [{:keys [pins commons-per-day rares-per-day epics-per-day legendaries-per-day]}]
  (let [most-missing-of-rarity (fn [rarity]
                                 (apply max (map :missing (vals (rarity pins)))))
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

(defn- update-vals [m f]
  (into {} (map (fn [[k v]] {k (update v :missing f)}) m)))

(defn- add-pins [rarity how-many rg]
  (update rg rarity (fn [m] (update-vals m #(- % how-many)))))

(defn- add-bundle [bundle pins]
  (->> pins
       (add-pins :common (/ (:commons bundle) (pins-of-rarity :common)))
       (add-pins :rare (/ (:rares bundle) (pins-of-rarity :rare)))
       (add-pins :epic (/ (:epics bundle) (pins-of-rarity :epic)))
       (add-pins :legendary (/ (:legendaries bundle) (pins-of-rarity :legendary)))))

(defn- bundle-progress [bundle {:keys [pins coins-missing coins-per-day days-played] :as state}]
  (max (/ (- coins-missing (:coins bundle)) (+ coins-per-day (/ (:coins bundle) days-played)))
       (pin-progress (update state :pins #(add-bundle bundle pins)))))

(defn- progress [{:keys [coins-missing coins-per-day] :as state}]
  (max (/ coins-missing coins-per-day)
       (pin-progress state)))

(defn- deal-progress [{rarity :rarity id :id} {coins-missing :coins-missing coins-per-day :coins-per-day days-played :days-played :as state}]
  (let [[c new-state] (condp = rarity
                        :common [(/ (+ coins-missing 200) (- coins-per-day (/ 200 days-played))) (update-in state [:pins :common (keyword (str "id" id)) :missing] #(- % 50))]
                        :rare [(/ (+ coins-missing 800) (- coins-per-day (/ 800 days-played))) (update-in state [:pins :rare (keyword (str "id" id)) :missing] #(- % 10))]
                        :epic [(/ (+ coins-missing 4000) (- coins-per-day (/ 4000 days-played))) (update-in state [:pins :epic (keyword (str "id" id)) :missing] #(- % 4))])]
    (- (progress state) (max c (pin-progress new-state)))))

(defn- make-state [per-day xp-required]
  (let [make-extra (fn [rarity id]
                     {:name (str "extra-" (name rarity) "-" (inc id)) :rarity rarity :level 0 :dupes 0})
        pins-with-extras (concat @pins
                                 (mapv (partial make-extra :common) (range @extra-commons))
                                 (mapv (partial make-extra :rare) (range @extra-rares))
                                 (mapv (partial make-extra :epic) (range @extra-epics))
                                 (mapv (partial make-extra :legendary) (range @extra-legendaries)))
        sorted-upgrades (sort-upgrades (mapcat (partial next-upgrades per-day) pins-with-extras))
        requirements (reductions + (map :xp sorted-upgrades))
        required (fn [xp-required] (ffirst (filter (fn [[_ xp]] (> xp xp-required)) (map-indexed vector requirements))))
        needed-for-level (take (required xp-required) sorted-upgrades)]
    (merge {:coins-missing (max (-> needed-for-level last :coins-missing -) 0)
            :pins (into {} (map (fn [[rarity ps]]
                                  {rarity
                                   (into {}
                                         (map (fn [[pin-name pins-for-id]]
                                                {(keyword (str "id" (-> pins-for-id first :id)))
                                                 {:pin-name pin-name
                                                  :missing (apply max (map :missing pins-for-id))
                                                  :level (apply max (map :pin-level pins-for-id))
                                                  :cost (reduce + (map :coins (filter #(pos? (:missing %)) pins-for-id)))}})
                                              (group-by :pin-name ps)))})
                                (group-by :rarity needed-for-level)))}
           per-day)))

(defn- progress-with-crystals [state]
  (let [update-state (fn [{:keys [pins days-played coins-missing coins-per-day commons-per-day rares-per-day epics-per-day legendaries-per-day]} b]
                       {:pins (add-bundle b pins)
                        :days-played days-played
                        :coins-missing (- coins-missing (:coins b))
                        :coins-per-day (+ coins-per-day (/ (:coins b) days-played))
                        :commons-per-day (+ commons-per-day (/ (:commons b) days-played))
                        :rares-per-day  (+ rares-per-day (/ (:rares b) days-played))
                        :epics-per-day  (+ epics-per-day (/ (:epics b) days-played))
                        :legendaries-per-day (+ legendaries-per-day (/ (:legendaries b) days-played))})
        shrink (fn [bundle percentage]
                 {:coins (* (:coins bundle) percentage)
                  :commons (* (:commons bundle) percentage)
                  :rares (* (:rares bundle) percentage)
                  :epics (* (:epics bundle) percentage)
                  :legendaries (* (:legendaries bundle) percentage)})]
    (loop [crystals @crystals
           state state
           pd (progress state)]
      (let [b1p (bundle-progress (first bundles) state)
            b2p (bundle-progress (last bundles) state)
            b1 (/ (- pd b1p) (-> bundles first :crystals))
            b2 (/ (- pd b2p) (-> bundles last :crystals))
            b (if (> b1 b2) (first bundles) (last bundles))
            c (/ crystals (:crystals b))]
        (if (< c 1)
          (progress (update-state state (shrink b c)))
          (recur (- crystals (:crystals b))
                 (update-state state b)
                 (if (> b1 b2) b1p b2p)))))))

(defn- last-level-estimates []
  (let [current-xp (current-xp)
        per-day (per-day)
        xp (if (and (> @target-level 1) (<= @target-level 20))
             (nth total-xps (- @target-level 2))
             0)
        xp-progress (double (/ current-xp (last total-xps)))]
    (if (< current-xp xp)
      (let [state (make-state per-day (- xp current-xp))
            path (->> state :pins vals (map vals) flatten)
            commons-missing (-> state :pins :common (missing))
            rares-missing (-> state :pins :rare (missing))
            epics-missing (-> state :pins :epic (missing))
            legendaries-missing (-> state :pins :legendary (missing))
            progress-with-crystals (progress-with-crystals state)]
        {:xp-progress xp-progress
         :path path
         :coins-required (:coins-missing state)
         :commons-required commons-missing
         :rares-required rares-missing
         :epics-required epics-missing
         :legendaries-required legendaries-missing
         :deals (vec (keep (fn [{:keys [name rarity] :as p}]
                             (when (and (not= rarity :legendary) (pos? (deal-progress p state)))
                               name)) @pins))
         :bundles (sort-by :value > (for [bundle bundles]
                                      (let [progress (progress state)
                                            bundle-progress (bundle-progress bundle state)]
                                        {:name (:name bundle) :value (* (/ (- progress bundle-progress) (:crystals bundle)) 1440)})))
         :date (when (pos? progress-with-crystals)
                 (completion-date (int progress-with-crystals)))
         :best-pin (best-pin per-day)})
      {:xp-progress xp-progress
       :best-pin (best-pin per-day)})))

(defn- path [{:keys [date path coins-required commons-required rares-required epics-required legendaries-required xp-progress best-pin bundles deals] :as est}]
  [:div.stats
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
       [:p.path-row {:class (if (zero? coins-required) "green" "red")}
        (str coins-required " coins missing")]
       [:p.path-row {:class (if (zero? commons-required) "green" "red")}
        (str commons-required " commons missing")]
       [:p.path-row {:class (if (zero? rares-required) "green" "red")}
        (str rares-required " rares missing")]
       [:p.path-row {:class (if (zero? epics-required) "green" "red")}
        (str epics-required " epics missing")]
       [:p.path-row {:class (if (zero? legendaries-required) "green" "red")}
        (str legendaries-required " legendaries missing")]
       (for [{:keys [pin-name level missing]} path]
         ^{:key (str pin-name level)}
         [:p.path-row {:class (if (zero? missing) "green" "red")}
          (str (upper-case pin-name) " to level " level " (" missing " missing)")])])]
   [:div.deals
    [:div (str "XP progress: " (gstring/format "%.2f" (* 100 xp-progress)) "%")]
    [:div (str "Best pin: " (-> best-pin :name upper-case) " (maxed on " (-> best-pin :days int completion-date) ")")]
    (when-not (empty? bundles)
      [:div "Bundles (value minutes per crystal):"
       (for [{:keys [name value]} bundles]
         ^{:key (str name value)}
         [:p (str (upper-case name) " (" (gstring/format "%.2f" value) " mpc)")])])
    [:div "Daily deals worth taking:" (if-not (empty? deals)
                                        (for [deal deals]
                                          ^{:key (str deal)}
                                          [:p (upper-case deal)])
                                        [:p "None"])]]])

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

(defn- do-input [id label-text type atom]
  (let [on-change-fn (fn [event]
                       (let [v (-> event .-target .-value)]
                         (if (= type "number")
                           (int v)
                           v)))
        input-map {:type type
                   :id id
                   :value @atom
                   :on-change #(reset! atom (on-change-fn %))}]
    [:span {:class id}
     [:label {:for id} label-text]
     [:input (if (= type "number")
               (assoc input-map :min 0)
               input-map)]]))

(defn- extra-inputs []
  [:span
   [:p "What if they were to add new pins?"]
   (do-input "extra-commons" "Commons:" "number" extra-commons)
   (do-input "extra-rares" "Rares:" "number" extra-rares)
   (do-input "extra-epics" "Epics:" "number" extra-epics)
   (do-input "extra-legendaries" "Legendaries:" "number" extra-legendaries)])

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
        (do-input "start-date" "Start date:" "date" start-date)
        (do-input "coins" "Coins:" "number" coins)
        (do-input "crystals" "Crystals:" "number" crystals)]
       [pin-inputs]
       [:div.other-inputs
        [extra-inputs]]]
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