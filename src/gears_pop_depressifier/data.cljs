(ns gears-pop-depressifier.data)

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
                   {:name "tai kaliso" :rarity :rare}
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

(defonce bundles [{:name "coin fortune"
                   :crystals 4500
                   :coins 1250000
                   :commons 0
                   :rares 0
                   :epics 0
                   :legendaries 0}
                  {:name "legendary bronze"
                   :crystals 2250
                   :coins 78656.5
                   :commons 5014
                   :rares 1038
                   :epics 173
                   :legendaries 4.6148}])