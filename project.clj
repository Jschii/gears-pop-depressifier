(defproject gears-pop-depressifier "0.0.1"
  :dependencies [[org.clojure/clojure "1.10.1"]
                 [org.clojure/clojurescript "1.10.597"]
                 [reagent "0.10.0"]
                 [figwheel "0.5.19"]]

  :plugins [[lein-cljsbuild "1.1.7"]
            [lein-figwheel "0.5.19"]]

  :resource-paths ["resources" "target"]
  :clean-targets ^{:protect false} [:target-path]

  :profiles {:dev {:cljsbuild
                   {:builds {:client
                             {:figwheel {:on-jsload "gears-pop-depressifier.core/run"}
                              :compiler {:main "gears-pop-depressifier.core"
                                         :optimizations :none}}}}}

             :prod {:cljsbuild
                    {:builds {:client
                              {:compiler {:optimizations :advanced
                                          :elide-asserts true
                                          :pretty-print false}}}}}}

  :figwheel {:repl false
             :http-server-root "public"}

  :cljsbuild {:builds {:client
                       {:source-paths ["src"]
                        :compiler {:output-dir "target/public/client"
                                   :asset-path "client"
                                   :output-to "target/public/client.js"}}}})