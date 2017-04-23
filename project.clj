(defproject gravity "0.1.0-SNAPSHOT"
  :dependencies [[org.clojure/clojure "1.8.0"]
                 [org.clojure/clojurescript "1.9.521"]
                 [org.omcljs/ambly "0.7.0"]]
  :plugins [[lein-cljsbuild "1.1.5"]]
  :source-paths ["dev" "src"]
  :clean-targets ["target" "out"]
  :cljsbuild {:builds {:dev
                       {:source-paths ["src"]
                        :compiler     {:output-to      "target/out/main.js"
                                       :output-dir     "target/out"
                                       :optimizations  :none}}
                       :rel
                       {:source-paths ["src"]
                        :compiler     {:output-to     "target/out/main.js"
                                       :optimizations :advanced
                                       :externs       ["externs.js"]
                                       :pretty-print  false
                                       :pseudo-names  false}}}})
