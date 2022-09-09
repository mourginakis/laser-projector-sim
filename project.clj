(defproject pucks "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0"
            :url "https://www.eclipse.org/legal/epl-2.0/"}
  :dependencies [[org.clojure/clojure "1.11.1"]
                 [scad-clj "0.5.4-SNAPSHOT"]
                 [org.clojure/core.logic "1.0.1"]
                 [org.clojure/algo.generic "0.1.3"]
                 [uncomplicate/neanderthal "0.45.0" :exclusions [org.jcuda/jcuda-natives, org.jcuda/jcublas-natives]]
                 [net.mikera/core.matrix "0.62.0"]
                 [org.clojure/math.combinatorics "0.1.6"]
                 [expresso "0.2.2"]
                 [same/ish "0.1.5"]]
  :repl-options {:init-ns pucks.core})
