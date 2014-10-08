(require '[clojure.java.shell])

(defn ^String system
  [^String command]
  (->> (into [] (.split command "\\s+"))
       (apply clojure.java.shell/sh)
       (:out)
       (.trim)))

(defn project-version
  "Return the current version string."
  [base-version & {:keys [since release?]}]
  (if-not (true? release?)
    (let [patch (-> (system (str "git describe --match " since))
                    (.split "-" 2)
                    (second))
          [_ major-minor] (re-matches #"(\d+\.\d+)\.\d+" base-version)]
      (str major-minor "." patch))
    base-version))

(defproject spellhouse/hemlock (project-version "0.1.0" :since "0.0" :release? false)
  :description "Zipper based data construction library"

  :url "http://github.com/spellhouse/hemlock"

  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}

  :source-paths
  ["target/generated-src/clj" "target/generated-src/cljs"]

  :dependencies
  [[org.clojure/clojure "1.6.0"]]

  :hooks
  [cljx.hooks]

  :profiles
  {:dev {:dependencies
         [[org.clojure/clojurescript "0.0-2311"]
          [com.cemerick/piggieback "0.1.2"]]

         :plugins
         [[com.cemerick/austin "0.1.3"]
          [lein-cljsbuild "1.0.3"]
          [com.keminglabs/cljx "0.4.0"]
          [com.cemerick/clojurescript.test "0.3.1"]]

         :repl-options
         {:nrepl-middleware [cemerick.piggieback/wrap-cljs-repl]}}}


  :cljx
  {:builds [{:source-paths ["src/"]
             :output-path "target/generated-src/clj"
             :rules :clj}
            {:source-paths ["src/"]
             :output-path "target/generated-src/cljs"
             :rules :cljs}]})
