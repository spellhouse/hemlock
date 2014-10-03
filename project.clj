(require '[clojure.java.shell])

(defn project-version
  "Return the current version string."
  [base-version {release? :release?}]
  (if-not (true? release?)
    (let [last-commit (-> (clojure.java.shell/sh "git" "rev-parse" "HEAD")
                          (:out)
                          (.trim))
          revision (-> (clojure.java.shell/sh "git" (str "rev-list.." last-commit))
                       (:out)
                       (.. trim (split "\\n"))
                       (count))
          sha (subs last-commit 0 6)]
      (str base-version "." revision "-" sha))
    base-version))

(defproject hemlock (project-version "0.1.0" {:release? false})
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
