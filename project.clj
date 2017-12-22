(defproject me.arrdem/guten-tag "_"
  :description "Good tags for a good day!"
  :url "http://github.com/arrdem/guten-tag"
  :license {:name "Eclipse Public License"
            :url  "http://www.eclipse.org/legal/epl-v10.html"}

  :dependencies [[org.clojure/clojure "1.9.0"]]

  :plugins [[me.arrdem/lein-git-version "LATEST"]]
  :git-version {:status-to-version
                (fn [{:keys [tag version ahead ahead? dirty?] :as git}]
                  (if (and tag (not ahead?) (not dirty?))
                    tag
                    (str tag
                         (when ahead? (str "." ahead))
                         (when dirty? "-SNAPSHOT"))))}

  :profiles {:dev {:dependencies [[org.clojure/core.match "0.3.0-alpha4"]]}})
