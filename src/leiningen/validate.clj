(ns leiningen.validate
  (:require [clojure.xml :as xml]))

(defn deep-merge
  ([] nil)
  ([m] m)
  ([m1 m2]
   (reduce-kv (fn [out k v]
                (let [v1 (get out k)]
                  (cond (nil? v1)
                        (assoc out k v)

                        (and (map? v) (map? v1))
                        (assoc out k (deep-merge v1 v))

                        (= v v1)
                        out

                        :else
                        (assoc out k v))))
              m1
              m2))
  ([m1 m2 & ms]
   (apply deep-merge (deep-merge m1 m2) ms)))

(defn dependencies?
  [element]
  (= (:tag element) :dependencies))

(defn clean-dependency
  [dependency]
  (let [{:keys [groupId artifactId version]} (into {} (map (fn [{:keys [tag content]}]
                                                             [tag (if (= 1 (count content))
                                                                    (first content)
                                                                    content)])
                                                           dependency))]
    [(str groupId "/" artifactId) version]))

(defn pom-file [project-full-name version]
  (let [[groupId artifactId] (clojure.string/split (str project-full-name) #"/")
        groupId (clojure.string/replace groupId #"\." "/")]
    (str (System/getProperty "user.home") "/.m2/repository/" groupId "/" artifactId "/" version "/"
         artifactId "-" version ".pom")))

(defn fetch-dependencies-from-pom
  [[project-full-name version]]
  (try
    (->> (pom-file project-full-name version)
         xml/parse
         :content
         (filter dependencies?)
         first
         :content
         (map :content)
         (mapv clean-dependency))
    (catch java.net.MalformedURLException _
      (println project-full-name version "could not be found, the pom file probably includes some variable version numbers")
      nil)
    (catch java.io.FileNotFoundException _
      (println project-full-name version "could not be found, run lein deps or ignore this message")
      nil)))

(defn project-dependency-map*
  [project]
  (reduce (fn [acc dependency]
            (assoc acc project dependency))
          {}
          (fetch-dependencies-from-pom project)))

(defn project-dependency-map
  [project dependencies]
  (zipmap dependencies
          (map  (fn [[dependency version]] (project-dependency-map dependency version)) dependencies)))

(defn validate
  [project & args]
  (let [{:keys [name version dependencies]} project]
    (-> (project-dependency-map [name version]
                                (map #(vector (first %) (second %)) dependencies)) ;; ignore exclusions)
        clojure.pprint/pprint)))

#_(clojure.pprint/pprint (project-dependency-map ["lambdawerk/ess" "0.66.0-SNAPSHOT"]
                                                 (fetch-dependencies-from-pom ["lambdawerk/ess" "0.66.0-SNAPSHOT"])))
