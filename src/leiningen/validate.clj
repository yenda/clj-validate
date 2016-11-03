(ns leiningen.validate
  (:require [clojure.xml :as xml]
            [clojure.pprint :as pprint]))

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

(defn dependency-to-map
  [dependency]
  (into {} (map (fn [{:keys [tag content]}]
                  [tag (if (= 1 (count content))
                         (first content)
                         content)])
                dependency)))

(defn dependency-to-vec
  [{:keys [groupId artifactId version]}]
  [(str groupId "/" artifactId) version])

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
         (map dependency-to-map)
         (remove #(= "test" (:scope %)))
         (mapv dependency-to-vec))
    (catch java.net.MalformedURLException _
      #_(println project-full-name version "could not be found, the pom file probably includes some variable version numbers")
      nil)
    (catch java.io.FileNotFoundException _
      #_(println project-full-name version "could not be found, run lein deps or ignore this message")
      nil)))

(defn project-dependency-map
  ([dependency]
   (project-dependency-map dependency (fetch-dependencies-from-pom dependency)))
  ([[project version :as dependency] dependencies]
   (zipmap dependencies
           (map project-dependency-map dependencies))))

(defn check-dependency-mismatch*
  [parents project dependency-map]
  (->> (reduce (fn [acc [[name version :as dependency] dependencies]]
                 (-> acc
                     (update-in [name version] #(if %1 (conj %1 (conj parents %2)) [(conj parents %2)]) project)
                     (deep-merge (check-dependency-mismatch* (conj parents project) dependency dependencies))))
               {}
               dependency-map)))

(defn check-dependency-mismatch
  [project dependency-map]
  (->> (check-dependency-mismatch* [] project dependency-map)
       (filter #(< 1 (count (second %))))
       (into {})))

(defn mismatches-printer
  [mismatches]
  (doseq [[dependency versions] mismatches]
    (println dependency)
    (println "--------------------------------------------------------------------")
    (doseq [[version usages] versions]
      (println "*" version)
      (doseq [usage usages]
        (println "  -" (if (string? (first usage))
                         usage
                         (clojure.string/join " -> " (map str usage)))))
      (println))
    (println "====================================================================")))

(defn validate
  [project & args]
  (let [{:keys [name version dependencies]} project
        printer (if args mismatches-printer pprint/pprint)]
    (->> dependencies
         (map (fn [[project version & _]] [(str project) version])) ;; ignore exclusions
         (project-dependency-map [name version])
         (check-dependency-mismatch [name version])
         printer)))
