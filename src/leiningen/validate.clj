(ns leiningen.validate
  (:require [clojure.xml :as xml]))

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

(defn pom-file [project version]
  (let [[groupId artifactId] (clojure.string/split (str project) #"/")
        groupId (clojure.string/replace groupId #"\." "/")]
    (str (System/getProperty "user.home") "/.m2/repository/" groupId "/" artifactId "/" version "/"
         artifactId "-" version ".pom")))

(defn get-dependencies
  [project version]
  (->> (pom-file project version)
       xml/parse
       :content
       (filter dependencies?)
       first
       :content
       (map :content)
       (map clean-dependency)))

(defn validate
  [project & args]
  (-> (reduce (fn [acc [dependency version]]
                (assoc acc dependency (get-dependencies dependency version)))
              {}
              (:dependencies project))
      clojure.pprint/pprint))
