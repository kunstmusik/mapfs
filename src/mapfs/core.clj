(ns mapfs.core
  (:require [clojure.string :as str]
            [clojure.edn :as edn])
  (:gen-class :main true))

(defonce FS_ROOT (atom nil))

(defonce CURRENT_DIR (atom []))

(defn write-fs! [filename]
  (spit filename (pr-str @FS_ROOT)))

(defn load-fs! [filename]
  (if (.exists (java.io.File. filename))
    (with-open [r (java.io.PushbackReader. 
                    (clojure.java.io/reader filename))]
      (reset! FS_ROOT (edn/read r)))
    (throw (Exception. "File not found :("))))

(defn mount 
  [m] 
  (reset! FS_ROOT m))

(defn ls []
  (doseq [ks (keys (get-in @FS_ROOT @CURRENT_DIR))]
    (let [path (conj @CURRENT_DIR ks)
          v (get-in @FS_ROOT path)]
      (if (and (map? v) (not (:tag v)))
        (println (str "[" ks "]"))
        (println ks)))))

(defn pwd []
  @CURRENT_DIR)

(defn resolve-path
  [parts]
  (reduce #(if (= :.. %2) (pop %1) (conj %1 %2))
          @CURRENT_DIR parts))

(defn cd 
  [& parts]
  (let [new-path (resolve-path parts)]
    (reset! CURRENT_DIR new-path)))

(defn cp 
  [src dest]
  (let [src-path (resolve-path src)
        dest-path (resolve-path dest)]
    (swap! FS_ROOT assoc-in dest-path (get-in @FS_ROOT src-path))))

(defn cat [key-name]
  (get-in @FS_ROOT (into @CURRENT_DIR [key-name])))

(defn put [key-name value]
  (swap! FS_ROOT assoc-in (into @CURRENT_DIR [key-name]) value))

(defn mkdir [key-name]
  (put key-name {}))

(defn print-path []
  (print @CURRENT_DIR "> "))

(defn -main [& args]
  (println "Map FS - 0.1.0")  
  (in-ns 'mapfs.core)
  (loop []
    (println)
    (let [v (read-line)]
      (when-not (= "exit" v)
        (try 
          (println (eval (read-string (str "(" v ")")))) 
          (catch Exception e
            (println "  ERROR: Invalid command")))
        (recur)
        ))))
