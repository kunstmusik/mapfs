(ns mapfs.core
  (:require [clojure.string :as str]
            [clojure.edn :as edn]
            ;[clojure.java.shell :refer [sh]]
            )
  (:import [jline TerminalFactory TerminalFactory$Flavor]
           [jline.console ConsoleReader]
           [java.lang ProcessBuilder$Redirect])
  (:gen-class :main true))

(defonce ^:private FS_ROOT (atom nil))
(defonce ^:private CURRENT_DIR (atom []))
(defonce ^:private FS_FILENAME (atom nil))
(defonce ^:private CONSOLE (atom nil))
(defonce ^:private TERMINAL (atom nil))

(defn write-fs! 
  "Write current filesystem to filename as EDN."
  [filename]
  (spit filename (pr-str @FS_ROOT)))

(defn load-fs! 
  "Load EDN from filename as current filesystem."
  [filename]
  (reset! FS_FILENAME filename)
  (if (.exists (java.io.File. filename))
    (with-open [r (java.io.PushbackReader. 
                    (clojure.java.io/reader filename))]
      (reset! FS_ROOT (edn/read r)))
    (reset! FS_ROOT {})))

(defn save-fs! 
  "Save current filesystem to file used with load-fs!"
  []
  (let [v @FS_FILENAME]
    (if (nil? v) 
      (str "ERROR: current filesystem not loaded from file")
      (do
        (write-fs! v)
        (str "Saved filesystem to: " v)))))

(defn mount! 
  "Mounts a map as the current filesystem."
  [m] 
  (reset! FS_ROOT m)
  (reset! CURRENT_DIR []))

(defn is-dir?
  "Checks if value is a directory (is a map and does not have :tag key)"
  [v] 
  (and (map? v) (not (:tag v))))

(defn- resolve-path
  [parts]
  (let [p (if (keyword? parts) [parts] parts)]
  (reduce #(case %2
            :.. (pop %1) 
            :. %1
            (conj %1 %2))
          @CURRENT_DIR p)))

;(defn split-path-regex
;  [parts]
  
;  )

(defn ls 
  "List keys in current directory of filesystem, or given path."
  [& key-path]
  (->>
    (sort (keys (get-in @FS_ROOT (resolve-path key-path))))
    (map 
      #(let [path (conj @CURRENT_DIR %)
             v (get-in @FS_ROOT path)]
         (if (is-dir? v)
           (str "D " %) 
           (str "- " %))))
    (str/join "\n")))

(defn pwd 
  "Print current working directory."
  []
  @CURRENT_DIR)

(defn cd 
  "Changes working directory to new path. Can use :.. as relative location, one up in path."
  [key-path]
  (let [new-path (resolve-path key-path)]
    (reset! CURRENT_DIR new-path)
    (str "Current path: " new-path)))

(defn cp 
  "Copy value from [src path] to [dest path]. src and dest should be relative path vectors."
  [src dest]
  (let [src-path (resolve-path src)
        dest-path (resolve-path dest)]
    (swap! FS_ROOT assoc-in dest-path (get-in @FS_ROOT src-path))
    (str "Copied value from " src-path " to " dest-path)))

(defn cat 
  "Prints value of :key-name associated in current directory." 
  [key-path]
  (get-in @FS_ROOT 
          (resolve-path key-path)))

(defn put 
  "Associates a new value in current directory with key-name/value." 
  [key-name value]
  (let [path (resolve-path key-name)
        existed (get-in @FS_ROOT path)] 
    (swap! FS_ROOT assoc-in (into @CURRENT_DIR [key-name]) value)
    (if (nil? existed)
      (str "Added value for " key-name)  
      (str "Updated value for key " key-name)))
  
  )

(defn path-split
  "Splits a path into base and f, where f is the last key and base is the rest."
  [path]
  [(pop path) (last path)])

(defn rename
  "Rename file to path. Arguments should be [:src :key :path] [:dest :key :path]."
  [src dest]
  (let [src-path (resolve-path src)
        src-val (get-in @FS_ROOT src-path)
        [base f] (path-split src-path)
        dest-path (resolve-path dest)]
    (cp src-path dest-path) 
    (if (zero? (count base))
      (swap! FS_ROOT dissoc f)
      (swap! FS_ROOT update-in base dissoc f))
    (println base " : " f)
    (str "Renamed from " src-path " to " dest-path)))

(defn mkdir 
  "Creates an empty directory (map) with key-name in working directory."
  [key-name]
  (put key-name {})
  (str "New path created: " (resolve-path [key-name])))

(defn rmdir 
  "Removes a directory"
  [key-name]
  (let [path (resolve-path [key-name])
        v (get-in @FS_ROOT path)]
    (cond 
      (nil? v) 
      (str "Error: path is nil: " path)
      (is-dir? v) 
      (do 
        (if (= 1 (count path)) 
          (swap! FS_ROOT dissoc key-name)
          (swap! FS_ROOT update-in @CURRENT_DIR dissoc key-name))
        (str "Path removed: " path))
      :else
      (str "Error: Path is not a directory: " path))))

(defn rm 
  "Removes a value"
  [key-name]
  (let [path (resolve-path [key-name])
        v (get-in @FS_ROOT path)]
    (cond 
      (nil? v) 
      (str "Error: path is nil: " path)
      (is-dir? v) 
      (str "Error: Path is not a directory: " path)
      :else
      (do 
        (if (= 1 (count path)) 
          (swap! FS_ROOT dissoc key-name)
          (swap! FS_ROOT update-in @CURRENT_DIR dissoc key-name))
        (str "Path removed: " path)))))

(defn mv
  "Move file to path. (Currently an alias to rename)."
  [src dest]
  (rename src dest))


;; SHELL COMMANDS

(defn help
  "Print available commands." 
  []
  (let [publics (sort-by first (ns-publics 'mapfs.core))]
    (->>
      (map (fn [[k v]] (str "  " k " - " (:doc (meta v)))) publics)
      (into ["Available commands:\n"])
      (str/join "\n" ))))

;; SHELL CODE

(def ^:private completion-handler
  (reify 
   jline.console.completer.Completer 
    (complete [_ buffer cursor candidates]
      (let [part (subs buffer 0 cursor)
            indx (.lastIndexOf part " ")
            part (if (pos? indx) (subs part (.lastIndexOf part " ")) part)
            part (str/trim part)
            ks (map str (keys (get-in @FS_ROOT @CURRENT_DIR)))]
        (if (empty? part)
          (.addAll candidates ks) 
          (.addAll candidates (filter #(.startsWith % part) ks)))
        (inc indx)))))

(defn- init-terminal []
  (let [term (TerminalFactory/create)
        console (ConsoleReader. System/in System/out term)]
    (.init term)
    (.addCompleter console completion-handler)
    (reset! TERMINAL term)
    (reset! CONSOLE console)
    [term console]))

(defn edit
  [value]
  (let [f (java.io.File/createTempFile "mapfs" "edn")]
    (spit f (pr-str value))
    (println f)
    (.shutdown @CONSOLE)
    ;(.restore @TERMINAL)
    (let [pb (ProcessBuilder. ["vim" (.getAbsolutePath f)])
          _ (.redirectInput pb ProcessBuilder$Redirect/INHERIT)
          _ (.redirectOutput pb ProcessBuilder$Redirect/INHERIT)
          p (.start pb)]
      (.waitFor p))
    (reset! CONSOLE (ConsoleReader. System/in System/out @TERMINAL))
    (.reset @TERMINAL)
    ;(init-terminal)
    (slurp f)  
    ))

(defn -main [& args]
  (println "Map FS - 0.1.0")  
  (when (pos? (count args))
    (load-fs! (first args))
    (println "Loading Filesystem: " (first args)))
  (let [bindings {#'*ns* *ns*}
        [term console] (init-terminal)] 
    (push-thread-bindings bindings)
    (in-ns 'mapfs.core)
    (loop []
      (println)
      (let [v (.readLine @CONSOLE "mapfs> ")]
        (when-not (= "exit" v)
          (try 
            (println (eval (read-string (str "(" v ")")))) 
            (catch Exception e
              (println "  ERROR: Invalid command:" (.getMessage e))))
          (recur)
          )))
    (pop-thread-bindings)
    ))
