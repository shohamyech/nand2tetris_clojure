(ns ex05.core
  (:use [ex05.lex])
  (:use [ex05.parser])
  (:require [clojure.string :as str])
  (:require [clojure.java.io :as io]))

;Shoham Yechezkely 314855503
;Jacob Fredman 037982519
;Group 150060.01.5782.48

(defn getVmFileName [file]
  (str/replace file #".jack" ".vm"))

(defn to-vm [path] 
  (->> path
       (slurp)
       (tokenize)
       (parse-file)
       ))

(defn -main [path]
  
  ;checking folder exists
  (when-not (.exists (java.io.File. path))
    (println "Invalid path")
    (System/exit 0))

(->> (io/file path)                                      ;get folder file obj
     (file-seq)                                          ;get all files from folder
     (filter #(.isFile %))                               ;filter files
     (map str)                                           ;map to files path as string
     (filter #(re-find #".jack$" %))                     ;filter jack files
     (map #(spit (getVmFileName %) (to-vm %)))           ;write to output file
     ))
