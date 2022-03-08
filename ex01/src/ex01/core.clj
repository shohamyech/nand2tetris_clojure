(ns ex01.core
  (:require [ex01.commands :as commands])
  (:require [clojure.string :as str])
  (:require [clojure.java.io :as io])
  (:gen-class))

;Shoham Yechezkely 314855503
;Jacob Fredman 037982519
;Group 150060.01.5782.48


(defn HandlePush [s file-name]
  (case (first s)
    "constant" (commands/push-const (last s))
    "local" (commands/push-lcl-arg-this-that "LCL" (last s))
    "argument" (commands/push-lcl-arg-this-that "ARG" (last s))
    "this" (commands/push-lcl-arg-this-that "THIS" (last s))
    "that" (commands/push-lcl-arg-this-that "THAT" (last s))
    "temp" (commands/push-temp (last s))
    "static" (commands/push-static (last s) file-name)
    "pointer" (commands/push-pointer (last s))
    "Unknowed command"))

(defn HandlePop [s file-name]
  (case (first s)
    "local" (commands/pop-lcl-arg-this-that "LCL" (last s))
    "argument" (commands/pop-lcl-arg-this-that "ARG" (last s))
    "this" (commands/pop-lcl-arg-this-that "THIS" (last s))
    "that" (commands/pop-lcl-arg-this-that "THAT" (last s))
    "temp" (commands/pop-temp (last s))
    "static" (commands/pop-static (last s) file-name)
    "pointer" (commands/pop-pointer (last s))
    "Unknowed command"))


(defn HandleCommand [s file-name]
  (case (first s)
    "push" (HandlePush (rest s) file-name)
    "pop" (HandlePop (rest s) file-name)
    "add" (commands/add)
    "sub" (commands/sub)
    "neg" (commands/neg)
    "eq" (commands/eq)
    "lt" (commands/lt)
    "gt" (commands/gt)
    "and" (commands/And)
    "or" (commands/Or)
    "not" (commands/Not)
    "//" ""
    "\n" ""
    "" ""
    "Unknowed command")
  )

(defn HandleFile [file]
  (apply str (->> (last file)
                  (str/split-lines)
                  (map #(str/split % #" "))
                  (map #(HandleCommand % (first file))))))


;gets vm files names from folder
(defn files-names [path]
  (->> (io/file path)
       (file-seq)
       (filter #(.isFile %))
       (map str)
       (map #(re-find #"\w*.vm$" %))
       (remove nil?)
       (map #(str/replace % #".vm$" ""))))

(defn -main []
  (println "Enter path")

  (let [path (read-line)
        output-file ;constructing the output file name - the same as the folder
        (str path "\\" (re-find #"\w*$" path) ".asm")]

    ;checking folder exists
    (when-not (.exists (java.io.File. path))
      (println "Invalid path")
      (System/exit 0))


    (->> (io/file path)                      ;get folder file obj
         (file-seq)                          ;get all files from folder
         (filter #(.isFile %))               ;filter files
         (map str)                           ;map to files path as string
         (filter #(re-find #".vm$" %))       ;filter vm files
         (map slurp)                         ;map to the files content
         (map vector (files-names path))     ;add file name to each file content
         (map HandleFile)                    ;convert to HandleFile result
         (str/join)                          ;join all files
         (spit output-file))                 ;write to output file
    )
  (str "Finished!"))
