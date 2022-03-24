(ns ex02.core
  (:use [ex02.commands])
  (:require [clojure.string :as str])
  (:require [clojure.java.io :as io])
  (:gen-class))

;Shoham Yechezkely 314855503
;Jacob Fredman 037982519
;Group 150060.01.5782.48


(defn HandlePush [s file-name]

  (case (first s)
    "constant" (push-const (nth s 1))
    "local"    (push-lcl-arg-this-that "LCL"  (nth s 1))
    "argument" (push-lcl-arg-this-that "ARG"  (nth s 1))
    "this"     (push-lcl-arg-this-that "THIS" (nth s 1))
    "that"     (push-lcl-arg-this-that "THAT" (nth s 1))
    "temp"     (push-temp    (nth s 1))
    "static"   (push-static  (nth s 1) file-name)
    "pointer"  (push-pointer (nth s 1))
    "Unknowed command"))

(defn HandlePop [s file-name]
  (case (first s)
    "local"     (pop-lcl-arg-this-that "LCL"  (nth s 1))
    "argument"  (pop-lcl-arg-this-that "ARG"  (nth s 1))
    "this"      (pop-lcl-arg-this-that "THIS" (nth s 1))
    "that"      (pop-lcl-arg-this-that "THAT" (nth s 1))
    "temp"      (pop-temp    (nth s 1))
    "static"    (pop-static  (nth s 1) file-name)
    "pointer"   (pop-pointer (nth s 1))
    "Unknowed command"))


(defn HandleCommand [s file-name]
  (case (first s)
    "push"     (HandlePush (rest s) file-name)
    "pop"      (HandlePop  (rest s) file-name)
    "add"      (add)
    "sub"      (sub)
    "neg"      (neg)
    "eq"       (eq)
    "lt"       (lt)
    "gt"       (gt)
    "and"      (And)
    "or"       (Or)
    "not"      (Not)
    "goto"     (goto      (get s 1))
    "if-goto"  (if-goto   (get s 1))
    "label"    (put-label (get s 1))
    "call"     (call (get s 1) (get s 2) file-name)
    "function" (function (get s 1) (get s 2) file-name)
    "return"   (return1)
    "//"      ""
    "\n"      ""
    ""        ""
    "Unknowed command"))

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

(defn -main [path]

  ;checking folder exists
  (when-not (.exists (java.io.File. path))
    (println "Invalid path")
    (System/exit 0))

  ;constructing the output file name - the same as the folder
  (let [output-file
        (str path "\\" (re-find #"\w*$" path) ".asm")]

    (->> (io/file path)                      ;get folder file obj
         (file-seq)                          ;get all files from folder
         (filter #(.isFile %))               ;filter files
         (map str)                           ;map to files path as string
         (filter #(re-find #".vm$" %))       ;filter vm files
         (map slurp)                         ;map to the files content
         (map vector (files-names path))     ;add file name to each file content
         (map HandleFile)                    ;convert to HandleFile result
         (str/join)                          ;join all files
         (str (init))
         (spit output-file))                 ;write to output file
    )
  (str "Finished!"))

