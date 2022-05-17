(ns ex04.core
  (:require [clojure.string :as str])
  (:require [clojure.java.io :as io])
  (:gen-class))

(def keywords ["class" "method" "function" "constructor"
               "int" "boolean" "char" "void" "var" "this"
               "static" "field" "let" "do" "if" "else"
               "while" "return" "true" "false" "null"])

(def token-expression
  (re-pattern
   (str
    "\\d+|"                              ;integerConstant
    "[\\w\\-]+|"                         ;identifier
    "\"[^\"\\n]*\"|"                     ;StringConstant
    "[{}()\\[\\]\\.,;+\\-*/&|<>=~]|"     ;symbols
    (str/join "|" keywords))))           ;keywords

(defn getSymbolName [symb]
  (case symb
    "<" "&lt;"
    ">" "&gt;"
    "\"" "&quet;"
    "&" "&amp;"
    symb))

(defn getToken [token]
  (cond
    (re-matches (re-pattern (str/join "|" keywords)) token)
    (str "<keyword> " token " </keyword>")
    (re-matches #"\d+" token)
    (str "<integerConstant> " token " </integerConstant>")
    (re-matches #"[\w\-]+" token)
    (str "<identifier> " token " </identifier>")
    (re-matches #"\"[^\"\n]*\"" token)
    (str "<stringConstant> " (str/replace token #"\"" "") " </stringConstant>")
    (re-matches #"[{}()\[\]\.,;+\-*/&|<>=~]" token)
    (str "<symbol> " (getSymbolName token) " </symbol>")))

(defn tokenize [content]
  (->> content
       (#(str/replace % #"//[^\n]*\n|/\*(.*?)\*/" ""))                    ;removing comments
       (re-seq token-expression)
       (map getToken)
       (str/join "\n")
       (#(str "<tokens>\n" % "\n</tokens>\n"))))


(defn getFileName [file]
  (str/replace file #".jack" "T.xml"))

(defn -main [path]

  ;checking folder exists
  (when-not (.exists (java.io.File. path))
    (println "Invalid path")
    (System/exit 0))

  (->> (io/file path)                                         ;get folder file obj
       (file-seq)                                             ;get all files from folder
       (filter #(.isFile %))                                  ;filter files
       (map str)                                              ;map to files path as string
       (filter #(re-find #".jack$" %))                        ;filter jack files
       (map #(vector (getFileName %) (tokenize (slurp %))))   ;map to new file name & tokenized file
       (map #(spit (first %) (last %)))                       ;write to output file
       ))
