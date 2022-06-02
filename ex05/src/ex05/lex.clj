(ns ex05.lex
  (:require [clojure.string :as str])
  (:require [clojure.java.io :as io]))

(def keywords ["class" "method" "function" "constructor"
               "int" "boolean" "char" "void" "var" "this"
               "static" "field" "let" "do" "if" "else"
               "while" "return" "true" "false" "null"])

(def comment-expression
  (re-pattern "//[^\\n]*\\n|/\\*([^*]|[\\r\\n]|(\\*+([^*/]|[\\r\\n])))*\\*+/"))

(def token-expression
  (re-pattern
   (str
    "[{}()\\[\\]\\.,;+\\-*/&|<>=~]|"     ;symbols
    "[a-zA-Z_][a-zA-Z0-9_]*|"            ;identifier
    "\\d+|"                              ;integerConstant
    "\"[^\"\\n]*\"|"                     ;StringConstant
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
    (re-matches #"[{}()\[\]\.,;+\-*/&|<>=~]" token)
    (str "<symbol> " (getSymbolName token) " </symbol>")
    (re-matches (re-pattern (str/join "|" keywords)) token)
    (str "<keyword> " token " </keyword>")
    (re-matches #"\d+" token)
    (str "<integerConstant> " token " </integerConstant>")
    (re-matches #"[a-zA-Z_][a-zA-Z0-9_]*" token)
    (str "<identifier> " token " </identifier>")
    (re-matches #"\"[^\"\n]*\"" token)
    (str "<stringConstant> " (str/replace token #"\"" "") " </stringConstant>")
    :else (str "error")))

(defn tokenize [content]
  (->> content
       (#(str/replace % comment-expression ""))               ;removing comments
       (re-seq token-expression)                              ;spliting to tokens
       (map getToken)                                         ;mapping to xml tags
       ))              
