(ns ex04.parser
  (:require [clojure.string :as str])
  (:require [clojure.java.io :as io]))

(declare CompileStatements)
(declare CompileExpressions)
(declare CompileTerm)

(defn accept-token [in-out]
  (let [in  (first in-out)
        out (last in-out)]
    (vector (rest in) (str out (first in) "\n"))))

(defn write [tok in-out]
  (let [in  (first in-out)
        out (last in-out)]
    (vector in (str out tok "\n"))))

(defn next-token [in-out]      
  (->> in-out
       (first)
       (first)
       (re-find #">.+<")
       (rest)
       (butlast)
       (#(if (= (first %) \space) (rest %) %))
       (#(if (= (last %) \space) (butlast %) %))
       (str/join "")))


(defn CompileExpressionList [in-out]
  (->> in-out
       (#(if (not= (next-token %) ")")
           (CompileExpressions %)
           %))
       (#(if (= (next-token %) ",")
           (->> %
                (accept-token)                   ;,
                (CompileExpressionList))
           %))))

(defn CompileTerm2 [in-out]
   (cond (= (next-token in-out) "[")
         (->> in-out
              (accept-token)                     ;[
              (CompileExpressions)
              (accept-token))                    ;]
         (= (next-token in-out) "(")
         (->> in-out
              (accept-token)                     ;(
              (write "<expressionList>")
              (CompileExpressionList)
              (write "</expressionList>")
              (accept-token))                    ;)
         (= (next-token in-out) ".")
         (->> in-out
              (accept-token)                     ;.
              (accept-token)                     ;sunroutineName
              (accept-token)                     ;(
              (write "<expressionList>")
              (CompileExpressionList)
              (write "</expressionList>")
              (accept-token)                     ;)
              )
         :else 
         in-out
         ))


(defn CompileTerm1 [in-out]
  (cond (= (next-token in-out) "(")
        (->> in-out
             (accept-token)                     ;(
             (CompileExpressions)
             (accept-token))                    ;)
        (re-matches #"[\-~]" (next-token in-out))
        (->> in-out
             (accept-token)                     ;unaryOp
             (CompileTerm))
        :else
        (CompileTerm2 (accept-token in-out))))  ;term

(defn CompileTerm [in-out]
  (->> in-out
       (write "<term>")
       (CompileTerm1)
       (write "</term>")))

(defn CompileExpression [in-out]
  (->> in-out
       (CompileTerm)
       (#(if (re-matches #"[+\-*/&|=]|&lt;|&gt;|&amp;" (next-token %))
           (CompileExpression (accept-token %))
           %)
        ))
       )

(defn CompileExpressions [in-out]
  (->> in-out
       (write "<expression>")
       (CompileExpression)
       (write "</expression>")))

(defn CompileReturn [in-out]
  (->> in-out
       (write "<returnStatement>")
       (accept-token)                 ;return
       (#(if (not= (next-token %) ";")
           (CompileExpressions %)
           %))
       (accept-token)                 ;;
       (write "</returnStatement>")
  ))

(defn CompileDo [in-out]
  (->> in-out
       (write "<doStatement>")
       (accept-token)                  ;do
       (accept-token)                  ;subroutineName|className|varName
       (#(if (= (next-token %) "(")
           (->> %
                (accept-token)         ;(
                (write "<expressionList>")
                (CompileExpressionList)
                (write "</expressionList>")
                (accept-token)         ;)
                )
           (->> %
                (accept-token)         ;.
                (accept-token)         ;subroutineName
                (accept-token)         ;(
                (write "<expressionList>")
                (CompileExpressionList)
                (write "</expressionList>")
                (accept-token)         ;)
                )
           ))
       (accept-token)                  ;;
       (write "</doStatement>"))
  )

(defn CompileWhile [in-out]
  (->> in-out
       (write "<whileStatement>")
       (accept-token)                  ;while
       (accept-token)                  ;(
       (CompileExpressions)
       (accept-token)                  ;)
       (accept-token)                  ;{
       (write "<statements>")
       (CompileStatements)
       (write "</statements>")
       (accept-token)                  ;}
       (write "</whileStatement>")))

(defn CompileElse [in-out]
  (if (= (next-token in-out) "else")
    (->> in-out
         (accept-token)                     ;else
         (accept-token)                     ;{
         (write "<statements>")
         (CompileStatements)
         (write "</statements>")
         (accept-token)                     ;}
         )
    in-out
    ))

(defn CompileIf [in-out]
  (->> in-out
       (write "<ifStatement>")
       (accept-token)                  ;if
       (accept-token)                  ;(
       (CompileExpressions)
       (accept-token)                  ;)
       (accept-token)                  ;{
       (write "<statements>")
       (CompileStatements)
       (write "</statements>")
       (accept-token)                  ;}
       (CompileElse)
       (write "</ifStatement>")))

(defn CompileSquareBrackets [in-out]
  (if (= (next-token in-out) "[")
    (->> in-out
         (accept-token)                ;[
         (CompileExpressions)
         (accept-token)                ;]
         )
    in-out))

(defn CompileLet [in-out]
  (->> in-out
       (write "<letStatement>")
       (accept-token)                  ;let
       (accept-token)                  ;varName
       (CompileSquareBrackets)
       (accept-token)                  ;=
       (CompileExpressions)
       (accept-token)                  ;;
       (write "</letStatement>")))

(defn CompileStatements [in-out]
  (cond (= (next-token in-out) "let")
        (CompileStatements (CompileLet in-out))
        (= (next-token in-out) "if")
        (CompileStatements  (CompileIf in-out))
        (= (next-token in-out) "while")
        (CompileStatements (CompileWhile in-out))
        (= (next-token in-out) "do")
        (CompileStatements (CompileDo in-out))     
        (= (next-token in-out) "return")
        (CompileStatements (CompileReturn in-out))
        :else in-out))


(defn CompileMultipleVars [in-out]
  (if (= (next-token in-out) ",")
    (->> in-out
         (accept-token)                ;,
         (accept-token)                ;identifier
         (CompileMultipleVars))
    in-out))

(defn CompileVarDec [in-out]
  (if (= (next-token in-out) "var")
    (->> in-out
         (write "<varDec>")
         (accept-token)                ;var 
         (accept-token)                ;type
         (accept-token)                ;varName
         (CompileMultipleVars)
         (accept-token)                ;;
         (write "</varDec>")
         (CompileVarDec)               
         )
    in-out))

(defn CompileSubroutineBody [in-out]
  (->> in-out
       (write "<subroutineBody>")
       (accept-token)                 ;{
       (CompileVarDec)
       (write "<statements>")
       (CompileStatements)
       (write "</statements>")
       (accept-token)                 ;}
       (write "</subroutineBody>")
       ))

(defn CompileMultipleParameterList [in-out]
  (if (= (next-token in-out) ",")
    (->> in-out
         (accept-token)                ;,
         (accept-token)                ;type
         (accept-token)                ;varName
         (CompileMultipleParameterList))
    in-out))

(defn CompileParameterList [in-out]
  (if (not= (next-token in-out) ")")
    (->> in-out
         (accept-token)               ;type
         (accept-token)               ;varName
         (CompileMultipleParameterList))
    in-out)
  )

(defn CompileSubroutineDec [in-out]
  (if (or (= (next-token in-out) "constructor")
          (= (next-token in-out) "function")
          (= (next-token in-out) "method"))
    (->> in-out
         (write "<subroutineDec>")
         (accept-token)                ;constructor | function | method
         (accept-token)                ;type
         (accept-token)                ;subroutine name
         (accept-token)                ;(
         (write "<parameterList>")
         (CompileParameterList)
         (write "</parameterList>")
         (accept-token)                ;)
         (CompileSubroutineBody)
         (write "</subroutineDec>")
         (CompileSubroutineDec))
    in-out))

(defn CompileClassVarDec [in-out]
  (if (or (= (next-token in-out) "static")
          (= (next-token in-out) "field"))
    (->> in-out
         (write "<classVarDec>")
         (accept-token)                ;static | field
         (accept-token)                ;type
         (accept-token)                ;identifier
         (CompileMultipleVars)
         (accept-token)                ;;
         (write "</classVarDec>")
         (CompileClassVarDec))
    in-out))

(defn CompileClass [in-out]
  (->> in-out
       (write "<class>")
       (accept-token)            ;class
       (accept-token)            ;<class-name>
       (accept-token)            ;{
       (CompileClassVarDec)
       (CompileSubroutineDec)
       (accept-token)            ;}
       (write "</class>")))

(defn parse-file [file]
  (->> file
       (slurp)               ;reading file
       (str/split-lines)     ;spliting to lines
       (rest)                ;removeing <tokens>
       (butlast)             ;removing </tokens>
       (#(vector % ""))      ;creating a vector of input and output
       (CompileClass)        ;calling root token
       ))      

(defn getXmlFileName [file]
  (str/replace file #"T.xml" ".xml"))

(defn parse-files [path]

  ;checking folder exists
  (when-not (.exists (java.io.File. path))
    (println "Invalid path")
    (System/exit 0))

  (->> (io/file path)                                      ;get folder file obj
       (file-seq)                                          ;get all files from folder
       (filter #(.isFile %))                               ;filter files
       (map str)                                           ;map to files path as string
       (filter #(re-find #"T.xml$" %))                     ;filter jack files
       (map #(vector (getXmlFileName %) (parse-file %)))   ;map to new file name & parsed file
       (map #(spit (first %) (last (last %))))             ;write to output file
       ))
