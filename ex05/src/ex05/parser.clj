(ns ex05.parser
  (:use [ex05.tables])
  (:require [clojure.string :as str]))

(declare CompileStatements)
(declare CompileExpressions)
(declare CompileTerm)

(def Label (atom -1))

(def binary-op {"+" "add"
                "-" "sub"
                "*" "call Math.multiply 2"
                "/" "call Math.divide 2"
                "&amp;" "and"
                "|" "or"
                "&lt;" "lt"
                "&gt;" "gt"
                "=" "eq"})

(def unary-op {"-" "neg"
               "~" "not"})

(defn accept-token [dict]
  (assoc dict :in (rest (dict :in))))

(defn write [tok dict]
  (assoc dict :out (str (dict :out) tok "\n")))

(defn write-string1 [string dict]
  (if (not-empty string)
    (->> dict
         (write (str "push constant " (int (first string))))
         (write "call String.appendChar 2")
         (write-string1 (rest string)))
    dict))

(defn write-string [string dict]
  (->> dict
       (write (str "push constant " (count string)))
       (write "call String.new 1")
       (write-string1 string)))

(defn next-token [dict]
  (->> (dict :in)
       (first)
       (re-find #">.+<")
       (rest)
       (butlast)
       (#(if (= (first %) \space) (rest %) %))
       (#(if (= (last %) \space) (butlast %) %))
       (str/join "")))

(defn next-token-type [dict]
  (->> (dict :in)
       (first)
       (drop-last)
       (str/join "")
       (re-find #"<.+>")
       (rest)
       (butlast)
       (str/join "")))

(defn CompileExpressionList [argsNum dict]
  (if (not= (next-token dict) ")")
    (->> (CompileExpressions dict)
         (#(if (= (next-token %) ",")
             (->> (accept-token %)                   ;,
                  (CompileExpressionList (+ argsNum 1)))
             [(+ argsNum 1) %])))
    [argsNum dict]))

(defn CompileIdentifier [id dict]
  (cond (= (next-token dict) "[")
        (->> dict
             (accept-token)                     ;[
             (CompileExpressions)
             (#(write (push-var id %) %))
             (write "add")
             (write "pop pointer 1")
             (write "push that 0")
             (accept-token))                    ;]
        (= (next-token dict) ".")
        (->> dict
             (accept-token)                     ;.
             (#(if (some? (get-var-kind id %))             ;if an object class is its type
                 (let [class_name (get-var-type id %)
                       func_name (next-token %)]
                   (->> (write (push-var id %) %)
                        (accept-token)                     ;sunroutineName
                        (accept-token)                     ;(
                        (CompileExpressionList 1)
                        ((fn [n]
                           (write (str "call " class_name "." func_name " " (first n))
                                  (last n))))
                        (accept-token)                     ;)
                        ))
                 (let [func_name (next-token %)]             ;class is the id
                   (->> (accept-token %)                     ;sunroutineName
                        (accept-token)                       ;(
                        (CompileExpressionList 0)
                        ((fn [n]
                           (write (str "call " id "." func_name " " (first n))
                                  (last n))))
                        (accept-token)                     ;)
                        )))))
        (= (next-token dict) "(")
        (->> dict
             (write "push pointer 0")
             (accept-token)                     ;(
             (CompileExpressionList 1)
             (#(write (str "call " ((dict :class_tbl) :name) "." id " " (first %)) (last %)))
             (accept-token))                    ;)

        :else
        (write (push-var id dict) dict)))


(defn CompileTerm [dict]
  (cond (= (next-token dict) "(")
        (->> dict
             (accept-token)                     ;(
             (CompileExpressions)
             (accept-token))                    ;)
        (re-matches #"[\-~]" (next-token dict))
        (->> dict
             (accept-token)                     ;unaryOp
             (CompileTerm)
             (write (str (unary-op (next-token dict)))))
        (= (next-token-type dict) "integerConstant")
        (write (str "push constant " (next-token dict)) (accept-token dict))
        (= (next-token-type dict) "stringConstant")
        (write-string (next-token dict) (accept-token dict))
        (= (next-token-type dict) "keyword")
        (if (= (next-token dict) "this")
          (write "push pointer 0" (accept-token dict))
          (->> (write "push constant 0" dict)
               (#(if (= (next-token %) "true") (write "not" %) %))
               (accept-token)))
        :else
        (CompileIdentifier (next-token dict) (accept-token dict))))


(defn CompileExpressions1 [dict]
  (let [action (next-token dict)]
    (if (re-matches #"[+\-*/&|=]|&lt;|&gt;|&amp;" action)
      (->> dict
           (accept-token)
           (CompileTerm)
           (write (str (binary-op action))))
      dict)))

(defn CompileExpressions [dict]
  (->> dict
       (CompileTerm)
       (CompileExpressions1)))

(defn CompileReturn [dict]
  (->> dict
       (accept-token)                 ;return
       (#(if (not= (next-token %) ";")
           (CompileExpressions %)
           (write "push constant 0" %)))
       (write "return")
       (accept-token)                 ;;
       ))

(defn CompileDo [dict]
  (->> dict
       (accept-token)                  ;do
       (CompileTerm)
       (write "pop temp 0")
       (accept-token)                  ;;
       ))

(defn CompileWhile [dict]
  (let [label (swap! Label inc)]
    (->> dict
         (accept-token)                  ;while
         (accept-token)                  ;(
         (write (str "label WHILE_EXP" label))
         (CompileExpressions)
         (accept-token)                  ;)
         (accept-token)                  ;{
         (write "not")
         (write (str "if-goto WHILE_END" label))
         (CompileStatements)
         (write (str "goto WHILE_EXP" label))
         (write (str "label WHILE_END" label))
         (accept-token)                  ;}
         )))

(defn CompileIf [dict]
  (let [label (swap! Label inc)]
    (->> dict
         (accept-token)                            ;if
         (accept-token)                            ;(
         (CompileExpressions)
         (accept-token)                            ;)
         (accept-token)                           ;{)
         (write (str "if-goto IF_TRUE" label))
         (write (str "goto IF_FALSE" label))
         (write (str "label IF_TRUE" label))
         (CompileStatements)
         (accept-token)                           ;}
         (#(if (= (next-token %) "else")
             (->> %
                  (write (str "goto IF_END" label))
                  (write (str "label IF_FALSE" label))
                  (accept-token)                     ;else
                  (accept-token)                     ;{
                  (CompileStatements)
                  (accept-token)                     ;}
                  (write (str "label IF_END" label)))
             (write (str "label IF_FALSE" label) %))))))


(defn CompileLet [dict]
  (let [var_name (next-token (accept-token dict))]
    (->> dict
         (accept-token)                          ;let
         (accept-token)                          ;varName
         (#(if (= (next-token %) "[")
             (->> (accept-token %)                ;[
                  (CompileExpressions)
                  (accept-token)                  ;]
                  (accept-token)                  ;=
                  (write (push-var var_name dict))
                  (write "add")
                  (CompileExpressions)
                  (write "pop temp 0")
                  (write "pop pointer 1")
                  (write "push temp 0")
                  (write "pop that 0"))
             (->> (accept-token %)                  ;=
                  (CompileExpressions)
                  (write (pop-var var_name %)))))
         (accept-token)                  ;;
         )))


(defn CompileStatements [dict]
  (cond (= (next-token dict) "let")
        (CompileStatements (CompileLet dict))
        (= (next-token dict) "if")
        (CompileStatements  (CompileIf dict))
        (= (next-token dict) "while")
        (CompileStatements (CompileWhile dict))
        (= (next-token dict) "do")
        (CompileStatements (CompileDo dict))
        (= (next-token dict) "return")
        (CompileStatements (CompileReturn dict))
        :else dict))


(defn CompileMultipleStaticVars [type dict]
  (if (= (next-token dict) ",")
    (->> dict
         (accept-token)                             ;,
         (#(add-static type (next-token %) %))
         (accept-token)                             ;identifier
         (CompileMultipleStaticVars type))
    dict))

(defn CompileMultipleFieldVars [type dict]
  (if (= (next-token dict) ",")
    (->> dict
         (accept-token)                ;,
         (#(add-field type (next-token %) %))
         (accept-token)                ;identifier
         (CompileMultipleFieldVars type))
    dict))

(defn CompileMultipleVars [type dict]
  (if (= (next-token dict) ",")
    (->> dict
         (accept-token)                ;,
         (#(add-var type (next-token %) %))
         (accept-token)                ;identifier
         (CompileMultipleVars type))
    dict))

(defn CompileVarDec [dict]
  (if (= (next-token dict) "var")
    (->> dict
         (accept-token)                ;var
         (#(let [type (next-token %)
                 name (next-token (accept-token %))]
             (->> (add-var type name %)
                  (accept-token)                ;type
                  (accept-token)                ;varName
                  (CompileMultipleVars type)
                  (accept-token)                ;;
                  (CompileVarDec)))))
    dict))

(defn CompileSubroutineBody [dict]
  (->> dict
       (accept-token)                 ;{
       (CompileVarDec)
       (#(write (get-function-dec %) %))
       (#(cond (= ((% :func_tbl) :sub_type) "constructor")
               (->> (write (str "push constant " ((% :class_tbl) :field_symb)) %)
                    (write "call Memory.alloc 1")
                    (write "pop pointer 0"))
               (= ((% :func_tbl) :sub_type) "method")
               (->> (write "push argument 0" %)
                    (write "pop pointer 0"))
               :else %))
       (CompileStatements)
       (accept-token)                 ;}
       ))

(defn CompileMultipleParameterList [dict]
  (if (= (next-token dict) ",")
    (->> dict
         (accept-token)                ;,
         (#(add-arg (next-token %) (next-token (accept-token %)) %))
         (accept-token)                ;type
         (accept-token)                ;varName
         (CompileMultipleParameterList))
    dict))

(defn CompileParameterList [dict]
  (if (not= (next-token dict) ")")
    (->> dict
         (#(add-arg (next-token %) (next-token (accept-token %)) %))
         (accept-token)               ;type
         (accept-token)               ;varName
         (CompileMultipleParameterList))
    dict))

(defn CompileSubroutineDec [dict]
  (if (or (= (next-token dict) "constructor")
          (= (next-token dict) "function")
          (= (next-token dict) "method"))
    (let [sub-type (next-token dict)
          ret-type (next-token (accept-token dict))
          name (next-token (accept-token (accept-token dict)))]
      (->> dict
           (init-func-tbl sub-type ret-type name)
           (accept-token)                ;constructor | function | method
           (accept-token)                ;type
           (accept-token)                ;subroutine name
           (accept-token)                ;(
           (#(if (= sub-type "method") (add-arg (% :name) "this" %) %))
           (CompileParameterList)
           (accept-token)                ;)
           (CompileSubroutineBody)
           (CompileSubroutineDec)))
    dict))

(defn CompileClassVarDec [dict]
  (cond (= (next-token dict) "static")
        (->> dict
             (accept-token)                         ;static | field
             (#(let [type (next-token %)
                     id (next-token (accept-token %))]
                 (->> (add-static type id %)
                      (accept-token)                ;type
                      (accept-token)                ;identifier
                      (CompileMultipleStaticVars type))))
             (accept-token)                ;;
             (CompileClassVarDec))
        (= (next-token dict) "field")
        (->> dict
             (accept-token)                         ;static | field
             (#(let [type (next-token %)
                     id (next-token (accept-token %))]
                 (->> (add-field type id %)
                      (accept-token)                ;type
                      (accept-token)                ;identifier
                      (CompileMultipleFieldVars type))))
             (accept-token)                ;;
             (CompileClassVarDec))
        :else dict))

(defn CompileClass [dict]
  (->> dict
       (accept-token)            ;class
       (#(init-class-tbl (next-token %) %))
       (accept-token)            ;<class-name>
       (accept-token)            ;{
       (CompileClassVarDec)
       (CompileSubroutineDec)
       (accept-token)            ;}
       ))

(defn parse-file [file]
  (->> {:in file :out "" :class_tbl {} :func_tbl {}}
       (CompileClass)        ;calling root token
       (#(get % :out))
       ;(seq)
       ))

(def f '("<keyword> class </keyword>" "<identifier> Main </identifier>"
         "<symbol> { </symbol>" "<keyword> field </keyword>"
         "<keyword> int </keyword>" "<identifier> a </identifier>"
         "<symbol> ; </symbol>" "<keyword> static </keyword>" "<keyword> int </keyword>" "<identifier> b </identifier>" 
         "<symbol> ; </symbol>" "<keyword> function </keyword>" "<keyword> void </keyword>" "<identifier> main </identifier>" "<symbol> ( </symbol>" "<keyword> int </keyword>" "<identifier> c </identifier>" 
         "<symbol> , </symbol>" "<keyword> char </keyword>" "<identifier> d </identifier>" "<symbol> ) </symbol>" "<symbol> { </symbol>" "<keyword> var </keyword>" "<identifier> Array </identifier>" "<identifier> a </identifier>" 
         "<symbol> ; </symbol>" "<keyword> var </keyword>" "<keyword> int </keyword>" "<identifier> length </identifier>" "<symbol> ; </symbol>" "<keyword> var </keyword>" "<keyword> int </keyword>" "<identifier> i </identifier>"
         "<symbol> , </symbol>" "<identifier> sum </identifier>" "<symbol> ; </symbol>" "<keyword> let </keyword>" "<identifier> a </identifier>" "<symbol> = </symbol>" "<identifier> Array </identifier>" "<symbol> . </symbol>" "<identifier> new </identifier>"
         "<symbol> ( </symbol>" "<identifier> length </identifier>" "<symbol> ) </symbol>" "<symbol> ; </symbol>" "<keyword> let </keyword>" "<identifier> i </identifier>" "<symbol> = </symbol>" "<integerConstant> 0 </integerConstant>"
         "<symbol> ; </symbol>" "<keyword> return </keyword>" "<symbol> ; </symbol>"))

;(parse-file f)

;; (defn sim [x] (+ 1 #break x))

;; (defn a []
;;   (->> '("ad " "sd" "asd ") 
;;         (str #break "asdasd"))
;;   )
;; (sim 2)
