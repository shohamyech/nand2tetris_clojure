(ns ex04.core
  (:use [ex04.lex])
  (:use [ex04.parser])
  (:gen-class))

(defn -main [path]
  (print (vector (tokenize-files path)
          (parse-files path))))  
  

;(-main "C:\\Users\\shoha\\Desktop\\SoftwarePrinciples\\nand2tetris\\nand2tetris\\projects\\10\\ArrayTest")
;(-main "C:\\Users\\shoha\\Desktop\\SoftwarePrinciples\\nand2tetris\\nand2tetris\\projects\\10\\ExpressionLessSquare")
;(-main "C:\\Users\\shoha\\Desktop\\SoftwarePrinciples\\nand2tetris\\nand2tetris\\projects\\10\\Square")