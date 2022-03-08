(ns ex01.commands)

;-----------------------------PUSH-----------------------------------
(defn pushD []
  (str
   "@SP
A=M
M=D
@SP
M=M+1

"))

(defn push-const [x]
  (str
   "//push " x "
@" x "
D=A
" (pushD)))


(defn push-lcl-arg-this-that [seg, idx]
  (str
   "//push " seg " " idx "
@" idx "
D=A
@" seg "
A=M+D
D=M
" (pushD)))

(defn push-temp [x]
  (str
   "//push temp " x " 
@" (+ 5 (Integer/parseInt x)) "
D=M
" (pushD)))

(defn push-static [x file-name]
  (str
   "//push static " x "
@" file-name "." x "
D=M
" (pushD)))

(defn push-pointer [x]
  (str
   "//push pointer " x "
@" (+ 3 (Integer/parseInt x)) "
D=M
" (pushD)))


;-----------------------------POP-------------------------------------

(defn popD []
  (str
   "@SP
AM=M-1
D=M
"))

(defn pop-lcl-arg-this-that [seg, idx]
  (str
   "//pop " seg " " idx"
@" seg "
D=M
@" idx "
D=D+A
@SP
M=M-1
A=M
A=M
A=A+D
D=A-D
A=A-D
M=D

"))

(defn pop-temp [x]
  (str
   "//pop temp " x "\n"
(popD)
"@" (+ 5 (Integer/parseInt x)) "
M=D

"))

(defn pop-static [x file-name]
  (str
   "//pop static " x "\n"
   (popD)
   "@" file-name "." x "
M=D

"))

(defn pop-pointer [x]
  (str
   "//pop pointer " x
   (popD)
   "@" (+ 3 (Integer/parseInt x)) "
M=D

"))


;------------------------------ARITHMETIC------------------------------
(defn add []
  (str
   "//add
@SP
A=M-1
D=M
A=A-1
M=M+D
@SP
M=M-1

"))

(defn sub []
  (str
   "//sub
@SP
A=M-1
D=M
A=A-1
M=M-D
@SP
M=M-1

"))

(defn neg []
  (str
   "//neg
@SP
A=M-1
M=-M

"))

;--------------------------LOGICAL------------------------------------
(def True (atom 0))
(def False (atom 0))

(defn eq []
  (swap! True inc)
  (swap! False inc)
  (str
   "//eq
@SP
A=M-1
D=M
A=A-1
D=D-M
@IF_TRUE" @True "
D;JEQ
D=0
@IF_FALSE" @False "
0;JMP
(IF_TRUE" @True ")
D=-1
(IF_FALSE" @False ")
@SP
A=M-1
A=A-1
M=D
@SP
M=M-1

"))

(defn lt []
  (swap! True inc)
  (swap! False inc)
  (str
   "//lt
@SP
M=M-1
A=M
D=M
@SP
A=M-1
D=D-M
@IF_TRUE" @True "
D;JGT
@SP
A=M-1
M=0
@IF_FALSE" @False "
0;JMP
(IF_TRUE" @True ")
@SP
A=M-1
M=-1
(IF_FALSE" @False ")

"))

(defn gt []
  (swap! True inc)
  (swap! False inc)
  (str
   "//gt
@SP
M=M-1
A=M
D=M
@SP
A=M-1
D=M-D
@IF_TRUE" @True "
D;JGT
@SP
A=M-1
M=0
@IF_FALSE" @False "
0;JMP
(IF_TRUE" @True ")
@SP
A=M-1
M=-1
(IF_FALSE" @False ")

"))

(defn And []
  (str
"//and
@SP
M=M-1
A=M
D=M
A=A-1
M=D&M

"))

(defn Or []
  (str
"//or
@SP
M=M-1
A=M
D=M
A=A-1
M=D|M

"))

(defn Not []
  (str
"//not
@SP
A=M-1
M=!M

"))
