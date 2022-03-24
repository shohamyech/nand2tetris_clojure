(ns ex02.commands)

;-----------------------------PUSH-----------------------------------
(defn pushD []
  (str
"@SP
A=M
M=D
@SP
M=M+1"))

(defn push-const [x]
  (str
"@" x " //---------push " x "
D=A
" (pushD) " //---end push " x "\n"))


(defn push-lcl-arg-this-that [seg, idx]
  (str
"@" idx " //---------push " seg " " idx "
D=A
@" seg "
A=M+D
D=M
" (pushD) " //---end push " seg " " idx "\n"))

(defn push-temp [x]
  (str
"@" (+ 5 (Integer/parseInt x)) " //---------push temp " x " 
D=M
" (pushD) " //---end push temp " x "\n"))

(defn push-static [x file-name]
  (str
"@" file-name "." x " //---------push static " x "
D=M
" (pushD) " //---end push static " x "\n"))

(defn push-pointer [x]
  (str
"@" (+ 3 (Integer/parseInt x)) "//---------push pointer " x "
D=M
" (pushD) " //---end push pointer " x "\n"))


;-----------------------------POP-------------------------------------

(defn popD []
  (str
"@SP
AM=M-1
D=M"))

(defn pop-lcl-arg-this-that [seg, idx]
  (str
"@" seg " //---------pop " seg " " idx"
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
M=D //---end pop " seg " " idx "\n"))

(defn pop-temp [x]
  (str
"@SP //---------pop temp "x"
AM=M-1
D=M 
@" (+ 5 (Integer/parseInt x)) "
M=D //---end pop temp " x "\n"))

(defn pop-static [x file-name]
  (str
"@SP //---------pop static "x"
AM=M-1
D=M 
@" file-name "." x "
M=D //---end pop static "x "\n"))

(defn pop-pointer [x]
  (str
"@SP //---------pop pointer " x"
AM=M-1
D=M
@" (+ 3 (Integer/parseInt x)) "
M=D //---pop pointer "x "\n"))


;------------------------------ARITHMETIC------------------------------
(defn add []
  (str
"@SP //---------add
A=M-1
D=M
A=A-1
M=M+D
@SP
M=M-1 //---end add
"))

(defn sub []
  (str
"@SP //---------sub
A=M-1
D=M
A=A-1
M=M-D
@SP
M=M-1 //---end sub
"))

(defn neg []
  (str
"@SP //---------neg
A=M-1
M=-M //---end neg
"))

;--------------------------LOGICAL------------------------------------
(def True (atom 0))
(def False (atom 0))

(defn eq []
  (swap! True inc)
  (swap! False inc)
  (str
"@SP //---------eq
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
M=M-1  //---end eq
"))

(defn lt []
  (swap! True inc)
  (swap! False inc)
  (str
"@SP //---------lt
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
(IF_FALSE" @False ")  //---end lt
"))

(defn gt []
  (swap! True inc)
  (swap! False inc)
  (str
"@SP //---------gt
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
(IF_FALSE" @False ")  //---end gt
"))

(defn And []
  (str
"@SP //---------and
M=M-1
A=M
D=M
A=A-1
M=D&M //---end and
"))

(defn Or []
  (str
"@SP //---------or

M=M-1
A=M
D=M
A=A-1
M=D|M  //---end or
"))

(defn Not []
  (str
"@SP //---------not
A=M-1
M=!M  //---end not
"))

;--------------------------BRANCHING------------------------------------
(defn goto [label]
  (str
"@"label"
0;JMP
"))

(defn if-goto [label]
  (str
"@SP //-------------if-goto "label"
AM=M-1
D=M
@"label"        
D;JNE //---end if-goto "label"
"))

(defn put-label [label]
  (str 
"("label") 
"))

;--------------------------FUNCTIONS------------------------------------

(def RET (atom 0))

(defn call [funcName nArgs file-name]
  (swap! RET inc)
  (str
"@"(str "RET" @RET)" //---------------------------call " funcName " " nArgs "
D=A
"(pushD)"
@LCL //save LCL
D=M
" (pushD) "
@ARG  //save ARG
D=M
" (pushD) "
@THIS  //save THIS
D=M
" (pushD) "
@THAT  //save THAT
D=M
" (pushD) "
@SP //reposition ARG
D=M
" (pushD) "\n" 
(push-const 5)
(sub)
(push-const nArgs)
(sub)
(popD) "\n"
"@ARG
M=D 
@SP //reposition LCL to SP
D=M
@LCL
M=D
"(goto funcName)
"(RET" @RET ") //---------------------------end call" funcName " " nArgs "\n"
))


(defn function [funcName nVars file-name]
  (str
"(" funcName ") //---------------------------"funcName" START
"(apply str (repeat (Integer/parseInt nVars) (push-const 0)))
))

(defn return []
  (str
"@SP //---------------return
AM=M-1 //---pop result put in ARG
D=M
@ARG
A=M
M=D //---end result
D=A+1
@777
M=D
@LCL //--reposition SP to LCL 
D=M
@SP
M=D //-----restoring THAT, THIS, ARG, LCL
" (popD) "
@THAT
M=D
" (popD) "
@THIS
M=D
" (popD) "
@ARG
M=D
" (popD) "
@LCL
M=D //------restore ret address
" (popD) "
@777
A=M
M=D
D=A
@SP
M=D         
@777
A=M
A=M
0;JMP  //---------------end return jump back
"))

(defn init []
  (str
"@256
D=A
@SP
M=D\n"
(call "Sys.init" 0 "")
))
;"@Sys.init\n0;JMP\n"

(defn return1 []
  (str
"@LCL  //-------------------------return
D=M
@R11
M=D
@5
A=D-A
D=M
@R12
M=D
@ARG
D=M
@0
D=D+A
@R13
M=D
@SP
AM=M-1
D=M
@R13
A=M
M=D
@ARG
D=M
@SP
M=D+1
@R11
D=M-1
AM=D
D=M
@THAT
M=D
@R11
D=M-1
AM=D
D=M
@THIS
M=D
@R11
D=M-1
AM=D
D=M
@ARG
M=D
@R11
D=M-1
AM=D
D=M
@LCL
M=D
@R12
A=M
0;JMP   //-----------------------end return
"))