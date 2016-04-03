module PrettyPrinterTAC where

import CodeGenerator

control :: TAC -> String
control x = case x of
            TACLabel label -> "\t" ++ label ++ ":" ++ "\n"
            TACBinaryOp id left op right -> id ++ " = " ++ left ++ " " ++ op ++ " " ++ right ++ "\n"
            TACAssign id var ->  id ++ " = " ++ var ++ "\n"
            TACUnaryOp id op var -> id ++ " = "  ++ op ++ var ++ "\n"
            TACCondition left op right -> left ++ " " ++ op ++ " " ++ right ++ "\n"
            TACIf (TACCondition left op right) l1 l2 -> "if " ++ left ++ op ++ right ++ " goto " ++ l1 ++ "\n" ++ "goto " ++ l2 ++ "\n"
            TACGoto label -> "goto " ++ label ++ "\n" 
            TACReturn label ->  "return " ++ label ++ "\n"
            TACPreamble s -> s ++ "\n"
            TACParam label ->  "param " ++ label ++ "\n"
            TACCallVoid  id npar ->  "Call " ++ id ++  "," ++ npar ++ "\n"
            TACCall var id npar ->  var ++ " = " ++ "Call " ++ id ++  "," ++ npar ++ "\n"

prettyPrintTAC :: [TAC] -> String
prettyPrintTAC [] = ""
prettyPrintTAC (x:xs) = (control x) ++ (prettyPrintTAC xs)