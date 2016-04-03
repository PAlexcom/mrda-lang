module PrettyPrinterTAC where

import CodeGenerator

control :: TAC -> String
control x = case x of
            TACLabel label                            -> label ++ ":" ++ "\n"
            TACBinaryOp id left op right              -> "\t" ++ id ++ " = " ++ left ++ " " ++ op ++ " " ++ right ++ "\n"
            TACAssign id var                          -> "\t" ++ id ++ " = " ++ var ++ "\n"
            TACUnaryOp id op var                      -> "\t" ++ id ++ " = "  ++ op ++ var ++ "\n"
            TACCondition left op right                -> "\t" ++ left ++ " " ++ op ++ " " ++ right ++ "\n"
            TACIf (TACCondition left op right) l1 l2  -> "\t" ++ "if " ++ left ++ op ++ right ++ " goto " ++ l1 ++ "\n" ++ "\t" ++ "goto " ++ l2 ++ "\n"
            TACGoto label                             -> "\t" ++ "goto " ++ label ++ "\n" 
            TACReturn label                           -> "\t" ++ "return " ++ label ++ "\n"
            TACPreamble s                             -> "\t" ++ s ++ "\n"
            TACParam label                            -> "\t" ++ "param " ++ label ++ "\n"
            TACCallVoid  id npar                      -> "\t" ++ "Call " ++ id ++  "," ++ npar ++ "\n"
            TACCall var id npar                       -> "\t" ++ var ++ " = " ++ "Call " ++ id ++  "," ++ npar ++ "\n"
            TACException label                        -> "\t" ++ "on exception goto " ++ label ++ "\n"

prettyPrintTAC :: [TAC] -> String
prettyPrintTAC [] = ""
prettyPrintTAC (x:xs) = (control x) ++ (prettyPrintTAC xs)