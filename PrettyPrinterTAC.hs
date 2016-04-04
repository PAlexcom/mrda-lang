{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module PrettyPrinterTAC where

import CodeGenerator

--control :: TAC -> String
--control x = case x of
--            TACLabel label                            -> label ++ ":" ++ "\n"
--            TACBinaryOp id left op right              -> "\t" ++ id ++ " = " ++ left ++ " " ++ op ++ " " ++ right ++ "\n"
--            TACAssign id var                          -> "\t" ++ id ++ " = " ++ var ++ "\n"
--            TACUnaryOp id op var                      -> "\t" ++ id ++ " = "  ++ op ++ var ++ "\n"
--            TACCondition left op right                -> "\t" ++ left ++ " " ++ op ++ " " ++ right ++ "\n"
--            TACIf (TACCondition left op right) l1 l2  -> "\t" ++ "if " ++ left ++ op ++ right ++ " goto " ++ l1 ++ "\n" ++ "\t" ++ "goto " ++ l2 ++ "\n"
--            TACGoto label                             -> "\t" ++ "goto " ++ label ++ "\n" 
--            TACReturn label                           -> "\t" ++ "return " ++ label ++ "\n"
--            TACPreamble s                             -> "\t" ++ s ++ "\n"
--            TACParam label                            -> "\t" ++ "param " ++ label ++ "\n"
--            TACCallVoid  id npar                      -> "\t" ++ "Call " ++ id ++  "," ++ npar ++ "\n"
--            TACCall var id npar                       -> "\t" ++ var ++ " = " ++ "Call " ++ id ++  "," ++ npar ++ "\n"
--            TACException label                        -> "\t" ++ "on exception goto " ++ label ++ "\n"

--prettyPrintTAC :: [TAC] -> String
--prettyPrintTAC [] = ""
--prettyPrintTAC (x:xs) = (control x) ++ (prettyPrintTAC xs)

import Text.PrettyPrint hiding (Str)

tab = 5

instance PrettyPrintTAC TACList where 
      prettyPrint tac = vcat . map prettyPrint $ tac

instance PrettyPrintTAC TAC where
      prettyPrint (TACLabel label)                    = text label <> colon
      prettyPrint (TACBinaryOp id left op right)      = nest tab $ text id <+> text "=" <+> text left <+> text op <+> text right
      prettyPrint (TACAssign id var)                  = nest tab $ text id <+> text "=" <+> text var
      prettyPrint (TACUnaryOp id op var)              = nest tab $ text id <+> text "=" <+> text op <+> text var
      prettyPrint (TACCondition left op right)        = nest tab $ text left <> text op <> text right
      prettyPrint (TACIf tacCondition l1 l2)          = nest tab $ text "if" <+> prettyPrint tacCondition <+> text "goto" <+> text l1 $$ text "goto" <+> text l2
      prettyPrint (TACGoto label)                     = nest tab $ text "goto" <+> text label 
      prettyPrint (TACReturn label)                   = nest tab $ text "return" <+> text label
      prettyPrint (TACPreamble s)                     = nest tab $ text s
      prettyPrint (TACParam label)                    = nest tab $ text "param" <+> text label
      prettyPrint (TACCallVoid  id npar)              = nest tab $ text "Call" <+> text id <> text "," <> text npar
      prettyPrint (TACCall var id npar)               = nest tab $ text var <+> text "=" <+> text "Call" <+> text id <> text "," <> text npar
      prettyPrint (TACException label)                = nest tab $ text "on exception goto" <+> text label

class PrettyPrintTAC a where
  prettyPrint :: a -> Doc