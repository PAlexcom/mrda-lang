{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module PrettyPrinterTAC where

import CodeGenerator
import Text.PrettyPrint

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