{-# OPTIONS_GHC -w #-}
module Parser where

import Lexer
import Error
import Control.Applicative(Applicative(..))
import Control.Monad (ap)

-- parser produced by Happy Version 1.19.5

data HappyAbsSyn t16
	= HappyTerminal (Token)
	| HappyErrorToken Int
	| HappyAbsSyn4 (AbsNode)
	| HappyAbsSyn8 ([AbsNode])
	| HappyAbsSyn16 t16

action_0 (10) = happyGoto action_3
action_0 (11) = happyGoto action_4
action_0 _ = happyReduce_46

action_1 (60) = happyShift action_2
action_1 _ = happyFail

action_2 _ = happyReduce_1

action_3 (87) = happyAccept
action_3 _ = happyFail

action_4 (72) = happyShift action_7
action_4 (73) = happyShift action_8
action_4 (78) = happyShift action_9
action_4 (12) = happyGoto action_5
action_4 (19) = happyGoto action_6
action_4 _ = happyReduce_45

action_5 _ = happyReduce_47

action_6 (86) = happyShift action_11
action_6 _ = happyFail

action_7 _ = happyReduce_64

action_8 _ = happyReduce_65

action_9 (86) = happyShift action_10
action_9 _ = happyFail

action_10 (35) = happyShift action_13
action_10 _ = happyFail

action_11 (29) = happyShift action_12
action_11 _ = happyFail

action_12 (60) = happyShift action_2
action_12 (62) = happyShift action_23
action_12 (66) = happyShift action_24
action_12 (68) = happyShift action_25
action_12 (75) = happyShift action_26
action_12 (4) = happyGoto action_20
action_12 (13) = happyGoto action_21
action_12 (14) = happyGoto action_22
action_12 _ = happyFail

action_13 (60) = happyReduce_66
action_13 (62) = happyReduce_66
action_13 (66) = happyReduce_66
action_13 (68) = happyReduce_66
action_13 (72) = happyShift action_17
action_13 (73) = happyShift action_18
action_13 (74) = happyShift action_19
action_13 (75) = happyReduce_66
action_13 (17) = happyGoto action_14
action_13 (18) = happyGoto action_15
action_13 (20) = happyGoto action_16
action_13 _ = happyReduce_60

action_14 (36) = happyShift action_34
action_14 _ = happyFail

action_15 (42) = happyShift action_33
action_15 _ = happyReduce_61

action_16 (60) = happyShift action_2
action_16 (62) = happyShift action_23
action_16 (66) = happyShift action_24
action_16 (68) = happyShift action_25
action_16 (75) = happyShift action_26
action_16 (4) = happyGoto action_31
action_16 (13) = happyGoto action_32
action_16 (14) = happyGoto action_22
action_16 _ = happyFail

action_17 _ = happyReduce_67

action_18 _ = happyReduce_69

action_19 _ = happyReduce_68

action_20 (51) = happyShift action_30
action_20 _ = happyReduce_51

action_21 (37) = happyShift action_27
action_21 (51) = happyShift action_28
action_21 (57) = happyShift action_29
action_21 _ = happyFail

action_22 _ = happyReduce_52

action_23 _ = happyReduce_2

action_24 _ = happyReduce_3

action_25 _ = happyReduce_4

action_26 _ = happyReduce_5

action_27 _ = happyReduce_55

action_28 (30) = happyShift action_43
action_28 (33) = happyShift action_44
action_28 (35) = happyShift action_45
action_28 (37) = happyShift action_46
action_28 (40) = happyShift action_47
action_28 (43) = happyShift action_48
action_28 (44) = happyShift action_49
action_28 (55) = happyShift action_50
action_28 (56) = happyShift action_51
action_28 (57) = happyShift action_52
action_28 (82) = happyShift action_53
action_28 (83) = happyShift action_54
action_28 (84) = happyShift action_55
action_28 (85) = happyShift action_56
action_28 (86) = happyShift action_57
action_28 (5) = happyGoto action_38
action_28 (6) = happyGoto action_39
action_28 (7) = happyGoto action_40
action_28 (9) = happyGoto action_41
action_28 (15) = happyGoto action_60
action_28 _ = happyFail

action_29 (58) = happyShift action_58
action_29 (82) = happyShift action_59
action_29 _ = happyFail

action_30 (30) = happyShift action_43
action_30 (33) = happyShift action_44
action_30 (35) = happyShift action_45
action_30 (37) = happyShift action_46
action_30 (40) = happyShift action_47
action_30 (43) = happyShift action_48
action_30 (44) = happyShift action_49
action_30 (55) = happyShift action_50
action_30 (56) = happyShift action_51
action_30 (57) = happyShift action_52
action_30 (82) = happyShift action_53
action_30 (83) = happyShift action_54
action_30 (84) = happyShift action_55
action_30 (85) = happyShift action_56
action_30 (86) = happyShift action_57
action_30 (5) = happyGoto action_38
action_30 (6) = happyGoto action_39
action_30 (7) = happyGoto action_40
action_30 (9) = happyGoto action_41
action_30 (15) = happyGoto action_42
action_30 _ = happyFail

action_31 _ = happyReduce_51

action_32 (37) = happyShift action_27
action_32 (57) = happyShift action_29
action_32 (86) = happyShift action_37
action_32 _ = happyFail

action_33 (60) = happyReduce_66
action_33 (62) = happyReduce_66
action_33 (66) = happyReduce_66
action_33 (68) = happyReduce_66
action_33 (72) = happyShift action_17
action_33 (73) = happyShift action_18
action_33 (74) = happyShift action_19
action_33 (75) = happyReduce_66
action_33 (17) = happyGoto action_36
action_33 (18) = happyGoto action_15
action_33 (20) = happyGoto action_16
action_33 _ = happyReduce_60

action_34 (29) = happyShift action_35
action_34 _ = happyFail

action_35 (60) = happyShift action_2
action_35 (62) = happyShift action_23
action_35 (66) = happyShift action_24
action_35 (68) = happyShift action_25
action_35 (75) = happyShift action_26
action_35 (4) = happyGoto action_94
action_35 _ = happyFail

action_36 _ = happyReduce_62

action_37 _ = happyReduce_63

action_38 (31) = happyShift action_80
action_38 (32) = happyShift action_81
action_38 (34) = happyShift action_82
action_38 (37) = happyShift action_83
action_38 (39) = happyShift action_84
action_38 (43) = happyShift action_85
action_38 (46) = happyShift action_86
action_38 (49) = happyShift action_87
action_38 (50) = happyShift action_88
action_38 (52) = happyShift action_89
action_38 (53) = happyShift action_90
action_38 (54) = happyShift action_91
action_38 (59) = happyShift action_92
action_38 (81) = happyShift action_93
action_38 _ = happyReduce_56

action_39 (40) = happyShift action_78
action_39 (44) = happyShift action_79
action_39 _ = happyReduce_31

action_40 _ = happyReduce_23

action_41 (57) = happyShift action_77
action_41 _ = happyReduce_38

action_42 (48) = happyShift action_76
action_42 _ = happyFail

action_43 (30) = happyShift action_43
action_43 (33) = happyShift action_44
action_43 (35) = happyShift action_45
action_43 (37) = happyShift action_46
action_43 (40) = happyShift action_47
action_43 (43) = happyShift action_48
action_43 (44) = happyShift action_49
action_43 (55) = happyShift action_50
action_43 (56) = happyShift action_51
action_43 (82) = happyShift action_53
action_43 (83) = happyShift action_54
action_43 (84) = happyShift action_55
action_43 (85) = happyShift action_56
action_43 (86) = happyShift action_57
action_43 (5) = happyGoto action_75
action_43 (6) = happyGoto action_39
action_43 (7) = happyGoto action_40
action_43 (9) = happyGoto action_41
action_43 _ = happyFail

action_44 (35) = happyShift action_67
action_44 (37) = happyShift action_46
action_44 (40) = happyShift action_47
action_44 (44) = happyShift action_49
action_44 (86) = happyShift action_68
action_44 (6) = happyGoto action_74
action_44 (9) = happyGoto action_41
action_44 _ = happyFail

action_45 (30) = happyShift action_43
action_45 (33) = happyShift action_44
action_45 (35) = happyShift action_45
action_45 (37) = happyShift action_46
action_45 (40) = happyShift action_47
action_45 (43) = happyShift action_48
action_45 (44) = happyShift action_49
action_45 (55) = happyShift action_50
action_45 (56) = happyShift action_51
action_45 (82) = happyShift action_53
action_45 (83) = happyShift action_54
action_45 (84) = happyShift action_55
action_45 (85) = happyShift action_56
action_45 (86) = happyShift action_57
action_45 (5) = happyGoto action_72
action_45 (6) = happyGoto action_73
action_45 (7) = happyGoto action_40
action_45 (9) = happyGoto action_41
action_45 _ = happyFail

action_46 (30) = happyShift action_43
action_46 (33) = happyShift action_44
action_46 (35) = happyShift action_45
action_46 (37) = happyShift action_46
action_46 (40) = happyShift action_47
action_46 (43) = happyShift action_48
action_46 (44) = happyShift action_49
action_46 (55) = happyShift action_50
action_46 (56) = happyShift action_51
action_46 (82) = happyShift action_53
action_46 (83) = happyShift action_54
action_46 (84) = happyShift action_55
action_46 (85) = happyShift action_56
action_46 (86) = happyShift action_57
action_46 (5) = happyGoto action_71
action_46 (6) = happyGoto action_39
action_46 (7) = happyGoto action_40
action_46 (9) = happyGoto action_41
action_46 _ = happyFail

action_47 (35) = happyShift action_67
action_47 (37) = happyShift action_46
action_47 (40) = happyShift action_47
action_47 (44) = happyShift action_49
action_47 (86) = happyShift action_68
action_47 (6) = happyGoto action_70
action_47 (9) = happyGoto action_41
action_47 _ = happyFail

action_48 (30) = happyShift action_43
action_48 (33) = happyShift action_44
action_48 (35) = happyShift action_45
action_48 (37) = happyShift action_46
action_48 (40) = happyShift action_47
action_48 (43) = happyShift action_48
action_48 (44) = happyShift action_49
action_48 (55) = happyShift action_50
action_48 (56) = happyShift action_51
action_48 (82) = happyShift action_53
action_48 (83) = happyShift action_54
action_48 (84) = happyShift action_55
action_48 (85) = happyShift action_56
action_48 (86) = happyShift action_57
action_48 (5) = happyGoto action_69
action_48 (6) = happyGoto action_39
action_48 (7) = happyGoto action_40
action_48 (9) = happyGoto action_41
action_48 _ = happyFail

action_49 (35) = happyShift action_67
action_49 (37) = happyShift action_46
action_49 (40) = happyShift action_47
action_49 (44) = happyShift action_49
action_49 (86) = happyShift action_68
action_49 (6) = happyGoto action_66
action_49 (9) = happyGoto action_41
action_49 _ = happyFail

action_50 _ = happyReduce_29

action_51 _ = happyReduce_28

action_52 (30) = happyShift action_43
action_52 (33) = happyShift action_44
action_52 (35) = happyShift action_45
action_52 (37) = happyShift action_46
action_52 (40) = happyShift action_47
action_52 (43) = happyShift action_48
action_52 (44) = happyShift action_49
action_52 (55) = happyShift action_50
action_52 (56) = happyShift action_51
action_52 (57) = happyShift action_52
action_52 (82) = happyShift action_53
action_52 (83) = happyShift action_54
action_52 (84) = happyShift action_55
action_52 (85) = happyShift action_56
action_52 (86) = happyShift action_57
action_52 (5) = happyGoto action_38
action_52 (6) = happyGoto action_39
action_52 (7) = happyGoto action_40
action_52 (9) = happyGoto action_41
action_52 (15) = happyGoto action_64
action_52 (16) = happyGoto action_65
action_52 _ = happyFail

action_53 _ = happyReduce_24

action_54 _ = happyReduce_25

action_55 _ = happyReduce_26

action_56 _ = happyReduce_27

action_57 (35) = happyShift action_63
action_57 _ = happyReduce_44

action_58 _ = happyReduce_54

action_59 (58) = happyShift action_62
action_59 _ = happyFail

action_60 (48) = happyShift action_61
action_60 _ = happyFail

action_61 _ = happyReduce_49

action_62 _ = happyReduce_53

action_63 (30) = happyShift action_43
action_63 (33) = happyShift action_44
action_63 (35) = happyShift action_45
action_63 (37) = happyShift action_46
action_63 (40) = happyShift action_47
action_63 (43) = happyShift action_48
action_63 (44) = happyShift action_49
action_63 (55) = happyShift action_50
action_63 (56) = happyShift action_51
action_63 (82) = happyShift action_53
action_63 (83) = happyShift action_54
action_63 (84) = happyShift action_55
action_63 (85) = happyShift action_56
action_63 (86) = happyShift action_57
action_63 (5) = happyGoto action_116
action_63 (6) = happyGoto action_39
action_63 (7) = happyGoto action_40
action_63 (8) = happyGoto action_117
action_63 (9) = happyGoto action_41
action_63 _ = happyReduce_40

action_64 (42) = happyShift action_115
action_64 _ = happyReduce_58

action_65 (58) = happyShift action_114
action_65 _ = happyFail

action_66 _ = happyReduce_34

action_67 (35) = happyShift action_67
action_67 (37) = happyShift action_46
action_67 (40) = happyShift action_47
action_67 (44) = happyShift action_49
action_67 (86) = happyShift action_68
action_67 (6) = happyGoto action_113
action_67 (9) = happyGoto action_41
action_67 _ = happyFail

action_68 _ = happyReduce_44

action_69 _ = happyReduce_21

action_70 _ = happyReduce_33

action_71 (59) = happyShift action_92
action_71 _ = happyReduce_32

action_72 (31) = happyShift action_80
action_72 (32) = happyShift action_81
action_72 (34) = happyShift action_82
action_72 (36) = happyShift action_112
action_72 (37) = happyShift action_83
action_72 (39) = happyShift action_84
action_72 (43) = happyShift action_85
action_72 (46) = happyShift action_86
action_72 (49) = happyShift action_87
action_72 (50) = happyShift action_88
action_72 (52) = happyShift action_89
action_72 (53) = happyShift action_90
action_72 (54) = happyShift action_91
action_72 (59) = happyShift action_92
action_72 (81) = happyShift action_93
action_72 _ = happyFail

action_73 (36) = happyShift action_111
action_73 (40) = happyShift action_78
action_73 (44) = happyShift action_79
action_73 _ = happyReduce_31

action_74 (40) = happyShift action_78
action_74 (44) = happyShift action_79
action_74 _ = happyReduce_22

action_75 (31) = happyShift action_80
action_75 (32) = happyShift action_81
action_75 (37) = happyShift action_83
action_75 (39) = happyShift action_84
action_75 (43) = happyShift action_85
action_75 (46) = happyShift action_86
action_75 (49) = happyShift action_87
action_75 (50) = happyShift action_88
action_75 (52) = happyShift action_89
action_75 (53) = happyShift action_90
action_75 (54) = happyShift action_91
action_75 (59) = happyShift action_92
action_75 _ = happyReduce_8

action_76 _ = happyReduce_48

action_77 (30) = happyShift action_43
action_77 (33) = happyShift action_44
action_77 (35) = happyShift action_45
action_77 (37) = happyShift action_46
action_77 (40) = happyShift action_47
action_77 (43) = happyShift action_48
action_77 (44) = happyShift action_49
action_77 (55) = happyShift action_50
action_77 (56) = happyShift action_51
action_77 (82) = happyShift action_53
action_77 (83) = happyShift action_54
action_77 (84) = happyShift action_55
action_77 (85) = happyShift action_56
action_77 (86) = happyShift action_57
action_77 (5) = happyGoto action_110
action_77 (6) = happyGoto action_39
action_77 (7) = happyGoto action_40
action_77 (9) = happyGoto action_41
action_77 _ = happyFail

action_78 _ = happyReduce_35

action_79 _ = happyReduce_36

action_80 (30) = happyShift action_43
action_80 (33) = happyShift action_44
action_80 (35) = happyShift action_45
action_80 (37) = happyShift action_46
action_80 (40) = happyShift action_47
action_80 (43) = happyShift action_48
action_80 (44) = happyShift action_49
action_80 (55) = happyShift action_50
action_80 (56) = happyShift action_51
action_80 (82) = happyShift action_53
action_80 (83) = happyShift action_54
action_80 (84) = happyShift action_55
action_80 (85) = happyShift action_56
action_80 (86) = happyShift action_57
action_80 (5) = happyGoto action_109
action_80 (6) = happyGoto action_39
action_80 (7) = happyGoto action_40
action_80 (9) = happyGoto action_41
action_80 _ = happyFail

action_81 (30) = happyShift action_43
action_81 (33) = happyShift action_44
action_81 (35) = happyShift action_45
action_81 (37) = happyShift action_46
action_81 (40) = happyShift action_47
action_81 (43) = happyShift action_48
action_81 (44) = happyShift action_49
action_81 (55) = happyShift action_50
action_81 (56) = happyShift action_51
action_81 (82) = happyShift action_53
action_81 (83) = happyShift action_54
action_81 (84) = happyShift action_55
action_81 (85) = happyShift action_56
action_81 (86) = happyShift action_57
action_81 (5) = happyGoto action_108
action_81 (6) = happyGoto action_39
action_81 (7) = happyGoto action_40
action_81 (9) = happyGoto action_41
action_81 _ = happyFail

action_82 (30) = happyShift action_43
action_82 (33) = happyShift action_44
action_82 (35) = happyShift action_45
action_82 (37) = happyShift action_46
action_82 (40) = happyShift action_47
action_82 (43) = happyShift action_48
action_82 (44) = happyShift action_49
action_82 (55) = happyShift action_50
action_82 (56) = happyShift action_51
action_82 (82) = happyShift action_53
action_82 (83) = happyShift action_54
action_82 (84) = happyShift action_55
action_82 (85) = happyShift action_56
action_82 (86) = happyShift action_57
action_82 (5) = happyGoto action_107
action_82 (6) = happyGoto action_39
action_82 (7) = happyGoto action_40
action_82 (9) = happyGoto action_41
action_82 _ = happyFail

action_83 (30) = happyShift action_43
action_83 (33) = happyShift action_44
action_83 (35) = happyShift action_45
action_83 (37) = happyShift action_46
action_83 (40) = happyShift action_47
action_83 (43) = happyShift action_48
action_83 (44) = happyShift action_49
action_83 (55) = happyShift action_50
action_83 (56) = happyShift action_51
action_83 (82) = happyShift action_53
action_83 (83) = happyShift action_54
action_83 (84) = happyShift action_55
action_83 (85) = happyShift action_56
action_83 (86) = happyShift action_57
action_83 (5) = happyGoto action_106
action_83 (6) = happyGoto action_39
action_83 (7) = happyGoto action_40
action_83 (9) = happyGoto action_41
action_83 _ = happyFail

action_84 (30) = happyShift action_43
action_84 (33) = happyShift action_44
action_84 (35) = happyShift action_45
action_84 (37) = happyShift action_46
action_84 (40) = happyShift action_47
action_84 (43) = happyShift action_48
action_84 (44) = happyShift action_49
action_84 (55) = happyShift action_50
action_84 (56) = happyShift action_51
action_84 (82) = happyShift action_53
action_84 (83) = happyShift action_54
action_84 (84) = happyShift action_55
action_84 (85) = happyShift action_56
action_84 (86) = happyShift action_57
action_84 (5) = happyGoto action_105
action_84 (6) = happyGoto action_39
action_84 (7) = happyGoto action_40
action_84 (9) = happyGoto action_41
action_84 _ = happyFail

action_85 (30) = happyShift action_43
action_85 (33) = happyShift action_44
action_85 (35) = happyShift action_45
action_85 (37) = happyShift action_46
action_85 (40) = happyShift action_47
action_85 (43) = happyShift action_48
action_85 (44) = happyShift action_49
action_85 (55) = happyShift action_50
action_85 (56) = happyShift action_51
action_85 (82) = happyShift action_53
action_85 (83) = happyShift action_54
action_85 (84) = happyShift action_55
action_85 (85) = happyShift action_56
action_85 (86) = happyShift action_57
action_85 (5) = happyGoto action_104
action_85 (6) = happyGoto action_39
action_85 (7) = happyGoto action_40
action_85 (9) = happyGoto action_41
action_85 _ = happyFail

action_86 (30) = happyShift action_43
action_86 (33) = happyShift action_44
action_86 (35) = happyShift action_45
action_86 (37) = happyShift action_46
action_86 (40) = happyShift action_47
action_86 (43) = happyShift action_48
action_86 (44) = happyShift action_49
action_86 (55) = happyShift action_50
action_86 (56) = happyShift action_51
action_86 (82) = happyShift action_53
action_86 (83) = happyShift action_54
action_86 (84) = happyShift action_55
action_86 (85) = happyShift action_56
action_86 (86) = happyShift action_57
action_86 (5) = happyGoto action_103
action_86 (6) = happyGoto action_39
action_86 (7) = happyGoto action_40
action_86 (9) = happyGoto action_41
action_86 _ = happyFail

action_87 (30) = happyShift action_43
action_87 (33) = happyShift action_44
action_87 (35) = happyShift action_45
action_87 (37) = happyShift action_46
action_87 (40) = happyShift action_47
action_87 (43) = happyShift action_48
action_87 (44) = happyShift action_49
action_87 (55) = happyShift action_50
action_87 (56) = happyShift action_51
action_87 (82) = happyShift action_53
action_87 (83) = happyShift action_54
action_87 (84) = happyShift action_55
action_87 (85) = happyShift action_56
action_87 (86) = happyShift action_57
action_87 (5) = happyGoto action_102
action_87 (6) = happyGoto action_39
action_87 (7) = happyGoto action_40
action_87 (9) = happyGoto action_41
action_87 _ = happyFail

action_88 (30) = happyShift action_43
action_88 (33) = happyShift action_44
action_88 (35) = happyShift action_45
action_88 (37) = happyShift action_46
action_88 (40) = happyShift action_47
action_88 (43) = happyShift action_48
action_88 (44) = happyShift action_49
action_88 (55) = happyShift action_50
action_88 (56) = happyShift action_51
action_88 (82) = happyShift action_53
action_88 (83) = happyShift action_54
action_88 (84) = happyShift action_55
action_88 (85) = happyShift action_56
action_88 (86) = happyShift action_57
action_88 (5) = happyGoto action_101
action_88 (6) = happyGoto action_39
action_88 (7) = happyGoto action_40
action_88 (9) = happyGoto action_41
action_88 _ = happyFail

action_89 (30) = happyShift action_43
action_89 (33) = happyShift action_44
action_89 (35) = happyShift action_45
action_89 (37) = happyShift action_46
action_89 (40) = happyShift action_47
action_89 (43) = happyShift action_48
action_89 (44) = happyShift action_49
action_89 (55) = happyShift action_50
action_89 (56) = happyShift action_51
action_89 (82) = happyShift action_53
action_89 (83) = happyShift action_54
action_89 (84) = happyShift action_55
action_89 (85) = happyShift action_56
action_89 (86) = happyShift action_57
action_89 (5) = happyGoto action_100
action_89 (6) = happyGoto action_39
action_89 (7) = happyGoto action_40
action_89 (9) = happyGoto action_41
action_89 _ = happyFail

action_90 (30) = happyShift action_43
action_90 (33) = happyShift action_44
action_90 (35) = happyShift action_45
action_90 (37) = happyShift action_46
action_90 (40) = happyShift action_47
action_90 (43) = happyShift action_48
action_90 (44) = happyShift action_49
action_90 (55) = happyShift action_50
action_90 (56) = happyShift action_51
action_90 (82) = happyShift action_53
action_90 (83) = happyShift action_54
action_90 (84) = happyShift action_55
action_90 (85) = happyShift action_56
action_90 (86) = happyShift action_57
action_90 (5) = happyGoto action_99
action_90 (6) = happyGoto action_39
action_90 (7) = happyGoto action_40
action_90 (9) = happyGoto action_41
action_90 _ = happyFail

action_91 (30) = happyShift action_43
action_91 (33) = happyShift action_44
action_91 (35) = happyShift action_45
action_91 (37) = happyShift action_46
action_91 (40) = happyShift action_47
action_91 (43) = happyShift action_48
action_91 (44) = happyShift action_49
action_91 (55) = happyShift action_50
action_91 (56) = happyShift action_51
action_91 (82) = happyShift action_53
action_91 (83) = happyShift action_54
action_91 (84) = happyShift action_55
action_91 (85) = happyShift action_56
action_91 (86) = happyShift action_57
action_91 (5) = happyGoto action_98
action_91 (6) = happyGoto action_39
action_91 (7) = happyGoto action_40
action_91 (9) = happyGoto action_41
action_91 _ = happyFail

action_92 (30) = happyShift action_43
action_92 (33) = happyShift action_44
action_92 (35) = happyShift action_45
action_92 (37) = happyShift action_46
action_92 (40) = happyShift action_47
action_92 (43) = happyShift action_48
action_92 (44) = happyShift action_49
action_92 (55) = happyShift action_50
action_92 (56) = happyShift action_51
action_92 (82) = happyShift action_53
action_92 (83) = happyShift action_54
action_92 (84) = happyShift action_55
action_92 (85) = happyShift action_56
action_92 (86) = happyShift action_57
action_92 (5) = happyGoto action_97
action_92 (6) = happyGoto action_39
action_92 (7) = happyGoto action_40
action_92 (9) = happyGoto action_41
action_92 _ = happyFail

action_93 (30) = happyShift action_43
action_93 (33) = happyShift action_44
action_93 (35) = happyShift action_45
action_93 (37) = happyShift action_46
action_93 (40) = happyShift action_47
action_93 (43) = happyShift action_48
action_93 (44) = happyShift action_49
action_93 (55) = happyShift action_50
action_93 (56) = happyShift action_51
action_93 (82) = happyShift action_53
action_93 (83) = happyShift action_54
action_93 (84) = happyShift action_55
action_93 (85) = happyShift action_56
action_93 (86) = happyShift action_57
action_93 (5) = happyGoto action_96
action_93 (6) = happyGoto action_39
action_93 (7) = happyGoto action_40
action_93 (9) = happyGoto action_41
action_93 _ = happyFail

action_94 (51) = happyShift action_95
action_94 _ = happyFail

action_95 (79) = happyShift action_122
action_95 _ = happyFail

action_96 (31) = happyShift action_80
action_96 (32) = happyShift action_81
action_96 (37) = happyShift action_83
action_96 (39) = happyShift action_84
action_96 (43) = happyShift action_85
action_96 (46) = happyShift action_86
action_96 (49) = happyShift action_87
action_96 (50) = happyShift action_88
action_96 (52) = happyShift action_89
action_96 (53) = happyShift action_90
action_96 (54) = happyShift action_91
action_96 (59) = happyShift action_92
action_96 _ = happyReduce_6

action_97 (59) = happyShift action_92
action_97 _ = happyReduce_20

action_98 (31) = happyFail
action_98 (32) = happyShift action_81
action_98 (37) = happyShift action_83
action_98 (39) = happyShift action_84
action_98 (43) = happyShift action_85
action_98 (46) = happyShift action_86
action_98 (49) = happyFail
action_98 (50) = happyFail
action_98 (52) = happyFail
action_98 (53) = happyFail
action_98 (54) = happyFail
action_98 (59) = happyShift action_92
action_98 _ = happyReduce_14

action_99 (31) = happyFail
action_99 (32) = happyShift action_81
action_99 (37) = happyShift action_83
action_99 (39) = happyShift action_84
action_99 (43) = happyShift action_85
action_99 (46) = happyShift action_86
action_99 (49) = happyFail
action_99 (50) = happyFail
action_99 (52) = happyFail
action_99 (53) = happyFail
action_99 (54) = happyFail
action_99 (59) = happyShift action_92
action_99 _ = happyReduce_13

action_100 (31) = happyFail
action_100 (32) = happyShift action_81
action_100 (37) = happyShift action_83
action_100 (39) = happyShift action_84
action_100 (43) = happyShift action_85
action_100 (46) = happyShift action_86
action_100 (49) = happyFail
action_100 (50) = happyFail
action_100 (52) = happyFail
action_100 (53) = happyFail
action_100 (54) = happyFail
action_100 (59) = happyShift action_92
action_100 _ = happyReduce_9

action_101 (31) = happyFail
action_101 (32) = happyShift action_81
action_101 (37) = happyShift action_83
action_101 (39) = happyShift action_84
action_101 (43) = happyShift action_85
action_101 (46) = happyShift action_86
action_101 (49) = happyFail
action_101 (50) = happyFail
action_101 (52) = happyFail
action_101 (53) = happyFail
action_101 (54) = happyFail
action_101 (59) = happyShift action_92
action_101 _ = happyReduce_12

action_102 (31) = happyFail
action_102 (32) = happyShift action_81
action_102 (37) = happyShift action_83
action_102 (39) = happyShift action_84
action_102 (43) = happyShift action_85
action_102 (46) = happyShift action_86
action_102 (49) = happyFail
action_102 (50) = happyFail
action_102 (52) = happyFail
action_102 (53) = happyFail
action_102 (54) = happyFail
action_102 (59) = happyShift action_92
action_102 _ = happyReduce_11

action_103 (59) = happyShift action_92
action_103 _ = happyReduce_18

action_104 (32) = happyShift action_81
action_104 (37) = happyShift action_83
action_104 (46) = happyShift action_86
action_104 (59) = happyShift action_92
action_104 _ = happyReduce_16

action_105 (32) = happyShift action_81
action_105 (37) = happyShift action_83
action_105 (46) = happyShift action_86
action_105 (59) = happyShift action_92
action_105 _ = happyReduce_15

action_106 (59) = happyShift action_92
action_106 _ = happyReduce_17

action_107 (31) = happyShift action_80
action_107 (32) = happyShift action_81
action_107 (37) = happyShift action_83
action_107 (39) = happyShift action_84
action_107 (43) = happyShift action_85
action_107 (46) = happyShift action_86
action_107 (49) = happyShift action_87
action_107 (50) = happyShift action_88
action_107 (52) = happyShift action_89
action_107 (53) = happyShift action_90
action_107 (54) = happyShift action_91
action_107 (59) = happyShift action_92
action_107 _ = happyReduce_7

action_108 (59) = happyShift action_92
action_108 _ = happyReduce_19

action_109 (31) = happyFail
action_109 (32) = happyShift action_81
action_109 (37) = happyShift action_83
action_109 (39) = happyShift action_84
action_109 (43) = happyShift action_85
action_109 (46) = happyShift action_86
action_109 (49) = happyFail
action_109 (50) = happyFail
action_109 (52) = happyFail
action_109 (53) = happyFail
action_109 (54) = happyFail
action_109 (59) = happyShift action_92
action_109 _ = happyReduce_10

action_110 (31) = happyShift action_80
action_110 (32) = happyShift action_81
action_110 (34) = happyShift action_82
action_110 (37) = happyShift action_83
action_110 (39) = happyShift action_84
action_110 (43) = happyShift action_85
action_110 (46) = happyShift action_86
action_110 (49) = happyShift action_87
action_110 (50) = happyShift action_88
action_110 (52) = happyShift action_89
action_110 (53) = happyShift action_90
action_110 (54) = happyShift action_91
action_110 (58) = happyShift action_121
action_110 (59) = happyShift action_92
action_110 (81) = happyShift action_93
action_110 _ = happyFail

action_111 _ = happyReduce_37

action_112 _ = happyReduce_30

action_113 (36) = happyShift action_111
action_113 (40) = happyShift action_78
action_113 (44) = happyShift action_79
action_113 _ = happyFail

action_114 _ = happyReduce_57

action_115 (30) = happyShift action_43
action_115 (33) = happyShift action_44
action_115 (35) = happyShift action_45
action_115 (37) = happyShift action_46
action_115 (40) = happyShift action_47
action_115 (43) = happyShift action_48
action_115 (44) = happyShift action_49
action_115 (55) = happyShift action_50
action_115 (56) = happyShift action_51
action_115 (57) = happyShift action_52
action_115 (82) = happyShift action_53
action_115 (83) = happyShift action_54
action_115 (84) = happyShift action_55
action_115 (85) = happyShift action_56
action_115 (86) = happyShift action_57
action_115 (5) = happyGoto action_38
action_115 (6) = happyGoto action_39
action_115 (7) = happyGoto action_40
action_115 (9) = happyGoto action_41
action_115 (15) = happyGoto action_64
action_115 (16) = happyGoto action_120
action_115 _ = happyFail

action_116 (31) = happyShift action_80
action_116 (32) = happyShift action_81
action_116 (34) = happyShift action_82
action_116 (37) = happyShift action_83
action_116 (39) = happyShift action_84
action_116 (42) = happyShift action_119
action_116 (43) = happyShift action_85
action_116 (46) = happyShift action_86
action_116 (49) = happyShift action_87
action_116 (50) = happyShift action_88
action_116 (52) = happyShift action_89
action_116 (53) = happyShift action_90
action_116 (54) = happyShift action_91
action_116 (59) = happyShift action_92
action_116 (81) = happyShift action_93
action_116 _ = happyReduce_41

action_117 (36) = happyShift action_118
action_117 _ = happyFail

action_118 _ = happyReduce_39

action_119 (30) = happyShift action_43
action_119 (33) = happyShift action_44
action_119 (35) = happyShift action_45
action_119 (37) = happyShift action_46
action_119 (40) = happyShift action_47
action_119 (43) = happyShift action_48
action_119 (44) = happyShift action_49
action_119 (55) = happyShift action_50
action_119 (56) = happyShift action_51
action_119 (82) = happyShift action_53
action_119 (83) = happyShift action_54
action_119 (84) = happyShift action_55
action_119 (85) = happyShift action_56
action_119 (86) = happyShift action_57
action_119 (5) = happyGoto action_116
action_119 (6) = happyGoto action_39
action_119 (7) = happyGoto action_40
action_119 (8) = happyGoto action_125
action_119 (9) = happyGoto action_41
action_119 _ = happyReduce_40

action_120 _ = happyReduce_59

action_121 _ = happyReduce_43

action_122 (11) = happyGoto action_123
action_122 (21) = happyGoto action_124
action_122 _ = happyReduce_46

action_123 (72) = happyShift action_7
action_123 (73) = happyShift action_8
action_123 (78) = happyShift action_9
action_123 (12) = happyGoto action_5
action_123 (19) = happyGoto action_6
action_123 (22) = happyGoto action_128
action_123 _ = happyReduce_71

action_124 (71) = happyShift action_127
action_124 (26) = happyGoto action_126
action_124 _ = happyFail

action_125 _ = happyReduce_42

action_126 (80) = happyShift action_143
action_126 _ = happyFail

action_127 (35) = happyShift action_141
action_127 (48) = happyShift action_142
action_127 _ = happyFail

action_128 (35) = happyShift action_67
action_128 (37) = happyShift action_46
action_128 (40) = happyShift action_47
action_128 (44) = happyShift action_49
action_128 (61) = happyShift action_135
action_128 (63) = happyShift action_136
action_128 (64) = happyShift action_137
action_128 (67) = happyShift action_138
action_128 (76) = happyShift action_139
action_128 (79) = happyShift action_140
action_128 (86) = happyShift action_57
action_128 (6) = happyGoto action_129
action_128 (7) = happyGoto action_130
action_128 (9) = happyGoto action_41
action_128 (23) = happyGoto action_131
action_128 (25) = happyGoto action_132
action_128 (27) = happyGoto action_133
action_128 (28) = happyGoto action_134
action_128 _ = happyReduce_70

action_129 (38) = happyShift action_152
action_129 (40) = happyShift action_78
action_129 (41) = happyShift action_153
action_129 (44) = happyShift action_79
action_129 (45) = happyShift action_154
action_129 (47) = happyShift action_155
action_129 (48) = happyShift action_156
action_129 (51) = happyShift action_157
action_129 (24) = happyGoto action_151
action_129 _ = happyFail

action_130 (48) = happyShift action_150
action_130 _ = happyFail

action_131 _ = happyReduce_72

action_132 (48) = happyShift action_149
action_132 _ = happyFail

action_133 _ = happyReduce_77

action_134 _ = happyReduce_76

action_135 _ = happyReduce_85

action_136 _ = happyReduce_86

action_137 (35) = happyShift action_67
action_137 (37) = happyShift action_46
action_137 (40) = happyShift action_47
action_137 (44) = happyShift action_49
action_137 (61) = happyShift action_135
action_137 (63) = happyShift action_136
action_137 (64) = happyShift action_137
action_137 (67) = happyShift action_138
action_137 (76) = happyShift action_139
action_137 (79) = happyShift action_140
action_137 (86) = happyShift action_57
action_137 (6) = happyGoto action_129
action_137 (7) = happyGoto action_130
action_137 (9) = happyGoto action_41
action_137 (23) = happyGoto action_148
action_137 (25) = happyGoto action_132
action_137 (27) = happyGoto action_133
action_137 (28) = happyGoto action_134
action_137 _ = happyFail

action_138 (35) = happyShift action_147
action_138 _ = happyFail

action_139 (35) = happyShift action_146
action_139 _ = happyFail

action_140 (11) = happyGoto action_123
action_140 (21) = happyGoto action_145
action_140 _ = happyReduce_46

action_141 (30) = happyShift action_43
action_141 (33) = happyShift action_44
action_141 (35) = happyShift action_45
action_141 (37) = happyShift action_46
action_141 (40) = happyShift action_47
action_141 (43) = happyShift action_48
action_141 (44) = happyShift action_49
action_141 (55) = happyShift action_50
action_141 (56) = happyShift action_51
action_141 (82) = happyShift action_53
action_141 (83) = happyShift action_54
action_141 (84) = happyShift action_55
action_141 (85) = happyShift action_56
action_141 (86) = happyShift action_57
action_141 (5) = happyGoto action_144
action_141 (6) = happyGoto action_39
action_141 (7) = happyGoto action_40
action_141 (9) = happyGoto action_41
action_141 _ = happyFail

action_142 _ = happyReduce_87

action_143 _ = happyReduce_50

action_144 (31) = happyShift action_80
action_144 (32) = happyShift action_81
action_144 (34) = happyShift action_82
action_144 (36) = happyShift action_163
action_144 (37) = happyShift action_83
action_144 (39) = happyShift action_84
action_144 (43) = happyShift action_85
action_144 (46) = happyShift action_86
action_144 (49) = happyShift action_87
action_144 (50) = happyShift action_88
action_144 (52) = happyShift action_89
action_144 (53) = happyShift action_90
action_144 (54) = happyShift action_91
action_144 (59) = happyShift action_92
action_144 (81) = happyShift action_93
action_144 _ = happyFail

action_145 (80) = happyShift action_162
action_145 _ = happyFail

action_146 (30) = happyShift action_43
action_146 (33) = happyShift action_44
action_146 (35) = happyShift action_45
action_146 (37) = happyShift action_46
action_146 (40) = happyShift action_47
action_146 (43) = happyShift action_48
action_146 (44) = happyShift action_49
action_146 (55) = happyShift action_50
action_146 (56) = happyShift action_51
action_146 (82) = happyShift action_53
action_146 (83) = happyShift action_54
action_146 (84) = happyShift action_55
action_146 (85) = happyShift action_56
action_146 (86) = happyShift action_57
action_146 (5) = happyGoto action_161
action_146 (6) = happyGoto action_39
action_146 (7) = happyGoto action_40
action_146 (9) = happyGoto action_41
action_146 _ = happyFail

action_147 (30) = happyShift action_43
action_147 (33) = happyShift action_44
action_147 (35) = happyShift action_45
action_147 (37) = happyShift action_46
action_147 (40) = happyShift action_47
action_147 (43) = happyShift action_48
action_147 (44) = happyShift action_49
action_147 (55) = happyShift action_50
action_147 (56) = happyShift action_51
action_147 (82) = happyShift action_53
action_147 (83) = happyShift action_54
action_147 (84) = happyShift action_55
action_147 (85) = happyShift action_56
action_147 (86) = happyShift action_57
action_147 (5) = happyGoto action_160
action_147 (6) = happyGoto action_39
action_147 (7) = happyGoto action_40
action_147 (9) = happyGoto action_41
action_147 _ = happyFail

action_148 (76) = happyShift action_159
action_148 _ = happyFail

action_149 _ = happyReduce_75

action_150 _ = happyReduce_74

action_151 (30) = happyShift action_43
action_151 (33) = happyShift action_44
action_151 (35) = happyShift action_45
action_151 (37) = happyShift action_46
action_151 (40) = happyShift action_47
action_151 (43) = happyShift action_48
action_151 (44) = happyShift action_49
action_151 (55) = happyShift action_50
action_151 (56) = happyShift action_51
action_151 (82) = happyShift action_53
action_151 (83) = happyShift action_54
action_151 (84) = happyShift action_55
action_151 (85) = happyShift action_56
action_151 (86) = happyShift action_57
action_151 (5) = happyGoto action_158
action_151 (6) = happyGoto action_39
action_151 (7) = happyGoto action_40
action_151 (9) = happyGoto action_41
action_151 _ = happyFail

action_152 _ = happyReduce_81

action_153 _ = happyReduce_82

action_154 _ = happyReduce_84

action_155 _ = happyReduce_83

action_156 _ = happyReduce_79

action_157 _ = happyReduce_80

action_158 (31) = happyShift action_80
action_158 (32) = happyShift action_81
action_158 (34) = happyShift action_82
action_158 (37) = happyShift action_83
action_158 (39) = happyShift action_84
action_158 (43) = happyShift action_85
action_158 (46) = happyShift action_86
action_158 (48) = happyShift action_168
action_158 (49) = happyShift action_87
action_158 (50) = happyShift action_88
action_158 (52) = happyShift action_89
action_158 (53) = happyShift action_90
action_158 (54) = happyShift action_91
action_158 (59) = happyShift action_92
action_158 (81) = happyShift action_93
action_158 _ = happyFail

action_159 (35) = happyShift action_167
action_159 _ = happyFail

action_160 (31) = happyShift action_80
action_160 (32) = happyShift action_81
action_160 (34) = happyShift action_82
action_160 (36) = happyShift action_166
action_160 (37) = happyShift action_83
action_160 (39) = happyShift action_84
action_160 (43) = happyShift action_85
action_160 (46) = happyShift action_86
action_160 (49) = happyShift action_87
action_160 (50) = happyShift action_88
action_160 (52) = happyShift action_89
action_160 (53) = happyShift action_90
action_160 (54) = happyShift action_91
action_160 (59) = happyShift action_92
action_160 (81) = happyShift action_93
action_160 _ = happyFail

action_161 (31) = happyShift action_80
action_161 (32) = happyShift action_81
action_161 (34) = happyShift action_82
action_161 (36) = happyShift action_165
action_161 (37) = happyShift action_83
action_161 (39) = happyShift action_84
action_161 (43) = happyShift action_85
action_161 (46) = happyShift action_86
action_161 (49) = happyShift action_87
action_161 (50) = happyShift action_88
action_161 (52) = happyShift action_89
action_161 (53) = happyShift action_90
action_161 (54) = happyShift action_91
action_161 (59) = happyShift action_92
action_161 (81) = happyShift action_93
action_161 _ = happyFail

action_162 _ = happyReduce_73

action_163 (48) = happyShift action_164
action_163 _ = happyFail

action_164 _ = happyReduce_88

action_165 (35) = happyShift action_67
action_165 (37) = happyShift action_46
action_165 (40) = happyShift action_47
action_165 (44) = happyShift action_49
action_165 (61) = happyShift action_135
action_165 (63) = happyShift action_136
action_165 (64) = happyShift action_137
action_165 (67) = happyShift action_138
action_165 (76) = happyShift action_139
action_165 (79) = happyShift action_140
action_165 (86) = happyShift action_57
action_165 (6) = happyGoto action_129
action_165 (7) = happyGoto action_130
action_165 (9) = happyGoto action_41
action_165 (23) = happyGoto action_171
action_165 (25) = happyGoto action_132
action_165 (27) = happyGoto action_133
action_165 (28) = happyGoto action_134
action_165 _ = happyFail

action_166 (35) = happyShift action_67
action_166 (37) = happyShift action_46
action_166 (40) = happyShift action_47
action_166 (44) = happyShift action_49
action_166 (61) = happyShift action_135
action_166 (63) = happyShift action_136
action_166 (64) = happyShift action_137
action_166 (67) = happyShift action_138
action_166 (76) = happyShift action_139
action_166 (79) = happyShift action_140
action_166 (86) = happyShift action_57
action_166 (6) = happyGoto action_129
action_166 (7) = happyGoto action_130
action_166 (9) = happyGoto action_41
action_166 (23) = happyGoto action_170
action_166 (25) = happyGoto action_132
action_166 (27) = happyGoto action_133
action_166 (28) = happyGoto action_134
action_166 _ = happyFail

action_167 (30) = happyShift action_43
action_167 (33) = happyShift action_44
action_167 (35) = happyShift action_45
action_167 (37) = happyShift action_46
action_167 (40) = happyShift action_47
action_167 (43) = happyShift action_48
action_167 (44) = happyShift action_49
action_167 (55) = happyShift action_50
action_167 (56) = happyShift action_51
action_167 (82) = happyShift action_53
action_167 (83) = happyShift action_54
action_167 (84) = happyShift action_55
action_167 (85) = happyShift action_56
action_167 (86) = happyShift action_57
action_167 (5) = happyGoto action_169
action_167 (6) = happyGoto action_39
action_167 (7) = happyGoto action_40
action_167 (9) = happyGoto action_41
action_167 _ = happyFail

action_168 _ = happyReduce_78

action_169 (31) = happyShift action_80
action_169 (32) = happyShift action_81
action_169 (34) = happyShift action_82
action_169 (36) = happyShift action_173
action_169 (37) = happyShift action_83
action_169 (39) = happyShift action_84
action_169 (43) = happyShift action_85
action_169 (46) = happyShift action_86
action_169 (49) = happyShift action_87
action_169 (50) = happyShift action_88
action_169 (52) = happyShift action_89
action_169 (53) = happyShift action_90
action_169 (54) = happyShift action_91
action_169 (59) = happyShift action_92
action_169 (81) = happyShift action_93
action_169 _ = happyFail

action_170 (65) = happyShift action_172
action_170 _ = happyReduce_90

action_171 _ = happyReduce_91

action_172 (35) = happyShift action_67
action_172 (37) = happyShift action_46
action_172 (40) = happyShift action_47
action_172 (44) = happyShift action_49
action_172 (61) = happyShift action_135
action_172 (63) = happyShift action_136
action_172 (64) = happyShift action_137
action_172 (67) = happyShift action_138
action_172 (76) = happyShift action_139
action_172 (79) = happyShift action_140
action_172 (86) = happyShift action_57
action_172 (6) = happyGoto action_129
action_172 (7) = happyGoto action_130
action_172 (9) = happyGoto action_41
action_172 (23) = happyGoto action_175
action_172 (25) = happyGoto action_132
action_172 (27) = happyGoto action_133
action_172 (28) = happyGoto action_134
action_172 _ = happyFail

action_173 (48) = happyShift action_174
action_173 _ = happyFail

action_174 _ = happyReduce_92

action_175 _ = happyReduce_89

happyReduce_1 = happySpecReduce_1  4 happyReduction_1
happyReduction_1 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn4
		 (BasicTypeNode (tpos happy_var_1) (BType "Boolean")
	)
happyReduction_1 _  = notHappyAtAll 

happyReduce_2 = happySpecReduce_1  4 happyReduction_2
happyReduction_2 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn4
		 (BasicTypeNode (tpos happy_var_1) (BType "Char")
	)
happyReduction_2 _  = notHappyAtAll 

happyReduce_3 = happySpecReduce_1  4 happyReduction_3
happyReduction_3 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn4
		 (BasicTypeNode (tpos happy_var_1) (BType "Float")
	)
happyReduction_3 _  = notHappyAtAll 

happyReduce_4 = happySpecReduce_1  4 happyReduction_4
happyReduction_4 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn4
		 (BasicTypeNode (tpos happy_var_1) (BType "Int")
	)
happyReduction_4 _  = notHappyAtAll 

happyReduce_5 = happySpecReduce_1  4 happyReduction_5
happyReduction_5 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn4
		 (BasicTypeNode (tpos happy_var_1) (BType "Unit")
	)
happyReduction_5 _  = notHappyAtAll 

happyReduce_6 = happySpecReduce_3  5 happyReduction_6
happyReduction_6 (HappyAbsSyn4  happy_var_3)
	_
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (RExprNode (pos happy_var_1) (OpBoolean happy_var_1 happy_var_3 "||")
	)
happyReduction_6 _ _ _  = notHappyAtAll 

happyReduce_7 = happySpecReduce_3  5 happyReduction_7
happyReduction_7 (HappyAbsSyn4  happy_var_3)
	_
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (RExprNode (pos happy_var_1) (OpBoolean happy_var_1 happy_var_3 "&&")
	)
happyReduction_7 _ _ _  = notHappyAtAll 

happyReduce_8 = happySpecReduce_2  5 happyReduction_8
happyReduction_8 (HappyAbsSyn4  happy_var_2)
	(HappyTerminal happy_var_1)
	 =  HappyAbsSyn4
		 (RExprNode (tpos happy_var_1) (Not happy_var_2)
	)
happyReduction_8 _ _  = notHappyAtAll 

happyReduce_9 = happySpecReduce_3  5 happyReduction_9
happyReduction_9 (HappyAbsSyn4  happy_var_3)
	_
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (RExprNode (pos happy_var_1) (OpRelation happy_var_1 happy_var_3 "==")
	)
happyReduction_9 _ _ _  = notHappyAtAll 

happyReduce_10 = happySpecReduce_3  5 happyReduction_10
happyReduction_10 (HappyAbsSyn4  happy_var_3)
	_
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (RExprNode (pos happy_var_1) (OpRelation happy_var_1 happy_var_3 "!=")
	)
happyReduction_10 _ _ _  = notHappyAtAll 

happyReduce_11 = happySpecReduce_3  5 happyReduction_11
happyReduction_11 (HappyAbsSyn4  happy_var_3)
	_
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (RExprNode (pos happy_var_1) (OpRelation happy_var_1 happy_var_3 "<")
	)
happyReduction_11 _ _ _  = notHappyAtAll 

happyReduce_12 = happySpecReduce_3  5 happyReduction_12
happyReduction_12 (HappyAbsSyn4  happy_var_3)
	_
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (RExprNode (pos happy_var_1) (OpRelation happy_var_1 happy_var_3 "<=")
	)
happyReduction_12 _ _ _  = notHappyAtAll 

happyReduce_13 = happySpecReduce_3  5 happyReduction_13
happyReduction_13 (HappyAbsSyn4  happy_var_3)
	_
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (RExprNode (pos happy_var_1) (OpRelation happy_var_1 happy_var_3 ">")
	)
happyReduction_13 _ _ _  = notHappyAtAll 

happyReduce_14 = happySpecReduce_3  5 happyReduction_14
happyReduction_14 (HappyAbsSyn4  happy_var_3)
	_
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (RExprNode (pos happy_var_1) (OpRelation happy_var_1 happy_var_3 ">=")
	)
happyReduction_14 _ _ _  = notHappyAtAll 

happyReduce_15 = happySpecReduce_3  5 happyReduction_15
happyReduction_15 (HappyAbsSyn4  happy_var_3)
	_
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (RExprNode (pos happy_var_1) (OpAritm happy_var_1 happy_var_3 "+")
	)
happyReduction_15 _ _ _  = notHappyAtAll 

happyReduce_16 = happySpecReduce_3  5 happyReduction_16
happyReduction_16 (HappyAbsSyn4  happy_var_3)
	_
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (RExprNode (pos happy_var_1) (OpAritm happy_var_1 happy_var_3 "-")
	)
happyReduction_16 _ _ _  = notHappyAtAll 

happyReduce_17 = happySpecReduce_3  5 happyReduction_17
happyReduction_17 (HappyAbsSyn4  happy_var_3)
	_
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (RExprNode (pos happy_var_1) (OpAritm happy_var_1 happy_var_3 "*")
	)
happyReduction_17 _ _ _  = notHappyAtAll 

happyReduce_18 = happySpecReduce_3  5 happyReduction_18
happyReduction_18 (HappyAbsSyn4  happy_var_3)
	_
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (RExprNode (pos happy_var_1) (OpAritm happy_var_1 happy_var_3 "/")
	)
happyReduction_18 _ _ _  = notHappyAtAll 

happyReduce_19 = happySpecReduce_3  5 happyReduction_19
happyReduction_19 (HappyAbsSyn4  happy_var_3)
	_
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (RExprNode (pos happy_var_1) (OpAritm happy_var_1 happy_var_3 "%")
	)
happyReduction_19 _ _ _  = notHappyAtAll 

happyReduce_20 = happySpecReduce_3  5 happyReduction_20
happyReduction_20 (HappyAbsSyn4  happy_var_3)
	_
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (RExprNode (pos happy_var_1) (OpAritm happy_var_1 happy_var_3 "^")
	)
happyReduction_20 _ _ _  = notHappyAtAll 

happyReduce_21 = happySpecReduce_2  5 happyReduction_21
happyReduction_21 (HappyAbsSyn4  happy_var_2)
	(HappyTerminal happy_var_1)
	 =  HappyAbsSyn4
		 (RExprNode (tpos happy_var_1) (Neg happy_var_2)
	)
happyReduction_21 _ _  = notHappyAtAll 

happyReduce_22 = happySpecReduce_2  5 happyReduction_22
happyReduction_22 (HappyAbsSyn4  happy_var_2)
	(HappyTerminal happy_var_1)
	 =  HappyAbsSyn4
		 (RExprNode (tpos happy_var_1) (Ref happy_var_2)
	)
happyReduction_22 _ _  = notHappyAtAll 

happyReduce_23 = happySpecReduce_1  5 happyReduction_23
happyReduction_23 (HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (RExprNode (pos happy_var_1) (FCall happy_var_1)
	)
happyReduction_23 _  = notHappyAtAll 

happyReduce_24 = happySpecReduce_1  5 happyReduction_24
happyReduction_24 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn4
		 (let (Token p (TokenInt v)) = happy_var_1 in RExprNode p (Int (read v::Integer))
	)
happyReduction_24 _  = notHappyAtAll 

happyReduce_25 = happySpecReduce_1  5 happyReduction_25
happyReduction_25 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn4
		 (let (Token p (TokenInt v)) = happy_var_1 in RExprNode p (Char (head v))
	)
happyReduction_25 _  = notHappyAtAll 

happyReduce_26 = happySpecReduce_1  5 happyReduction_26
happyReduction_26 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn4
		 (let (Token p (TokenInt v)) = happy_var_1 in RExprNode p (String v)
	)
happyReduction_26 _  = notHappyAtAll 

happyReduce_27 = happySpecReduce_1  5 happyReduction_27
happyReduction_27 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn4
		 (let (Token p (TokenInt v)) = happy_var_1 in RExprNode p (Float (read v::Double))
	)
happyReduction_27 _  = notHappyAtAll 

happyReduce_28 = happySpecReduce_1  5 happyReduction_28
happyReduction_28 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn4
		 (RExprNode (tpos happy_var_1) (Bool Boolean_True)
	)
happyReduction_28 _  = notHappyAtAll 

happyReduce_29 = happySpecReduce_1  5 happyReduction_29
happyReduction_29 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn4
		 (RExprNode (tpos happy_var_1) (Bool Boolean_False)
	)
happyReduction_29 _  = notHappyAtAll 

happyReduce_30 = happySpecReduce_3  5 happyReduction_30
happyReduction_30 _
	(HappyAbsSyn4  happy_var_2)
	_
	 =  HappyAbsSyn4
		 (happy_var_2
	)
happyReduction_30 _ _ _  = notHappyAtAll 

happyReduce_31 = happySpecReduce_1  5 happyReduction_31
happyReduction_31 (HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (RExprNode (pos happy_var_1) (Lexpr happy_var_1)
	)
happyReduction_31 _  = notHappyAtAll 

happyReduce_32 = happySpecReduce_2  6 happyReduction_32
happyReduction_32 (HappyAbsSyn4  happy_var_2)
	(HappyTerminal happy_var_1)
	 =  HappyAbsSyn4
		 (LExprNode (tpos happy_var_1) (Deref happy_var_2)
	)
happyReduction_32 _ _  = notHappyAtAll 

happyReduce_33 = happySpecReduce_2  6 happyReduction_33
happyReduction_33 (HappyAbsSyn4  happy_var_2)
	(HappyTerminal happy_var_1)
	 =  HappyAbsSyn4
		 (LExprNode (tpos happy_var_1) (PreInc happy_var_2)
	)
happyReduction_33 _ _  = notHappyAtAll 

happyReduce_34 = happySpecReduce_2  6 happyReduction_34
happyReduction_34 (HappyAbsSyn4  happy_var_2)
	(HappyTerminal happy_var_1)
	 =  HappyAbsSyn4
		 (LExprNode (tpos happy_var_1) (PreDecr happy_var_2)
	)
happyReduction_34 _ _  = notHappyAtAll 

happyReduce_35 = happySpecReduce_2  6 happyReduction_35
happyReduction_35 _
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (LExprNode (pos happy_var_1) (PostInc happy_var_1)
	)
happyReduction_35 _ _  = notHappyAtAll 

happyReduce_36 = happySpecReduce_2  6 happyReduction_36
happyReduction_36 _
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (LExprNode (pos happy_var_1) (PostDecr happy_var_1)
	)
happyReduction_36 _ _  = notHappyAtAll 

happyReduce_37 = happySpecReduce_3  6 happyReduction_37
happyReduction_37 _
	(HappyAbsSyn4  happy_var_2)
	_
	 =  HappyAbsSyn4
		 (happy_var_2
	)
happyReduction_37 _ _ _  = notHappyAtAll 

happyReduce_38 = happySpecReduce_1  6 happyReduction_38
happyReduction_38 (HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (LExprNode (pos happy_var_1) (BasLExpr happy_var_1)
	)
happyReduction_38 _  = notHappyAtAll 

happyReduce_39 = happyReduce 4 7 happyReduction_39
happyReduction_39 (_ `HappyStk`
	(HappyAbsSyn8  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyTerminal happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn4
		 (let (Token p (TokenIdent v)) = happy_var_1 in FunCallNode p (Call (Ident v) happy_var_3)
	) `HappyStk` happyRest

happyReduce_40 = happySpecReduce_0  8 happyReduction_40
happyReduction_40  =  HappyAbsSyn8
		 ([]
	)

happyReduce_41 = happySpecReduce_1  8 happyReduction_41
happyReduction_41 (HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn8
		 ((:[]) happy_var_1
	)
happyReduction_41 _  = notHappyAtAll 

happyReduce_42 = happySpecReduce_3  8 happyReduction_42
happyReduction_42 (HappyAbsSyn8  happy_var_3)
	_
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn8
		 ((:) happy_var_1 happy_var_3
	)
happyReduction_42 _ _ _  = notHappyAtAll 

happyReduce_43 = happyReduce 4 9 happyReduction_43
happyReduction_43 (_ `HappyStk`
	(HappyAbsSyn4  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn4  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn4
		 (BLExprNode (pos happy_var_1) (ArrayEl happy_var_1 happy_var_3)
	) `HappyStk` happyRest

happyReduce_44 = happySpecReduce_1  9 happyReduction_44
happyReduction_44 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn4
		 (let (Token p (TokenIdent v)) = happy_var_1 in BLExprNode p (Id (Ident v))
	)
happyReduction_44 _  = notHappyAtAll 

happyReduce_45 = happySpecReduce_1  10 happyReduction_45
happyReduction_45 (HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn4
		 (ProgramNode (Pn 0 1 1) (Prog (reverse happy_var_1))
	)
happyReduction_45 _  = notHappyAtAll 

happyReduce_46 = happySpecReduce_0  11 happyReduction_46
happyReduction_46  =  HappyAbsSyn8
		 ([]
	)

happyReduce_47 = happySpecReduce_2  11 happyReduction_47
happyReduction_47 (HappyAbsSyn4  happy_var_2)
	(HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn8
		 (flip (:) happy_var_1 happy_var_2
	)
happyReduction_47 _ _  = notHappyAtAll 

happyReduce_48 = happyReduce 7 12 happyReduction_48
happyReduction_48 (_ `HappyStk`
	(HappyAbsSyn4  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn4  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal happy_var_2) `HappyStk`
	(HappyAbsSyn4  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn4
		 (let (Token p (TokenIdent v)) = happy_var_2 in DeclNode (pos happy_var_1) (DvarBInit happy_var_1 (Ident v) happy_var_4 happy_var_6)
	) `HappyStk` happyRest

happyReduce_49 = happyReduce 7 12 happyReduction_49
happyReduction_49 (_ `HappyStk`
	(HappyAbsSyn4  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn4  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal happy_var_2) `HappyStk`
	(HappyAbsSyn4  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn4
		 (let (Token p (TokenIdent v)) = happy_var_2 in DeclNode (pos happy_var_1) (DvarCInit happy_var_1 (Ident v) happy_var_4 happy_var_6)
	) `HappyStk` happyRest

happyReduce_50 = happyReduce 12 12 happyReduction_50
happyReduction_50 (_ `HappyStk`
	(HappyAbsSyn4  happy_var_11) `HappyStk`
	(HappyAbsSyn4  happy_var_10) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn4  happy_var_7) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn8  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal happy_var_2) `HappyStk`
	(HappyTerminal happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn4
		 (let (Token p (TokenIdent v)) = happy_var_2 in DeclNode (tpos happy_var_1) (Dfun (Ident v) happy_var_4 happy_var_7 happy_var_10 happy_var_11)
	) `HappyStk` happyRest

happyReduce_51 = happySpecReduce_1  13 happyReduction_51
happyReduction_51 (HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (TypeSpecNode (pos happy_var_1) (BasTyp happy_var_1)
	)
happyReduction_51 _  = notHappyAtAll 

happyReduce_52 = happySpecReduce_1  13 happyReduction_52
happyReduction_52 (HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (TypeSpecNode (pos happy_var_1) (CompType happy_var_1)
	)
happyReduction_52 _  = notHappyAtAll 

happyReduce_53 = happyReduce 4 14 happyReduction_53
happyReduction_53 (_ `HappyStk`
	(HappyTerminal happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn4  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn4
		 (let (Token p (TokenIdent v)) = happy_var_3 in CompoundTypeNode (pos happy_var_1) (ArrDef happy_var_1 (read v::Integer))
	) `HappyStk` happyRest

happyReduce_54 = happySpecReduce_3  14 happyReduction_54
happyReduction_54 _
	_
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (CompoundTypeNode (pos happy_var_1) (ArrUnDef happy_var_1)
	)
happyReduction_54 _ _ _  = notHappyAtAll 

happyReduce_55 = happySpecReduce_2  14 happyReduction_55
happyReduction_55 _
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (CompoundTypeNode (pos happy_var_1) (Pointer happy_var_1)
	)
happyReduction_55 _ _  = notHappyAtAll 

happyReduce_56 = happySpecReduce_1  15 happyReduction_56
happyReduction_56 (HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (ComplexRExprNode (pos happy_var_1) (Simple happy_var_1)
	)
happyReduction_56 _  = notHappyAtAll 

happyReduce_57 = happySpecReduce_3  15 happyReduction_57
happyReduction_57 _
	(HappyAbsSyn16  happy_var_2)
	(HappyTerminal happy_var_1)
	 =  HappyAbsSyn4
		 (ComplexRExprNode (tpos happy_var_1) (Array happy_var_2)
	)
happyReduction_57 _ _ _  = notHappyAtAll 

happyReduce_58 = happySpecReduce_1  16 happyReduction_58
happyReduction_58 (HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn16
		 ((:[]) happy_var_1
	)
happyReduction_58 _  = notHappyAtAll 

happyReduce_59 = happySpecReduce_3  16 happyReduction_59
happyReduction_59 (HappyAbsSyn16  happy_var_3)
	_
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn16
		 ((:) happy_var_1 happy_var_3
	)
happyReduction_59 _ _ _  = notHappyAtAll 

happyReduce_60 = happySpecReduce_0  17 happyReduction_60
happyReduction_60  =  HappyAbsSyn8
		 ([]
	)

happyReduce_61 = happySpecReduce_1  17 happyReduction_61
happyReduction_61 (HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn8
		 ((:[]) happy_var_1
	)
happyReduction_61 _  = notHappyAtAll 

happyReduce_62 = happySpecReduce_3  17 happyReduction_62
happyReduction_62 (HappyAbsSyn8  happy_var_3)
	_
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn8
		 ((:) happy_var_1 happy_var_3
	)
happyReduction_62 _ _ _  = notHappyAtAll 

happyReduce_63 = happySpecReduce_3  18 happyReduction_63
happyReduction_63 (HappyTerminal happy_var_3)
	(HappyAbsSyn4  happy_var_2)
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (let (Token p (TokenIdent v)) = happy_var_3 in ParameterNode (pos happy_var_1) (Param happy_var_1 happy_var_2 (Ident v))
	)
happyReduction_63 _ _ _  = notHappyAtAll 

happyReduce_64 = happySpecReduce_1  19 happyReduction_64
happyReduction_64 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn4
		 (ModalityDeclNode (tpos happy_var_1) ModalityD_val
	)
happyReduction_64 _  = notHappyAtAll 

happyReduce_65 = happySpecReduce_1  19 happyReduction_65
happyReduction_65 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn4
		 (ModalityDeclNode (tpos happy_var_1) ModalityD_var
	)
happyReduction_65 _  = notHappyAtAll 

happyReduce_66 = happySpecReduce_0  20 happyReduction_66
happyReduction_66  =  HappyAbsSyn4
		 (ModalityParamNode (Pn 0 0 0) ModalityPEmpty
	)

happyReduce_67 = happySpecReduce_1  20 happyReduction_67
happyReduction_67 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn4
		 (ModalityParamNode (tpos happy_var_1) ModalityP_val
	)
happyReduction_67 _  = notHappyAtAll 

happyReduce_68 = happySpecReduce_1  20 happyReduction_68
happyReduction_68 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn4
		 (ModalityParamNode (tpos happy_var_1) ModalityP_valres
	)
happyReduction_68 _  = notHappyAtAll 

happyReduce_69 = happySpecReduce_1  20 happyReduction_69
happyReduction_69 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn4
		 (ModalityParamNode (tpos happy_var_1) ModalityP_var
	)
happyReduction_69 _  = notHappyAtAll 

happyReduce_70 = happySpecReduce_2  21 happyReduction_70
happyReduction_70 (HappyAbsSyn8  happy_var_2)
	(HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn4
		 (CompStmtNode (pos (head happy_var_1)) (BlockDecl (reverse happy_var_1) (reverse happy_var_2))
	)
happyReduction_70 _ _  = notHappyAtAll 

happyReduce_71 = happySpecReduce_0  22 happyReduction_71
happyReduction_71  =  HappyAbsSyn8
		 ([]
	)

happyReduce_72 = happySpecReduce_2  22 happyReduction_72
happyReduction_72 (HappyAbsSyn4  happy_var_2)
	(HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn8
		 (flip (:) happy_var_1 happy_var_2
	)
happyReduction_72 _ _  = notHappyAtAll 

happyReduce_73 = happySpecReduce_3  23 happyReduction_73
happyReduction_73 _
	(HappyAbsSyn4  happy_var_2)
	(HappyTerminal happy_var_1)
	 =  HappyAbsSyn4
		 (StmtNode (tpos happy_var_1) (Comp happy_var_2)
	)
happyReduction_73 _ _ _  = notHappyAtAll 

happyReduce_74 = happySpecReduce_2  23 happyReduction_74
happyReduction_74 _
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (StmtNode (pos happy_var_1) (ProcCall happy_var_1)
	)
happyReduction_74 _ _  = notHappyAtAll 

happyReduce_75 = happySpecReduce_2  23 happyReduction_75
happyReduction_75 _
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (StmtNode (pos happy_var_1) (Jmp happy_var_1)
	)
happyReduction_75 _ _  = notHappyAtAll 

happyReduce_76 = happySpecReduce_1  23 happyReduction_76
happyReduction_76 (HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (StmtNode (pos happy_var_1) (Iter happy_var_1)
	)
happyReduction_76 _  = notHappyAtAll 

happyReduce_77 = happySpecReduce_1  23 happyReduction_77
happyReduction_77 (HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (StmtNode (pos happy_var_1) (Sel happy_var_1)
	)
happyReduction_77 _  = notHappyAtAll 

happyReduce_78 = happyReduce 4 23 happyReduction_78
happyReduction_78 (_ `HappyStk`
	(HappyAbsSyn4  happy_var_3) `HappyStk`
	(HappyAbsSyn4  happy_var_2) `HappyStk`
	(HappyAbsSyn4  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn4
		 (StmtNode (pos happy_var_1) (Assgn happy_var_1 happy_var_2 happy_var_3)
	) `HappyStk` happyRest

happyReduce_79 = happySpecReduce_2  23 happyReduction_79
happyReduction_79 _
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (StmtNode (pos happy_var_1) (LExprStmt happy_var_1)
	)
happyReduction_79 _ _  = notHappyAtAll 

happyReduce_80 = happySpecReduce_1  24 happyReduction_80
happyReduction_80 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn4
		 (Assignment_opNode (tpos happy_var_1) Assign
	)
happyReduction_80 _  = notHappyAtAll 

happyReduce_81 = happySpecReduce_1  24 happyReduction_81
happyReduction_81 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn4
		 (Assignment_opNode (tpos happy_var_1) AssgnMul
	)
happyReduction_81 _  = notHappyAtAll 

happyReduce_82 = happySpecReduce_1  24 happyReduction_82
happyReduction_82 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn4
		 (Assignment_opNode (tpos happy_var_1) AssgnAdd
	)
happyReduction_82 _  = notHappyAtAll 

happyReduce_83 = happySpecReduce_1  24 happyReduction_83
happyReduction_83 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn4
		 (Assignment_opNode (tpos happy_var_1) AssgnDiv
	)
happyReduction_83 _  = notHappyAtAll 

happyReduce_84 = happySpecReduce_1  24 happyReduction_84
happyReduction_84 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn4
		 (Assignment_opNode (tpos happy_var_1) AssgnSub
	)
happyReduction_84 _  = notHappyAtAll 

happyReduce_85 = happySpecReduce_1  25 happyReduction_85
happyReduction_85 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn4
		 (JumpStmtNode (tpos happy_var_1) Break
	)
happyReduction_85 _  = notHappyAtAll 

happyReduce_86 = happySpecReduce_1  25 happyReduction_86
happyReduction_86 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn4
		 (JumpStmtNode (tpos happy_var_1) Continue
	)
happyReduction_86 _  = notHappyAtAll 

happyReduce_87 = happySpecReduce_2  26 happyReduction_87
happyReduction_87 _
	(HappyTerminal happy_var_1)
	 =  HappyAbsSyn4
		 (ReturnStmtNode (tpos happy_var_1) RetExpVoid
	)
happyReduction_87 _ _  = notHappyAtAll 

happyReduce_88 = happyReduce 5 26 happyReduction_88
happyReduction_88 (_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn4  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyTerminal happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn4
		 (ReturnStmtNode (tpos happy_var_1) (RetExp happy_var_3)
	) `HappyStk` happyRest

happyReduce_89 = happyReduce 7 27 happyReduction_89
happyReduction_89 ((HappyAbsSyn4  happy_var_7) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn4  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn4  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyTerminal happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn4
		 (SelectionStmtNode (tpos happy_var_1) (IfElse happy_var_3 happy_var_5 happy_var_7)
	) `HappyStk` happyRest

happyReduce_90 = happyReduce 5 27 happyReduction_90
happyReduction_90 ((HappyAbsSyn4  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn4  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyTerminal happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn4
		 (SelectionStmtNode (tpos happy_var_1) (IfNoElse happy_var_3 happy_var_5)
	) `HappyStk` happyRest

happyReduce_91 = happyReduce 5 28 happyReduction_91
happyReduction_91 ((HappyAbsSyn4  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn4  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyTerminal happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn4
		 (IterStmtNode (tpos happy_var_1) (While happy_var_3 happy_var_5)
	) `HappyStk` happyRest

happyReduce_92 = happyReduce 7 28 happyReduction_92
happyReduction_92 (_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn4  happy_var_5) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn4  happy_var_2) `HappyStk`
	(HappyTerminal happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn4
		 (IterStmtNode (tpos happy_var_1) (DoWhile happy_var_2 happy_var_5)
	) `HappyStk` happyRest

happyNewToken action sts stk [] =
	action 87 87 notHappyAtAll (HappyState action) sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = action i i tk (HappyState action) sts stk tks in
	case tk of {
	Token _ (TokenSymbols ":") -> cont 29;
	Token _ (TokenSymbols "!") -> cont 30;
	Token _ (TokenSymbols "!=") -> cont 31;
	Token _ (TokenSymbols "%") -> cont 32;
	Token _ (TokenSymbols "&") -> cont 33;
	Token _ (TokenSymbols "&&") -> cont 34;
	Token _ (TokenSymbols "(") -> cont 35;
	Token _ (TokenSymbols ")") -> cont 36;
	Token _ (TokenSymbols "*") -> cont 37;
	Token _ (TokenSymbols "*=") -> cont 38;
	Token _ (TokenSymbols "+") -> cont 39;
	Token _ (TokenSymbols "++") -> cont 40;
	Token _ (TokenSymbols "+=") -> cont 41;
	Token _ (TokenSymbols ",") -> cont 42;
	Token _ (TokenSymbols "-") -> cont 43;
	Token _ (TokenSymbols "--") -> cont 44;
	Token _ (TokenSymbols "-=") -> cont 45;
	Token _ (TokenSymbols "/") -> cont 46;
	Token _ (TokenSymbols "/=") -> cont 47;
	Token _ (TokenSymbols ";") -> cont 48;
	Token _ (TokenSymbols "<") -> cont 49;
	Token _ (TokenSymbols "<=") -> cont 50;
	Token _ (TokenSymbols "=") -> cont 51;
	Token _ (TokenSymbols "==") -> cont 52;
	Token _ (TokenSymbols ">") -> cont 53;
	Token _ (TokenSymbols ">=") -> cont 54;
	Token _ (TokenSymbols "false") -> cont 55;
	Token _ (TokenSymbols "true") -> cont 56;
	Token _ (TokenSymbols "[") -> cont 57;
	Token _ (TokenSymbols "]") -> cont 58;
	Token _ (TokenSymbols "^") -> cont 59;
	Token _ (TokenSymbols "Boolean") -> cont 60;
	Token _ (TokenSymbols "break") -> cont 61;
	Token _ (TokenSymbols "Char") -> cont 62;
	Token _ (TokenSymbols "continue") -> cont 63;
	Token _ (TokenSymbols "do") -> cont 64;
	Token _ (TokenSymbols "else") -> cont 65;
	Token _ (TokenSymbols "Float") -> cont 66;
	Token _ (TokenSymbols "if") -> cont 67;
	Token _ (TokenSymbols "Int") -> cont 68;
	Token _ (TokenSymbols "String") -> cont 69;
	Token _ (TokenSymbols "Array") -> cont 70;
	Token _ (TokenSymbols "return") -> cont 71;
	Token _ (TokenSymbols "val") -> cont 72;
	Token _ (TokenSymbols "var") -> cont 73;
	Token _ (TokenSymbols "valres") -> cont 74;
	Token _ (TokenSymbols "Unit") -> cont 75;
	Token _ (TokenSymbols "while") -> cont 76;
	Token _ (TokenSymbols "for") -> cont 77;
	Token _ (TokenSymbols "for") -> cont 78;
	Token _ (TokenSymbols "{") -> cont 79;
	Token _ (TokenSymbols "}") -> cont 80;
	Token _ (TokenSymbols "||") -> cont 81;
	Token _ (TokenInt _) -> cont 82;
	Token _ (TokenChar _) -> cont 83;
	Token _ (TokenAlpha _) -> cont 84;
	Token _ (TokenDouble _) -> cont 85;
	Token _ (TokenIdent _) -> cont 86;
	_ -> happyError' (tk:tks)
	}

happyError_ 87 tk tks = happyError' tks
happyError_ _ tk tks = happyError' (tk:tks)

happyThen :: () => Err a -> (a -> Err b) -> Err b
happyThen = (thenM)
happyReturn :: () => a -> Err a
happyReturn = (returnM)
happyThen1 m k tks = (thenM) m (\a -> k a tks)
happyReturn1 :: () => a -> b -> Err a
happyReturn1 = \a tks -> (returnM) a
happyError' :: () => [(Token)] -> Err a
happyError' = happyError

pProgram tks = happySomeParser where
  happySomeParser = happyThen (happyParse action_0 tks) (\x -> case x of {HappyAbsSyn4 z -> happyReturn z; _other -> notHappyAtAll })

happySeq = happyDontSeq


data AbsNode 
    = RExprNode         {pos::Posn, rExpr::RExpr}
    | FunCallNode       {pos::Posn, funCall::FunCall}
    | LExprNode         {pos::Posn, lExpr::LExpr}
    | BLExprNode        {pos::Posn, bLExpr::BLExpr}
    | ProgramNode       {pos::Posn, program::Program}
    | DeclNode          {pos::Posn, decl::Decl}
    | TypeSpecNode      {pos::Posn, typeSpec::TypeSpec}
    | BasicTypeNode     {pos::Posn, basicType::BasicType}
    | CompoundTypeNode  {pos::Posn, compoundType::CompoundType}
    | ComplexRExprNode  {pos::Posn, complexRExpr::ComplexRExpr}
    | ParameterNode     {pos::Posn, parameter::Parameter}
    | ModalityParamNode {pos::Posn, modalityParam::ModalityParam}
    | ModalityDeclNode  {pos::Posn, modalityDecl::ModalityDecl}
    | CompStmtNode      {pos::Posn, compStmt::CompStmt}
    | StmtNode          {pos::Posn, stmt::Stmt}
    | Assignment_opNode {pos::Posn, assignment_op::Assignment_op}
    | JumpStmtNode      {pos::Posn, jumpStmt::JumpStmt} 
    | ReturnStmtNode    {pos::Posn, returnStmt::ReturnStmt}
    | SelectionStmtNode {pos::Posn, selectionStmt::SelectionStmt}
    | IterStmtNode      {pos::Posn, iterStmt::IterStmt}
    deriving (Eq, Ord, Show)

newtype Ident = Ident String
    deriving (Eq, Ord, Show, Read)
data Boolean = Boolean_True | Boolean_False
    deriving (Eq, Ord, Show, Read)

data RExpr
    = OpRelation AbsNode AbsNode String
    | OpAritm AbsNode AbsNode String
    | OpBoolean AbsNode AbsNode String
    | Not AbsNode
    | Neg AbsNode
    | Ref AbsNode
    | FCall AbsNode
    | Int Integer
    | Char Char
    | String String
    | Float Double
    | Bool Boolean
    | Lexpr AbsNode
    deriving (Eq, Ord, Show)

data FunCall
    = Call Ident [AbsNode]
    deriving (Eq, Ord, Show)

data LExpr
    = Deref AbsNode
    | PreInc AbsNode
    | PreDecr AbsNode
    | PostInc AbsNode
    | PostDecr AbsNode
    | BasLExpr AbsNode
    deriving (Eq, Ord, Show)

data BLExpr
    = ArrayEl AbsNode AbsNode
    | Id Ident
    deriving (Eq, Ord, Show)

data Program =
    Prog [AbsNode]
    deriving (Eq, Ord, Show)

data Decl
    = DvarBInit AbsNode Ident AbsNode AbsNode
    | DvarCInit AbsNode Ident AbsNode AbsNode
    | Dfun Ident [AbsNode] AbsNode AbsNode AbsNode
    deriving (Eq, Ord, Show)

data TypeSpec
    = BasTyp AbsNode
    | CompType AbsNode
    deriving (Eq, Ord, Show)

data BasicType
    = BType String
    deriving (Eq, Ord, Show, Read)

data CompoundType
    = ArrDef AbsNode Integer
    | ArrUnDef AbsNode
    | Pointer AbsNode
    deriving (Eq, Ord, Show)

data ComplexRExpr
    = Simple AbsNode
    | Array [AbsNode]
    deriving (Eq, Ord, Show)

data Parameter
    = Param AbsNode AbsNode Ident
    deriving (Eq, Ord, Show)

data ModalityParam
    = ModalityPEmpty
    | ModalityP_val
    | ModalityP_var
    | ModalityP_valres
    deriving (Eq, Ord, Show)

data ModalityDecl
    = ModalityD_val
    | ModalityD_var
    deriving (Eq, Ord, Show)

data CompStmt
    = BlockDecl [AbsNode] [AbsNode]
    deriving (Eq, Ord, Show)

data Stmt
    = Comp AbsNode
    | ProcCall AbsNode
    | Jmp AbsNode
    | Iter AbsNode
    | Sel AbsNode
    | Assgn AbsNode AbsNode AbsNode
    | LExprStmt AbsNode
    deriving (Eq, Ord, Show)

data Assignment_op 
    = Assign
    | AssgnMul
    | AssgnAdd
    | AssgnDiv
    | AssgnSub
    | AssgnPow
    | AssgnAnd
    | AssgnOr
    deriving (Eq, Ord, Show, Read)

data JumpStmt 
    = Break
    | Continue
    deriving (Eq, Ord, Show, Read)

data ReturnStmt 
    = RetExpVoid
    | RetExp AbsNode
    deriving (Eq, Ord, Show) 

data SelectionStmt = IfNoElse AbsNode AbsNode | IfElse AbsNode AbsNode AbsNode
  deriving (Eq, Ord, Show)

data IterStmt = While AbsNode AbsNode | DoWhile AbsNode AbsNode
  deriving (Eq, Ord, Show)


returnM :: a -> Err a
returnM = return

thenM :: Err a -> (a -> Err b) -> Err b
thenM = (>>=)

happyError :: [Token] -> Err a
happyError ts =
  Bad $ "syntax error at " ++ tokenPos ts ++ 
  case ts of
    [] -> []
    [Err _] -> " due to lexer error"
    _ -> " before " ++ unwords (map (id . prToken) (take 4 ts))
{-# LINE 1 "templates/GenericTemplate.hs" #-}
{-# LINE 1 "templates/GenericTemplate.hs" #-}
{-# LINE 1 "<built-in>" #-}
{-# LINE 16 "<built-in>" #-}
{-# LINE 1 "/Applications/ghc-7.10.1.app/Contents/lib/ghc-7.10.1/include/ghcversion.h" #-}


















{-# LINE 17 "<built-in>" #-}
{-# LINE 1 "templates/GenericTemplate.hs" #-}
-- Id: GenericTemplate.hs,v 1.26 2005/01/14 14:47:22 simonmar Exp 


{-# LINE 13 "templates/GenericTemplate.hs" #-}


{-# LINE 46 "templates/GenericTemplate.hs" #-}









{-# LINE 67 "templates/GenericTemplate.hs" #-}


{-# LINE 77 "templates/GenericTemplate.hs" #-}










infixr 9 `HappyStk`
data HappyStk a = HappyStk a (HappyStk a)

-----------------------------------------------------------------------------
-- starting the parse

happyParse start_state = happyNewToken start_state notHappyAtAll notHappyAtAll

-----------------------------------------------------------------------------
-- Accepting the parse

-- If the current token is (1), it means we've just accepted a partial
-- parse (a %partial parser).  We must ignore the saved token on the top of
-- the stack in this case.
happyAccept (1) tk st sts (_ `HappyStk` ans `HappyStk` _) =
        happyReturn1 ans
happyAccept j tk st sts (HappyStk ans _) = 
         (happyReturn1 ans)

-----------------------------------------------------------------------------
-- Arrays only: do the next action


{-# LINE 155 "templates/GenericTemplate.hs" #-}

-----------------------------------------------------------------------------
-- HappyState data type (not arrays)



newtype HappyState b c = HappyState
        (Int ->                    -- token number
         Int ->                    -- token number (yes, again)
         b ->                           -- token semantic value
         HappyState b c ->              -- current state
         [HappyState b c] ->            -- state stack
         c)



-----------------------------------------------------------------------------
-- Shifting a token

happyShift new_state (1) tk st sts stk@(x `HappyStk` _) =
     let i = (case x of { HappyErrorToken (i) -> i }) in
--     trace "shifting the error token" $
     new_state i i tk (HappyState (new_state)) ((st):(sts)) (stk)

happyShift new_state i tk st sts stk =
     happyNewToken new_state ((st):(sts)) ((HappyTerminal (tk))`HappyStk`stk)

-- happyReduce is specialised for the common cases.

happySpecReduce_0 i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happySpecReduce_0 nt fn j tk st@((HappyState (action))) sts stk
     = action nt j tk st ((st):(sts)) (fn `HappyStk` stk)

happySpecReduce_1 i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happySpecReduce_1 nt fn j tk _ sts@(((st@(HappyState (action))):(_))) (v1`HappyStk`stk')
     = let r = fn v1 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_2 i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happySpecReduce_2 nt fn j tk _ ((_):(sts@(((st@(HappyState (action))):(_))))) (v1`HappyStk`v2`HappyStk`stk')
     = let r = fn v1 v2 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_3 i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happySpecReduce_3 nt fn j tk _ ((_):(((_):(sts@(((st@(HappyState (action))):(_))))))) (v1`HappyStk`v2`HappyStk`v3`HappyStk`stk')
     = let r = fn v1 v2 v3 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happyReduce k i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happyReduce k nt fn j tk st sts stk
     = case happyDrop (k - ((1) :: Int)) sts of
         sts1@(((st1@(HappyState (action))):(_))) ->
                let r = fn stk in  -- it doesn't hurt to always seq here...
                happyDoSeq r (action nt j tk st1 sts1 r)

happyMonadReduce k nt fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happyMonadReduce k nt fn j tk st sts stk =
      case happyDrop k ((st):(sts)) of
        sts1@(((st1@(HappyState (action))):(_))) ->
          let drop_stk = happyDropStk k stk in
          happyThen1 (fn stk tk) (\r -> action nt j tk st1 sts1 (r `HappyStk` drop_stk))

happyMonad2Reduce k nt fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happyMonad2Reduce k nt fn j tk st sts stk =
      case happyDrop k ((st):(sts)) of
        sts1@(((st1@(HappyState (action))):(_))) ->
         let drop_stk = happyDropStk k stk





             new_state = action

          in
          happyThen1 (fn stk tk) (\r -> happyNewToken new_state sts1 (r `HappyStk` drop_stk))

happyDrop (0) l = l
happyDrop n ((_):(t)) = happyDrop (n - ((1) :: Int)) t

happyDropStk (0) l = l
happyDropStk n (x `HappyStk` xs) = happyDropStk (n - ((1)::Int)) xs

-----------------------------------------------------------------------------
-- Moving to a new state after a reduction









happyGoto action j tk st = action j j tk (HappyState action)


-----------------------------------------------------------------------------
-- Error recovery ((1) is the error token)

-- parse error if we are in recovery and we fail again
happyFail (1) tk old_st _ stk@(x `HappyStk` _) =
     let i = (case x of { HappyErrorToken (i) -> i }) in
--      trace "failing" $ 
        happyError_ i tk

{-  We don't need state discarding for our restricted implementation of
    "error".  In fact, it can cause some bogus parses, so I've disabled it
    for now --SDM

-- discard a state
happyFail  (1) tk old_st (((HappyState (action))):(sts)) 
                                                (saved_tok `HappyStk` _ `HappyStk` stk) =
--      trace ("discarding state, depth " ++ show (length stk))  $
        action (1) (1) tk (HappyState (action)) sts ((saved_tok`HappyStk`stk))
-}

-- Enter error recovery: generate an error token,
--                       save the old token and carry on.
happyFail  i tk (HappyState (action)) sts stk =
--      trace "entering error recovery" $
        action (1) (1) tk (HappyState (action)) sts ( (HappyErrorToken (i)) `HappyStk` stk)

-- Internal happy errors:

notHappyAtAll :: a
notHappyAtAll = error "Internal Happy error\n"

-----------------------------------------------------------------------------
-- Hack to get the typechecker to accept our action functions







-----------------------------------------------------------------------------
-- Seq-ing.  If the --strict flag is given, then Happy emits 
--      happySeq = happyDoSeq
-- otherwise it emits
--      happySeq = happyDontSeq

happyDoSeq, happyDontSeq :: a -> b -> b
happyDoSeq   a b = a `seq` b
happyDontSeq a b = b

-----------------------------------------------------------------------------
-- Don't inline any functions from the template.  GHC has a nasty habit
-- of deciding to inline happyGoto everywhere, which increases the size of
-- the generated parser quite a bit.









{-# NOINLINE happyShift #-}
{-# NOINLINE happySpecReduce_0 #-}
{-# NOINLINE happySpecReduce_1 #-}
{-# NOINLINE happySpecReduce_2 #-}
{-# NOINLINE happySpecReduce_3 #-}
{-# NOINLINE happyReduce #-}
{-# NOINLINE happyMonadReduce #-}
{-# NOINLINE happyGoto #-}
{-# NOINLINE happyFail #-}

-- end of Happy Template.

