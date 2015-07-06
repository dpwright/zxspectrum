{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

{-| Useful locations from the original ZX Spectrum 48k ROM.

Taken from "The Complete Spectrum ROM Disassembly" -- http://dac.escet.urjc.es/~csanchez/pfcs/zxspectrum/CompleteSpectrumROMDisassemblyThe.pdf

In general, the names are kept the same where possible, with a few modifications
in order to conform to Haskell syntax.  The following systematic transformations
have been performed:

* @-@ is changed to @_@
* @/@ is changed to @_@
* @=@ is changed to @_@
* @\@@ is changed to @AT@
* @&@ is changed to @AND@
* @$@ is changed to @S@
* @*@ is changed to @x@
* @^@ is changed to @v@
* @?@ is changed to @_P@
* @.@ is changed to @_@ or removed if at the end of the label
* Parentheses are removed from labels.  The opening parenthesis is replaced with @_@
* Where the same label features twice, successive occurences are marked with primes @'@

Finally, @CH-ADD+1@ has been renamed 'CH_ADD_INC', since that reflects its
actual operation.

With the exception of the @-@ to @_@ change, all modified labels have their
original names included in the documentation.
-}

module ZXSpectrum.Rom48 where

import Z80.Operands

pattern START       = 0x0000 :: Location
pattern ERROR_1     = 0x0008 :: Location
pattern PRINT_A_1   = 0x0010 :: Location
pattern GET_CHAR    = 0x0018 :: Location
pattern TEST_CHAR   = 0x001C :: Location
pattern NEXT_CHAR   = 0x0020 :: Location
pattern FP_CALC     = 0x0028 :: Location
pattern BC_SPACES   = 0x0030 :: Location
pattern MASK_INT    = 0x0038 :: Location
pattern KEY_INT     = 0x0048 :: Location
pattern ERROR_2     = 0x0053 :: Location
pattern ERROR_3     = 0x0055 :: Location
pattern RESET       = 0x0066 :: Location
pattern NO_RESET    = 0x0070 :: Location

-- | This routine is called @CH-ADD+1@ in "The Complete ROM Disassembly".
-- It increments the address held in @CH-ADD@.
pattern CH_ADD_INC  = 0x0074 :: Location
pattern TEMP_PTR1   = 0x0077 :: Location
pattern TEMP_PTR2   = 0x0078 :: Location
pattern SKIP_OVER   = 0x007D :: Location
pattern SKIPS       = 0x0090 :: Location
pattern KEY_SCAN    = 0x028E :: Location
pattern KEY_LINE    = 0x0296 :: Location
pattern KEY_3KEYS   = 0x029F :: Location
pattern KEY_BITS    = 0x02A1 :: Location
pattern KEY_DONE    = 0x02AB :: Location
pattern KEYBOARD    = 0x02BF :: Location
pattern K_ST_LOOP   = 0x02C6 :: Location
pattern K_CH_SET    = 0x02D1 :: Location
pattern K_NEW       = 0x02F1 :: Location
pattern K_END       = 0x0308 :: Location
pattern K_REPEAT    = 0x0310 :: Location
pattern K_TEST      = 0x031E :: Location
pattern K_MAIN      = 0x032C :: Location
pattern K_DECODE    = 0x0333 :: Location
pattern K_E_LET     = 0x0341 :: Location
pattern K_LOOK_UP   = 0x034A :: Location
pattern K_KLC_LET   = 0x034F :: Location
pattern K_DIGIT     = 0x0367 :: Location

-- | This routine is called @K-8-&-9@ in "The Complete ROM Disassembly".
pattern K_8_AND_9   = 0x0382 :: Location

pattern K_GRA_DGT   = 0x0389 :: Location
pattern K_KLC_DGT   = 0x039D :: Location

-- | This routine is called @K-\@-CHAR@ in "The Complete ROM Disassembly".
pattern K_AT_CHAR   = 0x03B2 :: Location

pattern BEEPER      = 0x03B5 :: Location

-- | The original "Complete Spectrum ROM Disassembly" book defines labels for
-- three offsets before BE_IX, useful for timing purposes and labelled BE_IX+0,
-- BE_IX+1, etc.  Here, I just define BE_IX to the value of BE_IX+0 in the book,
-- since we can add the offsets ourselves in Haskell (though they will be negative
-- of course).
pattern BE_IX       = 0x03D4 :: Location

-- | This routine is called @BE-H&L-LP@ in "The Complete ROM Disassembly".
pattern BE_HANDL_LP = 0x03D6 :: Location

pattern BE_AGAIN    = 0x03F2 :: Location
pattern BE_END      = 0x03F6 :: Location
pattern BEEP        = 0x03F8 :: Location
pattern BE_I_OK     = 0x0425 :: Location
pattern BE_OCTAVE   = 0x0427 :: Location
pattern REPORT_B    = 0x046C :: Location
pattern SA_BYTES    = 0x04C2 :: Location
pattern SA_FLAG     = 0x04D0 :: Location
pattern SA_LEADER   = 0x04D8 :: Location
pattern SA_SYNC_1   = 0x04EA :: Location
pattern SA_SYNC_2   = 0x04F2 :: Location
pattern SA_LOOP     = 0x04FE :: Location
pattern SA_LOOP_P   = 0x0505 :: Location
pattern SA_START    = 0x0507 :: Location
pattern SA_PARITY   = 0x050E :: Location
pattern SA_BIT_2    = 0x0511 :: Location
pattern SA_BIT_1    = 0x0514 :: Location
pattern SA_SET      = 0x051A :: Location
pattern SA_OUT      = 0x051C :: Location
pattern SA_8_BITS   = 0x0525 :: Location
pattern SA_DELAY    = 0x053C :: Location

-- | This routine is called @SA/LD-RET@ in "The Complete Spectrum ROM Disassembly"
pattern SA_LD_RET   = 0x053F :: Location

pattern REPORT_D    = 0x0552 :: Location

-- | This routine is called @SA/LD-END@ in "The Complete Spectrum ROM Disassembly"
pattern SA_LD_END   = 0x0554 :: Location

pattern LD_BYTES    = 0x0556 :: Location
pattern LD_BREAK    = 0x056B :: Location
pattern LD_START    = 0x056C :: Location
pattern LD_WAIT     = 0x0574 :: Location
pattern LD_LEADER   = 0x0580 :: Location
pattern LD_SYNC     = 0x058F :: Location
pattern LD_LOOP     = 0x05A9 :: Location
pattern LD_FLAG     = 0x05B3 :: Location
pattern LD_VERIFY   = 0x05BD :: Location
pattern LD_NEXT     = 0x05C2 :: Location
pattern LD_DEC      = 0x05C4 :: Location
pattern LD_MARKER   = 0x05C8 :: Location
pattern LD_8_BITS   = 0x05CA :: Location
pattern LD_EDGE_2   = 0x05E3 :: Location
pattern LD_EDGE_1   = 0x05E7 :: Location
pattern LD_DELAY    = 0x05E9 :: Location
pattern LD_SAMPLE   = 0x05ED :: Location
pattern SAVE_ETC    = 0x0605 :: Location
pattern SA_SPACE    = 0x0621 :: Location
pattern SA_BLANK    = 0x0629 :: Location
pattern REPORT_F    = 0x0642 :: Location
pattern SA_NULL     = 0x0644 :: Location
pattern SA_NAME     = 0x064B :: Location
pattern SA_DATA     = 0x0652 :: Location
pattern REPORT_2    = 0x0670 :: Location
pattern SA_V_OLD    = 0x0672 :: Location
pattern SA_V_NEW    = 0x0685 :: Location
pattern SA_V_TYPE   = 0x068F :: Location
pattern SA_DATA_1   = 0x0692 :: Location

-- | This routine is called @SA-SCR$@ in "The Complete Spectrum ROM Disassembly"
pattern SA_SCRS     = 0x06A0 :: Location

pattern SA_CODE     = 0x06C3 :: Location
pattern SA_CODE_1   = 0x06E1 :: Location
pattern SA_CODE_2   = 0x06F0 :: Location
pattern SA_CODE_3   = 0x06F5 :: Location
pattern SA_CODE_4   = 0x06F9 :: Location
pattern SA_TYPE_3   = 0x0710 :: Location
pattern SA_LINE     = 0x0716 :: Location
pattern SA_LINE_1   = 0x0723 :: Location
pattern SA_TYPE_0   = 0x073A :: Location
pattern SA_ALL      = 0x075A :: Location
pattern LD_LOOK_H   = 0x0767 :: Location
pattern LD_TYPE     = 0x078A :: Location
pattern LD_NAME     = 0x07A6 :: Location
pattern LD_CH_PR    = 0x07AD :: Location
pattern VR_CONTRL   = 0x07CB :: Location
pattern VR_CONT_1   = 0x07E9 :: Location
pattern VR_CONT_2   = 0x07F4 :: Location
pattern VR_CONT_3   = 0x0800 :: Location
pattern LD_BLOCK    = 0x0802 :: Location
pattern REPORT_R    = 0x0806 :: Location
pattern LD_CONTRL   = 0x0808 :: Location
pattern LD_CONT_1   = 0x0819 :: Location
pattern LD_CONT_2   = 0x0825 :: Location
pattern LD_DATA     = 0x082E :: Location
pattern LD_DATA_1   = 0x084C :: Location
pattern LD_PROG     = 0x0873 :: Location
pattern LD_PROG_1   = 0x08AD :: Location
pattern ME_CONTRL   = 0x08B6 :: Location
pattern ME_NEW_LP   = 0x08D2 :: Location
pattern ME_OLD_LP   = 0x08D7 :: Location
pattern ME_OLD_L1   = 0x08DF :: Location
pattern ME_NEW_L2   = 0x08EB :: Location
pattern ME_VAR_LP   = 0x08F0 :: Location
pattern ME_OLD_VP   = 0x08F9 :: Location
pattern ME_OLD_V1   = 0x0901 :: Location
pattern ME_OLD_V2   = 0x0909 :: Location
pattern ME_OLD_V3   = 0x0912 :: Location
pattern ME_OLD_V4   = 0x091E :: Location
pattern ME_VAR_L1   = 0x0921 :: Location
pattern ME_VAR_L2   = 0x0923 :: Location
pattern ME_ENTER    = 0x092C :: Location
pattern ME_ENT_1    = 0x093E :: Location
pattern ME_ENT_2    = 0x0955 :: Location
pattern ME_ENT_3    = 0x0958 :: Location
pattern SA_CONTRL   = 0x0970 :: Location
pattern SA_1_SEC    = 0x0991 :: Location
pattern PRINT_OUT   = 0x09F4 :: Location
pattern PO_BACK_1   = 0x0A23 :: Location
pattern PO_BACK_2   = 0x0A38 :: Location
pattern PO_BACK_3   = 0x0A3A :: Location
pattern PO_RIGHT    = 0x0A3D :: Location
pattern PO_ENTER    = 0x0A4F :: Location
pattern PO_COMMA    = 0x0A5F :: Location
pattern PO_QUEST    = 0x0A69 :: Location
pattern PO_TV_2     = 0x0A6D :: Location
pattern PO_2_OPER   = 0x0A75 :: Location
pattern PO_1_OPER   = 0x0A7A :: Location
pattern PO_TV_1     = 0x0A7D :: Location
pattern PO_CONT     = 0x0A87 :: Location
pattern PO_AT_ERR   = 0x0AAC :: Location
pattern PO_AT_SET   = 0x0ABF :: Location
pattern PO_TAB      = 0x0AC2 :: Location
pattern PO_FILL     = 0x0AC3 :: Location
pattern PO_SPACE    = 0x0AD0 :: Location
pattern PO_STORE    = 0x0ADC :: Location
pattern PO_ST_E     = 0x0AF0 :: Location
pattern PO_ST_PR    = 0x0AFC :: Location
pattern PO_FETCH    = 0x0B03 :: Location
pattern PO_F_PR     = 0x0B1D :: Location
pattern PO_ANY      = 0x0B24 :: Location
pattern PO_GR_1     = 0x0B38 :: Location
pattern PO_GR_2     = 0x0B3E :: Location
pattern PO_GR_3     = 0x0B4C :: Location

-- | This routine is called @PO-T&UDG@ in "The Complete ROM Disassembly".
pattern PO_TANDUDG  = 0x0B52 :: Location

pattern PO_T        = 0x0B5F :: Location
pattern PO_CHAR     = 0x0B65 :: Location
pattern PO_CHAR_2   = 0x0B6A :: Location
pattern PO_CHAR_3   = 0x0B76 :: Location
pattern PR_ALL      = 0x0B7F :: Location
pattern PR_ALL_1    = 0x0B93 :: Location
pattern PR_ALL_2    = 0x0BA4 :: Location
pattern PR_ALL_3    = 0x0BB6 :: Location
pattern PR_ALL_4    = 0x0BB7 :: Location
pattern PR_ALL_5    = 0x0BC1 :: Location
pattern PO_ATTR     = 0x0BDB :: Location
pattern PO_ATTR_1   = 0x0BFA :: Location
pattern PO_ATTR_2   = 0x0C08 :: Location
pattern PO_MSG      = 0x0C0A :: Location
pattern PO_TOKENS   = 0x0C10 :: Location
pattern PO_TABLE    = 0x0C14 :: Location
pattern PO_EACH     = 0x0C22 :: Location
pattern PO_TR_SP    = 0x0C35 :: Location
pattern PO_SAVE     = 0x0C3B :: Location
pattern PO_SEARCH   = 0x0C41 :: Location
pattern PO_STEP     = 0x0C44 :: Location
pattern REPORT_5    = 0x0C86 :: Location
pattern PO_SCR_2    = 0x0C88 :: Location
pattern PO_SCR_3    = 0x0CD2 :: Location
pattern PO_SCR_3A   = 0x0CF0 :: Location
pattern REPORT_D'   = 0x0D00 :: Location
pattern PO_SCR_4    = 0x0D02 :: Location
pattern PO_SCR_4A   = 0x0D1C :: Location
pattern PO_SCR_4B   = 0x0D2D :: Location
pattern TEMPS       = 0x0D4D :: Location
pattern TEMPS_1     = 0x0D5B :: Location
pattern TEMPS_2     = 0x0D65 :: Location
pattern CLS_LOWER   = 0x0D6E :: Location
pattern CLS_1       = 0x0D87 :: Location
pattern CLS_2       = 0x0D89 :: Location
pattern CLS_3       = 0x0D8E :: Location
pattern CL_CHAN     = 0x0D94 :: Location
pattern CL_CHAN_A   = 0x0DA0 :: Location
pattern CL_ALL      = 0x0DAF :: Location
pattern CL_SET      = 0x0DD9 :: Location
pattern CL_SET_1    = 0x0DEE :: Location
pattern CL_SET_2    = 0x0DF4 :: Location
pattern CL_SC_ALL   = 0x0DFE :: Location
pattern CL_SCROLL   = 0x0E00 :: Location
pattern CL_SCR_1    = 0x0E05 :: Location
pattern CL_SCR_3    = 0x0E19 :: Location
pattern CL_LINE     = 0x0E44 :: Location
pattern CL_LINE_1   = 0x0E4A :: Location
pattern CL_LINE_2   = 0x0E4D :: Location
pattern CL_LINE_3   = 0x0E80 :: Location
pattern CL_ATTR     = 0x0E88 :: Location
pattern CL_ADDR     = 0x0E9B :: Location
pattern COPY        = 0x0EAC :: Location
pattern COPY_1      = 0x0EB2 :: Location
pattern COPY_2      = 0x0EC9 :: Location
pattern COPY_BUFF   = 0x0ECD :: Location
pattern COPY_3      = 0x0ED3 :: Location
pattern COPY_END    = 0x0EDA :: Location
pattern CLEAR_PRB   = 0x0EDF :: Location
pattern PRB_BYTES   = 0x0EE7 :: Location
pattern COPY_LINE   = 0x0EF4 :: Location
pattern COPY_L_1    = 0x0EFD :: Location
pattern COPY_L_2    = 0x0F0C :: Location
pattern COPY_L_3    = 0x0F14 :: Location
pattern COPY_L_4    = 0x0F18 :: Location
pattern COPY_L_5    = 0x0F1E :: Location
pattern EDITOR      = 0x0F2C :: Location
pattern ED_AGAIN    = 0x0F30 :: Location
pattern ED_LOOP     = 0x0F38 :: Location
pattern ED_CONTR    = 0x0F6C :: Location
pattern ADD_CHAR    = 0x0F81 :: Location
pattern ADD_CH_1    = 0x0F8B :: Location
pattern ED_KEYS     = 0x0F92 :: Location
pattern ED_EDIT     = 0x0FA9 :: Location
pattern ED_DOWN     = 0x0FF3 :: Location
pattern ED_STOP     = 0x1001 :: Location
pattern ED_LEFT     = 0x1007 :: Location
pattern ED_RIGHT    = 0x100C :: Location
pattern ED_CUR      = 0x1011 :: Location
pattern ED_DELETE   = 0x1015 :: Location
pattern ED_IGNORE   = 0x101E :: Location
pattern ED_ENTER    = 0x1024 :: Location
pattern ED_END      = 0x1026 :: Location
pattern ED_EDGE     = 0x1031 :: Location
pattern ED_EDGE_1   = 0x103E :: Location
pattern ED_EDGE_2   = 0x1051 :: Location
pattern ED_UP       = 0x1059 :: Location
pattern ED_LIST     = 0x106E :: Location
pattern ED_SYMBOL   = 0x1076 :: Location
pattern ED_GRAPH    = 0x107C :: Location
pattern ED_ERROR    = 0x107F :: Location
pattern CLEAR_SP    = 0x1097 :: Location
pattern KEY_INPUT   = 0x10A8 :: Location

-- | This routine is called @KEY-M&CL@ in "The Complete ROM Disassembly".
pattern KEY_MANDCL  = 0x10DB :: Location

pattern KEY_MODE    = 0x10E6 :: Location
pattern KEY_FLAG    = 0x10F4 :: Location
pattern KEY_CONTR   = 0x10FA :: Location
pattern KEY_DATA    = 0x1105 :: Location
pattern KEY_NEXT    = 0x110D :: Location
pattern KEY_CHAN    = 0x1113 :: Location
pattern KEY_DONE'   = 0x111B :: Location
pattern ED_COPY     = 0x111D :: Location
pattern ED_BLANK    = 0x1150 :: Location
pattern ED_SPACES   = 0x115E :: Location
pattern ED_FULL     = 0x1167 :: Location
pattern ED_C_DONE   = 0x117C :: Location
pattern ED_C_END    = 0x117E :: Location
pattern SET_HL      = 0x1190 :: Location
pattern SET_DE      = 0x1195 :: Location
pattern REMOVE_FP   = 0x11A7 :: Location
pattern NEW         = 0x11B7 :: Location

-- | This routine is called @START/NEW@ in "The Complete Spectrum ROM Disassembly"
pattern START_NEW   = 0x11CB :: Location

pattern RAM_CHECK   = 0x11DA :: Location
pattern RAM_FILL    = 0x11DC :: Location
pattern RAM_READ    = 0x11E2 :: Location
pattern RAM_DONE    = 0x11EF :: Location
pattern RAM_SET     = 0x1219 :: Location
pattern MAIN_EXEC   = 0x12A2 :: Location
pattern MAIN_1      = 0x12A9 :: Location
pattern MAIN_2      = 0x12AC :: Location
pattern MAIN_3      = 0x12CF :: Location
pattern MAIN_4      = 0x1303 :: Location
pattern MAIN_G      = 0x1313 :: Location
pattern MAIN_5      = 0x133C :: Location
pattern MAIN_6      = 0x1373 :: Location
pattern MAIN_7      = 0x1376 :: Location
pattern MAIN_8      = 0x1384 :: Location
pattern MAIN_9      = 0x1386 :: Location
pattern Report0     = 0x1392 :: Location
pattern Report1     = 0x1394 :: Location
pattern Report2     = 0x13A4 :: Location
pattern Report3     = 0x13B6 :: Location
pattern Report4     = 0x13C6 :: Location
pattern Report5     = 0x13D2 :: Location
pattern Report6     = 0x13DF :: Location
pattern Report7     = 0x13ED :: Location
pattern Report8     = 0x1401 :: Location
pattern Report9     = 0x140C :: Location
pattern ReportA     = 0x141A :: Location
pattern ReportB     = 0x142A :: Location
pattern ReportC     = 0x143E :: Location
pattern ReportD     = 0x144F :: Location
pattern ReportE     = 0x1463 :: Location
pattern ReportF     = 0x146E :: Location
pattern ReportG     = 0x147F :: Location
pattern ReportH     = 0x148F :: Location
pattern ReportI     = 0x149C :: Location
pattern ReportJ     = 0x14AC :: Location
pattern ReportK     = 0x14BE :: Location
pattern ReportL     = 0x14CC :: Location
pattern ReportM     = 0x14DE :: Location
pattern ReportN     = 0x14EC :: Location
pattern ReportO     = 0x14FA :: Location
pattern ReportP     = 0x1508 :: Location
pattern ReportQ     = 0x1516 :: Location
pattern ReportR     = 0x1525 :: Location
pattern REPORT_G    = 0x1555 :: Location
pattern MAIN_ADD    = 0x155D :: Location
pattern MAIN_ADD1   = 0x157D :: Location
pattern MAIN_ADD2   = 0x15AB :: Location
pattern REPORT_J    = 0x15C4 :: Location
pattern WAIT_KEY    = 0x15D4 :: Location
pattern WAIT_KEY1   = 0x15DE :: Location
pattern REPORT_8    = 0x15E4 :: Location
pattern INPUT_AD    = 0x15E6 :: Location
pattern OUT_CODE    = 0x15EF :: Location
pattern PRINT_A_2   = 0x15F2 :: Location
pattern CALL_SUB    = 0x15F7 :: Location
pattern CHAN_OPEN   = 0x1601 :: Location
pattern REPORT_O    = 0x160E :: Location
pattern CHAN_OP_1   = 0x1610 :: Location
pattern CHAN_FLAG   = 0x1615 :: Location
pattern CALL_JUMP   = 0x162C :: Location
pattern CHAN_K      = 0x1634 :: Location
pattern CHAN_S      = 0x1642 :: Location
pattern CHAN_S_1    = 0x1646 :: Location
pattern CHAN_P      = 0x164D :: Location
pattern ONE_SPACE   = 0x1652 :: Location
pattern MAKE_ROOM   = 0x1655 :: Location
pattern POINTERS    = 0x1664 :: Location
pattern PTR_NEXT    = 0x166B :: Location
pattern PTR_DONE    = 0x167F :: Location
pattern LINE_ZERO   = 0x168F :: Location
pattern LINE_NO_A   = 0x1691 :: Location
pattern LINE_NO     = 0x1695 :: Location
pattern RESERVE     = 0x169E :: Location
pattern SET_MIN     = 0x16B0 :: Location
pattern SET_WORK    = 0x16BF :: Location
pattern SET_STK     = 0x16C5 :: Location
pattern REC_EDIT    = 0x16D4 :: Location
pattern INDEXER_1   = 0x16DB :: Location
pattern INDEXER     = 0x16DC :: Location
pattern CLOSE       = 0x16E5 :: Location
pattern CLOSE_1     = 0x16FC :: Location
pattern CLOSE_2     = 0x1701 :: Location
pattern CLOSE_STR   = 0x171C :: Location
pattern STR_DATA    = 0x171E :: Location
pattern REPORT_O'   = 0x1725 :: Location
pattern STR_DATA1   = 0x1727 :: Location
pattern OPEN        = 0x1736 :: Location
pattern OPEN_1      = 0x1756 :: Location
pattern OPEN_2      = 0x175D :: Location
pattern REPORT_F'   = 0x1765 :: Location
pattern OPEN_3      = 0x1767 :: Location
pattern OPEN_K      = 0x1781 :: Location
pattern OPEN_S      = 0x1785 :: Location
pattern OPEN_P      = 0x1789 :: Location
pattern OPEN_END    = 0x178B :: Location

-- | This routine is called @CAT-ETC.@ in "The Complete Spectrum ROM Disassembly"
pattern CAT_ETC     = 0x1793 :: Location

pattern AUTO_LIST   = 0x1795 :: Location
pattern AUTO_L_1    = 0x17CE :: Location
pattern AUTO_L_2    = 0x17E1 :: Location
pattern AUTO_L_3    = 0x17E4 :: Location
pattern AUTO_L_4    = 0x17ED :: Location
pattern LLIST       = 0x17F5 :: Location
pattern LIST        = 0x17F9 :: Location
pattern LIST_1      = 0x17FB :: Location
pattern LIST_2      = 0x1814 :: Location
pattern LIST_3      = 0x181A :: Location
pattern LIST_4      = 0x181F :: Location
pattern LIST_5      = 0x1822 :: Location
pattern LIST_ALL    = 0x1833 :: Location
pattern LIST_ALL_1  = 0x1835 :: Location
pattern OUT_LINE    = 0x1855 :: Location
pattern OUT_LINE1   = 0x1865 :: Location
pattern OUT_LINE2   = 0x187D :: Location
pattern OUT_LINE3   = 0x1881 :: Location
pattern OUT_LINE4   = 0x1894 :: Location
pattern OUT_LINE5   = 0x18A1 :: Location
pattern OUT_LINE6   = 0x18B4 :: Location
pattern NUMBER      = 0x18B6 :: Location
pattern OUT_FLASH   = 0x18C1 :: Location
pattern OUT_CURS    = 0x18E1 :: Location
pattern OUT_C_1     = 0x18F3 :: Location
pattern OUT_C_2     = 0x1909 :: Location
pattern LN_FETCH    = 0x190F :: Location
pattern LN_STORE    = 0x191C :: Location
pattern OUT_SP_2    = 0x1925 :: Location
pattern OUT_SP_NO   = 0x192A :: Location
pattern OUT_SP_1    = 0x192B :: Location
pattern OUT_CHAR    = 0x1937 :: Location
pattern OUT_CH_2    = 0x1968 :: Location
pattern OUT_CH_3    = 0x196C :: Location
pattern LINE_ADDR   = 0x196E :: Location
pattern LINE_AD_1   = 0x1974 :: Location
pattern CP_LINES    = 0x1980 :: Location
pattern EACH_STMT   = 0x198B :: Location
pattern EACH_S_1    = 0x1990 :: Location
pattern EACH_S_2    = 0x1998 :: Location
pattern EACH_S_3    = 0x199A :: Location
pattern EACH_S_4    = 0x19A5 :: Location
pattern EACH_S_5    = 0x19AD :: Location
pattern EACH_S_6    = 0x19B1 :: Location
pattern NEXT_ONE    = 0x19B8 :: Location
pattern NEXT_O_1    = 0x19C7 :: Location
pattern NEXT_O_2    = 0x19CE :: Location
pattern NEXT_O_3    = 0x19D5 :: Location
pattern NEXT_O_4    = 0x19D6 :: Location
pattern NEXT_O_5    = 0x19DB :: Location
pattern DIFFER      = 0x19DD :: Location
pattern RECLAIM_1   = 0x19E5 :: Location
pattern RECLAIM_2   = 0x19E8 :: Location
pattern E_LINE_NO   = 0x19FB :: Location
pattern E_L_1       = 0x1A15 :: Location
pattern OUT_NUM_1   = 0x1A1B :: Location
pattern OUT_NUM_2   = 0x1A28 :: Location
pattern OUT_NUM_3   = 0x1A30 :: Location
pattern OUT_NUM_4   = 0x1A42 :: Location
pattern P_LET       = 0x1A7A :: Location
pattern P_GO_TO     = 0x1A7D :: Location
pattern P_IF        = 0x1A81 :: Location
pattern P_GO_SUB    = 0x1A86 :: Location
pattern P_STOP      = 0x1A8A :: Location
pattern P_RETURN    = 0x1A8D :: Location
pattern P_FOR       = 0x1A90 :: Location
pattern P_PRINT     = 0x1A9C :: Location
pattern P_INPUT     = 0x1A9F :: Location
pattern P_DIM       = 0x1AA2 :: Location
pattern P_REM       = 0x1AA5 :: Location
pattern P_NEW       = 0x1AA8 :: Location
pattern P_RUN       = 0x1AAB :: Location
pattern P_LIST      = 0x1AAE :: Location
pattern P_POKE      = 0x1AB1 :: Location
pattern P_RANDOM    = 0x1AB5 :: Location
pattern P_CONT      = 0x1AB8 :: Location
pattern P_CLEAR     = 0x1ABB :: Location
pattern P_CLS       = 0x1ABE :: Location
pattern P_PLOT      = 0x1AC1 :: Location
pattern P_PAUSE     = 0x1AC5 :: Location
pattern P_READ      = 0x1AC9 :: Location
pattern P_DATA      = 0x1ACC :: Location
pattern P_RESTORE   = 0x1ACF :: Location
pattern P_DRAW      = 0x1AD2 :: Location
pattern P_COPY      = 0x1AD6 :: Location
pattern P_LPRINT    = 0x1AD9 :: Location
pattern P_LLIST     = 0x1ADC :: Location
pattern P_SAVE      = 0x1ADF :: Location
pattern P_LOAD      = 0x1AE0 :: Location
pattern P_VERIFY    = 0x1AE1 :: Location
pattern P_MERGE     = 0x1AE2 :: Location
pattern P_BEEP      = 0x1AE3 :: Location
pattern P_CIRCLE    = 0x1AE7 :: Location
pattern P_INK       = 0x1AEB :: Location
pattern P_PAPER     = 0x1AEC :: Location
pattern P_FLASH     = 0x1AED :: Location
pattern P_BRIGHT    = 0x1AEE :: Location
pattern P_OVER      = 0x1AF0 :: Location
pattern P_OUT       = 0x1AF1 :: Location
pattern P_BORDER    = 0x1AF5 :: Location
pattern P_DEF_FN    = 0x1AF9 :: Location
pattern P_OPEN      = 0x1AFC :: Location
pattern P_CLOSE     = 0x1B02 :: Location
pattern P_FORMAT    = 0x1B06 :: Location
pattern P_MOVE      = 0x1B0A :: Location
pattern P_ERASE     = 0x1B10 :: Location
pattern P_CAT       = 0x1B14 :: Location
pattern LINE_SCAN   = 0x1B17 :: Location
pattern STMT_LOOP   = 0x1B28 :: Location
pattern STMT_L_1    = 0x1B29 :: Location
pattern SCAN_LOOP   = 0x1B52 :: Location
pattern GET_PARAM   = 0x1B55 :: Location
pattern SEPARATOR   = 0x1B6F :: Location
pattern STMT_RET    = 0x1B76 :: Location
pattern REPORT_L    = 0x1B7B :: Location
pattern STMT_R_1    = 0x1B7D :: Location
pattern LINE_RUN    = 0x1B8A :: Location
pattern LINE_NEW    = 0x1B9E :: Location
pattern REPORT_0    = 0x1BB0 :: Location
pattern REM         = 0x1BB2 :: Location
pattern LINE_END    = 0x1BB3 :: Location
pattern LINE_USE    = 0x1BBF :: Location
pattern NEXT_LINE   = 0x1BD1 :: Location
pattern REPORT_N    = 0x1BEC :: Location
pattern CHECK_END   = 0x1BEE :: Location
pattern STMT_NEXT   = 0x1BF4 :: Location
pattern CLASS_03    = 0x1C0D :: Location
pattern CLASS_00    = 0x1C10 :: Location
pattern CLASS_05    = 0x1C11 :: Location
pattern JUMP_C_R    = 0x1C16 :: Location
pattern CLASS_01    = 0x1C1F :: Location
pattern VAR_A_1     = 0x1C22 :: Location
pattern REPORT_2'   = 0x1C2E :: Location
pattern VAR_A_2     = 0x1C30 :: Location
pattern VAR_A_3     = 0x1C46 :: Location
pattern CLASS_02    = 0x1C4E :: Location
pattern VAL_FET_1   = 0x1C56 :: Location
pattern VAL_FET_2   = 0x1C59 :: Location
pattern CLASS_04    = 0x1C6C :: Location
pattern NEXT_2NUM   = 0x1C79 :: Location
pattern EXPT_2NUM   = 0x1C7A :: Location
pattern EXPT_1NUM   = 0x1C82 :: Location
pattern REPORT_C    = 0x1C8A :: Location
pattern EXPT_EXP    = 0x1C8C :: Location
pattern PERMS       = 0x1C96 :: Location
pattern CLASS_09    = 0x1CBE :: Location
pattern CL_09_1     = 0x1CD6 :: Location
pattern CLASS_0B    = 0x1CDB :: Location
pattern FETCH_NUM   = 0x1CDE :: Location
pattern USE_ZERO    = 0x1CE6 :: Location
pattern STOP        = 0x1CEE :: Location
pattern IF          = 0x1CF0 :: Location
pattern IF_1        = 0x1D00 :: Location
pattern FOR         = 0x1D03 :: Location
pattern F_USE_1     = 0x1D10 :: Location
pattern F_REORDER   = 0x1D16 :: Location

-- | This routine is called @F-L&S@ in "The Complete ROM Disassembly".
pattern F_LANDS     = 0x1D34 :: Location

pattern F_LOOP      = 0x1D64 :: Location
pattern F_FOUND     = 0x1D7C :: Location
pattern REPORT_I    = 0x1D84 :: Location
pattern LOOK_PROG   = 0x1D86 :: Location
pattern LOOK_P_1    = 0x1D8B :: Location
pattern LOOK_P_2    = 0x1DA3 :: Location
pattern NEXT        = 0x1DAB :: Location
pattern REPORT_1    = 0x1DD8 :: Location
pattern NEXT_LOOP   = 0x1DDA :: Location
pattern NEXT_1      = 0x1DE2 :: Location
pattern NEXT_2      = 0x1DE9 :: Location
pattern READ_3      = 0x1DEC :: Location
pattern READ        = 0x1DED :: Location
pattern REPORT_E    = 0x1E08 :: Location
pattern READ_1      = 0x1E0A :: Location
pattern READ_2      = 0x1E1E :: Location
pattern DATA        = 0x1E27 :: Location
pattern DATA_1      = 0x1E2C :: Location
pattern DATA_2      = 0x1E37 :: Location
pattern PASS_BY     = 0x1E39 :: Location
pattern RESTORE     = 0x1E42 :: Location
pattern REST_RUN    = 0x1E45 :: Location
pattern RANDOMIZE   = 0x1E4F :: Location
pattern RAND_1      = 0x1E5A :: Location
pattern CONTINUE    = 0x1E5F :: Location
pattern GO_TO       = 0x1E67 :: Location
pattern GO_TO_2     = 0x1E73 :: Location
pattern OUT         = 0x1E7A :: Location
pattern POKE        = 0x1E80 :: Location
pattern TWO_PARAM   = 0x1E85 :: Location
pattern TWO_P_1     = 0x1E8E :: Location
pattern FIND_INT1   = 0x1E94 :: Location
pattern FIND_INT2   = 0x1E99 :: Location
pattern FIND_I_1    = 0x1E9C :: Location
pattern REPORT_B'   = 0x1E9F :: Location
pattern RUN         = 0x1EA1 :: Location
pattern CLEAR       = 0x1EAC :: Location
pattern CLEAR_RUN   = 0x1EAF :: Location
pattern CLEAR_1     = 0x1EB7 :: Location
pattern REPORT_M    = 0x1EDA :: Location
pattern CLEAR_2     = 0x1EDC :: Location
pattern GO_SUB      = 0x1EED :: Location
pattern TEST_ROOM   = 0x1F05 :: Location
pattern REPORT_4    = 0x1F15 :: Location
pattern FREE_MEM    = 0x1F1A :: Location
pattern RETURN      = 0x1F23 :: Location
pattern REPORT_7    = 0x1F36 :: Location
pattern PAUSE       = 0x1F3A :: Location
pattern PAUSE_1     = 0x1F3D :: Location
pattern PAUSE_2     = 0x1F49 :: Location
pattern PAUSE_END   = 0x1F4F :: Location
pattern BREAK_KEY   = 0x1F54 :: Location
pattern DEF_FN      = 0x1F60 :: Location
pattern DEF_FN_1    = 0x1F6A :: Location
pattern DEF_FN_2    = 0x1F7D :: Location
pattern DEF_FN_3    = 0x1F86 :: Location
pattern DEF_FN_4    = 0x1F89 :: Location
pattern DEF_FN_5    = 0x1F94 :: Location
pattern DEF_FN_6    = 0x1FA6 :: Location
pattern DEF_FN_7    = 0x1FBD :: Location
pattern UNSTACK_Z   = 0x1FC3 :: Location
pattern LPRINT      = 0x1FC9 :: Location
pattern PRINT       = 0x1FCD :: Location
pattern PRINT_1     = 0x1FCF :: Location
pattern PRINT_2     = 0x1FDF :: Location
pattern PRINT_3     = 0x1FE5 :: Location
pattern PRINT_4     = 0x1FF2 :: Location
pattern PRINT_CR    = 0x1FF5 :: Location
pattern PR_ITEM_1   = 0x1FFC :: Location
pattern PR_ITEM_2   = 0x200E :: Location
pattern PR_AT_TAB   = 0x201E :: Location
pattern PR_ITEM_3   = 0x2024 :: Location
pattern PR_STRING   = 0x203C :: Location
pattern PR_END_Z    = 0x2045 :: Location
pattern PR_ST_END   = 0x2048 :: Location
pattern PR_POSN_1   = 0x204E :: Location
pattern PR_POSN_2   = 0x2061 :: Location
pattern PR_POSN_3   = 0x2067 :: Location
pattern PR_POSN_4   = 0x206E :: Location
pattern STR_ALTER   = 0x2070 :: Location
pattern INPUT       = 0x2089 :: Location
pattern INPUT_1     = 0x2096 :: Location
pattern INPUT_2     = 0x20AD :: Location
pattern IN_ITEM_1   = 0x20C1 :: Location
pattern IN_ITEM_2   = 0x20D8 :: Location
pattern IN_ITEM_3   = 0x20ED :: Location
pattern IN_PROMPT   = 0x20FA :: Location
pattern IN_PR_1     = 0x211A :: Location
pattern IN_PR_2     = 0x211C :: Location
pattern IN_PR_3     = 0x2129 :: Location
pattern IN_VAR_1    = 0x213A :: Location
pattern IN_VAR_2    = 0x2148 :: Location
pattern IN_VAR_3    = 0x215E :: Location
pattern IN_VAR_4    = 0x2161 :: Location
pattern IN_VAR_5    = 0x2174 :: Location
pattern IN_VAR_6    = 0x219B :: Location
pattern IN_NEXT_1   = 0x21AF :: Location
pattern IN_NEXT_2   = 0x21B2 :: Location
pattern IN_ASSIGN   = 0x21B9 :: Location
pattern REPORT_C'   = 0x21CE :: Location
pattern IN_STOP     = 0x21D0 :: Location
pattern REPORT_H    = 0x21D4 :: Location
pattern IN_CHAN_K   = 0x21D6 :: Location
pattern CO_TEMP_1   = 0x21E1 :: Location
pattern CO_TEMP_2   = 0x21E2 :: Location
pattern CO_TEMP_3   = 0x21F2 :: Location
pattern CO_TEMP_4   = 0x21FC :: Location
pattern CO_TEMP_5   = 0x2211 :: Location
pattern CO_TEMP_6   = 0x2228 :: Location
pattern CO_TEMP_7   = 0x2234 :: Location
pattern CO_TEMP_8   = 0x223E :: Location
pattern REPORT_K    = 0x2244 :: Location
pattern CO_TEMP_9   = 0x2246 :: Location
pattern CO_TEMP_A   = 0x2257 :: Location
pattern CO_TEMP_B   = 0x2258 :: Location
pattern CO_CHANGE   = 0x226C :: Location
pattern CO_TEMP_D   = 0x227D :: Location
pattern CO_TEMP_E   = 0x2287 :: Location
pattern BORDER      = 0x2294 :: Location

-- | BORDERFAST isn't documented in "The Complete Spectrum ROM Disassembly", but is
-- commonly used.  It skips the initial check that input values are in range, making
-- setting the border a little bit faster.
pattern BORDERFAST  = 0x229b :: Location

pattern BORDER_1    = 0x22A6 :: Location
pattern PIXEL_ADD   = 0x22AA :: Location
pattern POINT_SUB   = 0x22CB :: Location
pattern POINT_LP    = 0x22D4 :: Location
pattern PLOT        = 0x22DC :: Location
pattern PLOT_SUB    = 0x22E5 :: Location
pattern PLOT_LOOP   = 0x22F0 :: Location
pattern PL_TST_IN   = 0x22FD :: Location
pattern PLOT_END    = 0x2303 :: Location
pattern STK_TO_BC   = 0x2307 :: Location
pattern STK_TO_A    = 0x2314 :: Location
pattern CIRCLE      = 0x2320 :: Location
pattern C_R_GRE_1   = 0x233B :: Location
pattern C_ARC_GE1   = 0x2347 :: Location
pattern DRAW        = 0x2382 :: Location
pattern DR_3_PRMS   = 0x238D :: Location
pattern DR_SIN_NZ   = 0x23A3 :: Location
pattern DR_PRMS     = 0x23C1 :: Location
pattern DRW_STEPS   = 0x2420 :: Location
pattern ARC_LOOP    = 0x2425 :: Location
pattern ARC_START   = 0x2439 :: Location
pattern ARC_END     = 0x245F :: Location
pattern LINE_DRAW   = 0x2477 :: Location
pattern CD_PRMS1    = 0x247D :: Location
pattern USE_252     = 0x2495 :: Location
pattern DRAW_SAVE   = 0x2497 :: Location
pattern DRAW_LINE   = 0x24B7 :: Location
pattern DL_X_GE_Y   = 0x24C4 :: Location
pattern DL_LARGER   = 0x24CB :: Location
pattern D_L_LOOP    = 0x24CE :: Location
pattern D_L_DIAG    = 0x24D4 :: Location
pattern D_L_HR_VT   = 0x24DB :: Location
pattern D_L_STEP    = 0x24DF :: Location
pattern D_L_PLOT    = 0x24EC :: Location
pattern D_L_RANGE   = 0x24F7 :: Location
pattern REPORT_B''  = 0x24F9 :: Location
pattern SCANNING    = 0x24FB :: Location
pattern S_LOOP_1    = 0x24FF :: Location
pattern S_QUOTE_S   = 0x250F :: Location
pattern S_2_COORD   = 0x2522 :: Location
pattern S_RPORT_C   = 0x252D :: Location
pattern SYNTAX_Z    = 0x2530 :: Location

-- | This routine is called @S-SCRN$-S@ in "The Complete Spectrum ROM Disassembly"
pattern S_SCRNS_S   = 0x2535 :: Location

pattern S_SCRN_LP   = 0x254F :: Location
pattern S_SC_MTCH   = 0x255A :: Location
pattern S_SC_ROWS   = 0x255D :: Location
pattern S_SCR_NXT   = 0x2573 :: Location
pattern S_SCR_STO   = 0x257D :: Location
pattern S_ATTR_S    = 0x2580 :: Location
pattern S_U_PLUS    = 0x25AF :: Location
pattern S_QUOTE     = 0x25B3 :: Location
pattern S_Q_AGAIN   = 0x25BE :: Location
pattern S_Q_COPY    = 0x25CB :: Location
pattern S_Q_PRMS    = 0x25D9 :: Location
pattern S_STRING    = 0x25DB :: Location
pattern S_BRACKET   = 0x25E8 :: Location
pattern S_FN        = 0x25F5 :: Location
pattern S_RND       = 0x25F8 :: Location
pattern S_RND_END   = 0x2625 :: Location
pattern S_PI        = 0x2627 :: Location
pattern S_PI_END    = 0x2630 :: Location

-- | This routine is called @S-INKEY$@ in "The Complete Spectrum ROM Disassembly"
pattern S_INKEYS    = 0x2634 :: Location

-- | This routine is called @S-IK$-STK@ in "The Complete Spectrum ROM Disassembly"
pattern S_IKS_STK   = 0x2660 :: Location

-- | This routine is called @S-INK$-EN@ in "The Complete Spectrum ROM Disassembly"
pattern S_INKS_EN   = 0x2665 :: Location

-- | This routine is called @S-SCREEN$@ in "The Complete Spectrum ROM Disassembly"
pattern S_SCREENS   = 0x2668 :: Location

pattern S_ATTR      = 0x2672 :: Location
pattern S_POINT     = 0x267B :: Location
pattern S_ALPHNUM   = 0x2684 :: Location
pattern S_DECIMAL   = 0x268D :: Location
pattern S_STK_DEC   = 0x26B5 :: Location
pattern S_SD_SKIP   = 0x26B6 :: Location
pattern S_NUMERIC   = 0x26C3 :: Location
pattern S_LETTER    = 0x26C9 :: Location
pattern S_CONT_1    = 0x26DD :: Location
pattern S_NEGATE    = 0x26DF :: Location
pattern S_NO_TO_S   = 0x2707 :: Location
pattern S_PUSH_PO   = 0x270D :: Location
pattern S_CONT_2    = 0x2712 :: Location
pattern S_CONT_3    = 0x2713 :: Location
pattern S_OPERTR    = 0x2723 :: Location
pattern S_LOOP      = 0x2734 :: Location
pattern S_STK_LST   = 0x274C :: Location
pattern S_SYNTEST   = 0x275B :: Location
pattern S_RPORT_C'  = 0x2761 :: Location
pattern S_RUNTEST   = 0x2764 :: Location
pattern S_LOOPEND   = 0x2770 :: Location
pattern S_TIGHTER   = 0x2773 :: Location
pattern S_NOT_AND   = 0x2788 :: Location
pattern S_NEXT      = 0x2790 :: Location
pattern S_FN_SBRN   = 0x27BD :: Location
pattern SF_BRKT_1   = 0x27D0 :: Location
pattern SF_ARGMTS   = 0x27D9 :: Location
pattern SF_BRKT_2   = 0x27E4 :: Location
pattern SF_RPRT_C   = 0x27E6 :: Location
pattern SF_FLAG_6   = 0x27E9 :: Location
pattern SF_SYN_EN   = 0x27F4 :: Location
pattern SF_RUN      = 0x27F7 :: Location
pattern SF_ARGMT1   = 0x2802 :: Location
pattern SF_FND_DF   = 0x2808 :: Location
pattern REPORT_P    = 0x2812 :: Location
pattern SF_CP_DEF   = 0x2814 :: Location
pattern SF_NOT_FD   = 0x2825 :: Location
pattern SF_VALUES   = 0x2831 :: Location
pattern SF_ARG_LP   = 0x2843 :: Location
pattern SF_ARG_VL   = 0x2852 :: Location
pattern SF_R_BR_2   = 0x2885 :: Location
pattern REPORT_Q    = 0x288B :: Location
pattern SF_VALUE    = 0x288D :: Location
pattern FN_SKPOVR   = 0x28AB :: Location
pattern LOOK_VARS   = 0x28B2 :: Location
pattern V_CHAR      = 0x28D4 :: Location
pattern V_STR_VAR   = 0x28DE :: Location
pattern V_TEST_FN   = 0x28E3 :: Location

-- | This routine is called @V-RUN/SYN@ in "The Complete Spectrum ROM Disassembly"
pattern V_RUN_SYN   = 0x28EF :: Location

pattern V_RUN       = 0x28FD :: Location
pattern V_EACH      = 0x2900 :: Location
pattern V_MATCHES   = 0x2912 :: Location
pattern V_SPACES    = 0x2913 :: Location
pattern V_GET_PTR   = 0x2929 :: Location
pattern V_NEXT      = 0x292A :: Location
pattern V_80_BYTE   = 0x2932 :: Location
pattern V_SYNTAX    = 0x2934 :: Location
pattern V_FOUND_1   = 0x293E :: Location
pattern V_FOUND_2   = 0x293F :: Location
pattern V_PASS      = 0x2943 :: Location
pattern V_END       = 0x294B :: Location
pattern STK_F_ARG   = 0x2951 :: Location
pattern SFA_LOOP    = 0x295A :: Location
pattern SFA_CP_VR   = 0x296B :: Location
pattern SFA_MATCH   = 0x2981 :: Location
pattern SFA_END     = 0x2991 :: Location
pattern STK_VAR     = 0x2996 :: Location

-- | This routine is called @SV-SIMPLE$@ in "The Complete Spectrum ROM Disassembly"
pattern SV_SIMPLES  = 0x29A1 :: Location

pattern SV_ARRAYS   = 0x29AE :: Location
pattern SV_PTR      = 0x29C0 :: Location
pattern SV_COMMA    = 0x29C3 :: Location
pattern SV_CLOSE    = 0x29D8 :: Location
pattern SV_CH_ADD   = 0x29E0 :: Location
pattern SV_COUNT    = 0x29E7 :: Location
pattern SV_LOOP     = 0x29EA :: Location
pattern SV_MULT     = 0x29FB :: Location
pattern SV_RPT_C    = 0x2A12 :: Location
pattern REPORT_3    = 0x2A20 :: Location
pattern SV_NUMBER   = 0x2A22 :: Location

-- | This routine is called @SV-ELEM$@ in "The Complete Spectrum ROM Disassembly"
pattern SV_ELEMS    = 0x2A2C :: Location

pattern SV_SLICE    = 0x2A45 :: Location
pattern SV_DIM      = 0x2A48 :: Location

-- | This routine is called @SV-SLICE?@ in "The Complete Spectrum ROM Disassembly"
pattern SV_SLICE_P  = 0x2A49 :: Location

pattern SLICING     = 0x2A52 :: Location
pattern SL_RPT_C    = 0x2A7A :: Location
pattern SL_SECOND   = 0x2A81 :: Location
pattern SL_DEFINE   = 0x2A94 :: Location
pattern SL_OVER     = 0x2AA8 :: Location
pattern STK_ST_0    = 0x2AB1 :: Location

-- | This routine is called @STK-STO-S@ in "The Complete Spectrum ROM Disassembly"
pattern STK_STO_S   = 0x2AB2 :: Location

pattern STK_STORE   = 0x2AB6 :: Location
pattern INT_EXP1    = 0x2ACC :: Location
pattern INT_EXP2    = 0x2ACD :: Location
pattern I_CARRY     = 0x2AE8 :: Location
pattern I_RESTORE   = 0x2AEB :: Location

-- | This routine is called @GET-HL*DE@ in "The Complete Spectrum ROM Disassembly"
pattern GET_HLxDE   = 0x2AF4 :: Location

pattern LET         = 0x2AFF :: Location
pattern L_EACH_CH   = 0x2B0B :: Location
pattern L_NO_SP     = 0x2B0C :: Location
pattern L_TEST      = 0x2B1F :: Location
pattern L_SPACES    = 0x2B29 :: Location
pattern L_CHAR      = 0x2B3E :: Location
pattern L_SINGLE    = 0x2B4F :: Location
pattern L_NUMERIC   = 0x2B59 :: Location
pattern L_EXISTS    = 0x2B66 :: Location

-- | This routine is called @L-DELETE$@ in "The Complete Spectrum ROM Disassembly"
pattern L_DELETES   = 0x2B72 :: Location

pattern L_LENGTH    = 0x2B9B :: Location

-- | This routine is called @L-IN-W/S@ in "The Complete Spectrum ROM Disassembly"
pattern L_IN_W_S    = 0x2BA3 :: Location

pattern L_ENTER     = 0x2BA6 :: Location

-- | This routine is called @L-ADD$@ in "The Complete Spectrum ROM Disassembly"
pattern L_ADDS      = 0x2BAF :: Location

-- | This routine is called @L-NEWS@ in "The Complete Spectrum ROM Disassembly"
pattern L_NEWS      = 0x2BC0 :: Location

pattern L_STRING    = 0x2BC6 :: Location
pattern L_FIRST     = 0x2BEA :: Location
pattern STK_FETCH   = 0x2BF1 :: Location
pattern DIM         = 0x2C02 :: Location
pattern D_RPORT_C   = 0x2C05 :: Location
pattern D_RUN       = 0x2C15 :: Location
pattern D_LETTER    = 0x2C1F :: Location
pattern D_SIZE      = 0x2C2D :: Location
pattern D_NO_LOOP   = 0x2C2E :: Location
pattern DIM_CLEAR   = 0x2C7C :: Location
pattern DIM_SIZES   = 0x2C7F :: Location
pattern ALPHANUM    = 0x2C88 :: Location
pattern ALPHA       = 0x2C8D :: Location
pattern DEC_TO_FP   = 0x2C9B :: Location
pattern BIN_DIGIT   = 0x2CA2 :: Location
pattern BIN_END     = 0x2CB3 :: Location
pattern NOT_BIN     = 0x2CB8 :: Location
pattern DECIMAL     = 0x2CCB :: Location
pattern DEC_RPT_C   = 0x2CCF :: Location
pattern DEC_STO_1   = 0x2CD5 :: Location
pattern NXT_DGT_1   = 0x2CDA :: Location
pattern E_FORMAT    = 0x2CEB :: Location
pattern SIGN_FLAG   = 0x2CF2 :: Location
pattern SIGN_DONE   = 0x2CFE :: Location
pattern ST_E_PART   = 0x2CFF :: Location
pattern E_FP_JUMP   = 0x2D18 :: Location
pattern NUMERIC     = 0x2D1B :: Location
pattern STK_DIGIT   = 0x2D22 :: Location
pattern STACK_A     = 0x2D28 :: Location
pattern STACK_BC    = 0x2D2B :: Location
pattern INT_TO_FP   = 0x2D3B :: Location
pattern E_TO_FP     = 0x2D4F :: Location
pattern E_SAVE      = 0x2D55 :: Location
pattern E_LOOP      = 0x2D60 :: Location
pattern E_DIVSN     = 0x2D6D :: Location
pattern E_FETCH     = 0x2D6E :: Location
pattern E_TST_END   = 0x2D71 :: Location
pattern E_END       = 0x2D7B :: Location
pattern INT_FETCH   = 0x2D7F :: Location
pattern P_INT_STO   = 0x2D8C :: Location
pattern INT_STORE   = 0x2D8E :: Location
pattern FP_TO_BC    = 0x2DA2 :: Location
pattern FP_DELETE   = 0x2DAD :: Location

-- | This routine is called @LOG(2^A)@ in "The Complete Spectrum ROM Disassembly"
pattern LOG_2vA     = 0x2DC1 :: Location

pattern FP_TO_A     = 0x2DD5 :: Location
pattern FP_A_END    = 0x2DE1 :: Location
pattern PRINT_FP    = 0x2DE3 :: Location
pattern PF_NEGTVE   = 0x2DF2 :: Location
pattern PF_POSTVE   = 0x2DF8 :: Location
pattern PF_LOOP     = 0x2E01 :: Location
pattern PF_SAVE     = 0x2E1E :: Location
pattern PF_SMALL    = 0x2E24 :: Location
pattern PF_LARGE    = 0x2E56 :: Location
pattern PF_MEDIUM   = 0x2E6F :: Location
pattern PF_BITS     = 0x2E7B :: Location
pattern PF_BYTES    = 0x2E8A :: Location
pattern PF_DIGITS   = 0x2EA1 :: Location
pattern PF_INSERT   = 0x2EA9 :: Location
pattern PF_TEST_2   = 0x2EB3 :: Location
pattern PF_ALL_9    = 0x2EB8 :: Location
pattern PF_MORE     = 0x2ECB :: Location
pattern PF_FRACTN   = 0x2ECF :: Location
pattern PF_FRN_LP   = 0x2EDF :: Location
pattern PF_FR_DGT   = 0x2EEC :: Location
pattern PF_FR_EXX   = 0x2EEF :: Location
pattern PF_ROUND    = 0x2F0C :: Location
pattern PF_RND_LP   = 0x2F18 :: Location
pattern PF_R_BACK   = 0x2F25 :: Location
pattern PF_COUNT    = 0x2F2D :: Location
pattern PF_NOT_E    = 0x2F46 :: Location
pattern PF_E_SBRN   = 0x2F4A :: Location
pattern PF_OUT_LP   = 0x2F52 :: Location
pattern PF_OUT_DT   = 0x2F59 :: Location
pattern PF_DC_OUT   = 0x2F5E :: Location
pattern PF_DEC_0S   = 0x2F64 :: Location
pattern PF_E_FRMT   = 0x2F6C :: Location
pattern PF_E_POS    = 0x2F83 :: Location
pattern PF_E_SIGN   = 0x2F85 :: Location

-- | This routine is called @CA=10*A+C@ in "The Complete Spectrum ROM Disassembly"
pattern CA_10xA_C   = 0x2F8B :: Location
pattern PREP_ADD    = 0x2F9B :: Location
pattern NEG_BYTE    = 0x2FAF :: Location
pattern SHIFT_FP    = 0x2FDD :: Location
pattern ONE_SHIFT   = 0x2FE5 :: Location
pattern ADDEND_0    = 0x2FF9 :: Location

-- | This routine is called @ZEROS-4/5@ in "The Complete Spectrum ROM Disassembly"
pattern ZEROS_4_5   = 0x2FFB :: Location

pattern ADD_BACK    = 0x3004 :: Location
pattern ALL_ADDED   = 0x300D :: Location
pattern SUBTRACT    = 0x300F :: Location
addition            = 0x3014 :: Location
pattern ADDN_OFLW   = 0x303C :: Location
pattern FULL_ADDN   = 0x303E :: Location
pattern SHIFT_LEN   = 0x3055 :: Location
pattern TEST_NEG    = 0x307C :: Location
pattern ADD_REP_6   = 0x309F :: Location
pattern END_COMPL   = 0x30A3 :: Location
pattern GO_NC_MLT   = 0x30A5 :: Location

-- | This routine is called @HL=HL*DE@ in "The Complete Spectrum ROM Disassembly"
pattern HL_HLxDE    = 0x30A9 :: Location

pattern HL_LOOP     = 0x30B1 :: Location
pattern HL_AGAIN    = 0x30BC :: Location
pattern HL_END      = 0x30BE :: Location

-- | This routine is called @PREP-M/D@ in "The Complete Spectrum ROM Disassembly"
pattern PREP_M_D    = 0x30C0 :: Location

multiply            = 0x30CA :: Location
pattern MULT_RSLT   = 0x30EA :: Location
pattern MULT_OFLW   = 0x30EF :: Location
pattern MULT_LONG   = 0x30F0 :: Location
pattern MLT_LOOP    = 0x3114 :: Location
pattern NO_ADD      = 0x311B :: Location
pattern STRT_MLT    = 0x3125 :: Location
pattern MAKE_EXPT   = 0x313B :: Location
pattern DIVN_EXPT   = 0x313D :: Location
pattern OFLW1_CLR   = 0x3146 :: Location
pattern OFLW2_CLR   = 0x3151 :: Location
pattern TEST_NORM   = 0x3155 :: Location
pattern NEAR_ZERO   = 0x3159 :: Location
pattern ZERO_RSLT   = 0x315D :: Location
pattern SKIP_ZERO   = 0x315E :: Location
pattern NORMALISE   = 0x316C :: Location
pattern SHIFT_ONE   = 0x316E :: Location
pattern NORML_NOW   = 0x3186 :: Location
pattern OFLOW_CLR   = 0x3195 :: Location
pattern REPORT_6    = 0x31AD :: Location
division            = 0x31AF :: Location
pattern DIV_LOOP    = 0x31D2 :: Location
pattern DIV_34TH    = 0x31DB :: Location
pattern DIV_START   = 0x31E2 :: Location
pattern SUBN_ONLY   = 0x31F2 :: Location
pattern NO_RSTORE   = 0x31F9 :: Location
pattern COUNT_ONE   = 0x31FA :: Location
truncate            = 0x3214 :: Location
pattern T_GR_ZERO   = 0x3221 :: Location
pattern T_FIRST     = 0x3233 :: Location
pattern T_SMALL     = 0x323F :: Location
pattern T_NUMERIC   = 0x3252 :: Location
pattern T_TEST      = 0x325E :: Location
pattern T_SHIFT     = 0x3261 :: Location
pattern T_STORE     = 0x3267 :: Location
pattern T_EXPNENT   = 0x326C :: Location
pattern X_LARGE     = 0x326D :: Location
pattern NIL_BYTES   = 0x3272 :: Location
pattern BYTE_ZERO   = 0x327E :: Location
pattern BITS_ZERO   = 0x3283 :: Location
pattern LESS_MASK   = 0x328A :: Location
pattern IX_END      = 0x3290 :: Location
pattern RE_ST_TWO   = 0x3293 :: Location
pattern RESTK_SUB   = 0x3296 :: Location
pattern RE_STACK    = 0x3297 :: Location
pattern RS_NRMLSE   = 0x32B1 :: Location
pattern RSTK_LOOP   = 0x32B2 :: Location
pattern RS_STORE    = 0x32BD :: Location
stk_zero            = 0x32C5 :: Location
stk_one             = 0x32C8 :: Location
stk_half            = 0x32CC :: Location

-- | This routine is called @stk-pi/2@ in "The Complete Spectrum ROM Disassembly"
stk_pi_2            = 0x32CE :: Location

stk_ten             = 0x32D3 :: Location
pattern CALCULATE   = 0x335B :: Location
pattern GEN_ENT_1   = 0x335E :: Location
pattern GEN_ENT_2   = 0x3362 :: Location
pattern RE_ENTRY    = 0x3365 :: Location
pattern SCAN_ENT    = 0x336C :: Location
pattern FIRST_3D    = 0x3380 :: Location
pattern DOUBLE_A    = 0x338C :: Location
pattern ENT_TABLE   = 0x338E :: Location
delete              = 0x33A1 :: Location
fp_calc_2           = 0x33A2 :: Location
pattern TEST_5_SP   = 0x33A9 :: Location
pattern STACK_NUM   = 0x33B4 :: Location
pattern MOVE_FP     = 0x33C0 :: Location
pattern STK_DATA    = 0x33C6 :: Location
pattern STK_CONST   = 0x33C8 :: Location
pattern FORM_EXP    = 0x33DE :: Location
pattern STK_ZEROS   = 0x33F1 :: Location
pattern SKIP_CONS   = 0x33F7 :: Location
pattern SKIP_NEXT   = 0x33F8 :: Location
pattern LOC_MEM     = 0x3406 :: Location
get_mem_0           = 0x340F :: Location
stk_zero'           = 0x341B :: Location
st_mem_0            = 0x342D :: Location
pattern EXCHANGE    = 0x343C :: Location
pattern SWAP_BYTE   = 0x343E :: Location
series_06           = 0x3449 :: Location
pattern G_LOOP      = 0x3453 :: Location
abs                 = 0x346A :: Location
pattern NEGATE      = 0x346E :: Location
pattern NEG_TEST    = 0x3474 :: Location
pattern INT_CASE    = 0x3483 :: Location
sgn                 = 0x3492 :: Location
peek                = 0x34AC :: Location
pattern IN_PK_STK   = 0x34B0 :: Location
usr_no              = 0x34B3 :: Location

-- | This routine is called @usr-$@ in "The Complete Spectrum ROM Disassembly"
usr_S               = 0x34BC :: Location

pattern USR_STACK   = 0x34E4 :: Location
pattern REPORT_A    = 0x34E7 :: Location
pattern TEST_ZERO   = 0x34E9 :: Location
pattern GREATER_0   = 0x34F9 :: Location
less_0              = 0x3506 :: Location
pattern SIGN_TO_C   = 0x3507 :: Location

-- | This routine is called @FP-0/1@ in "The Complete Spectrum ROM Disassembly"
pattern FP_0_1      = 0x350B :: Location

or                  = 0x351B :: Location

-- | This routine is called @str-&-no@ in "The Complete ROM Disassembly".
str_and_no          = 0x352D :: Location

no_l_eql            = 0x353B :: Location
pattern EX_OR_NOT   = 0x3543 :: Location
pattern NU_OR_STR   = 0x354E :: Location
pattern STRINGS     = 0x3559 :: Location
pattern BYTE_COMP   = 0x3564 :: Location
pattern SECND_LOW   = 0x356B :: Location
pattern BOTH_NULL   = 0x3572 :: Location
pattern SEC_PLUS    = 0x3575 :: Location
pattern FRST_LESS   = 0x3585 :: Location
pattern STR_TEST    = 0x3588 :: Location
pattern END_TESTS   = 0x358C :: Location
pattern OTHER_STR   = 0x35B7 :: Location
pattern STK_PNTRS   = 0x35BF :: Location
chrs                = 0x35C9 :: Location
pattern REPORT_B''' = 0x35DC :: Location
val                 = 0x35DE :: Location
pattern V_RPORT_C   = 0x360C :: Location

-- | This routine is called @str$@ in "The Complete Spectrum ROM Disassembly"
strS                = 0x361F :: Location

read_in             = 0x3645 :: Location
pattern R_I_STORE   = 0x365F :: Location
code                = 0x3669 :: Location
pattern STK_CODE    = 0x3671 :: Location
len                 = 0x3674 :: Location
dec_jr_nz           = 0x367A :: Location
pattern JUMP_2      = 0x3687 :: Location
jump_true           = 0x368F :: Location
end_calc            = 0x369B :: Location
n_mod_m             = 0x36A0 :: Location
int                 = 0x36AF :: Location
pattern X_NEG       = 0x36B7 :: Location
pattern EXIT        = 0x36C2 :: Location
pattern EXP         = 0x36C4 :: Location
pattern REPORT_6'   = 0x3703 :: Location
pattern N_NEGTV     = 0x3705 :: Location
pattern RESULT_OK   = 0x370C :: Location
pattern RSLT_ZERO   = 0x370E :: Location
ln                  = 0x3713 :: Location
pattern REPORT_A'   = 0x371A :: Location
pattern VALID       = 0x371C :: Location

-- | This routine is called @GRE.8@ in "The Complete Spectrum ROM Disassembly"
pattern GRE_8       = 0x373D :: Location

get_argt            = 0x3783 :: Location
pattern ZPLUS       = 0x37A1 :: Location
pattern YNEG        = 0x37A8 :: Location
cos                 = 0x37AA :: Location
sin                 = 0x37B5 :: Location
pattern C_ENT       = 0x37B7 :: Location
tan                 = 0x37DA :: Location
atn                 = 0x37E2 :: Location
pattern SMALL       = 0x37F8 :: Location
pattern CASES       = 0x37FA :: Location
asn                 = 0x3833 :: Location
acs                 = 0x3843 :: Location
sqr                 = 0x384A :: Location
to_power            = 0x3851 :: Location
pattern XIS0        = 0x385D :: Location
pattern ONE         = 0x386A :: Location
pattern LAST        = 0x386C :: Location
