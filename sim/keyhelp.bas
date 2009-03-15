REM $INCLUDE: 'x86.bi'
DECLARE SUB KeyHelp ()
DECLARE SUB IntHelp ()
DECLARE SUB EditHelp ()
DECLARE SUB EnvHelp ()
DECLARE SUB Print24 (X$)

SUB EditHelp
LOCATE 3, 30
PRINT "Edit       "
LOCATE 4, 32
PRINT "Commands "
LOCATE 5, 32
PRINT "         "
LOCATE 6, 32
PRINT "C Change "
LOCATE 7, 32
PRINT "D Delete "
LOCATE 8, 32
PRINT "E Exit   "
LOCATE 9, 32
PRINT "F Flags  "
LOCATE 10, 32
PRINT "H Help   "
LOCATE 11, 32
PRINT "I Insert "
LOCATE 12, 32
PRINT "O write.O"
LOCATE 13, 32
PRINT "P Put    "
LOCATE 14, 32
PRINT "Q Quit   "
LOCATE 15, 32
PRINT "R Read   "
LOCATE 16, 32
PRINT "S Shorten"
LOCATE 17, 32
PRINT "U Undel  "
LOCATE 18, 32
PRINT "W Write  "
LOCATE 19, 32
PRINT "Y Yank   "
FOR K = 20 TO 23
    LOCATE K, 32
    PRINT SPACE$(9);
NEXT
END SUB

SUB EnvHelp
LOCATE 3, 30
PRINT "Environment"
LOCATE 4, 32
PRINT "Variables"
LOCATE 5, 32
PRINT "         "
LOCATE 6, 32
PRINT "ARCH     "
LOCATE 7, 32
PRINT "DOTO     "
LOCATE 8, 32
PRINT "INSTSET  "
LOCATE 9, 32
PRINT "INSTTIM  "
LOCATE 10, 32
PRINT "H Help   "
LOCATE 11, 32
PRINT "I Insert "
LOCATE 12, 32
PRINT "O write.O"
LOCATE 13, 32
PRINT "P Put    "
LOCATE 14, 32
PRINT "Q Quit   "
LOCATE 15, 32
PRINT "R Read   "
LOCATE 16, 32
PRINT "S Shorten"
LOCATE 17, 32
PRINT "U Undel  "
LOCATE 18, 32
PRINT "W Write  "
LOCATE 19, 32
PRINT "Y Yank   "
FOR K = 10 TO 23
    LOCATE K, 32
    PRINT SPACE$(9);
NEXT
END SUB

SUB HelpDisplay (PLACE)
    CALL Print24(SPACE$(25) + "More help: " + CHR$(25) + CHR$(24))
    LET PLACE = (PLACE + 209) MOD 3 + 1
    IF PLACE = 1 THEN
        CALL KeyHelp
    ELSEIF PLACE = 2 THEN
        CALL EditHelp
    ELSEIF PLACE = 3 THEN
        CALL EnvHelp
    END IF


END SUB

SUB KeyHelp
CALL Print24(SPACE$(25) + "More help: " + CHR$(25) + CHR$(24))
LOCATE 3, 30
PRINT "Main Keys  ";
LOCATE 4, 32
PRINT "         ";
LOCATE 5, 32
PRINT "         ";
LOCATE 6, 32
PRINT "Escape:  ";
LOCATE 7, 32
PRINT " intrpret";
LOCATE 8, 32
PRINT "         ";
LOCATE 9, 32
PRINT "Tab      ";
LOCATE 10, 32
PRINT "   ObjCde"
LOCATE 11, 32
PRINT "   Stack "
LOCATE 12, 32
PRINT "   Memory"
LOCATE 13, 32
PRINT "   SegReg"
LOCATE 14, 32
PRINT "   MscReg"
LOCATE 15, 32
PRINT "   Defs  "
LOCATE 16, 32
PRINT "   Help  "

LOCATE 17, 32
PRINT "         ";
LOCATE 18, 32
PRINT "F1 Help  ";
LOCATE 19, 32
PRINT "F4 Video ";
LOCATE 20, 32
PRINT "F5 Walk  ";
LOCATE 21, 32
PRINT "F8 Step  ";
LOCATE 22, 32
PRINT "F10 Skip ";
FOR K = 23 TO 23
    LOCATE K, 32
    PRINT SPACE$(9);
NEXT

END SUB

