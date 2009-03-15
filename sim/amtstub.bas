REM $INCLUDE: 'x86.bi'
DECLARE FUNCTION GetCorrection$ (pr$, X$)
DECLARE FUNCTION COUNT.CHAR! (X$, C$)
DECLARE SUB ProcessLine (STACK$, ifl%, AGAIN%)
DECLARE SUB HeaderRead (orig#, a.text#, a.data#, a.bss#, a.syms#, a.entry#, a.trsize#, a.drsize#)
DECLARE SUB LA.write (laddr#, value#, N.BYTES!)

SUB DisplayEnv (PLACE)
END SUB

SUB Do0F (OS, SREG, ADS, LF)
END SUB

SUB DotOInit (X#)
END SUB

SUB DotOLoad (B.NAME$, orig#)
OPEN B.NAME$ + ".o" FOR BINARY AS #1
LET POINTER# = orig#
FOR K = 1 TO 32
    LET C$ = INPUT$(1, #1)
    CALL LA.write(POINTER#, CDBL(ASC(C$)), 1)
    LET POINTER# = POINTER# + 1
NEXT
REM
CALL HeaderRead(orig#, a.text#, a.data#, a.bss#, a.syms#, a.entry#, a.trsize#, a.drsize#)
LET textoff# = 32     ' 32 = sizeof(struct exec)
LET dataoff# = textoff# + a.text#
LET treloff# = dataoff# + a.data#
LET dreloff# = treloff# + a.trsize#
LET symoff# = dreloff# + a.drsize#
LET stroff# = symoff# + a.syms#
WHILE POINTER# < LOAD.ORIGIN# + stroff#
    LET C$ = INPUT$(1, #1)
    CALL LA.write(POINTER#, CDBL(ASC(C$)), 1)
    LET POINTER# = POINTER# + 1
WEND
FOR K = 1 TO 4
    LET C$ = INPUT$(1, #1)
    CALL LA.write(POINTER#, CDBL(ASC(C$)), 1)
    LET POINTER# = POINTER# + 1
NEXT

LET A.strs# = LA.read#(POINTER# - 4, 4)
LET EOF.off# = stroff# + A.strs#
WHILE POINTER# < EOF.off#
    LET C$ = INPUT$(1, #1)
    CALL LA.write(POINTER#, CDBL(ASC(C$)), 1)
    LET POINTER# = POINTER# + 1
WEND
CLOSE #1
REM
END SUB

SUB FillinLinenum (LABEL$)
END SUB

SUB HeaderRead (orig#, a.text#, a.data#, a.bss#, a.syms#, a.entry#, a.trsize#, a.drsize#)
LET POINTER# = orig#
LET info# = LA.read#(POINTER#, 4)
LET POINTER# = POINTER# + 4
LET a.text# = LA.read#(POINTER#, 4)
LET POINTER# = POINTER# + 4
LET a.data# = LA.read#(POINTER#, 4)
LET POINTER# = POINTER# + 4
LET a.bss# = LA.read#(POINTER#, 4)
LET POINTER# = POINTER# + 4
LET a.syms# = LA.read#(POINTER#, 4)
LET POINTER# = POINTER# + 4
LET a.entry# = LA.read#(POINTER#, 4)
LET POINTER# = POINTER# + 4
LET a.trsize# = LA.read#(POINTER#, 4)
LET POINTER# = POINTER# + 4
LET a.drsize# = LA.read#(POINTER#, 4)

END SUB

SUB HelpDisplay (PLACE)
END SUB

SUB InstAssemble (RAW.CODE$, INDEX!, PF$, PACKED.CODE$, DISPLABEL$, DISPCODE, HX$, ERM$)
END SUB

SUB InstDelete
END SUB

SUB Jcond (OS, BYTE)
END SUB

SUB LineParse (MNEMONIC$, INDEX, LABEL$, PF$, CODE$, COMMENT.FIELD$, ERM$)
END SUB

SUB NumParse (RAW$, PACK$, value#, ERM$)
LET value# = VAL(RAW$)
END SUB

SUB ParseInit
END SUB

SUB ProcessLine (STACK$, icfl%, AGAIN%)
END SUB

SUB ShortenJump (N%, ERM$)
END SUB

SUB WriteDotO (X$)
END SUB

