REM $INCLUDE: 'x86.bi'
DECLARE FUNCTION GetCorrection$ (pr$, X$)
DECLARE FUNCTION COUNT.CHAR! (X$, C$)
DECLARE SUB ProcessLine (STACK$, ifl%, AGAIN%)
DECLARE SUB HeaderRead (orig#, a.text#, a.data#, a.bss#, a.syms#, a.entry#, a.trsize#, a.drsize#)
DECLARE SUB LA.write (laddr#, value#, N.BYTES!)

SUB check.brkpt (btype%, X#)
END SUB

SUB check.interrupts
END SUB

SUB checkpoint
END SUB

SUB DisplayEnv (PLACE)
END SUB

SUB DotOInit (X#)
END SUB

SUB DotOLoad (B.NAME$, orig#)
OPEN B.NAME$ + ".o" FOR BINARY AS #1
LET POINTER# = orig#

FOR K = 0 TO 7
    LET T& = 0&
    LET P& = 1&
    FOR J = 1 TO 4
        IF J = 1 THEN
            LET P& = 1&
        ELSE
            LET P& = P& * 256&
        END IF
        LET C$ = INPUT$(1, #1)
        LET T& = T& + ASC(C$) * P&
    NEXT
    LET HEADER(K) = T&
NEXT
REM
CALL HeaderRead(orig#, a.text#, a.data#, a.bss#, a.syms#, a.entry#, a.trsize#, a.drsize#)
LET textoff# = 0#     ' 32 = sizeof(struct exec)
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

SUB handle.intr.xcp (vecnum%, ef%, ec%)
END SUB

SUB HeaderRead (orig#, a.text#, a.data#, a.bss#, a.syms#, a.entry#, a.trsize#, a.drsize#)
REM LET info# = LA.read#(POINTER#, 4)
LET a.text# = HEADER(1)
LET a.data# = HEADER(2)
LET a.bss# = HEADER(3)
LET a.syms# = HEADER(4)
LET a.entry# = HEADER(5)
LET a.trsize# = HEADER(6)
LET a.drsize# = HEADER(7)

END SUB

SUB HelpDisplay (PLACE)
END SUB

SUB InstAssemble (RAW.CODE$, INDEX!, PF$, PACKED.CODE$, DISPLABEL$, DISPCODE, HX$, ERM$)
END SUB

SUB InstDelete
END SUB

SUB Iret (os)
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

SUB report.brkpts


END SUB

SUB reset.cpu
END SUB

SUB ShortenJump (N%, ERM$)
END SUB

SUB signal.fault (xcptype AS INTEGER, error.code AS INTEGER)
REM p 155
LET INTERRUPT.RECEIVED = TRUE
LET INTERRUPT.NUMBER = xcptype
END SUB

SUB WriteDotO (X$)
END SUB

