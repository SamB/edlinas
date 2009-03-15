REM $INCLUDE: 'x86.bi'
DECLARE SUB InstAssemble (RAW.CODE$, INDEX!, PF$, PACKED.CODE$, DISPLABEL$, DISPCODE!, HX$, ERM$)
DECLARE SUB LineParse (MNEMONIC$, INDEX!, LABEL$, PF$, CODE$, COMMENT.FIELD$, ERM$)
DECLARE SUB ProcessLine (RAW.CODE$, INSCOM.FLAG%, AGAIN%)
DECLARE SUB DirectiveParse (LABEL$, CODE$, INDEX!, PACK$, N.HX!, ERM$)
DECLARE SUB DefInsert (LBL$, CODE$)
DECLARE SUB NumParse (RAW.SOURCE$, PACK$, NUM.VALUE#, ERM$)
DECLARE FUNCTION GetCorrection$ (p$, X$)
DECLARE SUB DoQuasi (INDEX!)
DECLARE SUB Print24 (X$)
DECLARE FUNCTION MemCheck! (Z$, ERM$, N.BYTES)
DECLARE FUNCTION MemStore! (POINTER#, MemExp$, N.BYTES)
REM
DECLARE SUB SymTabInsert (SYM$, STORAGE.CODE!, STORAGE.VALUE#)
DECLARE SUB SymTabAddFix (SYM$, FIXED.ADDRESS#)
DECLARE FUNCTION DISP.WIDTH! (DISPCODE!)
DECLARE SUB DispAdjust (p#, AMOUNT!)
DECLARE SUB WidthFix (p#, n.BITS!)
DECLARE SUB DispInc (p#)
DECLARE SUB AoutSlide (INST.J%, NM.BYTES!)
DECLARE SUB TRelTabInsert (INST.NUM%, X#)
DECLARE SUB TRelTabDelete (INST.NUM%)
DECLARE SUB SymTabDelete (SYM$)
DECLARE FUNCTION SYM.TAB.POINTER# (SYM$)
DECLARE FUNCTION GetTableString$ (POINTER#)
DECLARE SUB TextSlide (INST.K%, SLIDE.BYTES!)
DECLARE SUB HeaderWrite (orig#, a.text#, a.data#, a.bss#, a.syms#, a.entry#, a.trsize#, a.drsize#)
DECLARE SUB HeaderRead (orig#, a.text#, a.data#, a.bss#, a.syms#, a.entry#, a.trsize#, a.drsize#)
DECLARE SUB DotOInit (origin#)
DECLARE SUB InstDelete ()
DECLARE SUB InstInsert (RAW.CODE$, PACKED.SOURCE$, MNEMONIC$, LABEL$, DISPLABEL$, DISPCODE, COMMENT.FIELD$, HX$, nhb)
DECLARE SUB WriteDotO (X$)
DECLARE FUNCTION AdAlign# (address#, N.BYTES!)
DECLARE SUB LEwrite (FILENUM!, X#, N.BYTES!)
DECLARE SUB Fetchexec (REPEAT, BYTE)
DECLARE FUNCTION CUT$ (X$)
DIM BROKEN.STACK(5)

FUNCTION AdAlign# (address#, N.BYTES)
LET X# = FIX((address# + CDBL(N.BYTES - 1)) / CDBL(N.BYTES))
LET AdAlign# = X# * CDBL(N.BYTES)
END FUNCTION

SUB AoutSlide (INST.J AS INTEGER, NM.BYTES)
SHARED BROKEN.STACK(), TOP.BSTACK
LET TOP.BSTACK = 0
CALL TextSlide(INST.J, NM.BYTES)
WHILE TOP.BSTACK > 0
    
    IF BROKEN.STACK(TOP.BSTACK) > 0 THEN
        LET K = BROKEN.STACK(TOP.BSTACK)
        LET Z# = 128#
    ELSEIF BROKEN.STACK(TOP.BSTACK) < 0 THEN
        LET K = -BROKEN.STACK(TOP.BSTACK)
        LET Z# = -128#
    ELSE
        PRINT "BROKEN STACK entry missing"
        SYSTEM
    END IF

    LET J% = K + 1
    LET BYTES.BACK = DISPLABEL.CODE(K) \ 16
    LET DISP.POINTER# = INST.ADR#(J%) - BYTES.BACK
            ' So POINTER# points to 1 byte jump displacement.

    LET POINTER# = DISP.POINTER# - 1#
    LET B1$ = HexStr$(LA.read#(POINTER#, 1), 2)
    LET UNCONDJUMP.FLAG = (B1$ = "EB")

    LET NEW.SIZE = USE.SIZE \ 8
  
    IF UNCONDJUMP.FLAG THEN
        LET SLIDE.BYTES = NEW.SIZE - 1
    ELSE
        LET SLIDE.BYTES = NEW.SIZE
    END IF

    LET CURRENT.TOP.BSTACK = TOP.BSTACK
    CALL TextSlide(J%, SLIDE.BYTES)
   
    LET X# = LA.read#(DISP.POINTER#, -1)
    LET X# = X# + Z#
    LET DISPLABEL.CODE(K) = DISPLABEL.CODE(K) + NEW.SIZE  'Looks wrong but its not.
REM
REM     Change width bits for this location in text relocation table.
REM
    CALL WidthFix(DISP.POINTER#, NEW.SIZE)
   
    LET TOP.BSTACK = TOP.BSTACK - 1
    FOR K = CURRENT.TOP.BSTACK TO TOP.BSTACK
        LET BROKEN.STACK(K) = BROKEN.STACK(K + 1)
    NEXT
  
    IF NOT UNCONDJUMP.FLAG THEN
        CALL DispAdjust(DISP.POINTER#, 1)
        LET DISP.POINTER# = DISP.POINTER# + 1
    END IF

    CALL LA.write(DISP.POINTER#, X#, -NEW.SIZE)

    IF UNCONDJUMP.FLAG THEN    ' Fix unconditional jumps
        LET Y# = HexVal#("E9")
        CALL LA.write(POINTER#, Y#, 1)
    ELSE
        LET B1$ = HexStr$(LA.read#(POINTER#, 1), 2)
        IF LEFT$(B1$, 1) = "7" THEN          'Fix conditional jumps
            LET Y# = HexVal#("0F")
            CALL LA.write(POINTER#, Y#, 1)
            LET B2$ = "8" + MID$(B1$, 2)
            LET Y# = HexVal#(B2$)
            LET POINTER# = POINTER# + 1#
            CALL LA.write(POINTER#, Y#, 1)
        ELSE
            LOCATE 24, 1
            PRINT "Jump broken, error recovery needed.";
            SYSTEM
        END IF
    END IF
WEND


END SUB

FUNCTION DISP.WIDTH (DISPCODE)

    LET X = 1
    FOR J = 1 TO (DISPCODE MOD 8) \ 2
        LET X = 2 * X
    NEXT

    LET DISP.WIDTH = X
END FUNCTION

SUB DispAdjust (p#, AMOUNT)
REM
REM Increment occurrences of address, P#, in the Text Relocation table
REM
CALL HeaderRead(LOAD.ORIGIN#, a.text#, a.data#, a.bss#, a.syms#, a.entry#, a.trsize#, a.drsize#)
REM
LET textoff# = 0#     ' 32 = sizeof(struct exec)
LET dataoff# = textoff# + a.text#
LET treloff# = dataoff# + a.data#

REM
REM  Text Relocation Table
REM
FOR K = 0 TO a.trsize# - 8 STEP 8
    LET POINTER# = LOAD.ORIGIN# + treloff# + CDBL(K)
    LET X# = LA.read#(POINTER#, 4)
    IF X# = p# THEN
        LET X# = X# + CDBL(AMOUNT)
        CALL LA.write(POINTER#, X#, 4)
    END IF
NEXT
END SUB

SUB DotOInit (origin#)
REM
REM Create the image of the object file
REM     Start with a header describing an empty file
REM     and update the header every time a new line
REM     is added.
REM
REM     Start with the a.out header
REM             Refer to the O'Reilly COFF book p27
REM
CONST OMAGIC = &H107   '0407 octal,
REM             object file, see /usr/include/linux/a.out.h
REM   
LET POINTER# = origin#
REM
REM LET magic# = OMAGIC
LET HEADER(0) = CDBL(OMAGIC)
REM CALL LA.write(POINTER#, CDBL(OMAGIC), 2)
REM LET POINTER# = POINTER# + 2#
LET vstamp# = 0           '  Put 386 in here?
REM CALL LA.write(POINTER#, vstamp#, 2)
LET a.text# = 0#   'INITIAL.TEXT.SIZE
LET a.data# = 0#   'INITIAL.DATA.SIZE
LET a.bss# = 0#
LET a.syms# = 0#
LET a.entry# = 0#
LET a.trsize# = 0#
LET a.drsize# = 0#
CALL HeaderWrite(origin#, a.text#, a.data#, a.bss#, a.syms#, a.entry#, a.trsize#, a.drsize#)
LET POINTER# = origin# + a.text# + a.data# + a.bss#
LET POINTER# = POINTER# + a.syms# + a.trsize# + a.drsize#
LET A.strs# = 4#
CALL LA.write(POINTER#, A.strs#, 4)

END SUB

SUB DotOLoad (B.NAME$, orig#)
OPEN B.NAME$ + ".o" FOR BINARY AS #1

FOR K = 0 TO 7
    LET t& = 0&
    LET p& = 1&
    FOR J = 1 TO 4
        IF J <> 1 THEN
            LET p& = p& * 256&
        END IF
        LET C$ = INPUT$(1, #1)
        LET t& = t& + ASC(C$) * p&
    NEXT
    LET HEADER(K) = t&
NEXT
LET POINTER# = orig#
REM FOR K = 1 TO 32
REM    LET C$ = INPUT$(1, #1)
REM     CALL LA.write(POINTER#, CDBL(ASC(C$)), 1)
REM     LET POINTER# = POINTER# + 1#
REM NEXT
REM
REM LET magic# = LA.read#(orig#, 2)
LET magic% = (HEADER(0) AND &HFFFF)
IF magic% <> &H107 THEN
    PRINT "Can only read .o files in a.out format"
    SYSTEM
END IF
CALL HeaderRead(orig#, a.text#, a.data#, a.bss#, a.syms#, a.entry#, a.trsize#, a.drsize#)
LET textoff# = 0#     ' 32 = sizeof(struct exec)
LET dataoff# = textoff# + a.text#
LET treloff# = dataoff# + a.data#
LET dreloff# = treloff# + a.trsize#
LET SYMOFF# = dreloff# + a.drsize#
LET stroff# = SYMOFF# + a.syms#
WHILE POINTER# < orig# + stroff#
    LET C$ = INPUT$(1, #1)
    CALL LA.write(POINTER#, CDBL(ASC(C$)), 1)
    LET POINTER# = POINTER# + 1#
WEND
FOR K = 1 TO 4
    LET C$ = INPUT$(1, #1)
    CALL LA.write(POINTER#, CDBL(ASC(C$)), 1)
    LET POINTER# = POINTER# + 1#
NEXT

LET A.strs# = LA.read#(POINTER# - 4#, 4)
LET EOF.off# = stroff# + A.strs#
WHILE POINTER# < orig# + EOF.off#
    LET C$ = INPUT$(1, #1)
    CALL LA.write(POINTER#, CDBL(ASC(C$)), 1)
    LET POINTER# = POINTER# + 1#
WEND
CLOSE #1
REM
END SUB

FUNCTION GetTableString$ (POINTER#)
LET p# = POINTER#
CONST STRING.MAX# = 255#
LET C = 1
LET RETVAL$ = ""
WHILE C <> 0 AND p# < POINTER# + STRING.MAX
    LET C = LA.read#(p#, 1)
    LET p# = p# + 1#
    IF C > 0 THEN
        LET RETVAL$ = RETVAL$ + CHR$(C)
    END IF
WEND
IF p# < POINTER# + STRING.MAX THEN
    LET GetTableString$ = RETVAL$
ELSE
    PRINT "Get string crashed"
    SYSTEM
END IF
END FUNCTION

SUB HeaderRead (orig#, a.text#, a.data#, a.bss#, a.syms#, a.entry#, a.trsize#, a.drsize#)

LET info# = HEADER(0)
LET a.text# = HEADER(1)
LET a.data# = HEADER(2)
LET a.bss# = HEADER(3)
LET a.syms# = HEADER(4)
LET a.entry# = HEADER(5)
LET a.trsize# = HEADER(6)
LET a.drsize# = HEADER(7)

REM LET POINTER# = orig#
REM LET info# = LA.read#(POINTER#, 4)
REM LET POINTER# = POINTER# + 4#
REM LET a.text# = LA.read#(POINTER#, 4)
REM LET POINTER# = POINTER# + 4#
REM LET a.data# = LA.read#(POINTER#, 4)
REM LET POINTER# = POINTER# + 4#
REM LET a.bss# = LA.read#(POINTER#, 4)
REM LET POINTER# = POINTER# + 4#
REM LET a.syms# = LA.read#(POINTER#, 4)
REM LET POINTER# = POINTER# + 4#
REM LET a.entry# = LA.read#(POINTER#, 4)
REM LET POINTER# = POINTER# + 4#
REM LET a.trsize# = LA.read#(POINTER#, 4)
REM LET POINTER# = POINTER# + 4#
REM LET a.drsize# = LA.read#(POINTER#, 4)
END SUB

SUB HeaderWrite (orig#, a.text#, a.data#, a.bss#, a.syms#, a.entry#, a.trsize#, a.drsize#)

HEADER(1) = a.text#
HEADER(2) = a.data#
HEADER(3) = a.bss#
HEADER(4) = a.syms#
HEADER(5) = a.entry#
HEADER(6) = a.trsize#
HEADER(7) = a.drsize#
REM LET POINTER# = orig#
REM LET POINTER# = POINTER# + 4#
REM CALL LA.write(POINTER#, a.text#, 4)
REM LET POINTER# = POINTER# + 4#
REM CALL LA.write(POINTER#, a.data#, 4)
REM LET POINTER# = POINTER# + 4#
REM CALL LA.write(POINTER#, a.bss#, 4)
REM LET POINTER# = POINTER# + 4#
REM CALL LA.write(POINTER#, a.syms#, 4)
REM LET POINTER# = POINTER# + 4#
REM CALL LA.write(POINTER#, a.entry#, 4)
REM LET POINTER# = POINTER# + 4#
REM CALL LA.write(POINTER#, a.trsize#, 4)
REM LET POINTER# = POINTER# + 4#
REM CALL LA.write(POINTER#, a.drsize#, 4)

END SUB

SUB InstDelete

LET N = INSTR(SHOW.SRCE$(CURRENT.INST), ".globl")
IF N > 0 THEN
    LET DOUBLED% = FALSE
    LET X$ = CUT$(MID$(SHOW.SRCE$(CURRENT.INST), N + 6))
    FOR K = 1 TO PROG.LEN
        IF K <> CURRENT.INST THEN
            LET M = INSTR(SHOW.SRCE$(K), ".globl")
            IF M > 0 AND INSTR(M + 6, SHOW.SRCE$(K), X$) > 0 THEN
                LET DOUBLED% = TRUE
            END IF
        END IF
    NEXT
    IF NOT DOUBLED% THEN
        LET SYM.POINTER# = SYM.TAB.POINTER#(X$)
        CALL LA.write(SYM.POINTER# + 4#, 1#, 4)
        LET TAKE.OUT% = TRUE
        FOR K = 1 TO PROG.LEN
            IF DISPLABEL.LIST$(K) = X$ THEN
                LET TAKE.OUT% = FALSE
            END IF
        NEXT
        FOR K = 1 TO PROG.LEN
            IF LABEL.LIST$(K) = X$ THEN
                LET TAKE.OUT% = TRUE
            END IF
        NEXT
        IF TAKE.OUT% THEN
            CALL SymTabDelete(X$)
        END IF
    END IF
END IF
LET DISPLABEL$ = DISPLABEL.LIST$(CURRENT.INST)
LET OTHER.REFERENCES% = FALSE
IF DISPLABEL$ <> "" THEN
    FOR K = 1 TO PROG.LEN
        IF K <> CURRENT.INST THEN
            IF DISPLABEL.LIST$(K) = DISPLABEL$ THEN
                LET OTHER.REFERENCES% = TRUE
            END IF
        END IF
    NEXT
    CALL TRelTabDelete(CURRENT.INST)
    IF NOT OTHER.REFERENCES% THEN
        CALL SymTabDelete(DISPLABEL$)
    END IF
END IF

LET LABEL$ = LABEL.LIST$(CURRENT.INST)
IF LABEL$ <> "" THEN
   
    LET OTHER.REFERENCES% = FALSE
    FOR K = 1 TO PROG.LEN
        IF K <> CURRENT.INST AND DISPLABEL.LIST$(K) = LABEL$ THEN
            LET OTHER.REFERENCES% = TRUE
        END IF
    NEXT
   
    IF OTHER.REFERENCES% THEN
        LET SYM.POINTER# = SYM.TAB.POINTER#(LABEL$)
        IF SYM.POINTER# = -1# THEN
            CALL SymTabInsert(LABEL$, 1, 0#)
        ELSE
            LET n.value# = LA.read#(SYM.POINTER# + 8#, 4)  'Need this later
            CALL LA.write(SYM.POINTER# + 8#, 0#, 4)
        END IF
        FOR K% = 1 TO PROG.LEN
            LET IPREL.FLAG% = ((DISPLABEL.CODE(K%) MOD 2) = 1)
            IF IPREL.FLAG% AND DISPLABEL.LIST$(K%) = LABEL$ THEN
                IF K% <> CURRENT.INST THEN
                    CALL TRelTabInsert(K%, SYM.POINTER#)
                END IF
                LET BYTES.BACK = DISPLABEL.CODE(K%) \ 16
                LET DWIDTH = DISP.WIDTH(DISPLABEL.CODE(K%))
                LET POINTER# = INST.ADR#(K% + 1) - BYTES.BACK
                LET X# = LOAD.ORIGIN# - INST.ADR#(K% + 1)
                CALL LA.write(POINTER#, X#, -DWIDTH)
            ELSEIF DISPLABEL.LIST$(K%) = LABEL$ THEN
                LET BYTES.BACK = DISPLABEL.CODE(K%) \ 16
                LET DWIDTH = DISP.WIDTH(DISPLABEL.CODE(K%))
                LET POINTER# = INST.ADR#(K% + 1) - BYTES.BACK
                LET X# = LA.read#(POINTER#, DWIDTH)
                LET X# = X# - n.value#
                CALL LA.write(POINTER#, X#, DWIDTH)
            END IF
        NEXT
    ELSE
        CALL SymTabDelete(LABEL$)
        LET SYM.POINTER# = SYM.TAB.POINTER#(LABEL$)
        IF SYM.POINTER# > -1# THEN
            CALL LA.write(SYM.POINTER# + 8#, 0#, 4)
            REM Address no longer valid.
        END IF
    END IF
END IF


LET DROP.BYTES = INST.ADR#(CURRENT.INST + 1) - INST.ADR#(CURRENT.INST)

FOR J = SRC.LINE(CURRENT.INST) TO SRC.LINE(PROG.LEN + 1) - 1
    LET RAW.SRCE$(J) = RAW.SRCE$(J + 1)
NEXT

FOR J = CURRENT.INST TO PROG.LEN + 1
    LET INST.ADR#(J) = INST.ADR#(J + 1)
    LET SRC.LINE(J) = SRC.LINE(J + 1) - 1
NEXT

FOR J = CURRENT.INST TO PROG.LEN
        LET DISPLABEL.CODE(J) = DISPLABEL.CODE(J + 1)
        LET DISPLABEL.LIST$(J) = DISPLABEL.LIST$(J + 1)
        LET LABEL.LIST$(J) = LABEL.LIST$(J + 1)
        LET SHOW.SRCE$(J) = SHOW.SRCE$(J + 1)
NEXT

LET DWIDTH = 0
IF CURRENT.INST > 1 AND DISPLABEL.CODE(CURRENT.INST - 1) > 0 THEN
    IF DISPLABEL.CODE(CURRENT.INST - 1) MOD 2 = 1 OR -1 THEN
        LET DISPLABEL.CODE(CURRENT.INST - 1) = DISPLABEL.CODE(CURRENT.INST - 1) + 16 * DROP.BYTES
        LET DWIDTH = DISP.WIDTH(DISPLABEL.CODE(CURRENT.INST - 1))
        LET BYTES.BACK# = DISPLABEL.CODE(CURRENT.INST - 1) \ 16
    END IF
    REM Fix DISPLABEL.CODE but leave jump displacement wrong.
    REM Fixing that might exceed the one byte limit.
    REM It will now be off by +DROP.BYTES.
END IF

LET PROG.LEN = PROG.LEN - 1

CALL AoutSlide(CURRENT.INST, -DROP.BYTES)

IF DWIDTH > 0 THEN  'Now fix the jump displacement.
    LET DISP.CODE = DISPLABEL.CODE(CURRENT.INST - 1)
    IF DISP.CODE MOD 2 = 1 THEN
        LET BYTES.BACK# = DISP.CODE \ 16
        REM DISP.LABEL.CODE gets changed by AoutSlide
        REM so BYTES.BACK# is stale unless refreshed.
        LET POINTER# = INST.ADR#(CURRENT.INST) - BYTES.BACK#
        LET DISP.VALUE# = LA.read#(POINTER#, DWIDTH) - CDBL(DROP.BYTES)
        CALL LA.write(POINTER#, DISP.VALUE#, DWIDTH)
    END IF
END IF

END SUB

SUB InstInsert (RAW.CODE$, PACKED.CODE$, MNEMONIC$, LABEL$, DISPLABEL$, DISPCODE, COMMENT.FIELD$, HX$, NEW.BYTES)
REM
REM  CURRENT.INST is a global variable.
REM

    IF NEW.BYTES = 0 AND LEN(HX$) > 0 THEN
        LET NEW.BYTES = LEN(HX$) \ 2
    END IF
    IF NEW.BYTES > 0 THEN
        CALL AoutSlide(CURRENT.INST, NEW.BYTES)
    END IF
    FOR J = PROG.LEN + 1 TO CURRENT.INST STEP -1
        LET INST.ADR#(J + 1) = INST.ADR#(J)
        LET SRC.LINE(J + 1) = SRC.LINE(J) + 1
    NEXT
    FOR J = SRC.LINE(PROG.LEN + 1) - 1 TO SRC.LINE(CURRENT.INST) STEP -1
        LET RAW.SRCE$(J + 1) = RAW.SRCE$(J)
    NEXT
   
    LET FIXUP$ = LABEL$ + CHR$(9) + MNEMONIC$ + " " + RAW.CODE$
    LET INSERT.CODE$ = FIXUP$ + COMMENT.FIELD$
    LET RAW.SRCE$(SRC.LINE(CURRENT.INST)) = INSERT.CODE$

    IF MNEMONIC$ = "" THEN
        LET LBL.FIELD$ = LEFT$(LABEL$ + SPACE$(13), 13)
    ELSE
        LET LBL.FIELD$ = LEFT$(LABEL$, 3) + SPACE$(4)
        LET LBL.FIELD$ = LEFT$(LBL.FIELD$, 4)
    END IF
   
    IF MNEMONIC$ = ".=" OR MNEMONIC$ = ".equ" THEN
        CALL DefInsert(LABEL$, RAW.CODE$)
        LET LABEL$ = ""
        REM Reseting LABEL$ to nul, keeps it from being the
        REM label of a line.  It may still function in assembler
        REM code like a "#define" macro expression.
    END IF

    FOR J = PROG.LEN TO CURRENT.INST STEP -1
        LET DISPLABEL.CODE(J + 1) = DISPLABEL.CODE(J)
        LET DISPLABEL.LIST$(J + 1) = DISPLABEL.LIST$(J)
        LET LABEL.LIST$(J + 1) = LABEL.LIST$(J)
        LET SHOW.SRCE$(J + 1) = SHOW.SRCE$(J)
    NEXT
    LET DISPLABEL.LIST$(CURRENT.INST) = DISPLABEL$
    LET LABEL.LIST$(CURRENT.INST) = LABEL$
    LET DISPLABEL.CODE(CURRENT.INST) = DISPCODE
    LET INST.ADR#(CURRENT.INST) = INST.ADR#(CURRENT.INST) - NEW.BYTES
    IF CURRENT.INST > 1 AND DISPLABEL.CODE(CURRENT.INST - 1) > 0 THEN
        LET DISPLABEL.CODE(CURRENT.INST - 1) = DISPLABEL.CODE(CURRENT.INST - 1) - 16 * NEW.BYTES
    END IF
    IF CURRENT.INST > 1 AND NEW.BYTES > 0 AND INST.ADR#(CURRENT.INST) = INST.ADR#(CURRENT.INST - 1) THEN
    END IF
    IF CURRENT.INST > 1 AND DISPLABEL.LIST$(CURRENT.INST - 1) <> "" THEN
        IF DISPLABEL.CODE(CURRENT.INST - 1) MOD 2 = 1 THEN
            LET BYTES.BACK = DISPLABEL.CODE(CURRENT.INST - 1) \ 16
            LET DWIDTH = DISP.WIDTH(DISPLABEL.CODE(CURRENT.INST - 1))
            LET POINTER# = INST.ADR#(CURRENT.INST) - BYTES.BACK
            LET X# = LA.read#(POINTER#, DWIDTH)
            LET X# = X# + NEW.BYTES
            CALL LA.write(POINTER#, X#, DWIDTH)
        END IF
    END IF
   
    IF LEFT$(PACKED.CODE$, 1) = "[" THEN
        LET GAP$ = ""
    ELSEIF LEN(MNEMONIC$) > 2 THEN
        LET GAP$ = " "
    ELSE
        LET GAP$ = SPACE$(4 - LEN(MNEMONIC$))
    END IF

    LET COMMA.SPOT = INSTR(PACKED.CODE$, ",")
    LET TOTAL$ = LBL.FIELD$ + MNEMONIC$ + GAP$ + PACKED.CODE$

    IF LEN(TOTAL$) < 19 THEN
        LET SHOW.SRCE$(CURRENT.INST) = TOTAL$ + ";"
    ELSEIF COMMA.SPOT > 0 AND COMMA.SPOT + LEN(MNEMONIC$ + GAP$) < 15 THEN
        LET SHOW.SRCE$(CURRENT.INST) = LBL.FIELD$ + MNEMONIC$ + GAP$ + LEFT$(PACKED.CODE$, COMMA.SPOT) + ";" + MID$(PACKED.CODE$, COMMA.SPOT + 1) + ";"
    ELSEIF LEN(MNEMONIC$ + GAP$) < 15 AND LEN(PACKED.CODE$) < 16 THEN
        LET SHOW.SRCE$(CURRENT.INST) = LBL.FIELD$ + MNEMONIC$ + GAP$ + ";" + PACKED.CODE$ + ";"
    ELSEIF COMMA.SPOT > 0 THEN
        LET SHOW.SRCE$(CURRENT.INST) = LBL.FIELD$ + MNEMONIC$ + GAP$ + ";" + LEFT$(PACKED.CODE$, COMMA.SPOT) + ";" + MID$(PACKED.CODE$, COMMA.SPOT + 1) + ";"
    ELSE
        LET SHOW.SRCE$(CURRENT.INST) = LBL.FIELD$ + MNEMONIC$ + GAP$ + ";" + "..;"
    END IF
REM
REM  Insert new text
REM
LET POINTER# = INST.ADR#(CURRENT.INST)
IF HX$ <> "" THEN
    FOR K% = 1 TO NEW.BYTES
        LET value# = HexVal#(MID$(HX$, 2 * K% - 1, 2))
        CALL LA.write(POINTER#, value#, 1)
        LET POINTER# = POINTER# + 1
    NEXT
ELSEIF MNEMONIC$ = ".db" THEN
    LET JUNK = MemStore(POINTER#, RAW.CODE$, 1)
ELSEIF MNEMONIC$ = ".dw" OR MNEMONIC$ = ".d2b" THEN
    LET JUNK = MemStore(POINTER#, RAW.CODE$, 2)
ELSEIF MNEMONIC$ = ".dd" OR MNEMONIC$ = ".d4b" THEN
    LET JUNK = MemStore(POINTER#, RAW.CODE$, 4)
END IF
LET PROG.LEN = PROG.LEN + 1
REM
REM
REM CURRENT.INST now points to the newly inserted instruction
REM
REM If LABEL$ is already listed in the tables remove it from them.
REM  I.e. call SymTabDelete
IF LABEL$ <> "" THEN
    REM  SymTabDelete will not delete a global symbol.
    CALL SymTabDelete(LABEL$)
    LET SYM.POINTER# = SYM.TAB.POINTER#(LABEL$)
    IF SYM.POINTER# > -1# THEN
        LET FIXED.ADDRESS# = INST.ADR#(CURRENT.INST) - LOAD.ORIGIN#
        CALL LA.write(SYM.POINTER# + 8#, FIXED.ADDRESS#, 4)
    END IF
    LET NOT.REALLY.EXTERNAL.FLAG% = FALSE
    FOR K% = 1 TO PROG.LEN
        IF DISPLABEL.LIST$(K%) = LABEL$ THEN
            LET ADRLEN = DISP.WIDTH(DISPLABEL.CODE(K%))
            IF DISPLABEL.CODE(K%) MOD 2 = 1 THEN    ' DISPLABEL is IP relative
                LET POINTER# = INST.ADR#(K% + 1) - DISPLABEL.CODE(K%) \ 16
                LET X# = INST.ADR#(CURRENT.INST) - INST.ADR#(K% + 1)
                CALL LA.write(POINTER#, X#, -ADRLEN)
                CALL TRelTabDelete(K%)
            ELSE                              ' DISPLABEL is absolute
                'Location of displacement bytes is not necessarily
                ' just the last four of the instruction
                LET POINTER# = INST.ADR#(K% + 1) - DISPLABEL.CODE(K%) \ 16
                LET X# = LA.read#(POINTER#, -ADRLEN)
                LET NEW.DISPLACEMENT# = INST.ADR#(CURRENT.INST) - LOAD.ORIGIN#
                LET X# = X# + NEW.DISPLACEMENT#
                CALL LA.write(POINTER#, X#, -ADRLEN)
                REM  Leave it in the tables
            END IF
        END IF
    NEXT
END IF
REM
REM Check DISPLABEL$. If it occurs on LABEL.LIST$() then
REM     just fix the jump offset or the displacement in the machine code.
REM     If it doesn't then add it to the tables.
REM
IF DISPLABEL$ <> "" THEN
    LET SYMTABPOINTER# = SYM.TAB.POINTER#(DISPLABEL$)
    LET IPREL.FLAG% = (DISPCODE MOD 2 = 1)
    LET WIDTH.BYTES = DISP.WIDTH(DISPCODE)
    LET BYTES.BACK = DISPCODE \ 16
    LET LOCAL.LINE.NUM% = 0
    FOR K% = 1 TO PROG.LEN
        IF LABEL.LIST$(K%) = DISPLABEL$ THEN
            LET LOCAL.LINE.NUM% = K%
        END IF
    NEXT
    IF IPREL.FLAG% THEN
        LET STORECODE = 1
    ELSE
        LET STORECODE = 5       '
    END IF
    IF SYMTABPOINTER# = -1# THEN
        IF LOCAL.LINE.NUM% = 0 THEN
            CALL SymTabInsert(DISPLABEL$, STORECODE, 0#)
            LET SYMTABPOINTER# = SYM.TAB.POINTER#(DISPLABEL$)
        END IF
    END IF
    IF IPREL.FLAG% THEN
        LET X# = LOAD.ORIGIN# - INST.ADR#(CURRENT.INST + 1)
    ELSE
        LET POINTER# = INST.ADR#(CURRENT.INST + 1) - BYTES.BACK
        LET X# = LA.read#(POINTER#, -WIDTH.BYTES)
        CALL TRelTabInsert(CURRENT.INST, SYMTABPOINTER#)
    END IF
    IF LOCAL.LINE.NUM% > 0 THEN
        IF IPREL.FLAG% THEN
            LET X# = INST.ADR#(LOCAL.LINE.NUM%) - INST.ADR#(CURRENT.INST + 1)
        ELSE
            LET X# = X# + INST.ADR#(LOCAL.LINE.NUM%) - LOAD.ORIGIN#
            REM Assuming labels are in the text segment.
        END IF
    ELSE
        IF IPREL.FLAG% THEN
            CALL TRelTabInsert(CURRENT.INST, SYMTABPOINTER#)
        END IF
    END IF
    LET POINTER# = INST.ADR#(CURRENT.INST + 1) - BYTES.BACK
    CALL LA.write(POINTER#, X#, -WIDTH.BYTES)
END IF

IF MNEMONIC$ = ".globl" THEN
    LET SYM.POINTER# = SYM.TAB.POINTER#(PACKED.CODE$)
    IF SYM.POINTER# = -1# THEN
        LET SPOT# = 0
        FOR K = 1 TO PROG.LEN
            IF LABEL.LIST$(K) = PACKED.CODE$ THEN
                LET SPOT# = INST.ADR#(K) - LOAD.ORIGIN#
            END IF
        NEXT
        CALL SymTabInsert(PACKED.CODE$, 5, SPOT#)
    ELSE
        CALL LA.write(SYM.POINTER# + 4#, 5#, 4)
    END IF
ELSEIF MNEMONIC$ = ".text" THEN
ELSEIF MNEMONIC$ = ".data" THEN
ELSEIF MNEMONIC$ = ".rodata" THEN
ELSEIF MNEMONIC$ = ".286" THEN
    CALL DefInsert("ARCH", "286")
    CALL DefInsert("INSTSET", "286")
END IF
REM
REM
LET CURRENT.INST = CURRENT.INST + 1
REM
REM
END SUB

SUB LEwrite (FILENUM, X#, N.BYTES)
LET Y# = X#
FOR K = 1 TO N.BYTES
    LET Z# = Mod2N#(Y#, 8)
    PRINT #FILENUM, CHR$(Z#);
    LET Y# = (Y# - Z#) / 256#
NEXT
END SUB

SUB ProcessLine (RAW.CODE$, INSCOM.FLAG%, ANOTHER%)
REM
REM  INSCOM.FLAG: TRUE if Insertion into the program is intended
REM               FALSE if Execution is intended.
REM  ANOTHER%: Carries a return value of TRUE ordinarily.
REM            It is FALSE when a Quit or Blank error message is received
REM            from LineParse.  This allows a user to escape from
REM            command or insert mode.
REM
REM  This function is called only from the X86 module.
    LET ANOTHER% = TRUE
    LET CODE$ = RAW.CODE$
    DO                    '  Repeat until syntax is OK.
        LET ERM$ = ""
        CALL LineParse(MNEMONIC$, INDEX, LABEL$, PF$, CODE$, COMMENT.FIELD$, ERM$)
        IF ERM$ = "Quit!" THEN
            LET ANOTHER% = FALSE
            COLOR 15, 1
            EXIT SUB
        ELSEIF ERM$ = "Blank!" THEN
            LET ANOTHER% = FALSE
            COLOR 15, 1
            EXIT SUB          'Blank means Quit during Interactive modes
        ELSEIF ERM$ = "Help!" THEN
            IF INSCOM.FLAG% THEN
                LET ERM$ = " Insert instruction:"
            ELSE
                LET ERM$ = " Enter command:"
            END IF
            COLOR 15, 1
        ELSEIF ERM$ = "" THEN 'Second chance for ERM$ to get non-Null value
            IF INDEX > 0 THEN
                CALL Print24(" Assembling ...")
                CALL InstAssemble(CODE$, INDEX, PF$, PACK$, DISPLABEL$, DISPCODE, HX$, ERM$)
            ELSEIF INDEX < 0 THEN
                CALL DirectiveParse(LABEL$, CODE$, INDEX, PACK$, nhb, ERM$)
                IF ERM$ = "" AND INDEX <> -1 AND INDEX <> -2 AND NOT INSCOM.FLAG% THEN
                    LET ERM$ = "Directives must be inserted."
                END IF
            END IF
        END IF
        IF ERM$ <> "" THEN  'Get user's correction.
            Print24 (ERM$)
            LET FIXUP$ = LABEL$ + SPACE$(1) + MNEMONIC$ + SPACE$(1) + CODE$
            IF INSCOM.FLAG% THEN
                LET PMPT$ = "*"
                COLOR 15, 1
            ELSE
                LET PMPT$ = ">"
                COLOR 14, 1
            END IF
            LET CODE$ = GetCorrection$(PMPT$, FIXUP$)
            LOCATE , 2
            PRINT SPACE$(38);
            LOCATE , 2
            LET RAW.CODE$ = CODE$ + COMMENT.FIELD$
        END IF
    LOOP WHILE ERM$ <> ""
    COLOR 15, 1
REM
REM   Syntax is OK now. So proceed:
REM
    IF INDEX = 0 AND LABEL$ = "" AND INSCOM.FLAG% AND CURRENT.INST = PROG.LEN + 1 THEN
        LET RAW.SRCE$(SRC.LINE(CURRENT.INST)) = RAW.CODE$
        LET SRC.LINE(CURRENT.INST) = SRC.LINE(CURRENT.INST) + 1
    ELSEIF INDEX = 0 AND LABEL$ = "" AND INSCOM.FLAG% THEN
        REM SRC.LINE(PROG.LEN + 1)     ' First empty line
        FOR J = SRC.LINE(PROG.LEN + 1) TO SRC.LINE(CURRENT.INST) STEP -1
            LET RAW.SRCE$(J + 1) = RAW.SRCE$(J)
        NEXT
        FOR J = PROG.LEN + 1 TO CURRENT.INST + 1
            LET SRC.LINE(J) = SRC.LINE(J) + 1
        NEXT
        LET INSERT.SRCE$ = LABEL$ + " " + MNEMONIC$ + " " + CODE$ + COMMENT.FIELD$
        LET RAW.SRCE$(SRC.LINE(CURRENT.INST + 1) - 1) = INSERT.SRCE$
    ELSEIF INSCOM.FLAG% THEN
        CALL InstInsert(CODE$, PACK$, MNEMONIC$, LABEL$, DISPLABEL$, DISPCODE, COMMENT.FIELD$, HX$, nhb)
    ELSEIF INDEX >= 0 THEN
        LET NEW.BYTES = LEN(HX$) \ 2
        CALL Print24(" Enter command:")
        REM  'Fill in the label if there was one.
        IF DISPLABEL$ <> "" THEN
            FOR K2 = 1 TO PROG.LEN
                IF DISPLABEL$ = LABEL.LIST$(K2) THEN
                    LET DWIDTH = DISP.WIDTH(DISPCODE)
                    LET BYTES.BACK = DISPCODE \ 16
                    IF DISPCODE MOD 2 = 1 THEN
                        LET IMM.NUM# = INST.ADR#(K2) - (STORE.ORIGIN# + CDBL(LEN(HX$) / 2))
                    ELSE
                        LET IMM.NUM# = HexVal#(BYTE.SWAP$(MID$(HX$, LEN(HX$) - 2 * BYTES.BACK + 1, 2 * DWIDTH)))
                        LET IMM.NUM# = IMM.NUM# + INST.ADR#(K2) - descrVec(DS).BaseAdd
                    END IF
                    MID$(HX$, LEN(HX$) - 2 * BYTES.BACK + 1, 2 * DWIDTH) = BYTE.SWAP$(HexStr$(IMM.NUM#, 2 * DWIDTH))
                END IF
            NEXT
        END IF
        LET SAVE.ARCHNUM = ARCHNUM
        LET ARCHNUM = 10
        LET STORE.COMMAND$ = HX$ + "00000000"
        LET EIP# = STORE.ORIGIN#
        LET REPEAT = 0
        LET BYTE = 0
        DO
            CALL Fetchexec(REPEAT, BYTE)   'execute the user's command
        LOOP WHILE REPEAT <> 0
        LET ARCHNUM = SAVE.ARCHNUM
    ELSEIF INDEX = -1 OR INDEX = -2 THEN    ' .= , .EQU
        CALL DefInsert(LABEL$, CODE$)
    END IF
END SUB

SUB ShortenJump (INST.K AS INTEGER, ERM$)
LET DISPCODE = DISPLABEL.CODE(INST.K) MOD 8
LET ERM$ = ""
IF DISPCODE = 3 THEN
    LET OLDSIZE = 2
ELSEIF DISPCODE = 5 THEN
    LET OLDSIZE = 4
ELSE
    LET ERM$ = "Already short"
    EXIT SUB
END IF

LET DISP.POINTER# = INST.ADR#(INST.K + 1) - DISPLABEL.CODE(INST.K) \ 16
LET POINTER# = DISP.POINTER# - 1    'No ModRM or SIB bytes??
LET B1$ = HexStr$(LA.read#(POINTER#, 1), 2)
LET X# = LA.read#(DISP.POINTER#, -OLDSIZE)
IF X# > 127# OR X# < -128# THEN
    LET ERM$ = "Jump isn't short"
    EXIT SUB
END IF
CALL WidthFix(DISP.POINTER#, 1)

LET DISPLABEL.CODE(INST.K) = DISPLABEL.CODE(INST.K) - OLDSIZE

LET UNCOND.FLAG = (B1$ = "E9")
IF UNCOND.FLAG THEN
    LET Y# = HexVal#("EB")
    LET SLIDE.BYTES = OLDSIZE - 1
ELSEIF LEFT$(B1$, 1) = "8" THEN
    LET B2$ = "7" + MID$(B1$, 2)
    LET Y# = HexVal#(B2$)
    LET POINTER# = POINTER# - 1
    CALL DispAdjust(DISP.POINTER#, -1)
    LET DISP.POINTER# = DISP.POINTER# - 1
    LET DISPLABEL.CODE(INST.K) = DISPLABEL.CODE(INST.K) + 16   'One byte adjust
    LET SLIDE.BYTES = OLDSIZE
END IF
CALL LA.write(POINTER#, Y#, 1)
CALL LA.write(DISP.POINTER#, X#, 1)


CALL AoutSlide(INST.K + 1, -SLIDE.BYTES)
END SUB

FUNCTION SYM.TAB.POINTER# (SYM$)
REM
CALL HeaderRead(LOAD.ORIGIN#, a.text#, a.data#, a.bss#, a.syms#, a.entry#, a.trsize#, a.drsize#)
REM
LET textoff# = 0#     ' 32 = sizeof(struct exec)
LET dataoff# = textoff# + a.text#
LET treloff# = dataoff# + a.data#
LET dreloff# = treloff# + a.trsize#
LET SYMOFF# = dreloff# + a.drsize#
LET stroff# = SYMOFF# + a.syms#
LET POINTER# = LOAD.ORIGIN# + stroff#
LET A.strs# = LA.read#(POINTER#, 4)
LET EOF.off# = stroff# + A.strs#
REM
REM
REM Find the symbol in the string table.
REM
LET SYM.OFFSET# = SYMOFF#
LET STRING.FOUND = FALSE
WHILE SYM.OFFSET# < stroff# AND NOT STRING.FOUND
    LET SYM.POINTER# = SYM.OFFSET# + LOAD.ORIGIN#   'Pointer to that Entry
    LET STR.OFFSET# = LA.read(SYM.POINTER#, 4) + stroff#    '4 bytes?
    LET POINTER# = STR.OFFSET# + LOAD.ORIGIN#
    IF SYM$ = GetTableString$(POINTER#) THEN
        LET STRING.FOUND = TRUE
    ELSE
        LET SYM.OFFSET# = SYM.OFFSET# + 12
    END IF
WEND
IF STRING.FOUND THEN
    LET SYM.TAB.POINTER# = SYM.POINTER#
ELSE
    LET SYM.TAB.POINTER# = -1#
END IF
END FUNCTION

SUB SymTabDelete (SYM$)
REM Remove a symbol from the symbol table.
REM (Don't worry about the relocation tables here.)
CALL HeaderRead(LOAD.ORIGIN#, a.text#, a.data#, a.bss#, a.syms#, a.entry#, a.trsize#, a.drsize#)
REM
LET textoff# = 0#     ' 32 = sizeof(struct exec)
LET dataoff# = textoff# + a.text#
LET treloff# = dataoff# + a.data#
LET dreloff# = treloff# + a.trsize#
LET SYMOFF# = dreloff# + a.drsize#
LET stroff# = SYMOFF# + a.syms#
LET A.strs# = LA.read#(LOAD.ORIGIN# + stroff#, 4)
LET EOF.off# = stroff# + A.strs#
REM
LET POINTER.SYM.OUT# = SYM.TAB.POINTER#(SYM$)
IF POINTER.SYM.OUT# > -1# THEN  'String found
    LET STR.TABOFFSET.STR.OUT# = LA.read(POINTER.SYM.OUT#, 4)
    LET OFFSET.SYM.OUT# = POINTER.SYM.OUT# - LOAD.ORIGIN#
    LET OFFSET.STR.OUT# = STR.TABOFFSET.STR.OUT# + stroff#
    LET POINTER.STR.OUT# = OFFSET.STR.OUT# + LOAD.ORIGIN#
ELSE
    REM This isn't an error.
    REM InstDelete makes this call without knowing.
    EXIT SUB
END IF
REM
REM  Check to see that symbol is not typed global.
REM
LET X# = LA.read#(POINTER.SYM.OUT# + 4#, 4)
IF X# = 5 OR X# = 7 THEN     'Symbol is global.
    EXIT SUB
END IF
REM
REM     Remove it From the String Table
REM
LET N.BYTES.OUT# = LEN(SYM$) + 1   ' Null Terminus add a byte
LET SOURCE.OFFSET# = OFFSET.STR.OUT# + N.BYTES.OUT#
LET POINTER# = POINTER.STR.OUT#
WHILE SOURCE.OFFSET# < EOF.off#
    LET SOURCE.POINTER# = SOURCE.OFFSET# + LOAD.ORIGIN#
    LET X# = LA.read#(SOURCE.POINTER#, 1)
    CALL LA.write(POINTER#, X#, 1)
    LET POINTER# = POINTER# + 1#
    LET SOURCE.OFFSET# = SOURCE.OFFSET# + 1#
WEND
REM
REM     Reduce String Table Length Indicator
REM
LET A.strs# = A.strs# - N.BYTES.OUT#
CALL LA.write(LOAD.ORIGIN# + stroff#, A.strs#, 4)
REM
LET EOF.off# = EOF.off# - N.BYTES.OUT#
REM
REM Revise symbol table offsets into the string table.
REM
LET OFFSET# = SYMOFF#
WHILE OFFSET# < stroff#
    LET POINTER# = OFFSET# + LOAD.ORIGIN#
    LET Y# = LA.read#(POINTER#, 4)
        IF Y# >= STR.TABOFFSET.STR.OUT# THEN
            LET Y# = Y# - N.BYTES.OUT#
            CALL LA.write(POINTER#, Y#, 4)
        END IF
    LET OFFSET# = OFFSET# + 12#
WEND

    REM
    REM Remove symbol from symbol table.
    REM
LET POINTER# = POINTER.SYM.OUT#
LET INDEX.SYM.OUT# = (OFFSET.SYM.OUT# - SYMOFF#) / 12#
REM
LET N.BYTES.OUT# = 12#
LET SOURCE.OFFSET# = OFFSET.SYM.OUT# + N.BYTES.OUT#
WHILE SOURCE.OFFSET# < EOF.off#
    LET SOURCE.POINTER# = SOURCE.OFFSET# + LOAD.ORIGIN#
    LET X# = LA.read#(SOURCE.POINTER#, 1)
    CALL LA.write(POINTER#, X#, 1)
    LET POINTER# = POINTER# + 1#
    LET SOURCE.OFFSET# = SOURCE.OFFSET# + 1#
WEND
LET a.syms# = a.syms# - 12#
LET stroff# = stroff# - 12#
LET EOF.off# = EOF.off# - 12#

REM
REM      Decrement those entries in the data relocation table
REM    pointing past the removed symbol table entry
REM
LET OFFSET# = dreloff#
WHILE OFFSET# < SYMOFF#
    LET POINTER# = OFFSET# + LOAD.ORIGIN# + 4#
    LET Y# = LA.read#(POINTER#, 3)
    LET Z% = LA.read#(OFFSET# + LOAD.ORIGIN# + 7#, 1)
    LET SYMPOINTER.FLAG% = (Z% AND 8)
    IF Y# > INDEX.SYM.OUT# AND SYMPOINTER.FLAG% THEN
        LET Y# = Y# - 1#
        CALL LA.write(POINTER#, Y#, 3)
        LET OFFSET# = OFFSET# + 8#
    ELSE
        LET OFFSET# = OFFSET# + 8#
    END IF
WEND


REM
REM      Decrement those entries in the text relocation table
REM    pointing past the removed symbol table entry
REM
LET OFFSET# = treloff#
WHILE OFFSET# < dreloff#
    LET POINTER# = OFFSET# + LOAD.ORIGIN# + 4#
    LET Y# = LA.read#(POINTER#, 3)
    LET Z% = LA.read#(OFFSET# + LOAD.ORIGIN# + 7#, 1)
    LET SYMPOINTER.FLAG% = (Z% AND 8)
    IF Y# > INDEX.SYM.OUT# AND SYMPOINTER.FLAG% THEN
        LET Y# = Y# - 1#
        CALL LA.write(POINTER#, Y#, 3)
        LET OFFSET# = OFFSET# + 8#
    ELSEIF Y# = INDEX.SYM.OUT# AND SYMPOINTER.FLAG% THEN
        LET Y# = 4#
        CALL LA.write(POINTER#, Y#, 3)
        LET Z% = Z% AND (&HF7)
        CALL LA.write(OFFSET# + LOAD.ORIGIN# + 7#, CDBL(Z%), 1)
        LET OFFSET# = OFFSET# + 8#
    ELSE
        LET OFFSET# = OFFSET# + 8#
    END IF
WEND

CALL HeaderWrite(LOAD.ORIGIN#, a.text#, a.data#, a.bss#, a.syms#, a.entry#, a.trsize#, a.drsize#)
END SUB

SUB SymTabInsert (SYM$, STORAGE.CODE, STORAGE.VALUE#)
REM
REM    Insert a symbol into the Symbol Table and into the String table
REM    If the symbol is already there, overwrite old STORAGE.CODE and
REM    STORAGE.VALUE# entries.
REM
REM  N.EXT = 1   '  Code for default external reference
REM          5   '           .global external reference
REM
CALL HeaderRead(LOAD.ORIGIN#, a.text#, a.data#, a.bss#, a.syms#, a.entry#, a.trsize#, a.drsize#)
REM
LET textoff# = 0#     ' 32 = sizeof(struct exec)
LET dataoff# = textoff# + a.text#
LET treloff# = dataoff# + a.data#
LET dreloff# = treloff# + a.trsize#
LET SYMOFF# = dreloff# + a.drsize#
LET stroff# = SYMOFF# + a.syms#
LET A.strs# = LA.read#(LOAD.ORIGIN# + stroff#, 4)
LET EOF.off# = stroff# + A.strs#

LET SYM.POINTER# = SYM.TAB.POINTER#(SYM$)

IF SYM.POINTER# > -1# THEN    ' If string found then
    CALL LA.write(SYM.POINTER# + 4#, CDBL(STORAGE.CODE), 4)
    CALL LA.write(SYM.POINTER# + 8#, STORAGE.VALUE#, 4)
ELSE
REM
REM         Add String to the End of the String Table
REM
    LET OFFSET.STR.IN# = EOF.off#
    LET POINTER.STR.IN# = EOF.off# + LOAD.ORIGIN#
    LET OFFSET.SYM.IN# = stroff#
    LET POINTER.SYM.IN# = stroff# + LOAD.ORIGIN#
    LET STR.OFFSET.STR.IN# = OFFSET.STR.IN# - stroff#
    REM
    LET IN.BYTES# = LEN(SYM$) + 1   ' Null Terminals add a byte
    LET A.strs# = A.strs# + IN.BYTES# 'Increase String Table Length Indicator
    CALL LA.write(LOAD.ORIGIN# + stroff#, A.strs#, 4)
    LET EOF.off# = EOF.off# + IN.BYTES#
    LET DEST.OFFSET# = EOF.off# - 1
    LET DEST.POINTER# = DEST.OFFSET# + LOAD.ORIGIN#
    LET CHAR.IN% = 0    'Null terminator
    LET X# = CDBL(CHAR.IN%)
    LET K = IN.BYTES#
    CALL LA.write(DEST.POINTER#, X#, 1)
    WHILE DEST.OFFSET# > OFFSET.STR.IN#
        LET DEST.OFFSET# = DEST.OFFSET# - 1#
        LET DEST.POINTER# = DEST.POINTER# - 1#
        LET K = K - 1
        LET CHAR.IN% = ASC(MID$(SYM$, K, 1))
        LET X# = CDBL(CHAR.IN%)
        CALL LA.write(DEST.POINTER#, X#, 1)
    WEND
    REM
    REM Add symbol to symbol table.
    REM
    REM First make room for the entry
    REM
    LET IN.BYTES# = 12#
    LET SOURCE.OFFSET# = EOF.off#
    LET OLD.stroff# = stroff#
    LET EOF.off# = EOF.off# + IN.BYTES#
    LET stroff# = stroff# + IN.BYTES#
    LET a.syms# = a.syms# + IN.BYTES#
    LET DEST.OFFSET# = EOF.off#
    LET DEST.POINTER# = DEST.OFFSET# + LOAD.ORIGIN#
    WHILE SOURCE.OFFSET# > OLD.stroff#
        LET SOURCE.OFFSET# = SOURCE.OFFSET# - 1#
        LET SOURCE.POINTER# = SOURCE.OFFSET# + LOAD.ORIGIN#
        LET DEST.POINTER# = DEST.POINTER# - 1#
        LET X# = LA.read#(SOURCE.POINTER#, 1)
        CALL LA.write(DEST.POINTER#, X#, 1)
    WEND
    REM
    REM Then Add Entry to the End of the Symbol Table
    REM
    LET ADR.SYM.IN# = POINTER.SYM.IN#
    CALL LA.write(ADR.SYM.IN#, STR.OFFSET.STR.IN#, 4)
    CALL LA.write(ADR.SYM.IN# + 4#, CDBL(STORAGE.CODE), 4)
    CALL LA.write(ADR.SYM.IN# + 8#, STORAGE.VALUE#, 4)

    CALL HeaderWrite(LOAD.ORIGIN#, a.text#, a.data#, a.bss#, a.syms#, a.entry#, a.trsize#, a.drsize#)

END IF

REM


END SUB

SUB TextSlide (INST.K AS INTEGER, SLIDE.BYTES)
SHARED TOP.BSTACK, BROKEN.STACK()
REM
REM If SLIDE.BYTES is positive then text beginning with the byte indexed
REM by INST.K is moved upwards in memory by SLIDE.BYTES.
REM  Conversely if SLIDE.BYTES is negative motion is downward;
REM  the magnitude of SLIDE.BYTES should not be greater than
REM the length of the preceding instruction.
REM It is assumed that all INST.ADR#() entries are correct
REM both on entry and on leaving, as well as all DISPLABEL.CODE() entries.
REM
REM  Displacements in the text are assumed to be correct unless they are
REM  on BROKEN.STACK(), and in that case they are assumed to be off by 128.
REM
IF SLIDE.BYTES = 0 THEN
    EXIT SUB
END IF
CALL HeaderRead(LOAD.ORIGIN#, a.text#, a.data#, a.bss#, a.syms#, a.entry#, a.trsize#, a.drsize#)
REM
REM LET textoff# = 0#     ' 32 = sizeof(struct exec)
LET dataoff# = 0# + a.text#
LET treloff# = dataoff# + a.data#
LET dreloff# = treloff# + a.trsize#
LET SYMOFF# = dreloff# + a.drsize#
LET stroff# = SYMOFF# + a.syms#
LET POINTER# = LOAD.ORIGIN# + stroff#
LET A.strs# = LA.read#(POINTER#, 4)
LET EOF.off# = stroff# + A.strs#
REM
REM Update Text Relocation Table
REM
LET slide.addr# = INST.ADR#(INST.K) - LOAD.ORIGIN# - textoff#
FOR K = 0 TO a.trsize# - 8 STEP 8
    LET POINTER# = LOAD.ORIGIN# + treloff# + CDBL(K)
    LET X# = LA.read#(POINTER#, 4)
    LET STOR.CODE = LA.read#(POINTER# + 7#, 1)
    IF STOR.CODE MOD 2 = 0 THEN       'Fix absolute offsets.
        REM Check 2nd field of relocation table.
        LET N.WIDTH = DISP.WIDTH(STOR.CODE)
        IF STOR.CODE AND &H8 THEN  ' If the symbol is in the table
            LET r.symndx = LA.read(POINTER# + 4#, 3)
            LET SYMPOINTER# = SYMOFF# + CDBL(12 * r.symndx) + 8#
            LET n.value# = LA.read(SYMPOINTER#, 4)
        ELSE
            LET n.value# = 0#
        END IF
        IF slide.addr# <= n.value# THEN
            LET Z# = LA.read#(X#, N.WIDTH)
            LET Z# = Z# + SLIDE.BYTES
            CALL LA.write(X#, Z#, N.WIDTH)
            REM LET n.value# = n.value# + SLIDE.BYTES
            REM CALL LA.write(SYMPOINTER#, n.value#, 4)
        END IF
    REM ELSEIF STOR.CODE MOD 2 = 1 THEN
    REM Field referenced is IP relative and is fixed
    REM in the IP relative section later in this SUB.
    END IF
    IF X# > slide.addr# THEN
        LET X# = X# + SLIDE.BYTES
        CALL LA.write(POINTER#, X#, 4)
    ELSEIF X# = slide.addr# THEN
        IF INST.ADR#(INST.K) = INST.ADR#(INST.K - 1) THEN
            REM Target may be a preceding label
            REM so don't move the reference.
        ELSE
            LET X# = X# + SLIDE.BYTES
            CALL LA.write(POINTER#, X#, 4)
        END IF
    END IF
NEXT
REM
REM Update Data Relocation Table
REM
LET slide.addr# = INST.ADR#(INST.K) - LOAD.ORIGIN# - dataoff#
LET slide.addr# = INST.ADR#(INST.K) - LOAD.ORIGIN# - textoff#
REM
REM     Won't know which is correct without examples of a data
REM     relocation table.
REM
FOR K = 0 TO a.drsize# - 8 STEP 8
    REM Fix each entry
    LET POINTER# = LOAD.ORIGIN# + dreloff# + K
    LET X# = LA.read#(POINTER#, 4)
    IF X# > slide.addr# THEN
        LET X# = X# + SLIDE.BYTES
        CALL LA.write(POINTER#, X#, 4)
    ELSEIF X# = slide.addr# THEN
        IF INST.ADR#(INST.K) = INST.ADR#(INST.K - 1) THEN
            REM Target may be a preceding label
            REM so don't move the reference.
        ELSE
            LET X# = X# + SLIDE.BYTES
            CALL LA.write(POINTER#, X#, 4)
        END IF
    END IF
NEXT
REM
REM     Update the Symbol Table
REM
FOR K = 0 TO a.syms# - 12 STEP 12
    LET POINTER# = LOAD.ORIGIN# + SYMOFF# + CDBL(K)
    LET Y# = LA.read#(POINTER# + 4#, 4)
    IF Y# = 5# OR Y# = 7# THEN      ' Cases observed to this point only!!
        LET POINTER# = POINTER# + 8#
        LET X# = LA.read(POINTER#, 4)
        IF X# > slide.addr# THEN
            LET X# = X# + SLIDE.BYTES
            CALL LA.write(POINTER#, X#, 4)
        ELSEIF X# = slide.addr# THEN
            IF INST.ADR#(INST.K) = INST.ADR#(INST.K - 1) THEN
                REM Target may be a preceding label
                REM so don't move the reference.
            ELSE
                REM LET SYMTABPOINTER# = SYM.TAB.POINTER#(DISPLABEL.LIST$(K))
                LET X# = X# + SLIDE.BYTES
                CALL LA.write(POINTER#, X#, 4)
            END IF
        END IF
    END IF
NEXT

REM
REM Change relative jumps (Fix local symbols.) in the text section.
REM
FOR K = 1 TO PROG.LEN
    LET IPREL.FLAG% = (DISPLABEL.CODE(K) MOD 2 = 1)
    LET BYTES.BACK# = CDBL(DISPLABEL.CODE(K) \ 16)
    LET SYM$ = DISPLABEL.LIST$(K)
    IF SYM$ <> "" THEN
        LET N.BYTES = DISP.WIDTH(DISPLABEL.CODE(K))
        LET POINTER# = INST.ADR#(K + 1) - BYTES.BACK#
        LET X# = LA.read#(POINTER#, -N.BYTES) ' probably for jumps this is ok
        IF IPREL.FLAG% THEN
            LET BJ = 0
            FOR J = 1 TO TOP.BSTACK
                IF K = BROKEN.STACK(J) THEN
                    LET X# = X# + 128#
                    LET BJ = J
                ELSEIF K = -BROKEN.STACK(J) THEN
                    LET X# = X# - 128#
                    LET BJ = J
                END IF
            NEXT
            IF K + 1 < INST.K THEN   'Jump from point stationary
                IF X# + INST.ADR#(K + 1) < INST.ADR#(INST.K) THEN
                    REM No Problem.
                ELSEIF X# + INST.ADR#(K + 1) = INST.ADR#(INST.K) THEN
                    IF X# + INST.ADR#(K + 1) = INST.ADR#(INST.K - 1) THEN
                        REM Presume that the jump is aimed
                        REM  at a preceding label-only line.
                        REM Don't do anything.
                    END IF
                ELSE                  'Jump to point moved
                    LET X# = X# + SLIDE.BYTES
                    IF N.BYTES = 1 THEN
                        IF X# > 127# THEN
                            LET X# = X# - 128
                            IF BJ > 0 THEN
                                LET BROKEN.STACK(BJ) = K
                            ELSE
                                LET TOP.BSTACK = TOP.BSTACK + 1
                                LET BROKEN.STACK(TOP.BSTACK) = K
                            END IF
                        ELSEIF X# < -128# THEN
                            LET X# = X# + 128#
                            IF BJ > 0 THEN
                                LET BROKEN.STACK(BJ) = -K
                            ELSE
                                LET TOP.BSTACK = TOP.BSTACK + 1
                                LET BROKEN.STACK(TOP.BSTACK) = -K
                            END IF
                        END IF
                    CALL LA.write(POINTER#, X#, -N.BYTES)
                    END IF
                END IF
            ELSE                       'Jump from point moved
                IF X# + INST.ADR#(K + 1) > INST.ADR#(INST.K) THEN
                    REM ' To point moved also.  No Problem
                ELSEIF X# + INST.ADR#(K + 1) = INST.ADR#(INST.K) AND INST.ADR#(INST.K) > INST.ADR#(INST.K - 1) THEN
                    REM No preceding label. Still no problem
                ELSE
                    REM Presume that if
                    REM IF X# + INST.ADR#(K + 1) = INST.ADR#(INST.K) AND
                    REM         INST.ADR#(INST.K) = INST.ADR#(INST.K - 1)
                    REM THEN 'Jump to point is stationary and we have a problem
                    LET X# = X# - SLIDE.BYTES
                    IF N.BYTES = 1 THEN
                        IF X# > 127# THEN
                            LET X# = X# - 128
                            IF BJ > 0 THEN
                                LET BROKEN.STACK(BJ) = K
                            ELSE
                                LET TOP.BSTACK = TOP.BSTACK + 1
                                LET BROKEN.STACK(TOP.BSTACK) = K
                            END IF
                        ELSEIF X# < -128# THEN
                            LET X# = X# + 128#
                            IF BJ > 0 THEN
                                LET BROKEN.STACK(BJ) = -K
                            ELSE
                                LET TOP.BSTACK = TOP.BSTACK + 1
                                LET BROKEN.STACK(TOP.BSTACK) = -K
                            END IF
                        END IF
                    END IF
                    CALL LA.write(POINTER#, X#, -N.BYTES)
                END IF
            END IF
        ELSE
            REM Not needed here fortunately! Stack overflow!
            REM LET SYMTABPOINTER# = SYM.TAB.POINTER#(SYM$)
            REM LET X# = X# + SLIDE.BYTES
            REM CALL LA.write(POINTER#, X#, N.BYTES)
        END IF
    END IF
NEXT
REM
REM         Perform the slide (finally!)
REM
LET INSERT.OFFSET# = INST.ADR#(INST.K) - LOAD.ORIGIN#
REM
IF SLIDE.BYTES > 0 THEN
    FOR K = EOF.off# - 1# TO INSERT.OFFSET# STEP -1
        LET POINTER# = LOAD.ORIGIN# + CDBL(K)
        LET X# = LA.read#(POINTER#, 1)
        CALL LA.write(POINTER# + CDBL(SLIDE.BYTES), X#, 1)
    NEXT
ELSE
    FOR K = INSERT.OFFSET# TO EOF.off# - 1#
        LET POINTER# = LOAD.ORIGIN# + CDBL(K)
        LET X# = LA.read#(POINTER#, 1)
        CALL LA.write(POINTER# + CDBL(SLIDE.BYTES), X#, 1)
    NEXT
END IF
REM
FOR K = INST.K TO PROG.LEN + 1
    LET INST.ADR#(K) = INST.ADR#(K) + SLIDE.BYTES
NEXT
REM
IF INST.K > 1 THEN   'This will fail if a huge number of bytes are inserted.
    IF DISPLABEL.CODE(INST.K - 1) > 0 THEN
        LET DISPLABEL.CODE(INST.K - 1) = DISPLABEL.CODE(INST.K - 1) + 16 * SLIDE.BYTES
    END IF
END IF

REM Adjust a.text#
LET a.text# = a.text# + SLIDE.BYTES
CALL HeaderWrite(LOAD.ORIGIN#, a.text#, a.data#, a.bss#, a.syms#, a.entry#, a.trsize#, a.drsize#)

END SUB

SUB TRelTabDelete (INST.NUM AS INTEGER)
CALL HeaderRead(LOAD.ORIGIN#, a.text#, a.data#, a.bss#, a.syms#, a.entry#, a.trsize#, a.drsize#)
REM
REM     Remove the entry from text relocation table which references
REM         the instruction whose number is INST.NUM
REM
LET textoff# = 0#     ' 32 = sizeof(struct exec)
LET dataoff# = textoff# + a.text#
LET treloff# = dataoff# + a.data#
LET dreloff# = treloff# + a.trsize#
LET SYMOFF# = dreloff# + a.drsize#
LET stroff# = SYMOFF# + a.syms#
LET POINTER# = LOAD.ORIGIN# + stroff#
LET A.strs# = LA.read#(POINTER#, 4)
LET EOF.off# = stroff# + A.strs#
REM
REM
LET OFFSET# = treloff#
WHILE OFFSET# < dreloff#
    LET POINTER# = OFFSET# + LOAD.ORIGIN#
    LET Y# = LA.read#(POINTER#, 4) + LOAD.ORIGIN#
    IF INST.ADR#(INST.NUM) <= Y# AND Y# < INST.ADR#(INST.NUM + 1) THEN
        LET DEST.POINTER# = POINTER#
        LET SOURCE.OFFSET# = OFFSET# + 8#
        WHILE SOURCE.OFFSET# < EOF.off#
            LET SOURCE.POINTER# = SOURCE.OFFSET# + LOAD.ORIGIN#
            LET X# = LA.read#(SOURCE.POINTER#, 1)
            CALL LA.write(DEST.POINTER#, X#, 1)
            LET DEST.POINTER# = DEST.POINTER# + 1#
            LET SOURCE.OFFSET# = SOURCE.OFFSET# + 1#
        WEND
        LET EOF.off# = EOF.off# - 8#
        LET stroff# = stroff# - 8#
        LET SYMOFF# = SYMOFF# - 8#
        LET dreloff# = dreloff# - 8#
        LET a.trsize# = a.trsize# - 8#
    ELSE
        LET OFFSET# = OFFSET# + 8#
    END IF
WEND

CALL HeaderWrite(LOAD.ORIGIN#, a.text#, a.data#, a.bss#, a.syms#, a.entry#, a.trsize#, a.drsize#)
END SUB

SUB TRelTabInsert (INST.NUM AS INTEGER, SYMTABPOINTER#)
CALL HeaderRead(LOAD.ORIGIN#, a.text#, a.data#, a.bss#, a.syms#, a.entry#, a.trsize#, a.drsize#)
REM
LET textoff# = 0#     ' 32 = sizeof(struct exec)
LET dataoff# = textoff# + a.text#
LET treloff# = dataoff# + a.data#
LET dreloff# = treloff# + a.trsize#
LET SYMOFF# = dreloff# + a.drsize#
LET stroff# = SYMOFF# + a.syms#
LET A.strs# = LA.read#(LOAD.ORIGIN# + stroff#, 4)
LET EOF.off# = stroff# + A.strs#
REM
REM Assume that SYMTABPOINTER# = -1 iff RELOC.CODE MOD 2 = 0
LET DISPCODE = DISPLABEL.CODE(INST.NUM)
   
    IF SYMTABPOINTER# > -1# THEN
        LET INDEX.SYM.IN# = (SYMTABPOINTER# - SYMOFF# - LOAD.ORIGIN#) / 12#
        LET DISPCODE = (DISPCODE OR 8)
        LET n.value# = INDEX.SYM.IN#
    ELSE
        LET n.value# = 4#       ' Is this always true?
    END IF

LET BYTES.BACK# = DISPCODE \ 16
LET RELOC.CODE = DISPCODE MOD 16


LET IN.BYTES# = 8#
LET SOURCE.OFFSET# = EOF.off#
LET EOF.off# = EOF.off# + IN.BYTES#
LET stroff# = stroff# + IN.BYTES#
LET SYMOFF# = SYMOFF# + IN.BYTES#

LET OFFSET# = dreloff#
LET PLACE.FOUND% = FALSE
WHILE OFFSET# > treloff# AND NOT PLACE.FOUND%
    LET TEXT.ADR# = LA.read#(LOAD.ORIGIN# + OFFSET# - 8#, 4) + LOAD.ORIGIN#
    IF TEXT.ADR# < INST.ADR#(INST.NUM) THEN
        LET PLACE.FOUND% = TRUE
    ELSE
        LET OFFSET# = OFFSET# - 8#
    END IF
WEND
IF PLACE.FOUND% THEN
    LET OFFSET.NEW.ENTRY# = OFFSET#
ELSE
    LET OFFSET.NEW.ENTRY# = treloff#
END IF
LET dreloff# = dreloff# + IN.BYTES#
LET DEST.OFFSET# = EOF.off#
LET DEST.POINTER# = DEST.OFFSET# + LOAD.ORIGIN#
WHILE SOURCE.OFFSET# > OFFSET.NEW.ENTRY#
    LET SOURCE.OFFSET# = SOURCE.OFFSET# - 1#
    LET SOURCE.POINTER# = SOURCE.OFFSET# + LOAD.ORIGIN#
    LET DEST.POINTER# = DEST.POINTER# - 1#
    LET X# = LA.read#(SOURCE.POINTER#, 1)
    CALL LA.write(DEST.POINTER#, X#, 1)
WEND

REM
LET a.trsize# = a.trsize# + IN.BYTES#
LET ADR.NEW.ENTRY# = OFFSET.NEW.ENTRY# + LOAD.ORIGIN#
LET value# = INST.ADR#(INST.NUM + 1) - BYTES.BACK# - LOAD.ORIGIN#
CALL LA.write(ADR.NEW.ENTRY#, value#, 4)
CALL LA.write(ADR.NEW.ENTRY# + 4#, n.value#, 3)
CALL LA.write(ADR.NEW.ENTRY# + 7#, CDBL(RELOC.CODE), 1)


CALL HeaderWrite(LOAD.ORIGIN#, a.text#, a.data#, a.bss#, a.syms#, a.entry#, a.trsize#, a.drsize#)

END SUB

SUB WidthFix (p#, NEW.SIZE)
REM
REM Make sure that the Text Relocation table indicates that
REM at the address P# in the text there is a jump with NEW.SIZE width
REM NEW.SIZE is in bytes.
REM
CALL HeaderRead(LOAD.ORIGIN#, a.text#, a.data#, a.bss#, a.syms#, a.entry#, a.trsize#, a.drsize#)
REM
LET textoff# = 32     ' 32 = sizeof(struct exec)
LET dataoff# = textoff# + a.text#
LET treloff# = dataoff# + a.data#

REM
REM  Text Relocation Table
REM
FOR K = 0 TO a.trsize# - 8 STEP 8
    LET POINTER# = LOAD.ORIGIN# + treloff# + K
    LET X# = LA.read#(POINTER#, 4)
    IF NEW.SIZE = 1 THEN
        FIX.BITS = 0
    ELSE
        FIX.BITS = NEW.SIZE
    END IF
    IF X# = p# THEN
        LET POINTER2# = POINTER# + 7#
        LET STOR.CODE% = LA.read#(POINTER2#, 1)
        LET STOR.CODE% = (STOR.CODE% AND (NOT 6)) OR FIX.BITS
        LET Y# = STORE.CODE%
        CALL LA.write(POINTER2#, Y#, 1)
    END IF
NEXT
END SUB

SUB WriteDotO (BASE.NAME$)
REM
REM Write the object file either a.out or ELF
REM a.out = 2
REM ELF = 3
REM COM = 4
CALL HeaderRead(LOAD.ORIGIN#, a.text#, a.data#, a.bss#, a.syms#, a.entry#, a.trsize#, a.drsize#)
REM
REM LOAD.ORIGIN# must be aligned to whatever degree of roundness is used.
LET textoff# = 0#     ' 32 = sizeof(struct exec)
LET dataoff# = textoff# + a.text#
LET treloff# = dataoff# + a.data#
LET dreloff# = treloff# + a.trsize#
LET SYMOFF# = dreloff# + a.drsize#
LET stroff# = SYMOFF# + a.syms#
LET POINTER# = LOAD.ORIGIN# + stroff#
LET A.strs# = LA.read#(POINTER#, 4)
LET EOF.off# = stroff# + A.strs#
LET EOF.POINTER# = EOF.off# + LOAD.ORIGIN#
LET POINTER# = LOAD.ORIGIN#
IF USE.O = 4 THEN
    OPEN BASE.NAME$ + ".COM" FOR OUTPUT AS #3
ELSE
    OPEN BASE.NAME$ + ".O" FOR OUTPUT AS #3
END IF
IF USE.O = 2 THEN
    FOR K = 0 TO 7  ' First print out the a.out header.
        LET t& = HEADER(K)
        FOR J = 1 TO 4
            PRINT #3, CHR$(t& MOD 256);
            LET t& = t& \ 256
        NEXT
    NEXT
    WHILE POINTER# < EOF.POINTER#
        LET C = LA.read#(POINTER#, 1)
        PRINT #3, CHR$(C);
        LET POINTER# = POINTER# + 1
    WEND
ELSEIF USE.O = 4 THEN
    LET POINTER# = LOAD.ORIGIN# + textoff#
    WHILE POINTER# < LOAD.ORIGIN# + dreloff#
        LET C = LA.read#(POINTER#, 1)
        PRINT #3, CHR$(C);
        LET POINTER# = POINTER# + 1
    WEND
ELSEIF USE.O = 3 THEN
    LET POINTER# = textoff# + LOAD.ORIGIN#
    LET OLDPOINTER# = POINTER# - CDBL(&H34)
    DIM SHINDEX(10), SHOFF#(10), SHLEN#(10)
    FOR K = 0 TO 10
        LET SHLEN#(K) = 0#
        LET SHINDEX(K) = 0
    NEXT
    LET K = 1
    LET SHOFF#(0) = 0#
    LET SCOUNT = 0
    WHILE POINTER# < textoff# + a.data# + a.text# + LOAD.ORIGIN#

        LET POINTER# = INST.ADR#(K)
        IF POINTER# = textoff# + a.data# + a.text# + LOAD.ORIGIN# THEN
            LET SHLEN#(SCOUNT) = POINTER# - OLDPOINTER#
            LET OLDPOINTER# = POINTER#
            LET SHOFF#(SCOUNT + 1) = AdAlign#(SHOFF#(SCOUNT) + SHLEN#(SCOUNT), 16)
            LET SCOUNT = SCOUNT + 1
        ELSEIF INSTR(SHOW.SRCE$(K), ".data") > 0 THEN
            LET SHLEN#(SCOUNT) = POINTER# - OLDPOINTER#
            LET OLDPOINTER# = POINTER#
            LET SHOFF#(SCOUNT + 1) = AdAlign#(SHOFF#(SCOUNT) + SHLEN#(SCOUNT), 4)
            LET SCOUNT = SCOUNT + 1
            LET SHINDEX(SCOUNT) = 3
        ELSEIF INSTR(SHOW.SRCE$(K), ".rodata") > 0 THEN
            LET SHLEN#(SCOUNT) = POINTER# - OLDPOINTER#
            LET OLDPOINTER# = POINTER#
            LET SHOFF#(SCOUNT + 1) = AdAlign#(SHOFF#(SCOUNT) + SHLEN#(SCOUNT), 4)
            LET SCOUNT = SCOUNT + 1
            LET SHINDEX(SCOUNT) = 6
        ELSEIF INSTR(SHOW.SRCE$(K), ".text") > 0 AND INSTR(SHOW.SRCE$(K), ".rel") = 0 THEN
            LET SHLEN#(SCOUNT) = POINTER# - OLDPOINTER#
            LET OLDPOINTER# = POINTER#
            LET SHOFF#(SCOUNT + 1) = AdAlign#(SHOFF#(SCOUNT) + SHLEN#(SCOUNT), 4)
            LET SCOUNT = SCOUNT + 1
            LET SHINDEX(SCOUNT) = 1
        ELSEIF POINTER# > textoff# + LOAD.ORIGIN# AND SCOUNT = 0 THEN
            LET POINTER# = textoff# + LOAD.ORIGIN#  'Backup momentarily
            LET SHLEN#(SCOUNT) = POINTER# - OLDPOINTER#
            LET OLDPOINTER# = POINTER#
            LET SHOFF#(SCOUNT + 1) = AdAlign#(SHOFF#(SCOUNT) + SHLEN#(SCOUNT), 4)
            LET SCOUNT = SCOUNT + 1
            LET SHINDEX(SCOUNT) = 1
        END IF
        LET K = K + 1
        IF SCOUNT > 4 THEN
        REM Write error message and exit
        END IF

    WEND
    LET boffset# = SHOFF#(SCOUNT)
    LET shstrset# = boffset# + CDBL(&H10)                   ' + .comment
    LET shoffset# = AdAlign#(shstrset# + CDBL(&H4D), 16)    ' + .shstrab
    LET smoffset# = AdAlign#(shoffset# + CDBL(&H28 * 11), 16)
    LET stsze# = (a.syms# / 12# + CDBL(SCOUNT)) * 16#   '16 bytes vs. 12
    LET stoffset# = smoffset# + stsze#
    LET reloffset# = AdAlign#(stoffset# + A.strs# - 3#, 8)
REM ELF Header
    REM e_ident[16]
        PRINT #3, CHR$(127);           ' EI_MAG0
        PRINT #3, "ELF";               ' EI_MAG 1,2,3
        PRINT #3, CHR$(1);             ' EI_CLASS = ELFCLASS32
        PRINT #3, CHR$(1);             ' EI_DATA = Little Endian
        PRINT #3, CHR$(1);             ' EI_VERSION =  EV_CURRENT
        PRINT #3, STRING$(9, 0);       ' EI_PAD
    PRINT #3, CHR$(1) + CHR$(0);       ' e_type = ET_REL = Relocatable file
    PRINT #3, CHR$(3) + CHR$(0);       ' e_machine = EM_386
    PRINT #3, CHR$(1) + STRING$(3, 0); ' e_version = EV_CURRENT
    PRINT #3, STRING$(4, 0);           ' e_entry Entry point
    PRINT #3, STRING$(4, 0);           ' e_phoff Program header
    CALL LEwrite(3, shoffset#, 4)      ' e_shoff Section header offset
    PRINT #3, STRING$(4, 0);           ' e_flags Flags
    PRINT #3, CHR$(&H34) + CHR$(0);    ' e_ehsize Size of Elf header
    PRINT #3, STRING$(2, 0);           ' e_phentsize
    PRINT #3, STRING$(2, 0);           ' e_phnum
    PRINT #3, CHR$(&H28) + CHR$(0);    ' e_shentsize
    PRINT #3, CHR$(11) + CHR$(0);      ' e_shnum  Number of entries
    PRINT #3, CHR$(8) + CHR$(0);       ' e_shstrndx  String table index
REM .text, .data, and .rodata sections
LET POINTER# = textoff# + LOAD.ORIGIN#
FOR J = 1 TO SCOUNT - 1
    SELECT CASE SHINDEX(J)
        CASE 1
            REM .text section
            LET C = SHOFF#(J) - SHOFF#(J - 1) - SHLEN#(J - 1)
            PRINT #3, STRING$(C, 0);
            LET oldtoff# = 0#
            FOR K = 1 TO J - 1
                LET oldtoff# = oldtoff# + SHLEN#(K)
            NEXT
            LET textpointoffset# = 0#
            WHILE textpointoffset# < SHLEN#(J)
                LET newrelocoffset# = SHLEN#(J) ' Plus infinity
                LET relocindex# = 0#
                WHILE relocindex# < a.trsize#
                    LET RPOINTER# = treloff# + LOAD.ORIGIN# + relocindex#
                    LET relocoffset# = LA.read#(RPOINTER#, 4) - oldtoff#
                    IF relocoffset# > textpointoffset# - 1# AND relocoffset# < newrelocoffset# THEN
                        LET newrelocoffset# = relocoffset#
                        LET newrelocindex# = relocindex#
                    END IF
                    LET relocindex# = relocindex# + 8#
                WEND
                LET relocoffset# = newrelocoffset#
                LET relocindex# = newrelocindex#
                WHILE textpointoffset# < relocoffset#
                    LET C = LA.read#(POINTER#, 1)
                    PRINT #3, CHR$(C);
                    LET POINTER# = POINTER# + 1#
                    LET textpointoffset# = textpointoffset# + 1#
                WEND
                IF textpointoffset# < SHLEN#(J) THEN
                    LET RPOINTER# = treloff# + LOAD.ORIGIN# + relocindex# + 7#
                    LET C = LA.read#(RPOINTER#, 1)
                    IF C MOD 2 = 1 THEN
                        PRINT #3, CHR$(&HFC) + STRING$(3, &HFF);
                    ELSEIF (C AND 8) = 0 THEN
                        LET K = J   ' Does this work if .text isn't first?
                        LET X# = LA.read#(POINTER#, 4)
                        WHILE X# - SHLEN#(K) >= 0
                            LET X# = X# - SHLEN#(K)
                            LET K = K + 1
                        WEND
                        CALL LEwrite(3, X#, 4)
                    ELSE
                        LET X# = LA.read#(POINTER#, 4)
                        LET X# = X# - LA.read#(LA.read#(RPOINTER# - 3#, 3) * 12# + SYMOFF# + LOAD.ORIGIN# + 8#, 4)
                        CALL LEwrite(3, X#, 4)
                    END IF
                    LET POINTER# = POINTER# + 4#
                    LET textpointoffset# = textpointoffset# + 4#
                END IF
            WEND
        CASE 3
            REM .data section
            LET C = SHOFF#(J) - SHOFF#(J - 1) - SHLEN#(J - 1)
            PRINT #3, STRING$(C, 0);
            LET OLDPOINTER# = POINTER#
            WHILE POINTER# < SHLEN#(J) + OLDPOINTER#
                PRINT #3, CHR$(LA.read#(POINTER#, 1));
                LET POINTER# = POINTER# + 1#
            WEND
        CASE 6
            REM .rodata section
            LET C = SHOFF#(J) - SHOFF#(J - 1) - SHLEN#(J - 1)
            PRINT #3, STRING$(C, 0);
            LET OLDPOINTER# = POINTER#
            WHILE POINTER# < SHLEN#(J) + OLDPOINTER#
                PRINT #3, CHR$(LA.read#(POINTER#, 1));
                LET POINTER# = POINTER# + 1#
            WEND
    END SELECT
NEXT
LET C = SHOFF#(SCOUNT) - SHOFF#(SCOUNT - 1) - SHLEN#(SCOUNT - 1)
PRINT #3, STRING$(C, 0);
REM .bss section
REM .note section
REM     Leave blank for now
REM .comment  section                         Total length = 10H
    PRINT #3, "Edlinas 0.91" + STRING$(4, 0);
REM .shstrtab section           Hex offsets   Total length = 50H
    PRINT #3, CHR$(0);                 '0H
    PRINT #3, ".symtab" + CHR$(0);     '1H
    PRINT #3, ".strtab" + CHR$(0);     '9H
    PRINT #3, ".shstrtab" + CHR$(0);  '11H
    PRINT #3, ".text" + CHR$(0);      '1BH
    PRINT #3, ".rel.text" + CHR$(0);  '21H
    PRINT #3, ".data" + CHR$(0);      '2BH
    PRINT #3, ".bss" + CHR$(0);       '31H
    PRINT #3, ".note" + CHR$(0);      '36H
    PRINT #3, ".rodata" + CHR$(0);    '3CH
    PRINT #3, ".comment" + CHR$(0);   '44H
                                      '4DH
    LET RPOINTER# = shstrset# + CDBL(&H4D)
    LET C = AdAlign#(RPOINTER#, 16) - RPOINTER#
    PRINT #3, STRING$(C, 0);
REM Section Header Table
    'Null Entry
        PRINT #3, STRING$(&H28, 0);
    '.text section header
        LET SHINSHIN = SCOUNT
        FOR K = 1 TO SCOUNT
            IF SHINDEX(K) = 1 THEN
                LET SHINSHIN = K
            END IF
        NEXT
        PRINT #3, CHR$(&H1B) + STRING$(3, 0);  'sh_name
        PRINT #3, CHR$(1) + STRING$(3, 0);     'sh_type = SHT_PROGBITS
        PRINT #3, CHR$(6) + STRING$(3, 0);     'sh_flags
        PRINT #3, STRING$(4, 0);               'sh_addr
        CALL LEwrite(3, SHOFF#(SHINSHIN), 4)   'sh_offset
        CALL LEwrite(3, SHLEN#(SHINSHIN), 4)   'sh_size
        PRINT #3, STRING$(4, 0);               'sh_link
        PRINT #3, STRING$(4, 0);               'sh_info
        PRINT #3, CHR$(4) + STRING$(3, 0);     'sh_addralign
        PRINT #3, STRING$(4, 0);               'sh_entsize
    '.rel.text section header
        PRINT #3, CHR$(&H21) + STRING$(3, 0);  'sh_name
        PRINT #3, CHR$(9) + STRING$(3, 0);     'sh_type = SHT_REL
        PRINT #3, STRING$(4, 0);               'sh_flags = ignore this
        PRINT #3, STRING$(4, 0);               'sh_addr
        CALL LEwrite(3, reloffset#, 4)         'sh_offset
        CALL LEwrite(3, a.trsize#, 4)          'sh_size
        PRINT #3, CHR$(9) + STRING$(3, 0);     'sh_link  symbol table index
        PRINT #3, CHR$(1) + STRING$(3, 0);     'sh_info  text index
        PRINT #3, CHR$(4) + STRING$(3, 0);     'sh_addralign
        PRINT #3, CHR$(8) + STRING$(3, 0);     'sh_entsize  = 8
    '.data section header
        LET SHINSHIN = SCOUNT
        FOR K = 1 TO SCOUNT
            IF SHINDEX(K) = 3 THEN
                LET SHINSHIN = K
            END IF
        NEXT
        PRINT #3, CHR$(&H2B) + STRING$(3, 0);  'sh_name
        PRINT #3, CHR$(1) + STRING$(3, 0);     'sh_type = SHT_PROGBITS
        PRINT #3, CHR$(3) + STRING$(3, 0);     'sh_flags
        PRINT #3, STRING$(4, 0);               'sh_addr
        CALL LEwrite(3, SHOFF#(SHINSHIN), 4)   'sh_offset
        CALL LEwrite(3, SHLEN#(SHINSHIN), 4)   'sh_size
        PRINT #3, STRING$(4, 0);               'sh_link
        PRINT #3, STRING$(4, 0);               'sh_info
        PRINT #3, CHR$(4) + STRING$(3, 0);     'sh_addralign
        PRINT #3, STRING$(4, 0);               'sh_entsize
    '.bss section header
        PRINT #3, CHR$(&H31) + STRING$(3, 0);  'sh_name
        PRINT #3, CHR$(8) + STRING$(3, 0);     'sh_type = SHT_NOBITS
        PRINT #3, CHR$(3) + STRING$(3, 0);     'sh_flags
        PRINT #3, STRING$(4, 0);               'sh_addr
        CALL LEwrite(3, boffset#, 4)           'sh_offset
        CALL LEwrite(3, 0#, 4)                 'sh_size
        PRINT #3, STRING$(4, 0);               'sh_link
        PRINT #3, STRING$(4, 0);               'sh_info
        PRINT #3, CHR$(4) + STRING$(3, 0);     'sh_addralign
        PRINT #3, STRING$(4, 0);               'sh_entsize
    '.note section header
        PRINT #3, CHR$(&H36) + STRING$(3, 0);  'sh_name
        PRINT #3, CHR$(7) + STRING$(3, 0);     'sh_type = SHT_NOTE
        PRINT #3, STRING$(4, 0);               'sh_flags = ignore this
        PRINT #3, STRING$(4, 0);               'sh_addr
        CALL LEwrite(3, boffset#, 4)           'sh_offset
        CALL LEwrite(3, 0#, 4)                 'sh_size
        PRINT #3, STRING$(4, 0);               'sh_link
        PRINT #3, STRING$(4, 0);               'sh_info
        PRINT #3, CHR$(1) + STRING$(3, 0);     'sh_addralign
        PRINT #3, STRING$(4, 0);               'sh_entsize
    '.rodata section header
        LET SHINSHIN = SCOUNT
        FOR K = 1 TO SCOUNT
            IF SHINDEX(K) = 6 THEN
                LET SHINSHIN = K
            END IF
        NEXT
        PRINT #3, CHR$(&H3C) + STRING$(3, 0);  'sh_name
        PRINT #3, CHR$(1) + STRING$(3, 0);     'sh_type = SHT_PROGBITS
        PRINT #3, CHR$(2) + STRING$(3, 0);     'sh_flags
        PRINT #3, STRING$(4, 0);               'sh_addr
        CALL LEwrite(3, SHOFF#(SHINSHIN), 4)   'sh_offset
        CALL LEwrite(3, SHLEN#(SHINSHIN), 4)   'sh_size
        PRINT #3, STRING$(4, 0);               'sh_link
        PRINT #3, STRING$(4, 0);               'sh_info
        PRINT #3, CHR$(1) + STRING$(3, 0);     'sh_addralign
        PRINT #3, STRING$(4, 0);               'sh_entsize
    '.comment section header
        PRINT #3, CHR$(&H44) + STRING$(3, 0);  'sh_name
        PRINT #3, CHR$(1) + STRING$(3, 0);     'sh_type = SHT_PROGBITS
        PRINT #3, STRING$(4, 0);               'sh_flags = ignore this
        PRINT #3, STRING$(4, 0);               'sh_addr
        CALL LEwrite(3, boffset#, 4)           'sh_offset
        CALL LEwrite(3, 16#, 4)                'sh_size
        PRINT #3, STRING$(4, 0);               'sh_link
        PRINT #3, STRING$(4, 0);               'sh_info
        PRINT #3, CHR$(1) + STRING$(3, 0);     'sh_addralign
        PRINT #3, STRING$(4, 0);               'sh_entsize
    '.shstrab section header
        PRINT #3, CHR$(&H11) + STRING$(3, 0);  'sh_name
        PRINT #3, CHR$(3) + STRING$(3, 0);     'sh_type = SHT_STRTAB
        PRINT #3, STRING$(4, 0);               'sh_flags = ignore this
        PRINT #3, STRING$(4, 0);               'sh_addr
        CALL LEwrite(3, shstrset#, 4)          'sh_offset
        CALL LEwrite(3, CDBL(&H4D), 4)         'sh_size
        PRINT #3, STRING$(4, 0);               'sh_link
        PRINT #3, STRING$(4, 0);               'sh_info
        PRINT #3, CHR$(1) + STRING$(3, 0);     'sh_addralign
        PRINT #3, STRING$(4, 0);               'sh_entsize
    '.symtab section header
        PRINT #3, CHR$(1) + STRING$(3, 0);     'sh_name
        PRINT #3, CHR$(2) + STRING$(3, 0);     'sh_type = SHT_SYMTAB
        PRINT #3, STRING$(4, 0);               'sh_flags = ignore this
        PRINT #3, STRING$(4, 0);               'sh_addr
        CALL LEwrite(3, smoffset#, 4)          'sh_offset
        CALL LEwrite(3, stsze#, 4)             'sh_size
        PRINT #3, CHR$(&HA) + STRING$(3, 0);   'sh_link   String table index
        CALL LEwrite(3, CDBL(SCOUNT), 4)       'sh_info  # of local symbols
        PRINT #3, CHR$(4) + STRING$(3, 0);     'sh_addralign
        PRINT #3, CHR$(&H10) + STRING$(3, 0);  'sh_entsize  = 16
    '.strtab section header
        PRINT #3, CHR$(9) + STRING$(3, 0);     'sh_name
        PRINT #3, CHR$(3) + STRING$(3, 0);     'sh_type = SHT_STRTAB
        PRINT #3, STRING$(4, 0);               'sh_flags = ignore this
        PRINT #3, STRING$(4, 0);               'sh_addr
        CALL LEwrite(3, stoffset#, 4)          'sh_offset
        CALL LEwrite(3, A.strs# - 3#, 4)       'sh_size
        PRINT #3, STRING$(4, 0);               'sh_link
        PRINT #3, STRING$(4, 0);               'sh_info
        PRINT #3, CHR$(1) + STRING$(3, 0);     'sh_addralign
        PRINT #3, STRING$(4, 0);               'sh_entsize
REM .symtab section
    LET RPOINTER# = shoffset# + CDBL(&H28 * 11)
    LET C = AdAlign#(RPOINTER#, 16) - RPOINTER#
    PRINT #3, STRING$(C, 0);  'Align by 16
    PRINT #3, STRING$(16, 0); 'Null entry
    FOR K = 1 TO SCOUNT - 1
        LET sttype = 3
        LET stbind = 0
        LET stinfo = 16 * stbind + sttype
        PRINT #3, STRING$(12, 0);
        PRINT #3, CHR$(stinfo);
        PRINT #3, STRING$(1, 0);                        'st_other
        CALL LEwrite(3, CDBL(SHINDEX(K)), 2)            'st_shndx
    NEXT
    LET RPOINTER# = SYMOFF# + LOAD.ORIGIN#
    WHILE RPOINTER# < a.syms# + SYMOFF# + LOAD.ORIGIN#
        CALL LEwrite(3, LA.read#(RPOINTER#, 4) - 3#, 4) 'st_name
        LET C = LA.read#(RPOINTER# + 4#, 4)
        LET X# = LA.read#(RPOINTER# + 8#, 4)
        LET K = 1
        WHILE X# - SHLEN#(K) > -1#
            LET X# = X# - SHLEN#(K)
            LET K = K + 1
        WEND
        CALL LEwrite(3, X#, 4)  'st_value
        PRINT #3, STRING$(4, 0);   ' Need length         st_size
        IF C = C THEN            ' symbol is global
            LET stbind = 1               '(Do we have local symbols?)
        ELSE                     ' symbol is local
            LET stbind = 0
        END IF
        IF C = 5 THEN  ' symbol is function
            LET sttype = 2
        ELSEIF C = 4 THEN ' symbol is object
            LET sttype = 1
        ELSE
            LET sttype = 0
        END IF
        LET stinfo = 16 * stbind + sttype
        PRINT #3, CHR$(stinfo);
        PRINT #3, STRING$(1, 0);                        'st_other
        IF C = 5 OR C = 4 THEN              'st_shndx
            PRINT #3, CHR$(SHINDEX(K)) + STRING$(1, 0);
        ELSEIF C = 1 THEN
            PRINT #3, STRING$(2, 0);
        ELSE
            PRINT #3, STRING$(2, 0);
        END IF
        LET RPOINTER# = RPOINTER# + 12#
    WEND
REM .strtab section
    LET RPOINTER# = stroff# + LOAD.ORIGIN# + 4#
    PRINT #3, CHR$(0); 'Null entry
    WHILE RPOINTER# < EOF.POINTER#
        LET C = LA.read#(RPOINTER#, 1)
        PRINT #3, CHR$(C);
        LET RPOINTER# = RPOINTER# + 1#
    WEND
REM .rel.text section
    LET RPOINTER# = stoffset# + A.strs# - 3#
    LET C = AdAlign#(RPOINTER#, 8) - RPOINTER#
    PRINT #3, STRING$(C, 0);                    ' Align by 8
    LET RPOINTER# = treloff# + LOAD.ORIGIN#
    WHILE RPOINTER# < dreloff# + LOAD.ORIGIN#
        LET AOUTOFFSET# = LA.read#(RPOINTER#, 4)
        CALL LEwrite(3, AOUTOFFSET# - oldtoff#, 4)
        LET C = LA.read#(RPOINTER# + 7#, 1)
        IF C MOD 2 = 1 THEN
            PRINT #3, CHR$(2);
        ELSE
            PRINT #3, CHR$(1);
        END IF
        IF (C AND 8) > 0 THEN
            CALL LEwrite(3, LA.read#(RPOINTER# + 4#, 3) + CDBL(SCOUNT), 3)
        ELSE
    REM elseif  .data or .rodata THEN
        LET X# = LA.read#(AOUTOFFSET# + textoff# + LOAD.ORIGIN#, 4)
        LET K = 1
        WHILE X# - SHLEN#(K) > -1#
            LET X# = X# - SHLEN#(K)
            LET K = K + 1
        WEND
            PRINT #3, CHR$(K) + STRING$(2, 0);
        END IF
        LET RPOINTER# = RPOINTER# + 8#
    WEND
END IF
CLOSE #3
END SUB

