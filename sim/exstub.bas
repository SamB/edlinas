REM $INCLUDE: 'x86.bi'
DECLARE FUNCTION FLAGS# (N.BYTES!)
DECLARE SUB FLAG.STORE (value#, N.BYTES!)
DECLARE FUNCTION REGZ# (REG.NUM!)
DECLARE SUB load.data.seg (SREG!, desc AS DESCR, TSSload!)
DECLARE FUNCTION in.limits! (desc AS DESCR, Offst#, Size)
REM Decoding Functions
DECLARE FUNCTION RM.BITS! (DEC.NUM)
DECLARE FUNCTION REG.BITS (DEC.NUM!)
DECLARE FUNCTION REG# (REG.NUM!)
DECLARE FUNCTION REG.CODE! (REG.NUM!, Size!)
DECLARE FUNCTION WORD.SIZE! (OS!, WIDTH.BIT!)
DECLARE SUB REG.STORE (REG.NUM!, NUM#)
REM Execution Functions
DECLARE FUNCTION GetOffset# (SGRG!, ADS!, DEC.NUM!)
REM Simulator Functions
REM Conversion Functions
DECLARE FUNCTION BYTE.SWAP$ (HEX.STRING$)
DECLARE FUNCTION Mod2N# (NUM#, N!)

SUB check.brkpt (btype%, X#)
END SUB

SUB check.interrupts
END SUB

SUB checkpoint
END SUB

SUB Fetchexec (REPEAT, BYTE)
END SUB

SUB FLAG.STORE (value#, N.BYTES)
LET N.BITS = 8 * N.BYTES
LET FLAG$ = BitStr$(value#, N.BITS)
IF N.BYTES = 4 THEN
    LET FLAG$ = RIGHT$(FLAG$, 18)
    LET AC = VAL(LEFT$(FLAG$, 1))
    LET FLAG$ = MID$(FLAG$, 2)
    LET VM = VAL(LEFT$(FLAG$, 1))
    LET FLAG$ = MID$(FLAG$, 2)
    LET RF = VAL(LEFT$(FLAG$, 1))
    LET FLAG$ = MID$(FLAG$, 2)
END IF
    LET FLAG$ = RIGHT$(FLAG$, 14)
    LET NT = VAL(LEFT$(FLAG$, 1))
    LET FLAG$ = MID$(FLAG$, 2)
    LET IOPL = BinVal#(LEFT$(FLAG$, 2))
    LET FLAG$ = MID$(FLAG$, 3)
    LET OF = VAL(LEFT$(FLAG$, 1))
    LET FLAG$ = MID$(FLAG$, 2)
    LET DF = VAL(LEFT$(FLAG$, 1))
    LET FLAG$ = MID$(FLAG$, 2)
    LET IE = VAL(LEFT$(FLAG$, 1))
    LET FLAG$ = MID$(FLAG$, 2)
    LET TF = VAL(LEFT$(FLAG$, 1))
    LET FLAG$ = MID$(FLAG$, 2)
    LET SF = VAL(LEFT$(FLAG$, 1))
    LET FLAG$ = MID$(FLAG$, 2)
    LET ZF = VAL(LEFT$(FLAG$, 1))
    LET FLAG$ = MID$(FLAG$, 2)
    LET AF = VAL(LEFT$(FLAG$, 1))
    LET FLAG$ = MID$(FLAG$, 3)
    LET PF = VAL(LEFT$(FLAG$, 1))
    LET FLAG$ = MID$(FLAG$, 3)
    LET CF = VAL(LEFT$(FLAG$, 1))


END SUB

FUNCTION FLAGS# (N.BYTES)

IF N.BYTES = 4 THEN
    LET X# = AC * 2# ^ (18)
    LET X# = X# + VM * 2# ^ (17)
    LET X# = X# + RF * 2# ^ (16)
ELSE
    LET X# = 0#
END IF
LET X# = X# + NT * 2# ^ (14)
LET X# = X# + IOPL * 2# ^ (12)
LET X# = X# + OF * 2# ^ (11)
LET X# = X# + DF * 2# ^ (10)
LET X# = X# + IE * 2# ^ (9)
LET X# = X# + TF * 2# ^ (8)
LET X# = X# + SF * 2# ^ (7)
LET X# = X# + ZF * 2# ^ (6)
LET X# = X# + 0 * 2# ^ (5)
LET X# = X# + AF * 2# ^ (4)
LET X# = X# + 0 * 2# ^ (3)
LET X# = X# + PF * 2# ^ (2)
LET X# = X# + 1 * 2# ^ (1)
LET X# = X# + CF * 2# ^ (0)

LET FLAGS# = X#
END FUNCTION

FUNCTION GetOffset# (SGRG, ADS, DEC.NUM)
END FUNCTION

SUB HelpDisplay (PLACE)
END SUB

FUNCTION REG# (REG.NUM)
REM See comment on REG.CODE

IF REG.NUM < 8 THEN
    LET X# = HexVal#(genReg(REG.NUM))
ELSEIF REG.NUM < 16 THEN
    LET RG$ = MID$(genReg(REG.NUM - 8), 5)
    LET X# = HexVal#(RG$)
ELSEIF REG.NUM < 20 THEN
    LET RG$ = MID$(genReg(REG.NUM - 16), 7)
    LET X# = HexVal#(RG$)
ELSEIF REG.NUM < 24 THEN
    LET RG$ = MID$(genReg(REG.NUM - 20), 5, 2)
    LET X# = HexVal#(RG$)
END IF

LET REG# = X#

END FUNCTION

SUB REG.STORE (REG.NUM, NUM#)
REM Value of NUM# should be ok whether it's 2's complement or not.

IF REG.NUM < 8 THEN
    LET genReg(REG.NUM) = HexStr$(NUM#, -8)
ELSEIF REG.NUM < 16 THEN
    MID$(genReg(REG.NUM - 8), 5, 4) = HexStr$(NUM#, -4)
ELSEIF REG.NUM < 20 THEN
    MID$(genReg(REG.NUM - 16), 7, 2) = HexStr$(NUM#, -2)
ELSEIF REG.NUM < 24 THEN
    MID$(genReg(REG.NUM - 20), 5, 2) = HexStr$(NUM#, -2)
END IF
END SUB

FUNCTION REGZ# (REG.NUM)
REM Same as REG# except return values in the 2's complement range.
IF REG.NUM < 8 THEN
    LET X# = HexVal#(genReg(REG.NUM))
    LET X# = Mod2N#(X#, -32)
ELSEIF REG.NUM < 16 THEN
    LET RG$ = MID$(genReg(REG.NUM - 8), 5)
    LET X# = HexVal#(RG$)
    LET X# = Mod2N(X#, -16)
ELSEIF REG.NUM < 20 THEN
    LET RG$ = MID$(genReg(REG.NUM - 16), 7)
    LET X# = HexVal#(RG$)
    LET X# = Mod2N(X#, -8)
ELSEIF REG.NUM < 24 THEN
    LET RG$ = MID$(genReg(REG.NUM - 20), 5, 2)
    LET X# = HexVal#(RG$)
    LET X# = Mod2N(X#, -8)
END IF

LET REGZ# = X#

END FUNCTION

SUB report.brkpts
END SUB

SUB reset.cpu
END SUB

SUB signal.fault (xcptype%, err.cde%)
REM p155
LET INTERRUPT.RECEIVED = TRUE
LET INTERRUPT.NUMBER = xcptype%
END SUB

