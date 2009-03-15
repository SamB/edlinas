REM $INCLUDE: 'x86.bi'
REM
REM Memory Access Functions
REM
DECLARE FUNCTION LA.wrChk# (laddr#)
DECLARE FUNCTION LA.rdChk# (laddr#)
DECLARE FUNCTION MemRead# (physaddr#, N.BYTES!)
DECLARE SUB MemWrite (physaddr#, value#, N.BYTES!)
DECLARE SUB check.brkpt (btype%, X#)
DECLARE SUB signal.fault (xcptype%, error.code%)
REM
REM Register Code Conversions
REM
DECLARE FUNCTION REG.CODE! (REG.NUM!, Size!)
DECLARE FUNCTION REG.BITS! (N!)
DECLARE FUNCTION RM.BITS! (DEC.NUM!)
DECLARE FUNCTION WORD.SIZE! (OS!, WIDTH.BIT!)
REM
REM Source Character Processing
REM
DECLARE FUNCTION LCUT$ (BBBX$)
DECLARE FUNCTION RCUT$ (BBBX$)
DECLARE FUNCTION CUT$ (BBBX$)
DECLARE FUNCTION COUNT.CHAR! (X$, C$)
MemFix:
OPEN MEM.PATH$ + "Memvec.dat" FOR RANDOM AS #2 LEN = 4
RESUME

FUNCTION BinVal# (BinString$)
LET N = LEN(BinString$)
LET TOTAL# = 0#
FOR K = 1 TO N
        LET CHAR$ = MID$(BinString$, K, 1)
        IF CHAR$ = "1" OR CHAR$ = "0" THEN
                LET TOTAL# = 2# * TOTAL# + VAL(CHAR$)
        ELSE
                LET BinVal# = -1
                RETURN
        END IF
NEXT
LET BinVal# = TOTAL#
END FUNCTION

FUNCTION Bits# (X#, hi, lo)
LET TOP = hi + 1
LET Bits# = FIX(Mod2N#(X#, TOP) / (2# ^ lo))
END FUNCTION

FUNCTION BitStr$ (value#, LENGTH)
REM
REM             Produces a low order bit string having LENGTH bits.
LET X$ = ""
LET M# = value#

FOR K = 1 TO LENGTH
   LET NEW.M# = INT(M# / 2#)      'INT means floor!  Negatives are ok!
   LET C% = CINT(M# - NEW.M# * 2#) + 48
   LET X$ = CHR$(C%) + X$
   LET M# = NEW.M#
NEXT

LET BitStr$ = X$
END FUNCTION

FUNCTION BYTE.SWAP$ (HEX.STRING$)

LET N = LEN(HEX.STRING$)   ' N must be even, no half-bytes allowed.
LET B$ = ""
FOR K = 1 TO N \ 2

    LET B$ = MID$(HEX.STRING$, 2 * K - 1, 2) + B$
NEXT
LET BYTE.SWAP$ = B$
END FUNCTION

FUNCTION COUNT.CHAR (X$, C$)
LET M = 0
LET COUNT = -1
DO
    LET COUNT = COUNT + 1
    LET M = INSTR(M + 1, X$, C$)
LOOP WHILE M > 0
LET COUNT.CHAR = COUNT
END FUNCTION

FUNCTION CUT$ (BBBX$)

     REM           Cut blanks and tabs off the right.
   
     LET N = LEN(BBBX$)

     IF N = 0 THEN
        LET CUT$ = ""
     ELSE
        LET LAST.POS = N + 1
        DO
                LET LAST.POS = LAST.POS - 1
                LET THIS.CHAR = ASC(MID$(BBBX$, LAST.POS, 1))
                LET THIS.CHAR.WHITE = (THIS.CHAR = 32 OR THIS.CHAR = 9)
  
        LOOP WHILE LAST.POS > 1 AND THIS.CHAR.WHITE
        REM  continue so long as this character is a space, a comma or a tab.
  
        IF NOT THIS.CHAR.WHITE THEN
                LET CUT$ = LCUT$(LEFT$(BBBX$, LAST.POS))
        ELSE
                LET CUT$ = ""
        END IF

     END IF

END FUNCTION

FUNCTION HexStr$ (value#, LENGTH)
    LET PLEN = ABS(LENGTH)
    IF value# < 0# THEN  ' Assume value lies in 2's complement range.
        LET X# = value# + 16# ^ PLEN
    ELSE
        LET X# = value#
    END IF

    LET Y$ = ""
    FOR K = 1 TO PLEN
        LET NEW.X# = INT(X# / 16#)
        LET C% = X# - 16# * NEW.X#
        IF C% < 10 THEN
            LET DIGIT$ = CHR$(48 + C%)
        ELSE
            LET DIGIT$ = CHR$(55 + C%)
        END IF
        LET Y$ = DIGIT$ + Y$
        LET X# = NEW.X#
    NEXT

    LET HexStr$ = Y$

END FUNCTION

FUNCTION HexVal# (HexString$)

    REM
    REM       Find the value of a Hexadecimal string
    LET Y# = 0#
    FOR K = 1 TO LEN(HexString$)
        LET DIGIT$ = MID$(HexString$, K, 1)
        IF DIGIT$ > "Z" THEN
            X = ASC(DIGIT$) - 87
        ELSEIF DIGIT$ > "9" THEN
            X = ASC(DIGIT$) - 55
        ELSE
            X = ASC(DIGIT$) - 48
        END IF
        LET Y# = 16# * Y# + X
    NEXT
    LET HexVal# = Y#
END FUNCTION

FUNCTION LA.rdChk# (laddr#)
REM p93
REM Bits identified on p86
IF ctrlReg#(0) < 2# ^ 31 THEN
    LET LA.rdChk# = laddr#
    EXIT FUNCTION
END IF
DIM err.code AS INTEGER
IF CPL = 3 THEN
    LET err.code = 4
ELSE
    LET err.code = 0
END IF
LET laddr.DTE# = Bits#(ctrlReg#(3), 31, 12) * 4096# + Bits#(laddr#, 31, 22) * 4
LET dte# = MemRead#(laddr.DTE#, 4)
IF Bits#(dte#, 0, 0) = 0 THEN
    LET ctrlReg#(2) = laddr#
    CALL signal.fault(PAGE%, err.code)
    EXIT FUNCTION
END IF
IF CPL = 3 AND (Bits#(dte#, 2, 2) = 0) THEN
    LET ctrlReg#(2) = laddr#
    CALL signal.fault(PAGE%, err.code OR 1)
    EXIT FUNCTION
END IF
IF Bits#(dte#, 5, 5) = 0 THEN
    LET dte# = dte# + 32#
END IF
CALL LA.write(laddr.DTE#, dte#, 4)
LET laddr.PTE# = Bits#(dte#, 31, 12) * 4096# + Bits#(laddr#, 21, 12) * 4#
LET pte# = MemRead#(laddr.PTE#, 4)
IF Bits#(pte#, 0, 0) = 0 THEN
    LET ctrlReg#(2) = laddr#
    CALL signal.fault(PAGE%, err.code)
    EXIT FUNCTION
END IF
IF CPL = 3 AND Bits#(pte#, 2, 2) = 0 THEN
    LET ctrlReg#(2) = laddr#
    CALL signal.fault(PAGE%, err.code OR 1)
    EXIT FUNCTION
END IF

IF Bits#(pte#, 5, 5) = 0 THEN
    LET pte# = pte# + 32#
END IF
CALL LA.write(laddr.PTE#, pte#, 4)
LET LA.rdChk# = laddr#
END FUNCTION

FUNCTION LA.read# (laddr#, N.BYTES)
REM See Agarwal's LA_read1, LA_read2, and LA_read4, pp 91-92.
REM  N.BYTES is +/- 1, 2, or 4.
LET M = ABS(N.BYTES) - 1         ' Used as a mask.
IF Bits#(laddr#, M, 0) > 0 AND CPL = 3 AND AM AND AC THEN
    CALL signal.fault(ALIGN%, -1)
    EXIT FUNCTION
END IF

FOR K = 0 TO M
    LET phys.addr# = LA.rdChk#(laddr# + K)
    CALL check.brkpt(3, laddr# + K) 'READ type = 3, see p 174
    LET RETV# = RETV# + MemRead#(phys.addr#, 1) * 256# ^ K
NEXT
IF N.BYTES < 0 AND 2# * RETV# >= 256# ^ -N.BYTES THEN
    LET LA.read# = RETV# - 256# ^ -N.BYTES
ELSE
    LET LA.read# = RETV#
END IF
END FUNCTION

FUNCTION LA.wrChk# (laddr#)
REM p96
REM Bits identified on p86
DIM err.code AS INTEGER
IF ctrlReg#(0) < 2# ^ 31 THEN
    LET LA.wrChk# = laddr#
    EXIT FUNCTION
END IF
IF CPL = 3 THEN
    LET err.code = 6
ELSE
    LET err.code = 2
END IF
LET phys.addr.DTE# = Bits#(ctrlReg#(3), 31, 12) * 4096# + Bits#(laddr#, 31, 22) * 4#
LET dte# = MemRead#(phys.addr.DTE#, 4)
IF Bits#(dte#, 0, 0) = 0 THEN
    LET ctrlReg#(2) = laddr#
    CALL signal.fault(PAGE%, err.code)
    EXIT FUNCTION
END IF
dte.check% = Bits#(dte#, 2, 1)
REM dte.check% < 3 iff ((dte# AND 4) = 0 OR (dte# AND 2) = 0)
IF CPL = 3 AND dte.check% < 3 THEN
    LET ctrlReg#(2) = laddr#
    CALL signal.fault(PAGE%, err.code OR 1)
    EXIT FUNCTION
END IF
IF Bits#(dte#, 5, 5) = 0 THEN
    LET dte# = dte# + 32#
END IF
CALL MemWrite(phys.addr.PTE#, dte#, 4)
LET phys.addr.PTE# = Bits#(dte#, 31, 12) * 4096# + Bits#(laddr#, 21, 12) * 4
LET pte# = MemRead#(phys.addr.PTE#, 4)
IF Bits#(pte#, 0, 0) = 0 THEN
    LET ctrlReg#(2) = laddr#
    CALL signal.fault(PAGE%, err.code)
    EXIT FUNCTION
END IF
LET pte.check% = Bits#(dte#, 2, 1)
IF CPL = 3 AND pte.check% < 3 THEN
    LET ctrlReg#(2) = laddr#
    CALL signal.fault(PAGE%, err.code OR 1)
    EXIT FUNCTION
END IF

IF Bits#(pte#, 5, 5) = 0# THEN
    LET pte# = pte# + 32
END IF
IF Bits#(pte#, 6, 6) = 0# THEN
    LET pte# = pte# + 64#
END IF
CALL MemWrite(phys.addr.PTE#, pte#, 4)
LET LA.wrChk# = Bits#(pte#, 31, 12) * 4096# + Bits#(laddr#, 11, 0)
END FUNCTION

SUB LA.write (laddr#, value#, N.BYTES)

REM                Store a number into Memory
REM    A  linear address is an unsigned 4 byte number.
REM    Use the convention that if a negative number is
REM    being stored then the two's complement system
REM    is implied.  Note that a range is given by N.BYTES

REM  See Agarwal's LA_write1, LA_write2, and LA_write4, pp95-96
REM     N.BYTES is 1, 2, or 4

        IF laddr# < 0 THEN
            RETURN              ' This will generate an error
        END IF
        LET M = ABS(N.BYTES)
        IF M = 1 THEN
            REM No problem
        ELSEIF M = 2 AND Bits#(laddr#, 0, 0) = 0# THEN
            REM No problem
        ELSEIF M = 4 AND Bits#(laddr#, 1, 0) = 0# THEN
            REM No problem
        ELSEIF CPL = 3 AND AM AND AC THEN
            CALL signal.fault(ALIGN%, -1)
            EXIT SUB
        END IF
        IF value# < 0 THEN
                LET quotient# = value# + 256# ^ M
        ELSE
                LET quotient# = value#
        END IF

    FOR K = 0 TO M - 1
        CALL check.brkpt(1, laddr# + K)  'Is this WRITE type ???
        LET phys.addr# = LA.wrChk#(laddr# + K)
        LET NEW.QUOTIENT# = FIX(quotient# / 256#)
        LET STORE# = quotient# - 256# * NEW.QUOTIENT#
        CALL MemWrite(phys.addr#, STORE#, 1)
        LET quotient# = NEW.QUOTIENT#
    NEXT
END SUB

FUNCTION LCUT$ (BBBX$)

     REM           Cut blanks and tabs off the left.

     LET N = LEN(BBBX$)

     IF N = 0 THEN
        LET LCUT$ = ""
     ELSE
        LET FIRST.POS = 0
        DO
                LET FIRST.POS = FIRST.POS + 1
                LET THIS.CHAR = ASC(MID$(BBBX$, FIRST.POS, 1))
                LET THIS.CHAR.WHITE = (THIS.CHAR = 32 OR THIS.CHAR = 9)
 
        LOOP WHILE FIRST.POS < N AND THIS.CHAR.WHITE
        REM  continue so long as this character is a space or a tab.
 
        IF NOT THIS.CHAR.WHITE THEN
                LET LCUT$ = MID$(BBBX$, FIRST.POS)
        ELSE
                LET LCUT$ = ""
        END IF

     END IF
END FUNCTION

FUNCTION MemRead# (physaddr#, N.BYTES)

    REM       Load a value from Physical Memory
    REM     IF N.BYTES is a negative number a value is returned
    REM        according to the two's complement system.
LET A31TO2& = FIX(physaddr# / 4#)
LET A2TO0 = physaddr# - 4# * A31TO2&

LET M = ABS(N.BYTES) + A2TO0
IF M > 4 THEN
    ON ERROR GOTO MemFix
    GET #2, A31TO2& + 2, MEM&
    ON ERROR GOTO 0
    LET retrieve# = (MEM& AND (256# ^ (M - 4) - 1))
    LET value# = retrieve# * 256# ^ (4 - A2TO0)
ELSE
    LET value# = 0#
END IF

    ON ERROR GOTO MemFix
GET #2, A31TO2& + 1, MEM&
    ON ERROR GOTO 0
IF M < 4 THEN
    LET retrieve# = MEM& AND (-(256# ^ A2TO0)) AND (256# ^ M - 1)
ELSE
    LET retrieve# = MEM& AND (-(256# ^ A2TO0))
END IF

IF retrieve# < 0 THEN
    LET retrieve# = retrieve# + 2# ^ 32
END IF

LET value# = value# + retrieve# * 256# ^ (-A2TO0)

IF N.BYTES < 0 AND value# >= 2# ^ (-(N.BYTES * 8) - 1) THEN
    LET MemRead# = value# - 2# ^ (8 * (-N.BYTES))
ELSE
    LET MemRead# = value#
END IF

END FUNCTION

SUB MemWrite (physaddr#, value#, N.BYTES)
REM File is declared using QuickBasic 4 byte integers.
REM These are two's complement "&" typed numbers.
REM They can do bitwise logic, floats must be in
REM in the correct range for the conversion to be valid.
REM
REM value# is assumed to be a number of size N.BYTES
REM  which is either unsigned or two's complement.
REM
REM
LET A31TO2& = Bits#(physaddr#, ADDR.BITS - 1, 2)
LET A2TO0 = Bits#(physaddr#, 1, 0)
LET N = ABS(N.BYTES)
IF value# < 0 THEN                ' Make positive if input value is valid.
    LET valu# = 256# ^ N + value#
ELSE
    LET valu# = value#
END IF
LET M = N + A2TO0

ON ERROR GOTO MemFix
GET #2, A31TO2& + 1, MEM&
ON ERROR GOTO 0

IF M < 4 THEN
    LET mask& = NOT ((256# ^ N - 1) * 256# ^ A2TO0)
    LET val.low& = valu# * 256 ^ A2TO0
    LET MEM& = (MEM& AND mask&) + val.low&
    PUT #2, A31TO2& + 1, MEM&
ELSEIF M = 4 AND A2TO0 > 0 THEN
    LET mask& = 256# ^ A2TO0 - 1
    LET valu# = valu# * 256 ^ A2TO0
    IF valu# >= 2# ^ 31 THEN
        LET valu# = valu# - 2# ^ 32
    END IF
    LET val.low& = valu#
    LET MEM& = (MEM& AND mask&) + val.low&
    PUT #2, A31TO2& + 1, MEM&
ELSEIF A2TO0 > 0 THEN
    LET mask& = 256# ^ A2TO0 - 1
    LET flipmask& = 256# ^ (4 - A2TO0) - 1
    IF valu# >= 2# ^ 31 THEN
        LET valu# = valu# - 2# ^ 32
    END IF
    LET val.lower# = (flipmask& AND valu#)
    LET val.upper# = valu# - val.lower#
    LET val.lower# = val.lower# * 256# ^ A2TO0
    LET val.upper# = val.upper# * 256# ^ (A2TO0 - 4)
    LET val.low& = val.lower#
    LET MEM& = (MEM& AND mask&) + val.low&
    PUT #2, A31TO2& + 1, MEM&
    GET #2, A31TO2& + 2, MEM&
    LET val.upper& = val.upper#
    LET mask& = NOT (256# ^ (M - 4) - 1)
    LET MEM& = (MEM& AND mask&) + val.upper&
    PUT #2, A31TO2& + 2, MEM&
ELSE
    IF valu# >= 2# ^ 31 THEN
        LET valu# = valu# - 2# ^ 32
    END IF
    LET MEM& = valu#
    PUT #2, A31TO2& + 1, MEM&
END IF


END SUB

FUNCTION Mod2N# (NUM#, N)

    IF N < 0 THEN
        LET MODULUS# = 2# ^ (-N)
    ELSE
        LET MODULUS# = 2# ^ (N)
    END IF
    LET Y# = NUM# - INT(NUM# / MODULUS#) * MODULUS#
    IF N < 0 AND Y# + Y# >= MODULUS# THEN
        LET Y# = Y# - MODULUS#
    END IF

    LET Mod2N# = Y#

END FUNCTION

FUNCTION RandomBytes# (N.BYTES)
            LET X# = 0#
            FOR K = 1 TO N.BYTES
                LET X# = 256# * X# + CDBL(FIX(256 * RND))
            NEXT
            LET RandomBytes# = X#
END FUNCTION

FUNCTION RCUT$ (BBBX$)

     REM           Cut blanks and tabs off the right.

     LET N = LEN(BBBX$)

     IF N = 0 THEN
        LET RCUT$ = ""
     ELSE
        LET FIRST.POS = N + 1
        DO
                LET FIRST.POS = FIRST.POS - 1
                LET THIS.CHAR = ASC(MID$(BBBX$, FIRST.POS, 1))
                LET THIS.CHAR.BAD = (THIS.CHAR = 32 OR THIS.CHAR = 9)
 
        LOOP WHILE FIRST.POS > 1 AND THIS.CHAR.BAD
        REM  continue so long as this character is a space or a tab.
 
        IF NOT THIS.CHAR.BAD THEN
                LET RCUT$ = LEFT$(BBBX$, FIRST.POS)
        ELSE
                LET RCUT$ = ""
        END IF

     END IF
END FUNCTION

FUNCTION REG.BITS (N)

    LET REG.BITS = (N MOD 64) \ 8
   
END FUNCTION

FUNCTION REG.CODE (REG.NUM, Size)
REM REG.NUM is a number less than 8                              Size
REM The return codes  follow:
REM      0- 7:                  EAX ECX EDX EBX ESP EBP ESI EDI    4
REM      8-15:                   AX  CX  DX  BX  SP  BP  SI  DI    2
REM     16-23:                   AL  CL  DL  BL  AH  CH  DH  BH    1
REM     24-31:                   ES  CS  SS  DS  FS  GS LDT TSS    5
REM     32-39:                  DR0 DR1 DR2 DR3 DR4 DR5 DR6 DR7    6
REM     40-47:                  TR0 TR1 TR2 TR3 TR4 TR5 TR6 TR7    7
REM     48-51:                  CR0 CR1 CR2 CR3                    8
IF Size = 1 THEN
    X = REG.NUM + 16
ELSEIF Size = 2 THEN
    X = REG.NUM + 8
ELSEIF Size = 4 THEN
    X = REG.NUM
ELSE
    X = REG.NUM + (Size - 2) * 8
END IF

LET REG.CODE = X
REM
END FUNCTION

FUNCTION RM.BITS (DEC.NUM)

    LET RM.BITS = DEC.NUM MOD 8

END FUNCTION

FUNCTION WORD.SIZE (OS, WIDTH.BIT)
IF WIDTH.BIT = 0 THEN
    LET WS = 1
ELSEIF OS = 16 THEN
    LET WS = 2
ELSE
    LET WS = 4
END IF

LET WORD.SIZE = WS

END FUNCTION

