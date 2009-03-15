REM $INCLUDE: 'x86.bi'
DECLARE SUB NumParse (RAW$, PACK$, value#, ERM$)
DECLARE SUB Print24 (X$)
DECLARE SUB signal.fault (xcptype%, error.code%)
REM
REM Decoding Functions
REM
DECLARE FUNCTION REG.BITS (DEC.NUM!)
DECLARE FUNCTION RM.BITS! (DEC.NUM!)
DECLARE FUNCTION REG.CODE! (REG.NUM!, Size!)
DECLARE FUNCTION WORD.SIZE! (OS!, WIDTH.BIT!)
DECLARE SUB REG.STORE (REG.NUM!, NUM#)
DECLARE FUNCTION Mod2N# (NUM#, N!)
DECLARE FUNCTION REGZ# (REG.NUM!)
REM
REM Execution Functions
REM
DECLARE FUNCTION IOPermission% (PORT.NUM, OS)
DECLARE SUB DoString (OS!, ADS!, DEC.NUM!)
DECLARE SUB Imul (OS!, segReg!, ADS, DEC.NUM!)
DECLARE FUNCTION DIF# (A#, B#, C#, N.BYTES)
DECLARE FUNCTION SUM# (A#, B#, C#, N.BYTES)
DECLARE FUNCTION DISJ# (A#, B#, OPER.NUM, N.BYTES)
DECLARE FUNCTION PROD# (A#, B#, N.BITS!)
DECLARE FUNCTION IPROD# (A#, B#, N.BYTES!)
DECLARE SUB IDIV (AT#, AB#, B#, N.BITS!)
DECLARE SUB DoIO (OS!, value#, DEC.NUM!)
DECLARE FUNCTION Parity8! (X#)
DECLARE SUB DIV (AT#, AB#, B#, N.BITS!)
DECLARE SUB push (value#, N.BYTES!)
DECLARE FUNCTION UserInput# (N.BYTES!)
DECLARE SUB DisplayOutput (value#)
DECLARE SUB GoFar (OS!, selector!, offset#, JMP.NOT.CALL!)

SUB AluMI (OS, SREG, ADS, DEC.NUM, LF)
REM
      REM               ALU rmv,imm  subroutine (opcodes 80, 81, 83)
   
    LET N.BYTES = WORD.SIZE(OS, DEC.NUM MOD 2)
    IF DEC.NUM = &H83 THEN
        LET N.IMM.BYTES = -1
    ELSE
        LET N.IMM.BYTES = N.BYTES
    END IF

    LET DEC.NUM = fetch.code#(1)
    LET REG.NUM = REG.BITS(DEC.NUM)
    IF DEC.NUM >= &HC0 AND LF THEN
        LET INSTRUCTION.RECOGNIZED = FALSE
        EXIT SUB
    ELSEIF DEC.NUM >= &HC0 THEN
        LET DEST = REG.CODE(RM.BITS(DEC.NUM), N.BYTES)
        LET MEM.NUM# = REG#(DEST)
    ELSE
        LET offset# = GetOffset#(SREG, ADS, DEC.NUM)
        LET MEM.NUM# = lg.read#(SREG, offset#, N.BYTES)
    END IF
    LET IMM.NUM# = fetch.code#(N.IMM.BYTES)
    IF REG.NUM = 0 THEN
        LET MEM.NUM# = SUM#(MEM.NUM#, IMM.NUM#, 0#, N.BYTES)
    ELSEIF REG.NUM = 1 OR REG.NUM = 4 OR REG.NUM = 6 THEN
        LET MEM.NUM# = DISJ#(MEM.NUM#, IMM.NUM#, REG.NUM, N.BYTES)
    ELSEIF REG.NUM = 2 THEN
        LET MEM.NUM# = SUM#(MEM.NUM#, IMM.NUM#, CDBL(CF), N.BYTES)
    ELSEIF REG.NUM = 3 THEN
        LET MEM.NUM# = DIF#(MEM.NUM#, IMM.NUM#, CDBL(CF), N.BYTES)
    ELSEIF REG.NUM = 5 THEN
        LET MEM.NUM# = DIF#(MEM.NUM#, IMM.NUM#, 0#, N.BYTES)
    ELSEIF REG.NUM = 7 THEN
        LET X# = DIF#(MEM.NUM#, IMM.NUM#, 0#, N.BYTES)
    END IF
    IF DEC.NUM >= &HC0 THEN
        CALL REG.STORE(DEST, MEM.NUM#)
    ELSE
        CALL lg.write(SREG, offset#, MEM.NUM#, N.BYTES)
    END IF
END SUB

FUNCTION DIF# (A#, B#, C#, W)
  
REM  This function works on the assumption that the first two
REM     arguments are in the unsigned 1, 2, or 4 byte integer range.
REM     W is 1, 2, or 4 bytes.
REM     The third argument is assumed to be a carry bit.
REM     It then returns a value also in this range.
REM     The overflow, sign, carry, and zero flags are set.

        LET D# = A# - B# - C#

        IF Bits(A#, 3, 0) - Bits(B#, 3, 0) - C# < 0 THEN
            LET AF = 1
        ELSE
            LET AF = 0
        END IF

        IF A# >= 2# ^ (8 * W - 1) THEN
                LET AZ# = A# - 2# ^ (8 * W)
        ELSE
                LET AZ# = A#
        END IF


        IF B# >= 2# ^ (8 * W - 1) THEN
                LET BZ# = B# - 2# ^ (8 * W)
        ELSE
                LET BZ# = B#
        END IF

        IF (AZ# - BZ# - C# < -2# ^ (8 * W - 1)) OR (AZ# - BZ# - C# >= 2# ^ (8 * W - 1)) THEN
            LET OF = 1
        ELSE
            LET OF = 0
        END IF

        IF D# < 0# THEN
            LET CF = 1
            LET D# = D# + 2# ^ (8 * W)
        ELSE
            LET CF = 0
        END IF
        IF D# >= 2# ^ (8 * W - 1) THEN
            LET SF = 1
        ELSE
            LET SF = 0
        END IF
       
        IF D# = 0# THEN
                LET ZF = 1
        ELSE
                LET ZF = 0
        END IF
        LET PF = Parity8(D#)
        LET DIF# = D#

END FUNCTION

FUNCTION DISJ# (A#, B#, OPER.NUM, W)
REM
REM     This function performs the bitwise OR, XOR, & AND operations.
REM
REM
        IF A# >= 2# ^ (31) THEN
                LET AC# = A# - 2# ^ (32)
        ELSE
                LET AC# = A#
        END IF
     
        IF B# >= 2# ^ (31) THEN
                LET BC# = B# - 2# ^ (32)
        ELSE
                LET BC# = B#
        END IF
        LET A& = AC#
        LET B& = BC#
        IF OPER.NUM = 1 THEN
            LET C& = (A& OR B&)
        ELSEIF OPER.NUM = 4 THEN
            LET C& = (A& AND B&)
        ELSEIF OPER.NUM = 6 THEN
            LET C& = (A& XOR B&)
        END IF
        LET C# = C&
        IF W = 4 THEN
            IF C# < 0 THEN
                LET SF = 1
                LET C# = C# + 2# ^ (32)
            ELSE
                LET SF = 0
            END IF
        ELSEIF W = 2 THEN
            IF C# >= 32768# THEN
                LET SF = 1
            ELSE
                LET SF = 0
            END IF
        ELSEIF W = 1 THEN
            IF C# >= 128# THEN
                LET SF = 1
            ELSE
                LET SF = 0
            END IF
        END IF
        LET DISJ# = C#

        IF C# = 0 THEN
                LET ZF = 1
        ELSE
                LET ZF = 0
        END IF
        LET OF = 0
        LET CF = 0
        LET PF = Parity8(C#)
        LET AF = CINT(RND)

END FUNCTION

SUB DisplayOutput (value#)

REM           subroutine to display program output
   
      LOCATE 25, 26
      PRINT SPACE$(12);
      LET OUT$ = STR$(value#)
      LOCATE 25, 19
      COLOR 30
      PRINT "OUTPUT "; OUT$;
      LET trap$ = ""
      WHILE trap$ = ""
        LET trap$ = INKEY$
      WEND
    COLOR 15, 1
    LOCATE 25, 19
    COLOR 14
    PRINT "OUTPUT "; OUT$;
    REM SLEEP 2 * INSTTIM
    LOCATE 25, 1
    PRINT SPACE$(39);
END SUB

SUB DIV (AT#, AB#, B#, N.BITS)
    LET HALF.SIZE = N.BITS \ 2
    LET BT# = Bits#(B#, N.BITS - 1, HALF.SIZE)
    LET BB# = Bits#(B#, HALF.SIZE - 1, 0)
    IF B# <= AT# THEN
        LET INTERRUPT.RECEIVED = TRUE
        LET INTERRUPT.NUMBER = 0
        EXIT SUB
    ELSEIF AT# = 0 THEN   ' Half size is big enough.
        LET Q# = INT(AB# / B#)
        LET AT# = AB# - B# * Q#
        LET AB# = Q#
        EXIT SUB
    END IF
        LET CT# = -INT(-(AT# * 2# ^ N.BITS + AB#) / B# * 2# ^ -HALF.SIZE) + 1
        DO
            LET CT# = CT# - 1

            LET D# = CT# * BB#
            LET DB# = Bits#(D#, HALF.SIZE - 1, 0)
            LET DT# = Bits#(D#, N.BITS - 1, HALF.SIZE)
            LET E# = CT# * BT# + DT#
            
            LET TOO.BIG1 = (E# > AT#)
            LET TOO.BIG2 = ((E# = AT#) AND (DB# * 2# ^ HALF.SIZE > AB#))
        LOOP WHILE TOO.BIG1 OR TOO.BIG2
        IF CT# >= 2# ^ HALF.SIZE THEN
            LET INTERRUPT.RECEIVED = TRUE
            LET INTERRUPT.NUMBER = 0
            EXIT SUB
        END IF
       
        LET AB# = (AT# - E#) * 2# ^ N.BITS + AB# - DB# * 2# ^ HALF.SIZE
        LET AC# = Bits#(AB#, N.BITS + HALF.SIZE - 1, HALF.SIZE)
        LET AD# = Bits#(AB#, HALF.SIZE - 1, 0)
    LET CB# = -INT(-AB# / B#) + 1
    DO
        LET CB# = CB# - 1

        LET D# = CB# * BB#
        LET DB# = Bits#(D#, HALF.SIZE - 1, 0)
        LET DT# = Bits#(D#, N.BITS - 1, HALF.SIZE)
        LET E# = CB# * BT# + DT#

       
            LET TOO.BIG1 = (E# > AC#)
            LET TOO.BIG2 = ((E# = AC#) AND (DB# > AD#))
     LOOP WHILE TOO.BIG1 OR TOO.BIG2

     LET AT# = (AC# - E#) * 2# ^ HALF.SIZE + AD# - DB#
     LET AB# = CT# * 2# ^ HALF.SIZE + CB#


END SUB

SUB DoAlu (OS, SGREG, ADS, OPER, OP.NUM, LOCK.FLAG)
  
    LET N.BYTES = WORD.SIZE(OS, OPER MOD 2)
    IF OPER \ 2 = 2 THEN  ' DEST = EAX, AX, or AL; SOURCE = imm
        LET DEST = 0
        LET DEST = REG.CODE(DEST, N.BYTES)
        LET OP1.NUM# = REG#(DEST)
        LET OP2.NUM# = fetch.code#(N.BYTES)
        IF LOCK.FLAG THEN
            LET INSTRUCTION.RECOGNIZED = FALSE
            EXIT SUB
        END IF
    ELSEIF OPER \ 2 = 1 THEN     ' DEST = r, SOURCE = rmv or rmb
        LET DEC.NUM = fetch.code#(1)
        LET DEST = REG.BITS(DEC.NUM)
        LET DEST = REG.CODE(DEST, N.BYTES)
        LET OP1.NUM# = REG#(DEST)
        IF DEC.NUM >= &HC0 THEN
            LET SOURCE = RM.BITS(DEC.NUM)
            LET SOURCE = REG.CODE(OS, N.BYTES)
            LET OP2.NUM# = REG#(SOURCE)
            IF LOCK.FLAG THEN
                LET INSTRUCTION.RECOGNIZED = FALSE
                EXIT SUB
            END IF
        ELSE
            LET offset# = GetOffset#(SGREG, ADS, DEC.NUM)
            LET OP2.NUM# = lg.read#(SGREG, offset#, N.BYTES)
        END IF
    ELSEIF OPER \ 2 = 0 THEN    ' DEST = rmv or rmb, SOURCE = r
        LET DEC.NUM = fetch.code#(1)
        LET SOURCE = REG.CODE(REG.BITS(DEC.NUM), N.BYTES)
        LET OP2.NUM# = REG#(SOURCE)
        IF DEC.NUM >= &HC0 THEN
            LET DEST = REG.CODE(RM.BITS(DEC.NUM), N.BYTES)
            LET OP1.NUM# = REG#(DEST)
        ELSE
            LET offset# = GetOffset#(SGREG, ADS, DEC.NUM)
            LET Save.Offset# = offset#
            LET OP1.NUM# = lg.read#(SGREG, offset#, N.BYTES)
        END IF
    END IF

    SELECT CASE OP.NUM
        CASE 0
            LET OP1.NUM# = SUM#(OP1.NUM#, OP2.NUM#, 0#, N.BYTES)
        CASE 1, 4, 6
            LET OP1.NUM# = DISJ#(OP1.NUM#, OP2.NUM#, OP.NUM, N.BYTES)
        CASE 2
            LET OP1.NUM# = SUM#(OP1.NUM#, OP2.NUM#, CDBL(CF), N.BYTES)
        CASE 3
            LET OP1.NUM# = DIF#(OP1.NUM#, OP2.NUM#, CDBL(CF), N.BYTES)
        CASE 5
            LET OP1.NUM# = DIF#(OP1.NUM#, OP2.NUM#, 0#, N.BYTES)
        CASE 7
            LET X# = DIF#(OP1.NUM#, OP2.NUM#, 0#, N.BYTES)
    END SELECT

    IF DEC.NUM < &HC0 AND OPER \ 2 = 0 THEN
        CALL lg.write(SGREG, offset#, OP1.NUM#, N.BYTES)
    ELSE
        CALL REG.STORE(DEST, OP1.NUM#)
    END IF

END SUB

SUB DoConv (OS, SREG, ADS, DEC.NUM)

REM CBW,CWDE,CWD,CDQ Subroutine (opcodes 98 and 99)

    LET N.BYTES = WORD.SIZE(OS, 1)
    IF DEC.NUM = &H98 THEN
        LET DEST = REG.CODE(EAX, N.BYTES)   ' EAX or just AX
        LET SOURCE = REG.CODE(EAX, N.BYTES / 2)
        LET X# = REGZ#(SOURCE)
    ELSE
        LET DEST = REG.CODE(EDX, N.BYTES)   ' EDX or just DX
        LET SOURCE = REG.CODE(EAX, N.BYTES)
        IF REGZ#(SOURCE) < 0 THEN
            LET X# = -1#
        ELSE
            LET X# = 0#
        END IF
    END IF

    CALL REG.STORE(DEST, X#)

END SUB

SUB DoFEFF (OS, SREG, ADS, DEC.NUM, LF)

    REM   INC, DEC, CALL, JMP, PUSH subroutines  (opcodes FE, FF)

    LET WIDTH.BIT = DEC.NUM MOD 2       ' Used by INC & DEC only
    LET DEC.NUM = fetch.code#(1)
    LET OP.NUM = REG.BITS(DEC.NUM) ' Not really a register.
    LET N.BYTES = WORD.SIZE(OS, WIDTH.BIT)

    IF WIDTH.BIT = 0 AND OP.NUM > 1 THEN
        LET INSTRUCTION.RECOGNIZED = FALSE
        EXIT SUB
    ELSEIF OP.NUM < 2 THEN                      ' INC DEC  subroutine.
        LET SAVE.CF = CF
        IF DEC.NUM >= &HC0 AND LF THEN
            LET INSTRUCTION.RECOGNIZED = FALSE
            EXIT SUB
        ELSEIF DEC.NUM >= &HC0 THEN
            LET DEST = REG.CODE(RM.BITS(DEC.NUM), N.BYTES)
            IF OP.NUM = 0 THEN
                CALL REG.STORE(DEST, SUM#(REG#(DEST), 1#, 0#, N.BYTES))
            ELSEIF OP.NUM = 1 THEN
                CALL REG.STORE(DEST, DIF#(REG#(DEST), 1#, 0#, N.BYTES))
            END IF
        ELSE
            LET offset# = GetOffset#(SREG, ADS, DEC.NUM)
            LET MEM.NUM# = lg.read#(SREG, offset#, N.BYTES)
            IF OP.NUM = 0 THEN
                LET MEM.NUM# = SUM#(MEM.NUM#, 1#, 0#, N.BYTES)
            ELSE
                LET MEM.NUM# = DIF#(MEM.NUM#, 1#, 0#, N.BYTES)
            END IF
            CALL lg.write(SREG, offset#, MEM.NUM#, N.BYTES)
        END IF
        LET CF = SAVE.CF
    ELSEIF LF THEN
            LET INSTRUCTION.RECOGNIZED = FALSE
            EXIT SUB
    ELSEIF OP.NUM = 2 OR OP.NUM = 4 THEN       ' Near CALL or JUMP r/m
        LET INSTRUCTION.RECOGNIZED = TRUE
        IF DEC.NUM >= &HC0 THEN
            LET RM.NUM = RM.BITS(DEC.NUM)
            LET SOURCE = REG.CODE(RM, N.BYTES)
            LET BRANCH.EIP# = EIP# + REGZ#(SOURCE)
        ELSE
            LET offset# = GetOffset#(SREG, ADS, DEC.NUM)
            LET BRANCH.EIP# = EIP# + lg.read#(SREG, offset#, N.BYTES)
        END IF
        LET BRANCH.EIP# = Mod2N#(BRANCH.EIP#, OS)
        IF OP.NUM = 2 THEN
            CALL push(EIP#, N.BYTES)
            LET CALL.DEPTH = CALL.DEPTH + 1
        END IF
        LET EIP# = BRANCH.EIP#
    ELSEIF OP.NUM = 3 OR OP.NUM = 5 THEN       '  Far CALL or JUMP r/m
        IF DEC.NUM >= &HC0 THEN
            LET INSTRUCTION.RECOGNIZED = FALSE
        ELSE
            LET offset# = GetOffset#(SREG, ADS, DEC.NUM)
            LET NewOffset# = lg.read#(SREG, offset#, N.BYTES)
            LET NewSel = lg.read#(SREG, offset# + N.BYTES, 2)
            IF OP.NUM = 3 THEN
                LET CALL.DEPTH = CALL.DEPTH + 1
                CALL push(CDBL(segReg(CS)), N.BYTES)
                CALL push(EIP#, N.BYTES)
                LET JMP.NOT.CALL = FALSE
            ELSE
                LET JMP.NOT.CALL = TRUE
            END IF
            CALL GoFar(OS, NewSel, NewOffset#, JMP.NOT.CALL)
        END IF
    ELSEIF OP.NUM = 6 THEN  ' PUSH rmv
        IF DEC.NUM >= &HC0 THEN
            LET SOURCE = REG.CODE(RM.BITS(DEC.NUM), N.BYTES)
            LET OPERAND# = REG#(SOURCE)
        ELSE
            LET offset# = GetOffset#(SREG, ADS, DEC.NUM)
            LET OPERAND# = lg.read#(SREG, offset#, N.BYTES)
        END IF
        CALL push(OPERAND#, N.BYTES)
    ELSE
        LET ERROR.MESSAGE$ = "FF uses /r = 0 to 6"
        LET INSTRUCTION.RECOGNIZED = FALSE
        EXIT SUB
    END IF

END SUB

SUB DoIncDec (OS, DEC.NUM)

      REM  Increment, Decrement  subroutine  (opcodes 4x)
      LET N.BYTES = OS \ 8
      LET DEST = REG.CODE(DEC.NUM MOD 8, N.BYTES)
      LET SAVE.CF = CF
      IF DEC.NUM > &H47 THEN
        LET X# = DIF#(REG#(DEST), 1#, 0#, N.BYTES)
      ELSE
        LET X# = SUM#(REG#(DEST), 1#, 0#, N.BYTES)
      END IF
      CALL REG.STORE(DEST, X#)
      LET CF = SAVE.CF

END SUB

SUB DoInOut (OS, DEC.NUM)
    LET N.BYTES = WORD.SIZE(OS, DEC.NUM MOD 2)
    IF DEC.NUM MOD 4 < 2 THEN
        CALL DoIO(OS, value#, DEC.NUM)
        LET DEST = REG.CODE(EAX, N.BYTES)        ' AL, AX, OR EAX
        CALL REG.STORE(DEST, value#)
    ELSE
        LET SOURCE = REG.CODE(EAX, N.BYTES)      ' AL, AX, OR EAX
        LET value# = REG#(SOURCE)
        CALL DoIO(OS, value#, DEC.NUM)
    END IF
END SUB

SUB DoIO (OS, value#, DEC.NUM)

    REM
    REM    Input and Output calls  (opcodes Ex, 3 < x < 8, By,  y > 11)
   
    IF DEC.NUM MOD 4 < 2 THEN  '   IN      (opcodes 6C, 6D, E4, E5, EC, & ED)
        IF DEC.NUM = &HE4 OR DEC.NUM = &HE5 THEN
            LET PORT.NUM = fetch.code#(1)
        ELSEIF DEC.NUM = &HEC OR DEC.NUM = &HED THEN
            LET PORT.NUM = REG#(10)      ' DX
        END IF

        IF PORT.NUM <> 0 THEN
            CALL signal.fault(7, 0)     ' Should be 18, 7 just for debugging
            EXIT SUB
        END IF
    ELSE                     '    OUT         (opcodes 6E, 6F, E6, E7, EE, & EF)
        IF DEC.NUM = &HE6 OR DEC.NUM = &HE7 THEN
            LET PORT.NUM = fetch.code#(1)
        ELSEIF DEC.NUM = &HEE OR DEC.NUM = &HEF THEN
            LET PORT.NUM = REG#(10)
        END IF
        IF PORT.NUM <> 1 THEN
            CALL signal.fault(7, 0)     ' Should be 18, 7 just for debugging
            EXIT SUB
        END IF

    END IF
  
    IF CPL > IOPL AND mode <> REAL THEN
        IF NOT IOPermission%(PORT.NUM, OS \ 8) THEN
            CALL signal.fault(GP, 0)
            EXIT SUB
         END IF
    END IF
  
    IF INTERRUPT.RECEIVED THEN EXIT SUB

    IF DEC.NUM MOD 4 < 2 THEN
        LET N.BYTES = WORD.SIZE(OS, DEC.NUM MOD 2)
        CALL Print24(" Enter data:")
        LET value# = UserInput#(N.BYTES)
    ELSE
        CALL DisplayOutput(value#)
    END IF
END SUB

SUB DoString (OS, ADS, DEC.NUM)

LET N.BYTES = WORD.SIZE(OS, DEC.NUM MOD 2)
LET N.ADBYTES = WORD.SIZE(ADS, 1)


IF DEC.NUM < &H70 THEN   '6x, x > B   INS & OUTS
    IF DEC.NUM < &H6E THEN       'INS
        LET DEST = REG.CODE(EDI, N.ADBYTES)
        CALL DoIO(OS, X#, &HED - N.BYTES MOD 2)
        CALL lg.write(ES, REG#(DEST), X#, N.BYTES)
        IF DF = 1 THEN
            CALL REG.STORE(DEST, REG#(DEST) - N.BYTES)
        ELSE
            CALL REG.STORE(DEST, REG#(DEST) + N.BYTES)
        END IF
    ELSE        'OUTS
        SOURCE = REG.CODE(ESI, N.ADBYTES)
        LET X# = lg.read#(DS, REG#(SOURCE), N.BYTES)
        CALL DoIO(OS, X#, &HEF - N.BYTES MOD 2)
        IF DF = 1 THEN
            CALL REG.STORE(SOURCE, REG#(SOURCE))
        ELSE
            CALL REG.STORE(SOURCE, REG#(SOURCE) + N.BYTES)
        END IF
    END IF
ELSE
    LET HB2 = DEC.NUM MOD 16
    LET INSTRUCTION.RECOGNIZED = TRUE
    IF HB2 \ 2 = 2 THEN                'A4 & A5, MOVSB, MOVSW
        LET SOURCE = REG.CODE(6, N.ADBYTES)
        LET DEST = REG.CODE(7, N.ADBYTES)
        LET X# = lg.read#(DS, REG#(SOURCE), N.BYTES)
        CALL lg.write(ES, REG#(DEST), X#, N.BYTES)
        IF DF = 1 THEN
            CALL REG.STORE(SOURCE, REG#(SOURCE) - N.BYTES)
            CALL REG.STORE(DEST, REG#(DEST) - N.BYTES)
        ELSE
            CALL REG.STORE(SOURCE, REG#(SOURCE) + N.BYTES)
            CALL REG.STORE(DEST, REG#(DEST) + N.BYTES)
        END IF
    ELSEIF HB2 \ 2 = 3 THEN                'A6 & A7, CMPSB, CMPSW
        LET SOURCE = REG.CODE(ESI, N.ADBYTES)
        LET DEST = REG.CODE(EDI, N.ADBYTES)
        LET X# = lg.read#(DS, REG#(SOURCE), N.BYTES)
        LET Y# = lg.read#(ES, REG#(DEST), N.BYTES)
        LET C# = 0#
        LET Z# = DIF#(X#, Y#, C#, N.BYTES)
        IF DF = 1 THEN
            CALL REG.STORE(SOURCE, REG#(SOURCE) - N.BYTES)
            CALL REG.STORE(DEST, REG#(DEST) - N.BYTES)
        ELSE
            CALL REG.STORE(SOURCE, REG#(SOURCE) + N.BYTES)
            CALL REG.STORE(DEST, REG#(DEST) + N.BYTES)
        END IF
    ELSEIF HB2 \ 2 = 5 THEN                'AA & AB, STOSB, STOSW
        SOURCE = REG.CODE(0, N.BYTES)
        LET DEST = REG.CODE(EDI, N.ADBYTES)
        CALL lg.write(ES, REG#(DEST), REG#(SOURCE), N.BYTES)
        IF DF = 1 THEN
            CALL REG.STORE(DEST, REG#(DEST) - N.BYTES)
        ELSE
            CALL REG.STORE(DEST, REG#(DEST) + N.BYTES)
        END IF
    ELSEIF HB2 \ 2 = 6 THEN                'AC & AD, LODSB, LODSW
        SOURCE = REG.CODE(ESI, N.ADBYTES)
        LET DEST = REG.CODE(EAX, N.BYTES)
        LET X# = lg.read#(DS, REG#(SOURCE), N.BYTES)
        CALL REG.STORE(DEST, X#)
        IF DF = 1 THEN
            CALL REG.STORE(SOURCE, REG#(SOURCE) - N.BYTES)
        ELSE
            CALL REG.STORE(SOURCE, REG#(SOURCE) + N.BYTES)
        END IF
    ELSE                     ' AE & AF  SCASB, SCASW
        LET SOURCE = REG.CODE(EAX, N.BYTES)
        LET DEST = REG.CODE(EDI, N.ADBYTES)
        LET X# = REG#(SOURCE)
        LET Y# = lg.read#(ES, REG#(DEST), N.BYTES)
        LET C# = 0#
        LET Z# = DIF#(X#, Y#, C#, N.BYTES)
        IF DF = 1 THEN
            CALL REG.STORE(DEST, REG#(DEST) - N.BYTES)
        ELSE
            CALL REG.STORE(DEST, REG#(DEST) + N.BYTES)
        END IF
    END IF
END IF

END SUB

SUB DoUnM (OS, SREG, ADS, DEC.NUM, LF)
REM
REM             Unary Operations (opcodes F6 and F7)
REM
REM   /r 0:TEST imm, 2:NOT, 3:NEG, 4:MUL, 5:IMUL, 6:DIV, 7:IDIV   rmv
  
    LET N.BYTES = WORD.SIZE(OS, DEC.NUM MOD 2)
   
    LET DEC.NUM = fetch.code#(1)
    
    LET REG.NUM = REG.BITS(DEC.NUM)
    IF REG.NUM = 1 THEN
        LET INSTRUCTION.RECOGNIZED = FALSE
        EXIT SUB
    END IF
  
    IF DEC.NUM >= &HC0 AND LF THEN
        LET INSTRUCTION.RECOGNIZED = FALSE
        EXIT SUB
    ELSEIF DEC.NUM >= &HC0 THEN
        LET DEST = REG.CODE(RM.BITS(DEC.NUM), N.BYTES)
        LET MEM.NUM# = REG#(DEST)
    ELSE
        LET offset# = GetOffset#(SREG, ADS, DEC.NUM)
        LET MEM.NUM# = lg.read#(SREG, offset#, N.BYTES)
    END IF
 
    IF REG.NUM = 0 THEN
        LET IMM.NUM# = fetch.code#(N.BYTES)
        LET X# = DISJ#(MEM.NUM#, IMM.NUM#, 4, N.BYTES)
    ELSEIF REG.NUM = 2 THEN
        LET MEM.NUM# = 2# ^ (8 * N.BYTES) - 1# - MEM.NUM#  'No Flags!
    ELSEIF REG.NUM = 3 THEN
        LET MEM.NUM# = DIF#(0#, MEM.NUM#, 0#, N.BYTES)
    ELSE                                    'MUL  IMUL  DIV IDIV
        LET ACCUM = REG.CODE(0, N.BYTES)
        LET X# = REG#(ACCUM)
        IF N.BYTES = 1 THEN
            LET Y# = REG#(AH)
        ELSEIF N.BYTES = 2 THEN
            LET Y# = REG#(DX)
        ELSEIF N.BYTES = 4 THEN
            LET Y# = REG#(EDX)
        END IF
        IF REG.NUM = 4 THEN
            LET Y# = PROD#(X#, MEM.NUM#, 8 * N.BYTES)
        ELSEIF REG.NUM = 5 THEN
            LET Y# = IPROD#(X#, MEM.NUM#, 8 * N.BYTES)
        ELSEIF REG.NUM = 6 THEN
            CALL DIV(Y#, X#, MEM.NUM#, 8 * N.BYTES)
        ELSEIF REG.NUM = 7 THEN
            CALL IDIV(Y#, X#, MEM.NUM#, 8 * N.BYTES)
        END IF
        IF N.BYTES = 1 THEN
            CALL REG.STORE(AL, X#)
            CALL REG.STORE(AH, Y#)
        ELSEIF N.BYTES = 2 THEN
            CALL REG.STORE(AX, X#)
            CALL REG.STORE(DX, Y#)
        ELSEIF N.BYTES = 4 THEN
            CALL REG.STORE(EAX, X#)
            CALL REG.STORE(EDX, Y#)
        END IF
    END IF
  
    IF REG.NUM = 2 OR REG.NUM = 3 THEN
        IF DEC.NUM >= &HC0 THEN
            CALL REG.STORE(DEST, MEM.NUM#)
        ELSE
            CALL lg.write(SREG, offset#, MEM.NUM#, N.BYTES)
        END IF
    END IF

END SUB

SUB IDIV (AT#, AB#, B#, N.BITS)
LET QUOT.SIGN = 1

IF AT# >= 2# ^ (N.BITS - 1) THEN
    IF AB# = 0 AND AT# = 0 THEN
        REM Do nothing.
    ELSEIF AB# = 0 THEN
        LET AT# = 2# ^ N.BITS - AT#
    ELSEIF AT# = 0 THEN
        LET AB# = 2# ^ N.BITS - AB#
    ELSE
        LET AB# = 2# ^ N.BITS - AB#
        LET AT# = 2# ^ N.BITS - AT# - 1
    END IF
    LET REM.SIGN = -1
    LET QUOT.SIGN = -QUOT.SIGN
END IF
IF B# >= 2# ^ (N.BITS - 1) THEN
        LET QUOT.SIGN = -QUOT.SIGN
        LET BU# = 2# ^ N.BITS - B#
ELSE
        LET BU# = B#
END IF

CALL DIV(AT#, AB#, BU#, N.BITS)
IF QUOT.SIGN = -1 AND AB# <> 0 THEN
    LET AB# = 2# ^ N.BITS - AB#
END IF

IF REM.SIGN = -1 AND AT# <> 0 THEN
    LET AT# = 2# ^ N.BITS - AT#
END IF

END SUB

SUB Imul (OS, SGRG, ADS, DEC.NUM)

    IF DEC.NUM = &H69 THEN
        LET IMM.WIDTH.BIT = 1
    ELSEIF DEC.NUM = &H6B THEN
        LET IMM.WIDTH.BIT = 0
    ELSEIF DEC.NUM = &HAF THEN   ' First byte of 0F AF already discarded.

    ELSEIF DEC.NUM = &HA7 THEN
        LET INSTRUCTION.RECOGNIZED = FALSE
        EXIT SUB
    END IF

    LET N.IMM.BYTES = WORD.SIZE(OS, IMM.WIDTH.BIT)
    LET N.BYTES = WORD.SIZE(OS, 1)
    LET DEC.NUM = fetch.code#(1)
    LET REG.NUM = REG.BITS(DEC.NUM)
    LET DEST = REG.CODE(REG.NUM, N.BYTES)
    LET NMEM = (DEC.NUM >= &HC0)
    IF NMEM THEN
        LET RM.NUM = RM.BITS(DEC.NUM)
        LET SOURCE = REG.CODE(RM.NUM, N.BYTES)
        LET OP2.NUM# = REG#(SOURCE)
    ELSE
        LET offset# = GetOffset#(SGRG, ADS, DEC.NUM)
        LET OP2.NUM# = lg.read#(SGRG, offset#, N.BYTES)
    END IF
    LET OP3.NUM# = fetch.code#(-N.IMM.BYTES)
    LET CARRY# = IPROD#(OP2.NUM#, OP3.NUM#, 8 * N.BYTES)
    IF CARRY# = 0 OR CARRY# = 256# ^ N.BYTES - 1 THEN
        LET CF = 0
        LET OF = 0
        LET OP1.NUM# = OP2.NUM#
        CALL REG.STORE(DEST, OP1.NUM#)
    ELSE
        LET CF = 1
        LET OF = 1
    END IF


END SUB

FUNCTION IOPermission% (PORT, N.BYTES)
REM p120
IF descrVec(TSS).Typ = NOTBUSY.TSS16 THEN
    LET RETVAL% = FALSE
ELSE
    LET saveCPL = CPL
    LET CPL = 0
    LET IOBase = lg.read#(TSS, &H66, 2)
    LET permbits# = lg.read#(TSS, IOBase + PORT \ 8, 2)
    LET CPL = saveCPL
    LET bi = (PORT AND 7)
    IF Bits#(permbits#, bi + N.BYTES - 1, bi) = 0 THEN
        LET RETVAL% = TRUE
    ELSE
        LET RETVAL% = FALSE
    END IF
END IF

LET IOPermission% = RETVAL%
END FUNCTION

FUNCTION IPROD# (A#, B#, N.BITS)
LET SIGN = 1
IF A# >= 2# ^ (N.BITS - 1) THEN
        LET SIGN = -SIGN
        LET AZ# = -(A# - 2# ^ (N.BITS))
ELSEIF A# < 0# THEN
    LET SIGN = -SIGN
    LET AZ# = -A#
ELSE
        LET AZ# = A#
END IF
IF B# >= 2# ^ (N.BITS - 1) THEN
        LET SIGN = -SIGN
        LET BZ# = -(B# - 2# ^ (N.BITS))
ELSEIF B# < 0# THEN
    LET SIGN = -SIGN
        LET BZ# = -B#
ELSE
        LET BZ# = B#
END IF

LET CZ# = PROD#(AZ#, BZ#, N.BITS)

LET A# = AZ#

IF SIGN < 0 AND AZ# > 0 THEN
        LET A# = 2# ^ (N.BITS) - AZ#
        LET CZ# = 2# ^ (N.BITS) - CZ# - 1
ELSEIF SIGN < 0 THEN
        LET A# = 0#
        LET CZ# = 2# ^ (N.BITS) - CZ#
ELSE
        LET A# = AZ#
END IF
IF CZ# = 2# ^ N.BITS - 1 OR CZ# = 0 THEN
    LET CF = 0
    LET OF = 0
ELSE
    LET CF = 1
    LET OF = 1
END IF
LET IPROD# = CZ#
END FUNCTION

FUNCTION Parity8 (X#)
LET TOTAL = 0
FOR K = 0 TO 7
    LET TOTAL = TOTAL + Bits#(X#, K, K)
NEXT
LET Parity8 = TOTAL MOD 2
END FUNCTION

FUNCTION PROD# (A#, B#, N.BITS)

REM This function returns the upper half of the product.
REM The lower half is the altered value of A#.
REM The value of B# is left alone.
REM
    LET HALF.SIZE = N.BITS \ 2
    LET AT# = Bits#(A#, N.BITS - 1, HALF.SIZE)
    LET BT# = Bits#(B#, N.BITS - 1, HALF.SIZE)
    LET AB# = Bits#(A#, HALF.SIZE - 1, 0)
    LET BB# = Bits#(B#, HALF.SIZE - 1, 0)

    LET CW# = AB# * BB#
    LET CT# = Bits#(CW#, N.BITS - 1, HALF.SIZE)
    LET CB# = Bits#(CW#, HALF.SIZE - 1, 0)

    LET DW# = AT# * BB# + AB# * BT# + CT#
    LET DT# = Bits#(DW#, N.BITS - 1, HALF.SIZE)
    LET DB# = Bits#(DW#, HALF.SIZE - 1, 0)
   
    LET EW# = AT# * BT# + DT#

    LET A# = DB# * 2 ^ HALF.SIZE + CB#
    LET PROD# = EW#
    IF EW# = 0 THEN
        LET OF = 0
        LET CF = 0
    ELSE
        LET OF = 1
        LET CF = 1
    END IF
    LET SF = CINT(RND)
    LET ZF = CINT(RND)
    LET AF = CINT(RND)
    LET PF = CINT(RND)
   
END FUNCTION

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

FUNCTION SUM# (A#, B#, C#, W)
REM  This function works on the assumption that the first two
REM     arguments are in the unsigned 1, 2, or 4 byte integer range.
REM     W is the number of bytes.
REM     The third argument is assumed to be a carry bit.
REM     It then returns a value also in this range.
REM     The overflow, sign, carry, and zero flags are set.

        LET D# = A# + B# + C#

        IF Bits#(A#, 3, 0) + Bits#(B#, 3, 0) + C# >= 16 THEN
            LET AF = 1
        ELSE
            LET AF = 0
        END IF

        IF A# >= 2# ^ (8 * W - 1) THEN
                LET AZ# = A# - 2# ^ (8 * W)
        ELSE
                LET AZ# = A#
        END IF

        IF B# >= 2# ^ (8 * W - 1) THEN
                LET BZ# = B# - 2# ^ (8 * W)
        ELSE
                LET BZ# = B#
        END IF
       
        IF AZ# + BZ# + C# < -2# ^ (8 * W - 1) OR AZ# + BZ# + C# >= 2# ^ (8 * W - 1) THEN
            LET OF = 1
        ELSE
            LET OF = 0
        END IF

        IF D# >= 2# ^ (8 * W) THEN
            LET CF = 1
            LET D# = D# - 2# ^ (8 * W)
        ELSE
            LET CF = 0
        END IF
   
        IF D# = 0# THEN
                LET ZF = 1
        ELSE
                LET ZF = 0
        END IF
      
        IF D# >= 2# ^ (8 * W - 1) THEN
            LET SF = 1
        ELSE
            LET SF = 0
        END IF
        
        LET PF = Parity8(D#)
        LET SUM# = D#
END FUNCTION

SUB Test (OS, SREG, ADS, DEC.NUM)

REM   TEST   (opcodes 84 and 85)

LET N.BYTES = WORD.SIZE(OS, DEC.NUM MOD 2)

IF DEC.NUM < &HA0 THEN
    LET DEC.NUM = fetch.code#(1)
    
    LET DEST = REG.CODE(REG.BITS(DEC.NUM), N.BYTES)

    IF DEC.NUM >= &HC0 THEN
        LET SOURCE = RM.BITS(DEC.NUM)
        LET X# = DISJ#(REG#(DEST), REG#(SOURCE), 4, N.BYTES)
    ELSE
        LET offset# = GetOffset#(SREG, ADS, DEC.NUM)
        LET IMM.NUM# = lg.read#(SREG, offset#, N.BYTES)
        LET X# = DISJ#(REG#(DEST), IMM.NUM#, 4, N.BYTES)
    END IF

ELSE

      LET DEST = REG.CODE(EAX, N.BYTES)
      LET IMM.NUM# = fetch.code#(N.BYTES)
      LET X# = DISJ#(REG#(DEST), IMM.NUM#, 4, N.BYTES)  '4 code for AND
  
END IF

END SUB

FUNCTION UserInput# (N.BYTES)
    LET FIRST.TIME = TRUE

    DO
        IF FIRST.TIME THEN
            LET FIRST.TIME = FALSE
        ELSEIF ERM$ <> "" THEN
            LOCATE 24, 1
            COLOR 1, 15
            PRINT LEFT$(ERM$ + SPACE$(39), 39);
            COLOR 15, 1
        ELSE
            LOCATE 24, 1
            COLOR 1, 15
            PRINT " Number must fit in " + STR$(ABS(N.BYTES)) + " byte(s)." + SPACE$(8);
            COLOR 15, 1
        END IF
        LET TOP# = 256# ^ ABS(N.BYTES)
        LET BOTTOM# = -TOP# / 2#
        COLOR 30
        LOCATE 25, 1
        PRINT "INPUT" + SPACE$(34);
        LOCATE 25, 8
        LINE INPUT ; "", A.IN$
        LET ERM$ = ""
        IF LTRIM$(A.IN$) = "" THEN
            LET X# = 0#
        ELSE
            LET A.IN$ = LTRIM$(RTRIM$(A.IN$))
            CALL NumParse(A.IN$, PACK$, X#, ERM$)
        END IF
    LOOP WHILE X# >= TOP# OR X# < BOTTOM# OR ERM$ <> ""
    COLOR 15, 1
    LOCATE 25, 1
    PRINT SPACE$(39);
 
    IF COMMAND.MODE THEN
        CALL Print24(SPACE$(25) + "More help: " + CHR$(25) + CHR$(24))
    ELSE
        CALL Print24(SPACE$(29) + "Enter=SetIP")
    END IF
    LET UserInput# = X#
END FUNCTION

SUB Xadd (OS, SREG, ADS, DEC.NUM, LF)
REM
REM   '     XADD subroutine

        LET N.BYTES = WORD.SIZE(OS, DEC.NUM MOD 2)
        LET DEC.NUM = fetch.code#(1)
        LET SOURCE = REG.BITS(DEC.NUM)
        IF DEC.NUM >= &HC0 AND LF THEN
            LET INSTRUCTION.RECOGNIZED = FALSE
        ELSEIF DEC.NUM >= &HC0 THEN
            LET DEST = REG.CODE(RM.BITS(DEC.NUM), N.BYTES)
            LET X# = REG#(DEST)
            CALL REG.STORE(DEST, SUM(X#, REG#(SOURCE), 0#, N.BYTES))
            CALL REG.STORE(SOURCE, X#)
        ELSE
            LET offset# = GetOffset#(SREG, ADS, DEC.NUM)
            LET IMM.NUM# = lg.read#(SREG, offset#, N.BYTES)
            LET X# = IMM.NUM#
            LET IMM.NUM# = SUM(X#, REG#(SOURCE), 0#, N.BYTES)
            CALL REG.STORE(SOURCE, X#)
            CALL lg.write(SREG, offset#, IMM.NUM#, N.BYTES)
        END IF

END SUB

SUB Xch (OS, SREG, ADS, DEC.NUM, LF)

REM XCHG Subroutine  (opcodes 86, 87 and 9y, y < 8)

IF DEC.NUM >= &H90 AND LF THEN
    LET INSTRUCTION.RECOGNIZED = FALSE
ELSEIF DEC.NUM >= &H90 THEN
    LET N.BYTES = WORD.SIZE(OS, 1)
    LET DEST = REG.CODE(EAX, N.BYTES)
    LET SOURCE = REG.CODE(DEC.NUM MOD 8, N.BYTES)
    LET X# = REG#(SOURCE)
    CALL REG.STORE(SOURCE, REG#(DEST))
    CALL REG.STORE(DEST, X#)
ELSE
    LET N.BYTES = WORD.SIZE(OS, DEC.NUM - &H86)
    LET DEC.NUM = fetch.code#(1)
    LET DEST = REG.CODE(REG.BITS(DEC.NUM), N.BYTES)
    IF DEC.NUM >= &HC0 AND LF THEN
        LET INSTRUCTION.RECOGNIZED = FALSE
    ELSEIF DEC.NUM >= &HC0 THEN
        LET SOURCE = RM.BITS(DEC.NUM)
        LET SOURCE = REG.CODE(SOURCE, N.BYTES)
        LET X# = REG#(SOURCE)
        CALL REG.STORE(SOURCE, REG#(DEST))
        CALL REG.STORE(DEST, X#)
    ELSE
        LET offset# = GetOffset#(SREG, ADS, DEC.NUM)
        LET IMM.NUM# = lg.read#(SREG, offset#, N.BYTES)
        LET X# = IMM.NUM#
        LET IMM.NUM# = REG#(DEST)
        CALL REG.STORE(DEST, X#)
        CALL lg.write(SREG, offset#, IMM.NUM#, N.BYTES)
    END IF
END IF
END SUB

