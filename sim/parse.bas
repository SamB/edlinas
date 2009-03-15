REM $INCLUDE: 'x86.bi'
DECLARE SUB RegMemEncode (REG.BITS!, REG.SIZE!, RM.TYPE$, RM$, PACK$, ERM$, LABEL$, DISPCODE, PF$, MX$, SGREG!)
DECLARE FUNCTION ImmEncode$ (value#, N.BYTES!)
DECLARE FUNCTION SegPref$ (SPECIFIED!, ENCODED!)
DECLARE FUNCTION MemEncode16$ (REG.NUM!, BASE.REG!, IND.REG!, N.DBYTES!)
DECLARE FUNCTION MemEncode32$ (REG.NUM!, SCALE!, BASE.REG!, IND.REG!, N.DBYTES!)
REM
REM  Parsing Functions
REM
DECLARE SUB InstAssemble (RAW.CODE$, INDEX!, PF$, PACKED.SOURCE$, DISPLABEL$, DISPCODE, HX$, ERM$)
DECLARE SUB MemAssemble (REG.BITS!, REG.SIZE!, RM.TYPE$, RM$, PACK$, ERM$, LABEL$, DISPCODE, PF$, MX$, SGREG!)
DECLARE SUB OperandParse (RAW$, OPTYP$, REG.NUM!, N.BYTES!, ERM$)
DECLARE SUB NumParse (RAW.SOURCE$, PACK$, NUM.VALUE#, ERM$)
DECLARE SUB Immparse (ARG$, PACKED.SOURCE$, LABEL$, ONE.B, value#, ERM$)
REM
REM   Decoding Functions
REM
DECLARE FUNCTION CUT$ (BBBX$)
DECLARE FUNCTION LCUT$ (BBBX$)
DECLARE FUNCTION ARG2$ (X$)
DECLARE FUNCTION ARG3$ (X$)
DECLARE FUNCTION COUNT.CHAR (X$, C$)
DECLARE FUNCTION REG.INDEX! (X$)
DECLARE SUB BracketCheck (X$, ERM$)

DIM SHARED N.MNEMONICS
CONST MNEM.DIM = 220     ' Needs to exceed N.MNEMONICS
DIM SHARED MNEMONIC.LIST$(MNEM.DIM), ARCHLEV(MNEM.DIM), ARITY(MNEM.DIM)
DIM SHARED INDEX.ROL, INDEX.JO, INDEX.SETO, INDEX.BT, INDEX.INSB
REM

SUB BracketCheck (X$, ERM$)
LET N = LEN(X$)
LET DEPTH = 0
LET COUNT = 0
LET K = 0
DO
    LET K = K + 1
    LET C$ = MID$(X$, K, 1)
    IF C$ = "[" THEN
        LET DEPTH = DEPTH + 1
    ELSEIF C$ = "]" THEN
        LET DEPTH = DEPTH - 1
        LET COUNT = COUNT + 1
    END IF
LOOP WHILE DEPTH >= 0 AND DEPTH <= 1 AND K < N
IF K = N AND DEPTH = 0 THEN
    LET ERM$ = ""
ELSEIF DEPTH = 2 THEN
    LET ERM$ = "No nested []s"
ELSEIF DEPTH = -1 THEN
    LET ERM$ = "Unbalanced []s"
END IF
END SUB

FUNCTION ImmEncode$ (value#, N.BYTES)
LET ImmEncode$ = BYTE.SWAP$(HexStr$(value#, 2 * N.BYTES))
END FUNCTION

SUB InstAssemble (RAW.CODE$, INDEX, PF$, PACKED.OPS$, DISPLABEL$, DISPCODE, HX$, ERM$)
REM
REM  Input INDEX is the mnemonic index.  It is a positive integer.
REM  RAW.CODE$ contains only the operand field. The mnemonic has
REM  been stripped off.
REM
REM  If the parse is successful the machine code is returned in HX$
REM  If a label that may require relocation is found it is stored in DISPLABEL$.
REM  Code describing this label is stored in DISPCODE.
REM
REM   Processed source for the graphic display is returned in PACKED.OPS$
REM   It includes operands only.
REM
REM  If the parse is successful ERM$ is returned empty.
REM  Otherwise a short error message is stored in ERM$.
REM
    LET DISPLABEL$ = ""
    LET DISPCODE = 0
    REM
    SELECT CASE PF$
        CASE "REP"
            LET RLPF$ = "F3"
        CASE "REPE"
            LET RLPF$ = "F3"
        CASE "REPNE"
            LET RLPF$ = "F2"
        CASE "LOCK"
            LET RLPF$ = "F0"
        CASE ""
            LET RLPF$ = ""
        CASE ELSE
            PRINT "Bad prefix given to InstAssemble";
    END SELECT
   
    LET MNEM$ = MNEMONIC.LIST$(INDEX)
    REM Create separate effective mnemonics for
    REM 3 IMUL cases and 2 RET cases.
    REM 2 JMPF cases and 2 CALLF cases.
    LET NC = COUNT.CHAR(RAW.CODE$, ",")
    IF MNEM$ = "IMUL" THEN
        IF NC < 3 THEN
            LET INDEX = INDEX + NC
        ELSE
            LET INDEX = INDEX + 2
        END IF
    END IF
    IF MNEM$ = "JMPF" OR MNEM$ = "CALLF" AND NC < 2 THEN
        LET INDEX = INDEX + NC
    END IF
    IF LEFT$(MNEM$, 3) = "RET" AND LEN(RAW.CODE$) > 0 THEN
        LET INDEX = INDEX + 1     ' 0-ary first, then unary.
    END IF
    IF ARCHLEV(INDEX) > USE.INSTSET THEN
        LET ERM$ = ARCHNAME$(ARCHLEV(INDEX)) + " > " + ARCHNAME$(USE.INSTSET)
        EXIT SUB
    END IF
   
    LET OPC$ = ""
   
    LET X$ = RAW.CODE$
    IF ARITY(INDEX) = 3 THEN
        IF NC <> 2 THEN
            LET ERM$ = "2 commas needed"
            EXIT SUB
        END IF

        LET COMMA1.SPOT = INSTR(X$, ",")
        LET COMMA2.SPOT = INSTR(COMMA1.SPOT + 1, X$, ",")

        IF COMMA2.SPOT = LEN(X$) THEN
            LET A3$ = ""
        ELSE
            LET A$ = MID$(X$, COMMA2.SPOT + 1)
            LET A3$ = CUT$(A$)
        END IF

        IF A3$ = "" THEN
            LET ERM$ = "arg 3 missing"
        ELSE
            CALL OperandParse(A3$, OP3.TYPE$, REG3.NUM, SIZE3, ERM$)
        END IF
        IF ERM$ <> "" THEN
            EXIT SUB
        END IF
        IF OP3.TYPE$ = "imm" THEN
            CALL Immparse(A3$, PACK3$, LABEL$, ONE.B3, VALUE3#, ERR.MES$)
        ELSEIF OP3.TYPE$ = "reg" THEN
            LET PACK3$ = UCASE$(A3$)
        END IF
    END IF
    IF ERM$ <> "" THEN
        EXIT SUB
    END IF
    IF ARITY(INDEX) >= 2 THEN
        IF ARITY(INDEX) = 2 AND NC <> 1 THEN
            LET ERM$ = "2 args, 1 comma"
        ELSE
            LET X$ = RAW.CODE$
            LET COMMA1.SPOT = INSTR(X$, ",")
            LET COMMA2.SPOT = INSTR(COMMA1.SPOT + 1, X$, ",")

            IF COMMA2.SPOT = 1 + COMMA1.SPOT THEN
                LET A2$ = ""
            ELSEIF COMMA2.SPOT = 0 THEN
                LET A$ = MID$(X$, COMMA1.SPOT + 1)
                LET A2$ = CUT$(A$)
            ELSE
                LET A$ = MID$(X$, COMMA1.SPOT + 1, COMMA2.SPOT - COMMA1.SPOT - 1)
                LET A2$ = CUT$(A$)
            END IF
           
            IF A2$ = "" THEN
                LET ERM$ = "arg 2 missing"
            END IF
        END IF
        IF ERM$ <> "" THEN
            EXIT SUB
        END IF
        IF MNEM$ = "IN" AND LEFT$(A2$, 1) = "[" AND RIGHT$(A2$, 1) = "]" THEN
            LET A$ = MID$(A2$, 2, LEN(A2$) - 2)
            LET A2$ = CUT$(A$)
        ELSEIF MNEM$ = "LEA" AND INSTR(A2$, "[") = 0 THEN
            LET A2$ = "[" + A2$ + "]"
        END IF
        CALL OperandParse(A2$, OP2.TYPE$, REG2.NUM, SIZE2, ERM$)
        IF ERM$ <> "" THEN
            EXIT SUB
        END IF
        IF OP2.TYPE$ = "imm" THEN
            CALL Immparse(A2$, PACK2$, LABEL2$, ONE.B2, VALUE2#, ERM$)
            IF ONE.B2 = 1 THEN
            END IF
        ELSEIF OP2.TYPE$ = "reg" AND SIZE2 = 1 THEN
            LET SIZE2.BIT = 1
            LET PACK2$ = UCASE$(A2$)
        ELSEIF OP2.TYPE$ = "reg" THEN
            LET SIZE2.BIT = 0
            LET PACK2$ = UCASE$(A2$)
        END IF
    END IF
    IF ERM$ <> "" THEN
        EXIT SUB
    END IF
    IF ARITY(INDEX) >= 1 THEN
        IF ARITY(INDEX) = 1 AND NC <> 0 THEN
            LET ERM$ = "1 arg, no comma"
        ELSE
            LET X$ = RAW.CODE$
            LET COMMA1.SPOT = INSTR(X$, ",")
            IF COMMA1.SPOT = 1 THEN
                LET A1$ = ""
            ELSEIF COMMA1.SPOT = 0 THEN
                LET A1$ = CUT$(X$)
            ELSE
                LET A$ = LEFT$(X$, COMMA1.SPOT - 1)
                LET A1$ = CUT$(A$)
            END IF

            IF A1$ = "" THEN
                LET ERM$ = "arg 1 missing"
                EXIT SUB
            END IF
        IF MNEM$ = "OUT" AND LEFT$(A1$, 1) = "[" AND RIGHT$(A1$, 1) = "]" THEN
            LET N = LEN(A1$)
            LET A1$ = MID$(A1$, 2, N - 2)
            LET A1$ = CUT$(A1$)
        END IF
            CALL OperandParse(A1$, OP1.TYPE$, REG1.NUM, SIZE1, ERM$)
            IF ERM$ <> "" THEN
                EXIT SUB
            END IF
            IF OP1.TYPE$ = "imm" THEN
                CALL Immparse(A1$, PACK1$, LABEL1$, ONE.B1, VALUE1#, ERR.MES$)
                LET DISPLABEL$ = LABEL1$
            ELSEIF SIZE1 = 1 THEN
                LET SIZE8.BIT = 1
                IF OP1.TYPE$ = "reg" THEN
                    LET PACK1$ = UCASE$(A1$)
                END IF
            ELSEIF OP1.TYPE$ = "reg" THEN
                LET SIZE8.BIT = 0
                LET PACK1$ = UCASE$(A1$)
            END IF
        END IF
    END IF
    IF ARITY(INDEX) = 0 AND NC > 0 THEN
        LET ERM$ = "Just 1 arg"
    END IF
    IF ERM$ <> "" THEN
        EXIT SUB
    END IF
       
    IF ARITY(INDEX) = 3 THEN
        IF SIZE1 > 4 OR SIZE2 > 4 OR SIZE3 > 4 THEN
            ERM$ = "Gen regs only"
        END IF
        SELECT CASE MNEM$
        CASE "IMUL"
            IF OP3.TYPE$ <> "imm" THEN
                LET ERM$ = "arg 3 must be imm"
            ELSEIF OP1.TYPE$ <> "reg" THEN
                LET ERM$ = "arg 1 must be reg"
            ELSEIF OP2.TYPE$ = "imm" THEN
                LET ERM$ = "arg 1 is reg or mem"
            ELSEIF SIZE2 > 0 AND SIZE2 <> SIZE1 THEN
                LET ERM$ = "Op size mismatch"
            ELSEIF SIZE1 = 1 AND NOT ONE.B3 THEN
                LET ERM$ = "1 byte or no?"
            ELSEIF SIZE1 <> USE.SIZE \ 8 THEN
                LET OSPF$ = "66"
            END IF
            IF ONE.B3 = 1 THEN
                LET OP.CODE = &H6B
                LET IMMC$ = ImmEncode$(VALUE3#, -1)
            ELSE
                LET OP.CODE = &H69
                LET IMMC$ = ImmEncode$(VALUE3#, -4)
            END IF
            IF ERM$ <> "" THEN
                EXIT SUB
            END IF
            CALL RegMemEncode(REG1.NUM, SIZE1, OP2.TYPE$, A2$, PACK2$, ERM$, DISPLABEL$, DISPCODE, MPF$, MX$, REG2.NUM)
         
        CASE "SHRD", "SHLD"
            IF MNEM$ = "SHRD" THEN
                LET OP.CODE = &HAC
            ELSEIF MNEM$ = "SHLD" THEN
                LET OP.CODE = &HA4
            END IF
            IF OP2.TYPE$ <> "reg" THEN
                LET ERM$ = "arg 2 must be reg"
            ELSEIF OP1.TYPE$ = "imm" THEN
                LET ERM$ = "arg 1 is reg or mem"
            END IF
            IF SIZE2 = 1 THEN
                LET ERM$ = "Not 1 byte"
            ELSEIF SIZE1 > 0 AND SIZE1 <> SIZE2 THEN
                LET ERM$ = "Op size mismatch"
            ELSEIF SIZE2 <> USE.SIZE \ 8 THEN
                LET OSPF$ = "66"
            END IF
            IF OP3.TYPE$ = "imm" AND ONE.B3 THEN
                LET IMMC$ = ImmEncode(VALUE3#, -1)
            ELSEIF OP3.TYPE$ = "imm" THEN
                LET ERM$ = "Arg 3 is 1 byte"
            ELSEIF OP3.TYPE$ = "reg" AND REG3.NUM = ECX AND SIZE3 = 1 THEN
                LET OP.CODE = OP.CODE + 1
            ELSE
                LET ERM$ = "Arg 3 is CL or imm"
            END IF
            LET OPC$ = "0F"
            IF ERM$ <> "" THEN
                EXIT SUB
            END IF
            CALL RegMemEncode(REG2.NUM, SIZE2, OP1.TYPE$, A1$, PACK1$, ERM$, DISPLABEL$, DISPCODE, MPF$, MX$, REG1.NUM)
        END SELECT
        LET PACKED.OPS$ = PACK1$ + "," + PACK2$ + "," + PACK3$
    ELSEIF ARITY(INDEX) = 2 AND OP1.TYPE$ = "imm" THEN
        SELECT CASE MNEM$
            CASE "OUT"
                IF NOT (OP2.TYPE$ = "reg" AND REG2.NUM = EAX) THEN
                    LET ERM$ = "arg 2 is A reg"
                ELSEIF ONE.B1 THEN
                    LET OP.CODE = &HE7 - SIZE2.BIT
                    LET IMMC$ = HexStr$(VALUE1#, 2)
                    LET PACK1$ = "[" + PACK1$ + "]"
                    LET REG1.NUM = -1
                ELSE
                    LET ERM$ = "Imm port is 1 byte"
                END IF
                IF SIZE2 > 1 AND SIZE2 <> USE.SIZE \ 8 THEN LET OSPF$ = "66"
            CASE "ENTER"
                IF OP2.TYPE$ <> "imm" THEN
                    LET ERM$ = "Arg 2 is imm"
                ELSEIF ONE.B1 THEN
                    LET OP.CODE = &HC8
                    LET IMMC$ = ImmEncode$(VALUE1#, 1) + ImmEncode$(VALUE2#, 2)
                    LET REG1.NUM = -1
                ELSE
                    LET ERM$ = "Frame size is 1 byte"
                END IF
            CASE "JMPF"
                IF OP2.TYPE$ <> "imm" THEN
                    LET ERM$ = "Arg 2 is imm"
                ELSE
                    LET OP.CODE = &HEA
                    LET IMMC$ = ImmEncode$(VALUE1#, USE.SIZE \ 8) + ImmEncode$(VALUE2#, 2)
                    LET REG1.NUM = -1
                END IF
            CASE "CALLF"
                IF OP2.TYPE$ <> "imm" THEN
                    LET ERM$ = "Arg 2 is imm"
                ELSE
                    LET OP.CODE = &H9A
                    LET IMMC$ = ImmEncode$(VALUE1#, USE.SIZE \ 8) + ImmEncode$(VALUE2#, 2)
                    LET REG1.NUM = -1
                END IF
            CASE ELSE
                    LET ERM$ = "arg 1 not imm"
        END SELECT
        IF ERM$ <> "" THEN
            EXIT SUB
        END IF
        LET PACKED.OPS$ = PACK1$ + "," + PACK2$
    ELSEIF ARITY(INDEX) = 2 AND OP1.TYPE$ = "reg" AND SIZE1 > 4 THEN
        IF MNEM$ = "MOV" THEN
            IF OP2.TYPE$ = "imm" THEN
                LET ERM$ = "Imm not ok"
            ELSEIF SIZE2 = 1 THEN
                LET ERM$ = A1$ + " isn't 1 byte"
            ELSEIF SIZE2 > 4 THEN
                LET ERM$ = "Need 1 gen reg"
            ELSEIF SIZE1 = 5 THEN  'Segment register
                LET OP.CODE = &H8E
            ELSEIF SIZE2 <> 4 OR OP2.TYPE$ <> "reg" THEN
                LET ERM$ = "Need 32 bit reg"
            ELSEIF SIZE1 = 6 THEN
                LET OPC$ = "0F"
                LET OP.CODE = &H23
            ELSEIF SIZE1 = 7 THEN
                LET OPC$ = "0F"
                LET OP.CODE = &H26
            ELSEIF SIZE1 = 8 THEN
                LET OPC$ = "0F"
                LET OP.CODE = &H22
            END IF
        ELSEIF SIZE1 = 5 THEN
            LET ERM$ = "only with MOV,PUSH, or POP"
        ELSE
            LET ERM$ = "Try MOV"
        END IF
        IF ERM$ <> "" THEN
                EXIT SUB
        END IF
        CALL RegMemEncode(REG1.NUM, SIZE2, OP2.TYPE$, A2$, PACK$, ERM$, DISPLABEL$, DISPCODE, MPF$, MX$, REG2.NUM)
        LET PACKED.OPS$ = PACK1$ + "," + PACK2$
    ELSEIF ARITY(INDEX) = 2 AND OP2.TYPE$ = "reg" AND SIZE2 > 4 THEN
        IF MNEM$ = "MOV" THEN
                IF SIZE1 = 1 THEN
                    LET ERM$ = A2$ + " isn't 1 byte"
                ELSEIF SIZE1 > 4 THEN
                    LET ERM$ = "Need 1 gen reg"
                ELSEIF SIZE2 = 5 THEN  'Segment register
                    LET OP.CODE = &H8C
                ELSEIF SIZE1 <> 4 OR OP1.TYPE$ <> "reg" THEN
                    LET ERM$ = "Need 32 bit reg"
                ELSEIF SIZE2 = 6 THEN
                    LET OPC$ = "0F"
                    LET OP.CODE = &H21
                ELSEIF SIZE2 = 7 THEN
                     LET OPC$ = "0F"
                     LET OP.CODE = &H24
                ELSEIF SIZE2 = 8 THEN
                     LET OPC$ = "0F"
                     LET OP.CODE = &H20
                END IF
        ELSEIF SIZE2 = 5 THEN
                LET ERM$ = "only with MOV,PUSH, or POP"
        ELSE
                LET ERM$ = "Try MOV"
        END IF
        IF ERM$ <> "" THEN
                EXIT SUB
        END IF
        CALL RegMemEncode(REG2.NUM, SIZE1, OP1.TYPE$, A1$, PACK$, ERM$, DISPLABEL$, DISPCODE, MPF$, MX$, REG1.NUM)
        LET PACKED.OPS$ = PACK1$ + "," + PACK2$
    ELSEIF ARITY(INDEX) = 2 AND OP1.TYPE$ = "mem" AND OP2.TYPE$ = "mem" THEN
        LET ERM$ = "2 mem ops?"
        EXIT SUB
    ELSEIF ARITY(INDEX) = 2 AND OP2.TYPE$ = "imm" THEN
        REM     LET SIZE8.BIT = 1 ' If sizes must be equal (OPCODE DECREMENT BIT)
        IF SIZE1 = 1 AND ONE.B2 = 0 THEN
            LET ERM$ = "Imm won't fit"
            EXIT SUB
        ELSEIF SIZE1 = 1 THEN
            LET SIZE8.BIT = 1
        ELSE
            LET SIZE8.BIT = 0
            LET SIZE1 = USE.SIZE \ 8
        END IF
        LET IMMC$ = ImmEncode$(VALUE2#, -SIZE1)
        IF SIZE1 <> 1 AND SIZE1 <> USE.SIZE \ 8 THEN
            LET OSPF$ = "66"
        END IF
        SELECT CASE MNEM$
        CASE "MOV"
            IF OP1.TYPE$ = "reg" THEN
                LET OP.CODE = &HB8 + REG1.NUM - SIZE8.BIT * 8
                LET REG2.NUM = -1
            ELSE
                LET OP.CODE = &HC7 - SIZE8.BIT
                LET REG2.NUM = 0
            END IF
        CASE "ADD", "OR", "ADC", "SBB", "AND", "SUB", "XOR", "CMP"
            LET OPER.CODE = INDEX - 1    ' Should have range 0-7
            IF OP1.TYPE$ = "reg" AND REG1.NUM = 0 THEN
                LET OP.CODE = 8 * OPER.CODE + 5 - SIZE8.BIT
                LET REG2.NUM = -1
            ELSE
                LET OP.CODE = &H81 - SIZE8.BIT
                LET REG2.NUM = OPER.CODE
            END IF
        CASE "ROL", "ROR", "RCL", "RCR", "SHL", "SHR", "SAR", "SAL"
            IF MNEM$ = "SAL" THEN
                LET REG2.NUM = 7
            ELSE
                LET REG2.NUM = INDEX - INDEX.ROL
            END IF
            IF VALUE2# = 1 THEN
                LET OP.CODE = &HD1 - SIZE8.BIT
                LET IMMC$ = ""  'Override default
            ELSEIF ONE.B2 THEN                    ' One byte shift count
                LET OP.CODE = &HC1 - SIZE8.BIT
                LET IMMC$ = ImmEncode(VALUE2#, -1)
            ELSE
                LET ERM$ = "Shift count is 1 byte"
            END IF
        CASE "TEST"
            IF OP1.TYPE$ = "reg" AND REG1.NUM = 0 THEN
                LET OP.CODE = &HA9 - SIZE8.BIT
                LET REG2.NUM = -1
            ELSE
                LET OP.CODE = &HF7 - SIZE8.BIT
                LET REG2.NUM = 0
            END IF
        CASE "BT", "BTS", "BTR", "BTC"
            IF ONE.B2 AND SIZE1 <> 1 THEN
                LET OPC$ = "0F"
                LET OP.CODE = &HBA
                LET REG2.NUM = INDEX - INDEX.BT + 4
                LET IMMC$ = ImmEncode$(VALUE2#, 1)
             ELSEIF SIZE1 = 1 THEN
                LET ERM$ = "Reg is not 1 byte"
             ELSE
                LET ERM$ = "Imm is 1 byte"
             END IF
        CASE "IN"
            IF OP1.TYPE$ <> "reg" OR REG1.NUM <> EAX THEN
                LET ERM$ = "arg1 is A reg"
                EXIT SUB
            ELSEIF ONE.B2 THEN
                LET IMMC$ = ImmEncode$(VALUE2#, 1)
            ELSE
                LET ERM$ = "Imm Port is 1 byte"
            END IF
            LET PACK2$ = "[" + PACK2$ + "]"
            LET OP.CODE = &HE5 - SIZE8.BIT
            LET REG2.NUM = -1
        CASE "OUT"
            LET ERM$ = "arg 2 is A reg"
            EXIT SUB
        CASE "IMUL"
                IF OP1.TYPE$ <> "reg" THEN
                    LET ERM$ = "Arg 1 must be reg"
                ELSEIF SIZE1 = 1 THEN
                    LET ERM$ = "1 byte reg not ok"
                ELSEIF ONE.B2 = 1 THEN
                    LET OP.CODE = &H6B
                    LET IMMC$ = ImmEncode$(VALUE2#, -1)
                ELSE
                    LET OP.CODE = &H69
                    LET IMMC$ = ImmEncode$(VALUE2#, -4)
                END IF
                LET REG2.NUM = REG1.NUM
                LET A2$ = A1$
                LET OP2.TYPE$ = OP1.TYPE$
                LET SAVE.PACK2$ = PACK2$
        CASE ELSE
            LET ERM$ = "Arg 2 not imm"
        END SELECT
        CALL RegMemEncode(REG2.NUM, SIZE1, OP1.TYPE$, A1$, PACK1$, ERM$, DISPLABEL$, DISPCODE, MPF$, MX$, REG1.NUM)
        IF MNEM$ = "IMUL" THEN
            LET PACK2$ = SAVE.PACK2$
        END IF
        LET PACKED.OPS$ = PACK1$ + "," + PACK2$
     ELSEIF ARITY(INDEX) = 2 AND (MNEM$ = "BSF" OR MNEM$ = "BSR" OR MNEM$ = "IMUL" OR MNEM$ = "LAR" OR MNEM$ = "LSL" OR MNEM$ = "MOVSX" OR MNEM$ = "MOVZX") THEN
        IF OP1.TYPE$ <> "reg" THEN      'Catch r,r/m types here
            LET ERM$ = "Arg 1 is reg"
        END IF
        LET OPC$ = "0F"
        SELECT CASE MNEM$
        CASE "LAR"
            LET OP.CODE = 2
        CASE "LSL"
            LET OP.CODE = 3
        CASE "BSF"
            LET OP.CODE = &HBC
        CASE "BSR"
            LET OP.CODE = &HBD
        CASE "IMUL"
            LET OP.CODE = &HAF
        CASE "MOVSX"
            LET OP.CODE = &HBF - SIZE2.BIT
            IF SIZE2 >= SIZE1 THEN
                LET ERM$ = "arg 2 < arg 1 "
            END IF
            LET SIZE2 = -1
        CASE "MOVZX"
            LET OP.CODE = &HB7 - SIZE2.BIT
            IF SIZE2 >= SIZE1 THEN
                LET ERM$ = "Arg2 < arg 1 "
            END IF
            LET SIZE2 = -1
        END SELECT
        IF SIZE1 = 1 THEN
            LET ERM$ = "Arg 1 > 1 byte"
        ELSEIF SIZE2 > -1 AND SIZE2 <> SIZE1 THEN
            LET ERM$ = "Arg size mismatch"
        ELSEIF SIZE1 <> USE.SIZE \ 8 THEN
            LET OSPF$ = "66"
        END IF
        CALL RegMemEncode(REG1.NUM, SIZE2, OP2.TYPE$, A2$, PACK$, ERM$, DISPLABEL$, DISPCODE, MPF$, MX$, REG2.NUM)
        LET PACKED.OPS$ = PACK1$ + "," + PACK2$
    ELSEIF ARITY(INDEX) = 2 AND OP2.TYPE$ = "reg" THEN
        SELECT CASE MNEM$
        CASE "MOV"
            LET OP.CODE = &H89 - SIZE2.BIT
        CASE "ADD", "OR", "ADC", "SBB", "AND", "SUB", "XOR", "CMP"
            LET OPER.CODE = INDEX - 1  'Should have range 0-7
            LET OP.CODE = 8 * OPER.CODE + 1 - SIZE2.BIT
        CASE "ROL", "ROR", "RCL", "RCR", "SHL", "SHR", "SAR", "SAL"
            IF REG2.NUM <> ECX OR SIZE2 <> 1 THEN
                LET ERM$ = "Count is CL or imm"
            ELSE
                LET OP.CODE = &HD3 - SIZE8.BIT
                LET REG2.NUM = INDEX - INDEX.ROL
                LET SIZE2 = SIZE1
                LET SIZE1 = -1
            END IF
            IF MNEMONIC$ = "SAL" THEN LET REG2.NUM = 4
        CASE "XCHG"
            IF OP.TYPE1$ = "reg" AND REG1.NUM = EAX AND SIZE8.BIT = 0 THEN
                LET OP.CODE = &H90 + REG2.NUM
                LET REG2.NUM = -1
            ELSE
                LET OP.CODE = &H87 - SIZE8.BIT
            END IF
        CASE "BT", "BTS", "BTR", "BTC"
            LET OPC$ = "0F"
            LET OP.CODE = &HA3 + 8 * (INDEX - INDEX.BT)
        CASE "TEST"
            LET OP.CODE = &H85 - SIZE2.BIT
        CASE "XADD"
            LET OPC$ = "0F"
            LET OP.CODE = &HC1 - SIZE2.BIT
        CASE "CMPXCHG"
            LET OPC$ = "0F"
            LET OP.CODE = &HA7 - SIZE2.BIT
        CASE "IN"
            IF REG2.NUM <> EDX OR SIZE2 <> 2 THEN
                LET ERM$ = "port is DX or imm byte"
            ELSEIF OP1.TYPE$ <> "reg" OR REG1.NUM <> EAX THEN
                LET ERM$ = "arg 1 is A reg"
            ELSE
                LET OP.CODE = &HED - SIZE8.BIT
                LET PACK2$ = "[" + PACK2$ + "]"
                LET REG2.NUM = -1
                LET SIZE2 = SIZE1
                LET SIZE1 = -1
            END IF
        CASE "OUT"
            IF REG2.NUM <> EAX THEN
                LET ERM$ = "arg 1 is A reg"
            ELSEIF OP1.TYPE$ <> "reg" OR REG1.NUM <> EDX OR SIZE1 <> 2 THEN
                LET ERM$ = "port is DX or imm byte"
            ELSE
                LET OP.CODE = &HEF - SIZE2.BIT
                LET PACK1$ = "[" + PACK1$ + "]"
                LET REG2.NUM = -1
                LET SIZE1 = -1
            END IF
        CASE "ARPL"
            IF SIZE2 <> 2 THEN
                LET ERM$ = "16 bit args"
            ELSE
                LET OP.CODE = &H63
                LET SIZE2 = -1
            END IF
        CASE ELSE
            LET ERM$ = "Arg 2 is not reg"
        END SELECT
        IF SIZE1 > 0 AND SIZE2 > 0 AND SIZE1 <> SIZE2 THEN
            LET ERM$ = "Op size mismatch"
        ELSEIF SIZE2 > 1 AND SIZE2 <> USE.SIZE \ 8 THEN
            LET OSPF$ = "66"
        END IF
        CALL RegMemEncode(REG2.NUM, SIZE1, OP1.TYPE$, A1$, PACK1$, ERM$, DISPLABEL$, DISPCODE, MPF$, MX$, REG1.NUM)
        LET PACKED.OPS$ = PACK1$ + "," + PACK2$
    ELSEIF ARITY(INDEX) = 2 THEN     'arg 2 is mem
        SELECT CASE MNEM$
        CASE "MOV"
            LET OP.CODE = &H8B - SIZE8.BIT
        CASE "ADD", "OR", "ADC", "SBB", "AND", "SUB", "XOR", "CMP"
            LET OPER.CODE = INDEX - 1  'Should have range 0-7
            LET OP.CODE = 8 * OPER.CODE + 3 - SIZE8.BIT
        CASE "ROL", "ROR", "RCL", "RCR", "SHL", "SHR", "SAR", "SAL"
            LET ERM$ = "Count is CL or imm"
        CASE "XCH", "XCHG"
            IF OP1.TYPE$ <> "reg" THEN
                LET ERM$ = "arg 1 is reg"
            ELSE
                LET OP.CODE = &H87 - SIZE8.BIT
            END IF
        CASE "LEA"
            IF SIZE8.BIT = 1 THEN
                LET ERM$ = "no 1 byte addresses"
            ELSE
                LET OP.CODE = &H8D
                IF LEFT$(A2$, 1) = "[" AND RIGHT$(A2$, 1) = "]" THEN
                    LET PACK2$ = MID$(A2$, 2, LEN(A2$) - 2)
                END IF
            END IF
        CASE "TEST"
            LET OP.CODE = &HF6
        CASE "BOUND"
            LET OPC$ = ""
            LET OP.CODE = &H62
        CASE "LDS"
            LET OPC$ = ""
            LET OP.CODE = &HC5
        CASE "LES"
            LET OPC$ = ""
            LET OP.CODE = &HC4
        CASE "LFS"
            LET OP.CODE = &HB4
        CASE "LGS"
            LET OPC$ = "0F"
            LET OP.CODE = &HB5
        CASE "LSS"
            LET OP.CODE = &HB2
        CASE "IN"
            LET ERM$ = "Port is DX or imm byte"
        CASE "OUT"
            LET ERM$ = "arg 2 is A reg"
        CASE ELSE
            LET ERM$ = "Arg 2 is not mem"
        END SELECT
            IF SIZE2 > 0 AND SIZE1 <> SIZE2 THEN
                LET ERM$ = "Op size mismatch"
            ELSEIF SIZE1 > 1 AND SIZE1 <> USE.SIZE \ 8 THEN
                LET OSPF$ = "66"
            END IF
            IF ERM$ <> "" THEN
                EXIT SUB
            END IF
            CALL RegMemEncode(REG1.NUM, SIZE2, OP2.TYPE$, A2$, PACK2$, ERM$, DISPLABEL$, DISPCODE, MPF$, MX$, REG2.NUM)
            LET PACKED.OPS$ = PACK1$ + "," + PACK2$
    ELSEIF ARITY(INDEX) = 1 AND OP1.TYPE$ = "reg" AND SIZE1 = 5 THEN
        IF MNEM$ = "PUSH" THEN
            SELECT CASE UCASE$(A1$)
            CASE "DS"
                LET OP.CODE = &H1E
            CASE "ES"
                LET OP.CODE = &H6
            CASE "CS"
                LET OP.CODE = &HE
            CASE "SS"
                LET OP.CODE = &H16
            CASE "FS"
                LET OPC$ = "0F"
                LET OP.CODE = &HA0
            CASE "GS"
                LET OPC$ = "0F"
                LET OP.CODE = &HA8
            END SELECT
        ELSEIF MNEM$ = "POP" THEN
            SELECT CASE UCASE$(A1$)
            CASE "DS"
                LET OP.CODE = &H1F
            CASE "ES"
                LET OP.CODE = &H7
            CASE "CS"
                LET ERM$ = "Can't  POP CS"
            CASE "SS"
                LET OP.CODE = &H17
            CASE "FS"
                LET OPC$ = "0F"
                LET OP.CODE = &HA1
            CASE "GS"
                LET OPC$ = "0F"
                LET OP.CODE = &HA9
            END SELECT
        END IF
        LET PACKED.OPS$ = PACK1$
    ELSEIF ARITY(INDEX) = 1 AND OP1.TYPE$ = "reg" AND SIZE1 > 1 AND (MNEM$ = "INC" OR MNEM$ = "DEC" OR MNEM$ = "POP" OR MNEM$ = "PUSH" OR MNEM$ = "BSWAP") THEN
        SELECT CASE MNEM$
            CASE "INC"
                LET OP.CODE = &H40 + REG1.NUM
            CASE "DEC"
                LET OP.CODE = &H48 + REG1.NUM
            CASE "PUSH"
                LET OP.CODE = &H50 + REG1.NUM
            CASE "POP"
                LET OP.CODE = &H58 + REG1.NUM
            CASE "BSWAP"
                LET OPC$ = "0F"
                LET OP.CODE = &HC8 + REG1.NUM
        END SELECT
        IF MNEM$ = "BSWAP" AND SIZE1 < 4 THEN
            LET ERM$ = "32 bit only"
        ELSEIF SIZE1 <> USE.SIZE \ 8 THEN
            LET OSPF$ = "66"
        END IF
        LET PACKED.OPS$ = PACK1$
    ELSEIF ARITY(INDEX) = 1 AND LEFT$(MNEM$, 3) = "SET" THEN
        IF OP1.TYPE$ = "imm" THEN
            LET ERM$ = "Arg is reg or mem"
        ELSEIF SIZE1 > 1 THEN
            LET ERM$ = "Set 1 byte only"
        ELSE
            LET OPC$ = "0F"
            LET OP.CODE = &H90 + INDEX - INDEX.SETO
            IF OP.CODE > &HA0 THEN LET OP.CODE = OP.CODE - &H10
            LET REG.BITS = 0
            CALL RegMemEncode(REG.BITS, SIZE1, OP1.TYPE$, A1$, PACK1$, ERM$, DISPLABEL$, DISPCODE, MPF$, MX$, REG1.NUM)
        END IF
        LET PACKED.OPS$ = PACK1$
    ELSEIF ARITY(INDEX) = 1 AND OP1.TYPE$ = "imm" THEN
        IF LABEL1$ = "" THEN
            SELECT CASE MNEM$
            CASE "PUSH"
                IF ONE.B1 THEN
                    LET OP.CODE = &H6A
                    LET IMMC$ = ImmEncode$(VALUE1#, -1)
                ELSEIF USE.SIZE = 16 THEN
                    LET OP.CODE = &H68
                    LET IMMC$ = ImmEncode$(VALUE1#, 2)
                ELSE
                    LET OP.CODE = &H68
                    LET IMMC$ = ImmEncode$(VALUE1#, 4)
                END IF
            CASE "RET", "RETN"
                LET OP.CODE = &HC2
                LET IMMC$ = ImmEncode$(VALUE1#, 2)
            CASE "RETF"
                LET OP.CODE = &HCA
                LET IMMC$ = ImmEncode$(VALUE1#, 2)
            CASE "INT"
                IF value# = 3 THEN
                    LET OP.CODE = &HCC
                    LET IMMC$ = ""
                ELSE
                    LET OP.CODE = &HCD
                    LET IMMC$ = ImmEncode$(VALUE1#, 1)
                END IF
            CASE "MUL", "DIV", "IDIV", "POP", "INC", "DEC", "NOT", "NEG"
                LET ERM$ = "Arg is reg or mem"
            CASE ELSE
                LET ERM$ = "Label needed"
            END SELECT
        ELSE   'Short jumps will be a user option.
            IF USE.SIZE = 32 THEN
                LET IMMC$ = ImmEncode$(VALUE1#, -4)
                LET DISPCODE = &HD   'Four byte IP Relative
            ELSEIF USE.SIZE = 16 THEN
                LET IMMC$ = ImmEncode$(VALUE1#, -2)
                LET DISPCODE = &HB   'Two byte IP Relative
            END IF
            LET DISPLABEL$ = LABEL1$
            SELECT CASE MNEM$
            CASE "PUSH"
                LET OP.CODE = &H68
                IF USE.SIZE = 16 THEN
                    LET DISPCODE = 2
                ELSEIF USE.SIZE = 32 THEN
                    LET DISPCODE = 4
                END IF
            CASE "JO", "JNO", "JB", "JAE", "JE", "JNE", "JBE", "JA", "JS", "JNS", "JL", "JGE", "JLE", "JG"
                LET TEST.NUM = INDEX - INDEX.JO
                LET OPC$ = "0F"
                LET OP.CODE = &H80 + TEST.NUM
            CASE "JNAE", "JNB", "JZ", "JNZ", "JNA", "JNBE", "JNGE", "JNL", "JNG", "JNLE"
                LET OP.CODE = &H80 + INDEX - INDEX.JO - 16
                LET OPC$ = "0F"
            CASE "JC", "JNC"
                LET OP.CODE = &H80 + INDEX - INDEX.JO - 32
                LET OPC$ = "0F"
            CASE "JMP"
                LET OP.CODE = &HE9
            CASE "CALL"
                LET OP.CODE = &HE8
            CASE "JMPF"
                LET OP.CODE = &HEA
            CASE "CALLF"
                LET OP.CODE = &H9A
            CASE "RET", "RETN", "RETF", "INT"
                LET ERM$ = "Just a number, no label"
            CASE "LOOPNZ", "LOOPNE"
                LET OP.CODE = &HE0
                LET IMMC$ = "00"
                LET DISPCODE = 9  ' One byte IP Relative
            CASE "LOOPZ", "LOOPE"
                LET OP.CODE = &HE1
                LET IMMC$ = "00"
                LET DISPCODE = 9 ' One byte IP Relative
            CASE "LOOP"
                LET OP.CODE = &HE2
                LET IMMC$ = "00"
                LET DISPCODE = 9 ' One byte IP Relative
            CASE "JECXZ"
                LET OP.CODE = &HE3
                LET IMMC$ = "00"
                IF USE.SIZE = 16 THEN
                        LET OSPF$ = "66"
                END IF
                LET DISPCODE = 9 ' One byte IP Relative
            CASE "JCXZ"
                LET OP.CODE = &HE3
                LET IMMC$ = "00"
                IF USE.SIZE = 32 THEN
                        LET OSPF$ = "66"
                END IF
                LET DISPCODE = 9 ' One byte IP Relative
            CASE ELSE
                LET ERM$ = "Arg is reg or mem"
            END SELECT
        END IF
        LET PACKED.OPS$ = PACK1$
    ELSEIF ARITY(INDEX) = 1 THEN
        IF SIZE1 > 1 AND SIZE1 <> USE.SIZE \ 8 THEN
            LET OSPF$ = "66"
        END IF
        SELECT CASE MNEM$
        CASE "INC"
                LET REG.BITS = 0
                LET OP.CODE = &HFF - SIZE8.BIT
            CASE "DEC"
                LET REG.BITS = 1
                LET OP.CODE = &HFF - SIZE8.BIT
            CASE "NOT"
                LET REG.BITS = 2
                LET OP.CODE = &HF7 - SIZE8.BIT
            CASE "NEG"
                LET REG.BITS = 3
                LET OP.CODE = &HF7 - SIZE8.BIT
            CASE "MUL"
                LET REG.BITS = 4
                LET OP.CODE = &HF7 - SIZE8.BIT
            CASE "IMUL"
                LET REG.BITS = 5
                LET OP.CODE = &HF7 - SIZE8.BIT
            CASE "DIV"
                LET REG.BITS = 6
                LET OP.CODE = &HF7 - SIZE8.BIT
            CASE "IDIV"
                LET REG.BITS = 7
                LET OP.CODE = &HF7 - SIZE8.BIT
            CASE "PUSH"
                LET REG.BITS = 6
                LET OP.CODE = &HFF
            CASE "POP"
                LET OP.CODE = &H8F
                LET REG.BITS = 0
            CASE "JMP"
                LET OP.CODE = &HFF
                LET REG.BITS = 4
            CASE "CALL"
                LET OP.CODE = &HFF
                LET REG.BITS = 2
            CASE "JMPF"
                LET OP.CODE = &HFF
                LET REG.BITS = 5
            CASE "CALLF"
                LET OP.CODE = &HFF
                LET REG.BITS = 3
        END SELECT
        SELECT CASE MNEM$
            CASE "SLDT"
                LET OP.CODE = 0
                LET REG.BITS = 0
            CASE "STR"
                LET OP.CODE = 0
                LET REG.BITS = 1
            CASE "LLDT"
                LET OP.CODE = 0
                LET REG.BITS = 2
            CASE "LTR"
                LET OP.CODE = 0
                LET REG.BITS = 3
            CASE "VERR"
                LET OP.CODE = 0
                LET REG.BITS = 4
            CASE "VERW"
                LET OP.CODE = 0
                LET REG.BITS = 5
            CASE "SGDT"
                LET OP.CODE = 1
                LET REG.BITS = 0
            CASE "SIDT"
                LET OP.CODE = 1
                LET REG.BITS = 1
            CASE "LGDT"
                LET OP.CODE = 1
                LET REG.BITS = 2
            CASE "LIDT"
                LET OP.CODE = 1
                LET REG.BITS = 3
            CASE "SMSW"
                LET OP.CODE = 1
                LET REG.BITS = 4
            CASE "LMSW"
                LET OP.CODE = 1
                LET REG.BITS = 6
            CASE "INVLPG"
                LET OP.CODE = 1
                LET REG.BITS = 7
        END SELECT
        IF OP.CODE = 0 OR OP.CODE = 1 THEN  'OF00 & 0F01
            LET OPC$ = "0F"
            IF SIZE1 <> 2 THEN
                LET ERM$ = "2 bytes!"
            ELSE
                REM LET OSPF$ = ""
            END IF
        END IF
            IF SIZE1 = 1 AND (MNEM$ = "PUSH" OR MNEM$ = "POP") THEN
                LET ERM$ = "Not 1 byte"
            ELSEIF OP1.TYPE$ = "reg" AND (MNEM$ = "SGDT" OR MNEM$ = "SIDT" OR MNEM$ = "LIDT" OR MNEM$ = "LGDT") THEN
                LET ERM$ = "mem only"
            ELSEIF REG.BITS > -1 THEN
                 CALL RegMemEncode(REG.BITS, SIZE1, OP1.TYPE$, A1$, PACK1$, ERM$, DISPLABEL$, DISPCODE, MPF$, MX$, REG1.NUM)
            END IF
        IF ERM$ <> "" THEN
            EXIT SUB
        END IF
        LET PACKED.OPS$ = PACK1$
 ELSE
        IF LEN(RAW.CODE$) > 0 THEN
            LET ERM$ = "arg for " + MNEM$ + "?"
            
            EXIT SUB
        END IF
        IF LEFT$(MNEM$, 4) = "REPN" THEN
            LET OPC$ = "F2"
            LET MNEM$ = "MID$(MNEM$,6)"
        ELSEIF LEFT$(MNEM$, 3) = "REP " THEN
            LET OPC$ = "F3"
            LET MNEM$ = LTRIM$(LEFT$(MNEM$, 4))
        END IF

      
        SELECT CASE MNEM$
        CASE "AAA"
            LET OP.CODE = &H37
        CASE "AAD"
            LET OPC$ = "D5"
            LET OP.CODE = &HA
        CASE "AAM"
            LET OPC$ = "D4"
            LET OP.CODE = &HA
        CASE "AAS"
            LET OP.CODE = &H3F
        CASE "CBW"
            LET OP.CODE = &H98
            IF USE.SIZE = 32 THEN
                LET OPC$ = "66"
            END IF
        CASE "CDQ"
            LET OP.CODE = &H99
            IF USE.SIZE = 16 THEN
                LET OPC$ = "66"
            END IF
        CASE "CLC"
            LET OP.CODE = &HF8
        CASE "CLD"
            LET OP.CODE = &HFC
        CASE "CLI"
            LET OP.CODE = &HFA
        CASE "CLTS"
            LET OPC$ = "0F"
            LET OP.CODE = &H6
        CASE "CMC"
            LET OP.CODE = &HF5
        CASE "CMPSB"
            LET OP.CODE = &HA6
        CASE "CMPSD", "CMPSW"
            LET OP.CODE = &HA7
        CASE "CWD"
            LET OP.CODE = &H99
            IF USE.SIZE = 32 THEN
                LET OPC$ = "66"
            END IF
        CASE "CWDE"
            LET OP.CODE = &H98
            IF USE.SIZE = 16 THEN
                LET OPC$ = "66"
            END IF
        CASE "DAA"
            LET OP.CODE = &H27
        CASE "DAS"
            LET OP.CODE = &H2F
        CASE "HLT"
            LET OP.CODE = &HF4
        CASE "INSB"
            LET OP.CODE = &H6C
        CASE "INSW", "INSD"
            LET OP.CODE = &H6D
        CASE "INTO"
            LET OP.CODE = &HCE
        CASE "INVD"
            LET OPC$ = "0F"
            LET OP.CODE = &H8
        CASE "IRET", "IRETD"
            LET OP.CODE = &HCF
        CASE "LAHF"
            LET OP.CODE = &H9F
        CASE "LEAVE"
            LET OP.CODE = &HC9
        CASE "LOADALL286"
            LET OPC$ = "0F"
            LET OP.CODE = &H5
        CASE "LODSB"
            LET OP.CODE = &HAC
        CASE "LODSW", "LODSD"
            LET OP.CODE = &HAD
        CASE "MOVSB"
            LET OP.CODE = &HA4
        CASE "MOVSW", "MOVSD"
            LET OP.CODE = &HA5
        CASE "NOP"
            LET OP.CODE = &H90
        CASE "OUTSB"
            LET OP.CODE = &H6E
        CASE "OUTSW", "OUTSD"
            LET OP.CODE = &H6F
        CASE "POPA", "POPAD"
            LET OP.CODE = &H61
        CASE "POPF"
            LET OP.CODE = &H9D
            IF USE.SIZE = 32 THEN
                LET OSPF$ = "66"
            END IF
        CASE "POPFD"
            LET OP.CODE = &H9D
            IF USE.SIZE = 16 THEN
                LET OSPF$ = "66"
            END IF
        CASE "PUSHA", "PUSHAD"
            LET OP.CODE = &H60
        CASE "PUSHF"
            LET OP.CODE = &H9C
            IF USE.SIZE = 32 THEN
                LET OSPF$ = "66"
            END IF
        CASE "PUSHFD"
            LET OP.CODE = &H9C
            IF USE.SIZE = 16 THEN
                LET OSPF$ = "66"
            END IF
        CASE "RET", "RETN"
            LET OP.CODE = &HC3
        CASE "RETF"
            LET OP.CODE = &HCB
        CASE "SAHF"
            LET OP.CODE = &H9E
        CASE "SCASB"
            LET OP.CODE = &HAE
        CASE "SCASW", "SCASD"
            LET OP.CODE = &HAF
        CASE "STC"
            LET OP.CODE = &HF9
        CASE "STD"
            LET OP.CODE = &HFD
        CASE "STI"
            LET OP.CODE = &HFB
        CASE "STOSB"
            LET OP.CODE = &HAA
        CASE "STOSW", "STOSD"
            LET OP.CODE = &HAB
        CASE "WAIT"
            LET OP.CODE = &H9B
        CASE "WBINVD"
            LET OPC$ = "0F"
            LET OP.CODE = &H9
        CASE "XLATB"
            LET OP.CODE = &HD7
        END SELECT
       
    END IF
    IF ERM$ <> "" THEN
        EXIT SUB
    END IF
   
    IF OP.CODE > -1 THEN
        LET OPC$ = OPC$ + HexStr$(CDBL(OP.CODE), 2)
    ELSE
        LET OPC$ = ""
    END IF
    IF DISPLABEL$ <> "" THEN
        LET DISPCODE = (LEN(IMMC$) \ 2) * 16 + DISPCODE
        LET DISPCODE = (DISPCODE OR 8) ' Should be in the symbol table?!?
    ELSE
        LET DISPCODE = 0
    END IF
    LET HX$ = RLPF$ + OSPF$ + MPF$ + OPC$ + MX$ + IMMC$
END SUB

SUB LineParse (MNEMONIC$, INDEX, LABEL$, PF$, CODE$, COMMENT.FIELD$, ERM$)
REM
REM Prefixes are returned in PF$.
REM Both the prefix and the mnemonic are returned in MNEMONIC$.
REM LineParse checks the applicability of the REP prefixes
REM InstAssemble checks the applicability of LOCK
REM INDEX is returned positive if an assembler mnemonic is found.
REM INDEX is returned negative if an assembler directive is found.
REM INDEX is returned zero if either just a label or a comment is found.
REM
LET ERM$ = ""
LET COMMENT.FIELD$ = ""
LET LABEL$ = ""
LET PF$ = ""
LET UC$ = ""

LET CODE$ = LCUT$(CODE$)
LET N = INSTR(CODE$, CHR$(10))
IF N > 0 THEN
    LET CODE$ = LEFT$(CODE$, N - 1)
    ERM$ = "Ctrl-M missing."
    EXIT SUB
END IF
IF LEN(CODE$) = 0 THEN
    LET ERM$ = "Blank!"
    EXIT SUB
ELSEIF CODE$ = "." OR CODE$ = CHR$(26) OR CODE$ = CHR$(4) OR CODE$ = CHR$(27) THEN
    LET ERM$ = "Quit!"
    EXIT SUB
ELSEIF CODE$ = "?" OR CODE$ = "H" OR CODE$ = "h" THEN
    LET ERM$ = "Help!"
    EXIT SUB
END IF

LET SEMICOLON.SPOT = INSTR(CODE$, ";")

IF SEMICOLON.SPOT > 0 THEN
    LET COMMENT.FIELD$ = MID$(CODE$, SEMICOLON.SPOT)
    IF SEMICOLON.SPOT = 1 THEN
        LET INDEX = 0  ' Only a Comment line.
        EXIT SUB
    ELSE
        LET CODE$ = LEFT$(CODE$, SEMICOLON.SPOT - 1)
    END IF
END IF
REM
REM Get the label if it exists, and the mnemonic.
REM
    LET MNEMONIC.FOUND = FALSE
    LET LABEL$ = ""
    LET MNEMONIC$ = ""
DO
    LET N = LEN(CODE$)
    LET K = 1
    LET END.FOUND = FALSE
    REM Check for Spaces, tabs, colons, equal signs, and left brackets.
    WHILE K <= N AND NOT END.FOUND
        LET C = ASC(MID$(CODE$, K, 1))
        IF C = 32 OR C = 9 OR C = 91 THEN  ' Space, tab, left bracket found
            LET END.FOUND = TRUE
            LET KSTART = K
        ELSEIF C = 61 THEN        ' =  found
            LET END.FOUND = TRUE
            LET K = K + 1
            LET KSTART = K
        ELSEIF C = 58 THEN        ' colon found
            LET END.FOUND = TRUE
            LET KSTART = K + 1
        ELSE
            LET K = K + 1
        END IF
    WEND

    LET WORD$ = MID$(CODE$, 1, K - 1)
    IF END.FOUND THEN
        LET CODE$ = LCUT$(MID$(CODE$, KSTART))
    ELSE
        LET CODE$ = ""
    END IF
    LET INDEX = 0
    LET K = 0
    WHILE INDEX = 0 AND K < N.MNEMONICS
        LET K = K + 1
        IF UCASE$(WORD$) = MNEMONIC.LIST$(K) THEN
            LET INDEX = K
            LET MNEMONIC$ = MNEMONIC.LIST$(K)
        END IF
    WEND

    IF INDEX = 0 AND LEFT$(WORD$, 1) = "." THEN
        LET K = 0
        WHILE INDEX = 0 AND K < N.DIRECTIVES
            LET K = K + 1
            IF LCASE$(WORD$) = DIRECTIVE.LIST$(K) THEN
                LET INDEX = -K
                LET MNEMONIC$ = DIRECTIVE.LIST$(K)
            END IF
        WEND
        IF INDEX = 0 THEN
            LET ERM$ = "Unknown directive"
        END IF
    END IF
    LET UC$ = UCASE$(WORD$)
    IF UC$ = "LOCK" OR UC$ = "REP" OR UC$ = "REPE" OR UC$ = "REPNE" OR UC$ = "REPZ" OR UC$ = "REPNZ" THEN
        IF PF$ = "" THEN
            LET PF$ = UC$
            LET UC$ = ""
        ELSE
            LET ERM$ = "Prefix conflict"
        END IF
    ELSEIF INDEX <> 0 THEN
        LET MNEMONIC.FOUND = TRUE
    ELSEIF LABEL$ = "" THEN
        LET LABEL$ = WORD$
    ELSEIF LABEL$ <> "" THEN
        LET ERM$ = LABEL$ + ", " + WORD$ + " not mnemonics"
    END IF
LOOP UNTIL CODE$ = "" OR MNEMONIC.FOUND OR ERM$ <> ""
    IF ERM$ <> "" THEN
        EXIT SUB
    ELSEIF PF$ <> "" AND PF$ <> "LOCK" AND (INDEX < INDEX.INSB OR INDEX > (INDEX.INSB + 20)) THEN
        LET ERM$ = "Can't repeat " + WORD$
        EXIT SUB  ' There are 21 mnemonics which take REP
    ELSEIF PF$ <> "" THEN
        LET MNEMONIC$ = PF$ + SPACE$(1) + MNEMONIC$
    END IF

IF LABEL$ <> "" THEN
    FOR K = 1 TO PROG.LEN
        IF LABEL.LIST$(K) = LABEL$ THEN
            LET ERM$ = "Label in use already"
        END IF
    NEXT
END IF
END SUB

SUB MemAssemble (REG.BITS, REG.SIZE, RM.TYPE$, RMARG$, PACK$, ERM$, LBL$, DPC, PF$, MX$, SGREG)
REM  If the parse is successful ERM$ will be returned empty.
REM  If not it will contain a short message.
REM  REG.BITS is the 3 bit integer to be inserted into the register field.
REM   It's value is left untouched.
REM  RMARG$ is the input code which is a memory designation stripped of
REM    segment or size designation.
REM  The SEGREG is the specified segment, -1 means take the default.
REM    The return value of SEGREG is the segment encoded.
REM     If the value was changed and not originally -1, a prefix is required.
REM  ADLEN is the address length specified by the source code.
REM  Its input value is not used.
REM  Arg for packed source is needed.
REM
CALL BracketCheck(RMARG$, ERM$)
IF ERM$ <> "" THEN
    EXIT SUB
END IF
IF LEFT$(RMARG$, 1) = "[" THEN
    LET RMARG$ = MID$(RMARG$, 2)
END IF
LET N = LEN(RMARG$)
LET ADDITIVE.CONSTANT# = 0#
LET SCALEF# = 1#
LET BASE.REG1 = -1
LET BASE.REG2 = -1
LET IND.REG = -1
LET LBL$ = ""
LET SIGN.IS.NEG = FALSE
LET TERM.IN.MIDST = FALSE
LET START = 0
LET PAREN.DEPTH = 0
DO
    LET K = START
    DO
        LET K = K + 1
        LET C$ = MID$(RMARG$, K, 1)
        IF C$ = "(" THEN
            LET PAREN.DEPTH = PAREN.DEPTH + 1
        ELSEIF C$ = ")" THEN
            LET PAREN.DEPTH = PAREN.DEPTH - 1
        END IF
        IF PAREN.DEPTH > 0 THEN
            LET C$ = ""
        END IF
    LOOP WHILE C$ <> "+" AND C$ <> "-" AND C$ <> "*" AND C$ <> "[" AND K < N
    IF K = N AND C$ <> "+" AND C$ <> "-" AND C$ <> "*" AND C$ <> "[" THEN
        LET K = K + 1
    END IF
    REM
    REM START and K index left and right separating characters
    REM
    IF K = START + 1 THEN
        IF K <> N AND START <> 0 THEN
            LET ERM$ = "Empty field: " + MID$(RMARG$, START, 2)
            EXIT SUB
        ELSEIF START = 0 THEN
            LET ERM$ = "Empty field: " + MID$(RMARG$, 1, 1)
            EXIT SUB
        ELSE
            LET ERM$ = "Empty field: " + MID$(RMARG$, N, 1)
            EXIT SUB
        END IF
    END IF
    REM
    LET X$ = MID$(RMARG$, START + 1, K - START - 1)
    REM
    REM X$ is everything in between
    REM
    LET START = K
    REM
    REM Get rid of right brackets:
    REM
    LET X$ = LTRIM$(RTRIM$(X$))
    IF RIGHT$(X$, 1) = "]" THEN
        LET M = LEN(X$)
        IF M = 1 THEN
            LET ERM$ = "Empty field: " + X$
            EXIT SUB
        END IF
        LET X$ = LEFT$(X$, M - 1)
        LET X$ = LTRIM$(RTRIM$(X$))
    END IF

    LET REG.NUM = REG.INDEX(UCASE$(X$))
    IF REG.NUM > 15 THEN
        LET ERM$ = "Need 16 or 32 bit gen reg"
        EXIT SUB
    END IF
    IF TERM.IN.MIDST THEN
        IF REG.NUM >= 0 AND IND.REG >= 0 THEN
            LET ERM$ = "Reg can't be scale"
            EXIT SUB
        ELSEIF REG.NUM >= 0 THEN
            LET IND.REG = REG.NUM
        ELSE
            CALL Immparse(X$, PACK$, INLBL$, ONE.B, value#, ERM$)
            IF INLBL$ = "" THEN
                LET SCALEF# = SCALEF# * value#
            ELSE
                LET ERM$ = "Labels can't be scaled"
                EXIT SUB
            END IF
        END IF
        IF C$ <> "*" THEN
            IF IND.REG < 0 AND SIGN.IS.NEG THEN
                LET ADDITIVE.CONSTANT# = ADDITIVE.CONSTANT# - SCALEF#
                LET SCALEF# = 1
            ELSEIF IND.REG < 0 THEN
                LET ADDITIVE.CONSTANT# = ADDITIVE.CONSTANT# + SCALEF#
                LET SCALEF# = 1
            ELSEIF SCALEF# <> 1# AND SCALEF# <> 2# AND SCALEF# <> 4# AND SCALEF# <> 8# THEN
                LET ERM$ = "Scale is 1, 2, or 4"
                EXIT SUB
            ELSEIF SCALEF# = 1# THEN
                IF BASE.REG1 < 0 THEN
                    LET BASE.REG1 = IND.REG
                    LET IND.REG = -1
                ELSEIF BASE.REG2 < 0 THEN
                    LET BASE.REG2 = IND.REG
                    LET IND.REG = -1
                ELSE
                    LET ERM$ = "2 regs max!"
                    EXIT SUB
                END IF
            END IF
            LET TERM.IN.MIDST = FALSE
            LET SIGN.IS.NEG = (C$ = "-")
        END IF
    ELSEIF C$ = "*" THEN
        IF REG.NUM >= 0 AND IND.REG >= 0 THEN
            LET ERM$ = "1 scaled reg only"
            EXIT SUB
        ELSEIF REG.NUM >= 0 THEN
            LET IND.REG = REG.NUM
        ELSE
            CALL Immparse(X$, PACK$, INLBL$, ONE.B, value#, ERM$)
            IF INLBL$ = "" THEN
                LET SCALEF# = SCALEF# * value#
            ELSE
                LET ERM$ = "Labels can't be scaled"
                EXIT SUB
            END IF
        END IF
        LET TERM.IN.MIDST = TRUE
    ELSE
        IF REG.NUM >= 0 THEN
            IF BASE.REG1 < 0 THEN
                LET BASE.REG1 = REG.NUM
            ELSEIF BASE.REG2 < 0 THEN
                LET BASE.REG2 = REG.NUM
            ELSE
                LET ERM$ = "2 regs max!"
                EXIT SUB
            END IF
            IF SIGN.IS.NEG THEN
                LET ERM$ = "Can't subtract reg"
            END IF
        ELSE
            CALL Immparse(X$, PACK$, INLBL$, ONE.B, value#, ERM$)
            IF ERM$ <> "" THEN
                EXIT SUB
            END IF
            IF INLBL$ = "" AND SIGN.IS.NEG THEN
                LET ADDITIVE.CONSTANT# = ADDITIVE.CONSTANT# - value#
            ELSEIF INLBL$ = "" THEN
                LET ADDITIVE.CONSTANT# = ADDITIVE.CONSTANT# + value#
            ELSEIF LBL$ <> "" THEN
                LET ERM$ = "1 label only"
                EXIT SUB
            ELSE
                LET LBL$ = INLBL$
                REM Modify additive constant here? No in the loader
            END IF
            LET SIGN.IS.NEG = (C$ = "-")
        END IF
        LET TERM.IN.MIDST = FALSE
    END IF

LOOP WHILE START < N

REM
REM     Check results.
REM
IF IND.REG >= 0 AND BASE.REG1 >= 0 AND BASE.REG2 >= 0 THEN
    LET ERM$ = "2 regs max!"
    EXIT SUB
ELSEIF IND.REG >= 16 OR BASE.REG1 >= 16 OR BASE.REG2 >= 16 THEN
    LET ERM$ = "No 1 byte addresses"
    EXIT SUB
ELSEIF IND.REG >= 0 AND (BASE.REG1 >= 8 OR IND.REG >= 8) THEN
    LET ERM$ = "Scaling is 32 bit!"
    EXIT SUB
ELSEIF IND.REG >= 0 AND IND.REG = ESP THEN
    LET ERM$ = "Can't scale stack!"
    EXIT SUB
ELSEIF BASE.REG2 >= 0 AND (BASE.REG2 >= 8 OR BASE.REG1 >= 8) AND (BASE.REG1 < 8 OR BASE.REG2 < 8) THEN
    LET ERM$ = "8 bits or 16?"
    EXIT SUB
ELSEIF BASE.REG2 >= 0 AND BASE.REG2 < 8 THEN
    IF SGREG = SS AND (BASE.REG2 = ESP OR BASE.REG2 = EBP) THEN
        SWAP BASE.REG2, BASE.REG1
    ELSEIF SGREG = DS AND (BASE.REG2 <> ESP AND BASE.REG2 <> EBP) THEN
        SWAP BASE.REG2, BASE.REG1
    END IF
    SWAP BASE.REG2, IND.REG
    LET ADLEN = 32
ELSEIF IND.REG >= 0 THEN
    LET ADLEN = 32
ELSEIF BASE.REG1 >= 0 AND BASE.REG1 < 8 THEN
    LET ADLEN = 32
ELSEIF BASE.REG2 >= 0 THEN
    IF BASE.REG2 = BX OR BASE.REG2 = BP THEN
        SWAP BASE.REG2, BASE.REG1
    END IF
    IF BASE.REG1 <> BX AND BASE.REG1 <> BP THEN
        LET ERM$ = "16 bit base is BX or BP"
        EXIT SUB
    END IF
    IF BASE.REG2 <> SI AND BASE.REG2 <> DI THEN
        LET ERM$ = "16 bit index is SI or DI"
        EXIT SUB
    END IF
    REM 16 bits ok!
    SWAP BASE.REG2, IND.REG
    LET ADLEN = 16
ELSEIF BASE.REG1 >= 8 THEN
    IF BASE.REG1 < 3 OR BASE.REG1 = 4 THEN
        LET ERM$ = "Need BX,BP,SI,or DI"
        EXIT SUB
    END IF
    IF BASE.REG1 = SI OR BASE.REG1 = DI THEN
        SWAP BASE.REG1, IND.REG
    END IF
    LET ADLEN = 16
ELSEIF BASE.REG1 >= 0 THEN
    LET ADLEN = 32
ELSE
    LET ADLEN = USE.SIZE
END IF
IF ADLEN <> USE.SIZE THEN
    LET PF$ = "67"
ELSE
    LET PF$ = ""
END IF


IF ADDITIVE.CONSTANT# < -2# ^ (31) AND ADDITIVE.CONSTANT# >= 2# ^ (31) THEN
    LET ERM$ = "32 bits max!"
    EXIT SUB
ELSEIF ADLEN = 32 THEN
    IF LBL$ <> "" OR (BASE.REG1 < 0) OR ADDITIVE.CONSTANT# < -2# ^ (7) OR ADDITIVE.CONSTANT# >= 2# ^ (7) THEN
        LET N.DBYTES = 4
    ELSEIF ADDITIVE.CONSTANT# <> 0 OR (BASE.REG1 = EBP) THEN
        LET N.DBYTES = 1
    ELSE
        LET N.DBYTES = 0
    END IF
ELSEIF ADLEN = 16 THEN
    IF LBL$ <> "" OR (BASE.REG1 = -1) OR ADDITIVE.CONSTANT# < -2# ^ (7) OR ADDITIVE.CONSTANT# >= 2# ^ (7) THEN
        LET N.DBYTES = 2
    ELSEIF ADDITIVE.CONSTANT# <> 0 OR (BASE.REG1 = BP AND IND.REG = -1) THEN
        LET N.DBYTES = 1
    ELSE
        LET N.DBYTES = 0
    END IF
END IF
       
LET SCALE = SCALEF#
IF ADLEN = 32 THEN
    IF BASE.REG1 = ESP OR BASE.REG1 = EBP THEN
        LET SGREG = SS
    ELSE
        LET SGREG = DS
    END IF
    LET MEMB$ = MemEncode32$(REG.BITS, SCALE, BASE.REG1, IND.REG, N.DBYTES)
ELSE
    IF BASE.REG1 = BP THEN
        LET SGREG = SS
    ELSE
        LET SGREG = DS
    END IF
    LET MEMB$ = MemEncode16$(REG.BITS, BASE.REG1, IND.REG, N.DBYTES)
END IF

LET PACK$ = ""
IF BASE.REG1 > -1 THEN
    LET PACK$ = REGISTER.LIST$(BASE.REG1)
END IF
IF SCALE > 1 AND PACK$ <> "" THEN
    LET PACK$ = PACK$ + "+" + CHR$(48 + CINT(SCALE)) + REGISTER.LIST$(IND.REG)
ELSEIF SCALE > 1 THEN
    LET PACK$ = CHR$(48 + CINT(SCALE)) + REGISTER.LIST$(IND.REG)
ELSEIF IND.REG > -1 AND PACK$ <> "" THEN
    LET PACK$ = PACK$ + "+" + REGISTER.LIST$(IND.REG)
ELSEIF IND.REG > -1 THEN
    LET PACK$ = REGISTER.LIST$(IND.REG)
END IF
IF LBL$ <> "" AND PACK$ <> "" THEN
    LET PACK$ = LEFT$(LBL$, 3) + "+" + PACK$
ELSEIF LBL$ <> "" THEN
    LET PACK$ = LBL$
END IF
LET DISPLACEMENT$ = ImmEncode$(ADDITIVE.CONSTANT#, -N.DBYTES)
IF ADDITIVE.CONSTANT# > 0# AND PACK$ <> "" THEN
    LET PACK$ = PACK$ + "+" + LTRIM$(STR$(ADDITIVE.CONSTANT#))
ELSEIF ADDITIVE.CONSTANT# < 0# THEN
    LET PACK$ = PACK$ + STR$(ADDITIVE.CONSTANT#)
ELSEIF ADDITIVE.CONSTANT# >= 0# AND PACK$ = "" THEN
    LET PACK$ = LTRIM$(STR$(ADDITIVE.CONSTANT#))
END IF

LET PACK$ = "[" + PACK$ + "]"
LET MX$ = MEMB$ + DISPLACEMENT$
LET DP.BYTES = LEN(DISPLACEMENT$) \ 2     ' BYTES.BACK encoding.
IF DP.BYTES = 1 THEN
    LET DPC = 0
ELSEIF DP.BYTES = 2 THEN
    LET DPC = 2
ELSEIF DP.BYTES = 4 THEN
    LET DPC = 4
END IF
LET DPC = DPC + 16 * DP.BYTES       ' BYTES.BACK encoding.
END SUB

FUNCTION MemEncode16$ (REG.NUM, BASE.REG, IND.REG, N.DBYTES)

   
LET DISP.BITS = N.DBYTES
   
IF N.DBYTES = 0 AND BASE.REG = BP AND IND.REG < 0 THEN
    REM
    REM This case is not supposed to be here. Block it in MemParse.
    REM
ELSE

    IF IND.REG < 0 THEN         'BX or BP
        LET PARITY = (BP - BASE.REG) \ 2
    ELSE
        LET PARITY = IND.REG - SI
    END IF
    IF IND.REG < 0 AND BASE.REG < O THEN
        LET RM.NUM = 6
        LET RM = REG.NUM * 8 + RM.NUM
    ELSE
        IF IND.REG < 0 THEN
            LET MID.NUM = 3
        ELSEIF BASE.REG < 0 THEN
            LET MID.NUM = 2
        ELSE
            LET MID.NUM = (BASE.REG - BX) \ 2
        END IF
   
        LET RM.NUM = 2 * MID.NUM + PARITY
        LET RM = DISP.BITS * 64 + REG.NUM * 8 + RM.NUM
    END IF
    
    LET MemEncode16$ = HexStr$(CDBL(RM), 2)

END IF

END FUNCTION

FUNCTION MemEncode32$ (REG.NUM, SCALE, BASE.REG, IND.REG, N.DBYTES)

REM     REG.NUM is /r.   This may or may not code a register.
REM     BASE.REG is the base register.  -1 if there is none.
REM         The default segment register is SS if this is ESP or EBP
REM                                         DS otherwise.
REM     IND.REG is the index register.  -1 if there is none.
REM     N.DBYTES is the number of displacement bytes to follow.

LET SIB = -1     ' Default for no SIB byte.

IF N.DBYTES = 4 THEN
    LET DISP.BITS = 2
ELSEIF N.DBYTES = 1 THEN
    LET DISP.BITS = 1
ELSEIF N.DBYTES = 0 THEN
    LET DISP.BITS = 0
END IF

IF SCALE = 8 THEN
    LET SCALE.BITS = 3
ELSEIF SCALE = 4 THEN
    LET SCALE.BITS = 2
ELSEIF SCALE = 2 THEN
    LET SCALE.BITS = 1
ELSEIF SCALE = 1 THEN
    LET SCALE.BITS = 0
END IF

IF IND.REG = ESP THEN

ELSEIF N.DBYTES = 0 AND BASE.REG = EBP THEN

ELSEIF BASE.REG = -1 AND (N.DBYTES = 0 OR N.DBYTES = 1) THEN
    REM
    REM This case is not supposed to be here. Block it in MemAssemble.
    REM
ELSEIF IND.REG >= 0 AND BASE.REG >= 0 THEN
    LET RM = DISP.BITS * 64 + REG.NUM * 8 + ESP
    LET SIB = SCALE.BITS * 64 + IND.REG * 8 + BASE.REG
ELSEIF BASE.REG = ESP THEN
    LET RM = DISP.BITS * 64 + REG.NUM * 8 + BASE.REG
    LET SIB = &H24
ELSEIF IND.REG >= 0 THEN
    LET RM = 0 * 64 + REG.NUM * 8 + ESP
    LET SIB = SCALE.BITS * 64 + IND.REG * 8 + EBP
ELSEIF BASE.REG >= 0 THEN
    LET RM = DISP.BITS * 64 + REG.NUM * 8 + BASE.REG
ELSE
    LET RM = 0 * 64 + REG.NUM * 8 + EBP
END IF


LET RM.BYTE$ = HexStr$(CDBL(RM), 2)

IF SIB <> -1 THEN
    LET SIB.BYTE$ = HexStr$(CDBL(SIB), 2)
ELSE
    LET SIB.BYTE$ = ""
END IF
  
LET MemEncode32$ = RM.BYTE$ + SIB.BYTE$

END FUNCTION

SUB OperandParse (RAW$, OPTYP$, REG.NUM, N.BYTES, ERM$)
REM
REM               Operand Parsing subroutine.
REM
REM    This program distinguishes three operand types:
REM     register, memory, and immediate.  Memory operands
REM     must have brackets. Operands which are not registers
REM     and have no left bracket will be returned as immediate.
REM     Memory operands will be checked for operand size indicators
REM     and segment override prefixes.
REM
    LET RAW$ = LTRIM$(RAW$)
    LET RAW$ = RTRIM$(RAW$)
    LET BRACKET.POS = INSTR(RAW$, "[")
    LET N.BYTES = -1
   
    LET REG.NUM = REG.INDEX(RAW$)
    IF REG.NUM > -1 THEN
        LET OPTYP$ = "reg"
        IF REG.NUM < 8 THEN
            LET N.BYTES = 4
        ELSEIF REG.NUM < 16 THEN
            LET N.BYTES = 2
        ELSEIF REG.NUM < 24 THEN
            LET N.BYTES = 1
        ELSE
            LET N.BYTES = (REG.NUM - 24) \ 8 + 5  ' Just a code.
        END IF
        LET REG.NUM = REG.NUM MOD 8
        IF REG.NUM > 5 AND N.BYTES = 5 THEN   'LDT & TSS
            LET ERM$ = RAW$ + " not addressable"
            EXIT SUB
        END IF
    ELSEIF BRACKET.POS > 0 THEN
        LET OPTYP$ = "mem"
        REM
        REM     First strip off BYTE PTR, WORD PTR, and DWORD PTR markers.
        REM
        IF UCASE$(LEFT$(RAW$, 4)) = "BYTE" THEN
            LET CRAW$ = LTRIM$(MID$(RAW$, 5))
            IF UCASE$(LEFT$(CRAW$, 3)) = "PTR" THEN
                LET N.BYTES = 1
                LET RAW$ = LTRIM$(MID$(CRAW$, 4))
            END IF
        ELSEIF UCASE$(LEFT$(RAW$, 4)) = "WORD" THEN
            LET CRAW$ = LTRIM$(MID$(RAW$, 5))
            IF UCASE$(LEFT$(CRAW$, 3)) = "PTR" THEN
                LET N.BYTES = 2
                LET RAW$ = LTRIM$(MID$(CRAW$, 4))
            END IF
        ELSEIF UCASE$(LEFT$(RAW$, 5)) = "DWORD" THEN
            LET CRAW$ = LTRIM$(MID$(RAW$, 6))
            IF UCASE$(LEFT$(CRAW$, 3)) = "PTR" THEN
                LET N.BYTES = 4
                LET RAW$ = LTRIM$(MID$(CRAW$, 4))
            END IF
        END IF
        REM
        REM     Check for segment override prefix.
        REM
        
        IF LEN(RAW$) > 3 THEN
            LET SEGMENT.OVERRIDE$ = MID$(RAW$, 1, 2)
            LET SREG.NUM = -1
            FOR K = 0 TO 7  '     Get Segregister Index
                IF REGISTER.LIST$(24 + K) = UCASE$(SEGMENT.OVERRIDE$) THEN
                    LET SREG.NUM = K
                END IF
            NEXT
            IF SREG.NUM > -1 THEN    'If no colon then forget it.
                LET CRAW$ = LTRIM$(MID$(RAW$, 3))
                IF LEFT$(CRAW$, 1) = ":" THEN
                    LET RAW$ = LTRIM$(MID$(CRAW$, 2))
                    LET REG.NUM = SREG.NUM
                END IF
            END IF
        END IF
     ELSE
        LET OPTYP$ = "imm"
     END IF

END SUB

SUB ParseInit

LET PROCLEV = 0  '  8086 instructions must come first.

OPEN DAT.PATH$ + "X86.DAT" FOR INPUT AS #1
  
REM
REM   Setup Simulated Hardware
REM


    LET K = 0
    WHILE K <= ARCH.DIM
        INPUT #1, X$
        IF LEFT$(X$, 1) <> "#" THEN
            LET ARCHNAME$(K) = X$
            LET K = K + 1
        END IF
    WEND
    LET K = 0
    WHILE K < N.REGISTERS
        INPUT #1, X$
        IF LEFT$(X$, 1) <> "#" THEN
            LET REGISTER.LIST$(K) = X$
            LET K = K + 1
        END IF
    WEND
    LET K = 0
    WHILE K < N.DIRECTIVES
        INPUT #1, X$
        IF LEFT$(X$, 1) <> "#" THEN
            LET K = K + 1
            LET DIRECTIVE.LIST$(K) = X$
        END IF
    WEND
      
    LET MNEMONIC.LIST$(0) = ""
    LET K = 1
    WHILE NOT EOF(1)
        IF K > MNEM.DIM THEN
            PRINT "MNEM.DIM needs to be increased"
            SYSTEM
        END IF
        INPUT #1, X$
        IF LEFT$(X$, 1) <> "#" THEN
            LET MNEMONIC.LIST$(K) = X$
            LET ARCHLEV(K) = PROCLEV
            IF MNEMONIC.LIST$(K) = "ROL" THEN LET INDEX.ROL = K
            IF MNEMONIC.LIST$(K) = "JO" THEN LET INDEX.JO = K
            IF MNEMONIC.LIST$(K) = "BT" THEN LET INDEX.BT = K
            IF MNEMONIC.LIST$(K) = "SETO" THEN LET INDEX.SETO = K
            IF MNEMONIC.LIST$(K) = "INSB" THEN LET INDEX.INSB = K
            INPUT #1, ARITY(K)
            IF ARITY(K) > 50 THEN
                LET PROCLEV = (ARITY(K) \ 100)
            ELSE
                LET K = K + 1
            END IF
        END IF
    WEND
    LET N.MNEMONICS = K - 1
   
CLOSE #1
  

END SUB

FUNCTION REG.INDEX (X$)
LET REG.NUM = -1

FOR K = 0 TO N.REGISTERS - 1
    IF REGISTER.LIST$(K) = UCASE$(X$) THEN
        LET REG.NUM = K
    END IF
NEXT

LET REG.INDEX = REG.NUM

END FUNCTION

SUB RegMemEncode (REG.BITS, REG.SIZE, RM.TYPE$, RM$, PACK$, ERM$, LABEL$, DISPCODE, PF$, MX$, SGREG)
IF REG.BITS = -1 THEN
    LET MX$ = ""
    LET PF$ = ""
ELSEIF RM.TYPE$ = "reg" THEN
    LET MX$ = HEX$(&HC0 + REG.BITS * 8 + SGREG)
    LET PF$ = ""
    LET PACK$ = UCASE$(RM$)
ELSE
    LET SREG = SGREG
    CALL MemAssemble(REG.BITS, REG.SIZE, RM.TYPE$, RM$, PACK$, ERM$, LABEL$, DISPCODE, PF$, MX$, SREG)
    IF ERM$ <> "" THEN
        EXIT SUB
    END IF
    LET SGP$ = SegPref$(SGREG, SREG)
    LET PF$ = PF$ + SGP$
END IF
END SUB

FUNCTION SegPref$ (SPECIFIED, ENCODED)
    IF SPECIFIED = -1 OR SPECIFIED = ENCODED THEN
        LET SegPref$ = ""
    ELSEIF SPECIFIED = CS THEN
        LET SegPref$ = "2E"
    ELSEIF SPECIFIED = DS THEN
        LET SegPref$ = "1E"
    ELSEIF SPECIFIED = ES THEN
        LET SegPref$ = "26"
    ELSEIF SPECIFIED = FS THEN
        LET SegPref$ = "64"
    ELSEIF SPECIFIED = GS THEN
        LET SegPref$ = "65"
    END IF
END FUNCTION

