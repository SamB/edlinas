REM $INCLUDE: 'x86.bi'
DECLARE SUB Bswap (DEC.NUM!)
DECLARE SUB Cmpxchg8 (SREG!, ADS!)
DECLARE SUB Cmpxchg (OS!, SGRG!, ADS!, DEC.NUM!, LF)
DECLARE FUNCTION DIF# (A#, B#, C#, N.BYTES)
DECLARE SUB DoBits (OS!, segReg!, ADS!, DEC.NUM!, LF)
DECLARE SUB DoLoadSeg (OS!, segReg!, ADS!, DEC.NUM!)
DECLARE SUB DoPop (OS!, SREG!, ADS!, DEC.NUM!)
DECLARE SUB DoPush (OS!, DEC.NUM!)
DECLARE SUB DoShift (OS!, SREG!, ADS!, DEC.NUM!)
DECLARE SUB DoMovireg (OS!, SREG!, ADS!, DEC.NUM!)
DECLARE SUB signal.fault (xcptype%, error.code%)
DECLARE SUB read.descr (sel!, desc AS DESCR, FLAG%)
DECLARE SUB Imul (OS!, segReg!, ADS!, DEC.NUM!)
DECLARE SUB Jcond (OS!, JUMP.NUM!)
DECLARE SUB Lar (OS!, SREG!, ADS!)
DECLARE SUB Lsl (OS!, SREG!, ADS!)
DECLARE SUB Setcond (OS!, SREG!, ADS!, FLAG.NUM!)
DECLARE SUB Xadd (OS!, SREG!, ADS!, DEC.NUM!, LF)
DECLARE FUNCTION BitFlip# (X#, INDEX!, OPER.NUM!, N.BYTES)
DECLARE FUNCTION BitScan! (X#, N.BYTES!, FORWARD.FLAG!)
DECLARE SUB DoMovx (OS!, SREG!, ADS!, HB2!)
DECLARE SUB Do0F01 (OS!, SREG!, ADS!)
DECLARE SUB Do0F00 (OS!, SREG!, ADS!)
DECLARE SUB DTAB.write (err.code!, desc AS ANY, offt!, value#, N.BYTES!)
DECLARE FUNCTION DTAB.read# (err.code, desc AS DESCR, offt, FLAG%, N.BYTES)
DECLARE SUB lg.write (descind!, offset#, value#, N.BYTES!)
DECLARE FUNCTION inBounds (sel)
DECLARE FUNCTION fetch.code# (N.BYTES!)
DECLARE SUB REG.STORE (REG.NUM!, NUM#)
DECLARE FUNCTION REG.CODE! (REG.NUM!, Size!)
DECLARE FUNCTION REGZ# (REG.NUM!)
DECLARE FUNCTION WORD.SIZE! (OS!, WIDTH.BIT!)
DECLARE FUNCTION RM.BITS! (DEC.NUM!)
DECLARE FUNCTION REG.BITS! (N!)

FUNCTION BitFlip# (X#, INDEX, OPER.NUM, N.BYTES)

    LET N.BITS = N.BYTES * 8
  
    CF = Bits#(X#, INDEX, INDEX)
    IF OPER.NUM = 4 THEN       'Test
        LET NEW.BIT = CF
    ELSEIF OPER.NUM = 5 THEN   'Set
        LET NEW.BIT = 1
    ELSEIF OPER.NUM = 6 THEN  'Reset
        LET NEW.BIT = 0
    ELSEIF OPER.NUM = 7 THEN    'Complement
        LET NEW.BIT = 1 - CF
    END IF
    LET BitFlip# = Bits#(X#, N.BITS - 1, INDEX + 1) * 2# ^ (INDEX + 1) + NEW.BIT * 2# ^ INDEX + Bits#(X#, INDEX - 1, 0)


END FUNCTION

FUNCTION BitScan (X#, N.BYTES, FORWARD.FLAG)
    LET N.BITS = 8 * N.BYTES
  
    IF FORWARD.FLAG THEN
        LET i = 0
        WHILE (Bits#(X#, i, i) = 0 AND i < N.BITS)
            LET i = i + 1
        WEND
        IF i = N.BITS THEN
            LET ZF = 1
        ELSE
            LET ZF = 0
            LET R = i
        END IF
    ELSE
        LET i = N.BITS - 1
        WHILE (Bits#(X#, i, i) = 0 AND i >= 0)
            LET i = i - 1
        WEND
        IF i = -1 THEN
            LET ZF = 1
        ELSE
            LET ZF = 0
            LET R = i
        END IF
    END IF
    LET BitScan = R
END FUNCTION

SUB Bswap (DEC.NUM)
REM
REM Byte swap   (opcodes 0F Cx, x > 7)

LET REG.NUM = DEC.NUM - &HC8    'First Byte 0F already discarded
LET REG.VAL$ = HexStr$(REG#(REG.NUM), 8) '8  digits
LET NEW.VAL$ = BYTE.SWAP$(REG.VAL$)
CALL REG.STORE(REG.NUM, HexVal#(NEW.VAL$))

END SUB

SUB Cmpxchg (OS, SGRG, ADS, DEC.NUM, LF)

LET N.BYTES = WORD.SIZE(OS, DEC.NUM MOD 2)
LET ACC.REG = REG.CODE(EAX, N.BYTES)
LET ACC# = REG#(ACC.REG)

LET DEC.NUM = fetch.code#(1)
LET REG.NUM = REG.CODE(REG.BITS(DEC.NUM), N.BYTES)
LET OP2# = REG#(REG.NUM)

IF DEC.NUM >= &HC0 AND LF THEN
    LET INSTRUCTION.RECOGNIZED = FALSE
    EXIT SUB
ELSEIF DEC.NUM >= &HC0 THEN
    LET OP1.NUM = REG.CODE(RM.BITS(DEC.NUM), N.BYTES)
    LET OP1# = REG#(OP1.NUM)
ELSE
    LET offset# = GetOffset#(SGRG, ADS, DEC.NUM)
    LET OP1# = lg.read#(SGRG, offset#, N.BYTES)
END IF

LET result# = DIF#(OP1#, ACC#, 0#, N.BYTES)

IF ZF = 1 THEN
    IF DEC.NUM >= &HC0 THEN
        CALL REG.STORE(OP1.NUM, OP2#)
    ELSE
        CALL lg.write(SGRG, offset#, OP2#, N.BYTES)
    END IF
ELSE
    CALL REG.STORE(ACC.REG, OP1#)
END IF


END SUB

SUB Cmpxchg8 (SREG, ADS)

LET DEC.NUM = fetch.code#(1)
IF DEC.NUM >= &HC0 THEN
    LET INSTRUCTION.RECOGNIZED = FALSE
ELSE
    LET offset# = GetOffset#(SREG, ADS, DEC.NUM)
    LET offset2# = offset# + 4#
    LET m1# = lg.read#(SREG, offset#, 4)
    LET m2# = lg.read#(SREG, offset2#, 4)
    IF REG#(EAX) = m1# AND REG#(EDX) = m2# THEN
        CALL lg.write(SREG, offset#, REG#(EBX), 4)
        CALL lg.write(SREG, offset2#, REG#(ECX), 4)
    ELSE
        CALL REG.STORE(EAX, m1#)
        CALL REG.STORE(EDX, m2#)
    END IF
END IF
END SUB

SUB Do0F (OS, SREG, ADS, LF)
REM Second Hop   (opcodes 0F)
 
    LET BYTE2 = fetch.code#(1)       'Get second byte
    LET HB1 = BYTE2 \ 16
    LET HB2 = BYTE2 MOD 16
    IF HB1 = &HB AND ARCHNUM > 2 THEN
        IF HB2 < 2 AND ARCHNUM > 3 THEN
            CALL Cmpxchg(OS, SGRG, ADS, BYTE2, LF)
        ELSEIF HB2 = 3 OR (HB2 > 9 AND HB2 < &HE) THEN
            CALL DoBits(OS, SGRG, ADS, BYTE2, LF)
        ELSEIF LF THEN
            LET INSTRUCTION.RECOGNIZED = FALSE
        ELSEIF HB2 = 2 OR HB2 = 4 OR HB2 = 5 THEN
            CALL DoLoadSeg(OS, SGRG, ADS, BYTE2)
        ELSEIF HB2 MOD 8 > 5 THEN
            CALL DoMovx(OS, SREG, ADS, HB2)
        ELSE
            LET INSTRUCTION.RECOGNIZED = FALSE
        END IF
    ELSEIF HB1 = &HC AND ARCHNUM > 3 THEN
        IF HB2 < 2 THEN
            CALL Xadd(OS, SREG, ADS, BYTE2, LF)
        ELSEIF LF THEN
            LET INSTRUCTION.RECOGNIZED = FALSE
        ELSEIF HB2 = 7 AND ARCHNUM > 5 THEN
            CALL Cmpxchg8(SREG, ADS)   'Pentium 8 byte mem
        ELSEIF HB2 > 7 THEN
            CALL Bswap(BYTE2)
        ELSE
            LET INSTRUCTION.RECOGNIZED = FALSE
        END IF
    ELSEIF LF THEN
        LET INSTRUCTION.RECOGNIZED = FALSE
    ELSEIF HB1 = 0 AND ARCHNUM > 1 THEN
        IF HB2 = 0 THEN
            CALL Do0F00(OS, SREG, ADS)
        ELSEIF HB2 = 1 THEN
            CALL Do0F01(OS, SREG, ADS)
        ELSEIF HB2 = 2 THEN
            CALL Lar(OS, SREG, ADS)
        ELSEIF HB2 = 3 THEN
            CALL Lsl(OS, SREG, ADS)
        ELSEIF HB2 = 6 THEN   ' CLTS
            IF Bits#(ctrlReg#(0), 3, 3) = 1# THEN        ' LET TS = 0
                LET ctrlReg#(0) = ctrlReg#(0) - 8#
            END IF
REM     ELSEIF HB2 = 5 THEN
            REM CALL Loadall(OS, SREG, ADS, BYTE2)     '286 only
        ELSEIF HB2 = 8 AND ARCHNUM > 3 THEN
            REM Invd   ' Cache. No action needed
        ELSEIF HB2 = 9 AND ARCHNUM > 3 THEN
            REM  Wbinvd ' Cache. No action needed.
        ELSE
            LET INSTRUCTION.RECOGNIZED = FALSE
            EXIT SUB
        END IF
    ELSEIF HB1 = 2 AND ARCHNUM > 2 THEN
        IF HB2 < 7 AND HB2 <> 5 THEN
            CALL DoMovireg(OS, SREG, ADS, BYTE2)
        ELSE
            LET INSTRUCTION.RECOGNIZED = FALSE
            EXIT SUB
        END IF
    ELSEIF HB1 = 3 AND ARCHNUM > 5 THEN
        IF HB2 = 0 THEN
            REM CALL Wrmsr            'Pentium
        ELSEIF HB2 = 1 THEN
            REM CALL Rdtsc            'Pentium
            CALL REG.STORE(EDX, CDBL(TIMER))   ' No cycle counting implemented.
            CALL REG.STORE(EAX, RandomBytes#(4))
        ELSEIF HB2 = 2 THEN
            REM CALL Rdmsr            'Pentium
        ELSE
            LET INSTRUCTION.RECOGNIZED = FALSE
        END IF
    ELSEIF HB1 = 8 AND ARCHNUM > 2 THEN
        CALL Jcond(OS, BYTE2)
    ELSEIF HB1 = 9 AND ARCHNUM > 2 THEN
        CALL Setcond(OS, SREG, ADS, HB2)
    ELSEIF HB1 = &HA AND ARCHNUM > 2 THEN
        IF HB2 = 2 AND ARCHNUM > 5 THEN
            REM Cpuid            'Pentium
            IF REG#(EAX) = 0# THEN
                LET X$ = BYTE.SWAP$("Wide")
                CALL REG.STORE(EBX, HexVal#(X$))
                LET X$ = BYTE.SWAP$("ner ")
                CALL REG.STORE(EDX, HexVal#(X$))
                LET X$ = BYTE.SWAP$("CS  ")
                CALL REG.STORE(ECX, HexVal#(X$))
                CALL REG.STORE(EAX, 1#)
            ELSEIF REG#(EAX) = 1# THEN
                CALL REG.STORE(EAX, CDBL(HexVal#("51B")))
            END IF
        ELSEIF HB2 = 10 AND ARCHNUM > 5 THEN
            REM Rsm              'Pentium
        ELSEIF HB2 MOD 8 = 0 THEN
            CALL DoPush(OS, BYTE2)
        ELSEIF HB2 MOD 8 = 1 THEN
            CALL DoPop(OS, SGRG, ADS, BYTE2)
        ELSEIF HB2 MOD 8 = 3 THEN
            CALL DoBits(OS, SGRG, ADS, BYTE2, LF)
        ELSEIF HB2 MOD 8 < 6 THEN
            CALL DoShift(OS, SGRG, ADS, BYTE2)
        ELSEIF HB2 < 8 THEN
            CALL Cmpxchg(OS, SGRG, ADS, BYTE2, LF)    '486  on the A step ??
        ELSEIF HB2 = 15 THEN
            CALL Imul(OS, SGRG, ADS, BYTE2)
        ELSE
            LET INSTRUCTION.RECOGNIZED = FALSE
        END IF
    ELSE
        LET INSTRUCTION.RECOGNIZED = FALSE
    END IF

END SUB

SUB Do0F00 (OS, SREG, ADS)

DIM dsc AS DESCR
DIM csel AS INTEGER

IF mode = REAL OR mode = VM86 THEN
    CALL signal.fault(INVALID.INSTR%, -1)
    EXIT SUB
END IF

    LET DEC.NUM = fetch.code#(1)
    LET OP.NUM = REG.BITS(DEC.NUM)
IF DEC.NUM >= &HC0 THEN
    LET NMEM = 1
    LET REG.NUM = REG.CODE(RM.BITS(DEC.NUM), 2)
ELSE
    LET NMEM = 0
    LET offset# = GetOffset#(SREG, ADS, DEC.NUM)
END IF

    IF OP.NUM = 0 THEN             'SLDT
        IF NMEM THEN
            CALL REG.STORE(REG.NUM, CDBL(segReg(LDT)))
        ELSE
            CALL lg.write(SREG, offset#, CDBL(segReg(LDT)), 2)
        END IF
    ELSEIF OP.NUM = 1 THEN             'STR
        IF NMEM THEN
            CALL REG.STORE(REG.NUM, CDBL(segReg(TSS)))
        ELSE
            CALL lg.write(SREG, offset#, CDBL(segReg(TSS)), 2)
        END IF
    ELSEIF OP.NUM = 2 THEN             'LLDT
        IF CPL > 0 THEN
            CALL signal.fault(GP, 0)
            EXIT SUB
        END IF
        IF NMEM THEN
            LET sel = REG#(REG.NUM)
        ELSE
            LET sel = lg.read#(SREG, offset#, 2)
        END IF
        LET csel = sel AND -4
        IF (sel AND 4) > 0 THEN
            CALL signal.fault(GP, csel)
            EXIT SUB
        END IF
        IF (sel AND (NOT 3)) = 0 THEN
            LET dsc.Valid = FALSE
        ELSE
            IF LOCKS.OK THEN LOCK #2
            CALL read.descr(sel, dsc, FALSE)
            LET csel = sel AND -4
            IF dsc.Typ <> LDT.SEG THEN
                CALL signal.fault(GP, csel)
                EXIT SUB
            ELSEIF NOT dsc.Valid THEN
                CALL signal.fault(NP, csel)
                EXIT SUB
            END IF
            IF LOCKS.OK THEN UNLOCK #2
        END IF
        LET segReg(LDT) = sel
        LET descrVec(LDT) = dsc
    ELSEIF OP.NUM = 3 THEN             'LTR
        IF CPL > 0 THEN
            CALL signal.fault(GP, 0)
            EXIT SUB
        END IF
        IF NMEM THEN
            LET sel = REG#(REG.NUM)
        ELSE
            LET sel = lg.read#(SREG, offset#, 2)
        END IF
        LET csel = (sel AND (-4))
        IF (sel AND -4) = 0 OR (sel AND 4) > 0 THEN
            CALL signal.fault(GP, csel)
            EXIT SUB
        END IF
        IF LOCKS.OK THEN LOCK #2
        CALL read.descr(sel, dsc, FALSE)
        IF dsc.Typ = NOTBUSY.TSS16 OR dsc.Typ = NOTBUSY.TSS32 THEN
            IF NOT dsc.Valid THEN
                CALL signal.fault(NP, csel)
                EXIT SUB
            END IF
            LET sel.Index = (sel AND -8) + 5
            LET temp# = DTAB.read#(sel, descrVec(GDT), sel.Index, FALSE, 1)
            LET temp2# = CDBL(CINT(temp#) OR 2)
            CALL DTAB.write(sel, dsc, sel.Index, temp2#, 1)
            IF LOCKS.OK THEN UNLOCK #2
            LET segReg(TSS) = sel
            LET descrVec(TSS) = dsc
        END IF
    ELSEIF OP.NUM = 4 THEN             'VERR
        IF NMEM THEN
            LET sel = REG#(REG.NUM)
        ELSE
            LET sel = lg.read#(SREG, offset#, 2)
        END IF
  
        IF NOT inBounds(sel) THEN
            LET ZF = 0
        ELSE
            IF (sel AND 4) = 0 THEN
                LET dsc = descrVec(GDT)
            ELSE
                LET dsc = descrVec(LDT)
            END IF
            
            CALL read.descr(sel, dsc, FALSE)
            IF ((dsc.Readable OR dsc.Writable) AND (CPL <= dsc.DPL AND sel MOD 4 <= dsc.DPL) OR dsc.Executable AND dsc.Readable AND CPL <= dsc.DPL AND sel MOD 4 <= dsc.DPL) OR (dsc.Conforming AND dsc.Readable) THEN
                LET ZF = 1
            ELSE
                LET ZF = 0
            END IF
        END IF
    ELSEIF OP.NUM = 5 THEN             'VERW
        IF NMEM THEN
            LET sel = REG#(REG.NUM)
        ELSE
            LET sel = lg.read#(SREG, offset#, 2)
        END IF
  
        IF NOT inBounds(sel) THEN
            LET ZF = 0
        ELSE
            IF (sel AND 4) = 0 THEN
                LET dsc = descrVec(GDT)
            ELSE
                LET dsc = descrVec(LDT)
            END IF
            
            CALL read.descr(sel, dsc, FALSE)
            IF dsc.Writable AND (CPL <= dsc.DPL) AND ((sel AND (NOT 3)) <= dsc.DPL) THEN
                LET ZF = 1
            ELSE
                LET ZF = 0
            END IF
        END IF
    ELSE
        LET INSTRUCTION.RECOGNIZED = FALSE
    END IF
END SUB

SUB Do0F01 (OS, SREG, ADS)

    LET DEC.NUM = fetch.code#(1)
    LET OP.NUM = REG.BITS(DEC.NUM)
    IF DEC.NUM >= &HC0 THEN
        LET NMEM = 1
        LET REG.NUM = REG.CODE(RM.BITS(DEC.NUM), 2)
    ELSE
        LET NMEM = 0
        LET offset# = GetOffset#(SREG, ADS, DEC.NUM)
    END IF

    IF OP.NUM = 0 THEN    'SGDT
        CALL lg.write(SREG, offset#, descrVec(GDT).Limit, 2)
        IF OS = 32 THEN
            CALL lg.write(SREG, offset# + 2#, descrVec(GDT).BaseAdd, 4)
        ELSE
            CALL lg.write(SREG, offset# + 2#, descrVec(GDT).BaseAdd AND &HFFFFFF, 4)
        END IF
    ELSEIF OP.NUM = 1 THEN    'SIDT
        CALL lg.write(SREG, offset#, descrVec(IDT).Limit, 2)
        IF OS = 32 THEN
            CALL lg.write(SREG, offset# + 2#, descrVec(IDT).BaseAdd, 4)
        ELSE
            CALL lg.write(SREG, offset# + 2#, descrVec(IDT).BaseAdd AND &HFFFFFF, 4)
        END IF
    ELSEIF OP.NUM = 2 THEN    'LGDT
        IF CPL > 0 THEN
            CALL signal.fault(GP, 0)
            EXIT SUB
        END IF
        LET descrVec(GDT).Limit = lg.read#(SREG, offset#, 2)
        IF OS = 32 THEN
            LET descrVec(GDT).BaseAdd = lg.read#(SREG, offset# + 2#, 4)
        ELSE
            LET descrVec(GDT).BaseAdd = lg.read#(SREG, offset# + 2#, 4) AND &HFFFFFF
        END IF
        LET descrVec(GDT).Valid = TRUE
    ELSEIF OP.NUM = 3 THEN    'LIDT
        IF CPL > 0 THEN
            CALL signal.fault(GP, 0)
            EXIT SUB
        END IF
        LET descrVec(IDT).Limit = lg.read#(SREG, offset#, 2)
        IF OS = 32 THEN
            LET descrVec(IDT).BaseAdd = lg.read#(SREG, offset# + 2#, 4)
        ELSE
            LET descrVec(IDT).BaseAdd = lg.read#(SREG, offset# + 2#, 4) AND &HFFFFFF
        END IF
        LET descrVec(IDT).Valid = TRUE
    ELSEIF OP.NUM = 4 THEN    'SMSW
        LET Y# = ctrlReg#(0)
        LET Z = Bits#(Y#, 4, 0)
        LET Y# = Bits#(Y#, 31, 16) * 2# ^ 16
        LET X# = Y# + FIX(RandomBytes#(2) / 32#) * 32# + CDBL(Z)
        IF NMEM THEN
            CALL REG.STORE(REG.NUM, X#)
        ELSE
            CALL lg.write(SREG, offset#, X#, 2)
        END IF
    ELSEIF OP.NUM = 5 THEN
        LET INSTRUCTION.RECOGNIZED = FALSE
    ELSEIF OP.NUM = 6 THEN    'LMSW
        IF CPL > 0 THEN
            CALL signal.fault(GP, 0)
            EXIT SUB
        ELSEIF NMEM THEN
            LET X# = REG#(REG.NUM)
        ELSE
            LET X# = lg.read#(SREG, offset#, 2)
        END IF
        LET Z = Bits#(X#, 3, 0)  ' Only relevant bits from source
        LET Y# = ctrlReg#(0)  'CR0
        LET PE = Bits#(Y#, 0, 0)
        IF PE = 0 THEN
            LET PE = Z MOD 2
        END IF
        LET Z = (Z AND 6) OR PE
        LET Y# = RandomBytes#(2)
        LET Y# = FIX(Y# / 16#) * 16# + CDBL(Z)
        LET ctrlReg#(0) = Y#
    ELSEIF OP.NUM = 7 AND ARCHNUM > 3 THEN
        REM Invlpg  'Cache not Simulated. No action needed.
    ELSE
        LET INSTRUCTION.RECOGNIZED = FALSE
    END IF

END SUB

SUB DoBits (OS, SREG, ADS, DEC.NUM, LF)

REM
REM Bitstrings (Opcodes 0F Ay, y = 3,8 and 0F Bx, where x=3,A,B,C, or D)
REM
    LET N.BYTES = WORD.SIZE(OS, 1)
    LET SCAN.FLAG = (DEC.NUM = &HBC OR DEC.NUM = &HBD)

    IF SCAN.FLAG THEN
        LET FORWARD.FLAG = (DEC.NUM = &HBC)
    ELSE
        LET OPER.NUM = (DEC.NUM - &HA3) \ 8 + 4
    END IF

    LET IMM.FLAG = (DEC.NUM = &HBA)
   

    LET DEC.NUM = fetch.code#(1)
    LET REG.NUM = REG.BITS(DEC.NUM)
   
   
    IF DEC.NUM >= &HC0 AND LF THEN
        LET INSTRUCTION.RECOGNIZED = FALSE
        EXIT SUB
    ELSEIF DEC.NUM >= &HC0 THEN
        LET DEST = RM.BITS(DEC.NUM)
        LET DEST = REG.CODE(DEST, N.BYTES)
        LET X# = REG#(DEST)
    ELSE
        LET offset# = GetOffset#(SREG, ADS, DEC.NUM)
        LET POINTER# = offset# + INDEX \ OS
        LET X# = lg.read#(SREG, POINTER#, N.BYTES)
    END IF

   
   
    IF IMM.FLAG THEN
        OPER.NUM = REG.NUM
        LET INDEX = fetch.code#(1)
    ELSE
        LET SOURCE = REG.CODE(REG.NUM, N.BYTES)
        LET INDEX = REG#(SOURCE)
    END IF
   
   
    IF SCAN.FLAG THEN
        LET Y# = BitScan(X#, N.BYTES, FORWARD.FLAG)
        IF ZF = 0 THEN
            CALL REG.STORE(SOURCE, Y#)
        END IF
    ELSE
        LET INDEX = INDEX MOD OS
        LET Y# = BitFlip#(X#, INDEX, OPER.NUM, N.BYTES)
        IF DEC.NUM >= &HC0 THEN
            CALL REG.STORE(DEST, Y#)
        ELSE
            CALL lg.write(SREG, POINTER#, Y#, N.BYTES)
        END IF
    END IF


END SUB

SUB DoMovireg (OS, SREG, ADS, DEC.NUM)
    LET OP.NUM = DEC.NUM - &H20
    LET DEC.NUM = fetch.code#(1)
    LET IREG.NUM = REG.BITS(DEC.NUM)
    LET REG.NUM = RM.BITS(DEC.NUM)
    IF OP.NUM = 0 OR OP.NUM = 2 THEN
        IF IREG.NUM > 4 THEN
            LET INSTRUCTION.RECOGNIZED = FALSE
        ELSEIF IREG.NUM = 4 AND ARCHNUM < 6 THEN
            LET INSTRUCTION.RECOGNIZED = FALSE
        END IF
    ELSEIF OP.NUM = 1 OR OP.NUM = 3 THEN
        IF IREG.NUM = 4 OR IREG.NUM = 5 THEN
            LET INSTRUCTION.RECOGNIZED = FALSE
        END IF
    ELSEIF OP.NUM = 4 OR OP.NUM = 6 THEN
        IF IREG.NUM < 3 THEN
            LET INSTRUCTION.RECOGNIZED = FALSE
        END IF
    END IF
    IF NOT INSTRUCTION.RECOGNIZED THEN
        EXIT SUB
    END IF
    IF OP.NUM = 0 THEN
        CALL REG.STORE(REG.NUM, ctrlReg#(IREG.NUM))
    ELSEIF OP.NUM = 1 THEN
        CALL REG.STORE(REG.NUM, debReg#(IREG.NUM))
    ELSEIF OP.NUM = 2 THEN
        LET ctrlReg#(IREG.NUM) = REG#(REG.NUM)
    ELSEIF OP.NUM = 3 THEN
        LET debReg#(IREG.NUM) = REG#(REG.NUM)
    ELSEIF OP.NUM = 4 THEN
        CALL REG.STORE(REG.NUM, testReg#(IREG.NUM))
    ELSEIF OP.NUM = 6 THEN
        LET testReg#(IREG.NUM) = REG#(REG.NUM)
    END IF
END SUB

SUB DoMovx (OS, SREG, ADS, HB2)


        LET SOURCE.WIDTH.BIT = HB2 MOD 2
        LET DEST.SIGN.BIT = HB2 \ 8

        LET N.DEST.BYTES = WORD.SIZE(OS, 1)
        IF SOURCE.WIDTH.BIT = 0 THEN
            LET N.SOURCE.BYTES = 1
        ELSE
            LET N.SOURCE.BYTES = N.DEST.BYTES \ 2
        END IF
     
        LET DEC.NUM = fetch.code#(1)
        LET DEST = REG.BITS(DEC.NUM)
        LET DEST = REG.CODE(DEST, N.DEST.BYTES)
     
    IF DEC.NUM >= &HC0 THEN
        LET RM.NUM = RM.BITS(DEC.NUM)
        LET Mem.REG = REG.CODE(RM.NUM, N.SOURCE.BYTES)
        IF DEST.SIGN.BIT THEN
            LET X# = REGZ#(Mem.REG)
        ELSE
            LET X# = REG#(Mem.REG)
        END IF
    ELSE
        LET offset# = GetOffset#(SREG, ADS, DEC.NUM)
        IF DEST.SIGN.BIT THEN
                LET N.DEST.BYTES = -N.DEST.BYTES
        END IF
        LET X# = lg.read#(SREG, offset#, N.DEST.BYTES)
     
    END IF
 
    CALL REG.STORE(DEST, X#)
      


END SUB

SUB Jcond (OS, DEC.NUM)
REM
REM  Conditional jumps 7x, short jumps, 0F 8x, long jumps
REM
    LET N.BYTES = WORD.SIZE(OS, DEC.NUM \ 16 - 7)
    LET JUMP.NUM = DEC.NUM MOD 16
    LET DELTA.EIP# = fetch.code#(-N.BYTES)
    LET BRANCH.EIP# = EIP# + DELTA.EIP#
    LET SAVE.EIP# = EIP#
    LET EIP# = -1#
    SELECT CASE JUMP.NUM
        CASE 0 ' O
            IF OF = 1 THEN EIP# = BRANCH.EIP#
        CASE 1
            IF OF = 0 THEN EIP# = BRANCH.EIP#
        CASE 2 ' B, C, NAE
            IF CF = 1 THEN EIP# = BRANCH.EIP#
        CASE 3
            IF OF = 0 THEN EIP# = BRANCH.EIP#
        CASE 4 ' E, Z
            IF ZF = 1 THEN EIP# = BRANCH.EIP#
        CASE 5 
            IF ZF = 0 THEN EIP# = BRANCH.EIP#
        CASE 6 ' BE, NA
            IF (CF OR ZF) = 1 THEN EIP# = BRANCH.EIP#
        CASE 7
            IF (CF OR ZF) = 0 THEN EIP# = BRANCH.EIP#
        CASE 8 ' S
            IF SF = 1 THEN EIP# = BRANCH.EIP#
        CASE 9
            IF SF = 0 THEN EIP# = BRANCH.EIP#
        CASE 10 ' P, PE
            IF PF = 1 THEN EIP# = BRANCH.EIP#
        CASE 11
            IF PF = 0 THEN EIP# = BRANCH.EIP#
        CASE 12 ' L, NGE
            IF (SF XOR OF) = 1 THEN EIP# = BRANCH.EIP#
        CASE 13
            IF (SF XOR OF) = 0 THEN EIP# = BRANCH.EIP#
        CASE 14 ' LE, NG
            IF ((SF XOR OF) OR ZF) = 1 THEN EIP# = BRANCH.EIP#
        CASE 15
            IF ((SF XOR OF) OR ZF) = 0 THEN EIP# = BRANCH.EIP#
    END SELECT
    IF EIP# = -1# THEN     'If no jump to be taken restore EIP#
        LET EIP# = SAVE.EIP#
    ELSE                 ' otherwise make sure to leave command mode.
        LET COMMAND.MODE = FALSE
    END IF


END SUB

SUB Lar (OS, SREG, ADS)

IF mode = REAL OR mode = VM86 THEN
    CALL signal.fault(INVALID.INSTR, 0)
    EXIT SUB
END IF

LET N.BYTES = WORD.SIZE(OS, 1)
LET DEC.NUM = fetch.code#(1)
IF DEC.NUM >= &HC0 THEN
    LET RM.NUM = RM.BITS(DEC.NUM)
    LET SOURCE = REG.CODE(RM.NUM, 2)
    LET sel = REG#(SOURCE)
ELSE
    LET offset# = GetOffset#(SREG, ADS, DEC.NUM)
    LET sel = lg.read#(SREG, offset#, 2)
END IF
LET REG.NUM = REG.BITS(RM.NUM)


DIM desc AS DESCR
IF (sel AND (NOT 3)) = 0 OR NOT inBounds(sel) THEN
    LET ZF = 0
ELSE
    CALL read.descr(sel, desc, FALSE)
    IF (desc.Conforming OR ((CPL <= desc.DPL AND (sel AND (NOT 3)) <= desc.DPL) AND (desc.Readable OR (NOT desc.Conforming AND desc.Executable) OR desc.Typ = BUSY.TSS16 OR desc.Typ = BUSY.TSS32 OR desc.Typ = NOTBUSY.TSS16 OR desc.Typ = NOTBUSY.TSS32 _
 OR desc.Typ = CALLGATE16 OR desc.Typ = CALLGATE32 OR desc.Typ = TASKGATE OR desc.Typ = LDT.SEG))) THEN
        LET dst24$ = BitStr$(0#, 8)
        LET dst20$ = BitStr$(CDBL(desc.GDField), 4)
        LET dst16$ = BitStr$(RandomBytes#(1), 4)
        LET dst15$ = BitStr$(CDBL(desc.Valid), 1)
        LET dst13$ = BitStr$(CDBL(desc.DPL), 2)
        LET dst12$ = BitStr$(CDBL(desc.CDSeg), 1)
        IF desc.CDSeg THEN
            LET dst11$ = BitStr$(CDBL(desc.Executable), 1)
            IF desc.Executable THEN
                LET dst10$ = BitStr$(CDBL(desc.Conforming), 1)
                LET dst09$ = BitStr$(CDBL(desc.Readable), 1)
            ELSE
                LET dst10$ = BitStr$(CDBL(desc.ExpandDown), 1)
                LET dst09$ = BitStr$(CDBL(desc.Writable), 1)
            END IF
            LET dst08$ = BitStr$(CDBL(desc.Accessed), 1)
            LET dst08$ = dst11$ + dst10$ + dst09$ + dst08$
            LET dst00$ = BitStr$(0#, 8)
        ELSE
            LET dst08$ = BitStr$(CDBL(desc.Typ), 4)
        END IF
        LET ZF = 1
        LET dst$ = dst24$ + dst20$ + dst16$ + dst15$ + dst13$ + dst12$ + dst08$ + dst00$
        CALL REG.STORE(REG.NUM, BinVal#(dst$))
    ELSE
        LET ZF = 0
    END IF

END IF


END SUB

SUB Lsl (OS, SREG, ADS)

IF mode = REAL OR mode = VM86 THEN
    CALL signal.fault(INVALID.INSTR, 0)
    EXIT SUB
END IF

LET N.BYTES = WORD.SIZE(OS, 1)
LET DEC.NUM = fetch.code#(1)
IF DEC.NUM >= &HC0 THEN
    LET RM.NUM = RM.BITS(DEC.NUM)
    LET SOURCE = REG.CODE(RM.NUM, 2)
    LET sel = REG#(SOURCE)
ELSE
    LET offset# = GetOffset#(SREG, ADS, DEC.NUM)
    LET sel = lg.read#(SREG, offset#, 2)
END IF
LET REG.NUM = REG.BITS(RM.NUM)


DIM desc AS DESCR
IF (sel AND (NOT 3)) = 0 OR NOT inBounds(sel) THEN
    LET ZF = 0
ELSE
    CALL read.descr(sel, desc, FALSE)
    IF (desc.Conforming OR ((CPL <= desc.DPL AND (sel AND (NOT 3)) <= desc.DPL) AND (desc.Readable OR (NOT desc.Conforming AND desc.Executable) OR desc.Typ = BUSY.TSS16 OR desc.Typ = BUSY.TSS32 OR desc.Typ = NOTBUSY.TSS16 OR desc.Typ = NOTBUSY.TSS32 _
 OR desc.Typ = LDT.SEG))) THEN
        CALL REG.STORE(REG.NUM, desc.Limit)
        LET ZF = 1
    ELSE
        LET ZF = 0
    END IF

END IF

END SUB

SUB Setcond (OS, SREG, ADS, FLAG.NUM)

     
    LET SET.VAL = 0
    IF FLAG.NUM = 0 THEN
        IF OF = 1 THEN LET SET.VAL = 1
    ELSEIF FLAG.NUM = 1 THEN
        IF OF = 0 THEN LET SET.VAL = 1
    ELSEIF FLAG.NUM = 2 THEN
        IF CF = 1 THEN LET SET.VAL = 1
    ELSEIF FLAG.NUM = 3 THEN
        IF OF = 0 THEN LET SET.VAL = 1
    ELSEIF FLAG.NUM = 4 THEN
        IF ZF = 1 THEN LET SET.VAL = 1
    ELSEIF FLAG.NUM = 5 THEN
        IF ZF = 0 THEN LET SET.VAL = 1
    ELSEIF FLAG.NUM = 6 THEN
        IF (CF OR ZF) = 1 THEN LET SET.VAL = 1
    ELSEIF FLAG.NUM = 7 THEN
        IF (CF OR ZF) = 0 THEN LET SET.VAL = 1
    ELSEIF FLAG.NUM = 8 THEN
        IF SF = 1 THEN LET SET.VAL = 1
    ELSEIF FLAG.NUM = 9 THEN
        IF SF = 0 THEN LET SET.VAL = 1
    ELSEIF FLAG.NUM = 10 THEN
        IF PF = 1 THEN LET SET.VAL = 1
    ELSEIF FLAG.NUM = 11 THEN
        IF PF = 0 THEN LET SET.VAL = 1
    ELSEIF FLAG.NUM = 12 THEN
        IF (SF XOR OF) = 1 THEN LET SET.VAL = 1
    ELSEIF FLAG.NUM = 13 THEN
        IF (SF XOR OF) = 1 THEN LET SET.VAL = 1
    ELSEIF FLAG.NUM = 14 THEN
        IF ((SF XOR OF) OR ZF) = 1 THEN LET SET.VAL = 1
    ELSEIF FLAG.NUM = 15 THEN
        IF ((SF XOR OF) OR ZF) = 0 THEN LET SET.VAL = 1
    END IF

    LET DEC.NUM = fetch.code#(1)
    IF DEC.NUM >= &HC0 THEN
        LET REG.NUM = REG.CODE(RM.BITS(DEC.NUM), 1)
        CALL REG.STORE(REG.NUM, CDBL(SET.VAL))
    ELSE
        LET offset# = GetOffset#(SREG, ADS, DEC.NUM)
        CALL lg.write(SREG, offset#, CDBL(SET.VAL), 1)
    END IF




END SUB

