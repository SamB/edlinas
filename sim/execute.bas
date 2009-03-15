REM $INCLUDE: 'x86.bi'
DECLARE SUB check.brkpt (btype%, X#)
DECLARE SUB report.brkpts ()
DECLARE SUB handle.intr.xcp (vecnum AS INTEGER, err.flag%, err.code AS INTEGER)
REM
REM Execution Functions
REM
DECLARE FUNCTION fetch.code# (N.BYTES!)
DECLARE FUNCTION GetOffset# (SEG.REGISTER!, ADDRESS.SIZE!, MODRM.NUM!)
DECLARE FUNCTION GetSibAdd# (SREG!, ROW.NUM!)
DECLARE SUB Do0F (OS, SREG, ADS, LF)
DECLARE SUB DoAaCrap (DEC.NUM!)
DECLARE SUB DoAlu (OS, segReg!, ADS, OPER!, OP.NUM!, LF)
DECLARE SUB DoBound (OS!, segReg!, ADS)
DECLARE SUB DoCall (OS, DEC.NUM)
DECLARE SUB DoConv (OS!, SREG!, ADS, DEC.NUM!)
DECLARE SUB DoInOut (OS!, DEC.NUM!)
DECLARE SUB DoIncDec (OS, DEC.NUM)
DECLARE SUB DoInterrupt (DEC.NUM)
DECLARE SUB DoString (OS!, ADS!, DEC.NUM!)
DECLARE SUB Iret (OS!)
DECLARE SUB Jcond (OS!, DEC.NUM!)
DECLARE SUB Jmp (OS, DEC.NUM)
DECLARE SUB JumpShort (SREG!)
DECLARE SUB DoLoadSeg (OS!, segReg!, ADS, DEC.NUM!)
DECLARE SUB DoLoop (OS!, ADS, DEC.NUM!)
DECLARE SUB DoMVI (OS, SREG, ADS, DEC.NUM)
DECLARE SUB DoMovrI (OS, DEC.NUM)
DECLARE SUB DoMovs (OS!, segReg!, ADS, DEC.NUM!)
DECLARE SUB DoMova (OS!, SREG!, ADS, DEC.NUM!)
DECLARE SUB DoMovx (OS!, SREG!, ADS, DEC.NUM!)
DECLARE SUB DoPop (SREG!, OS!, ADS, DEC.NUM!)
DECLARE SUB DoPush (OS!, DEC.NUM!)
DECLARE SUB DoRep (OS!, ADS, DEC.NUM!, REPEAT!)
DECLARE SUB DoShiftd (OS!, SREG!, ADS, DEC.NUM!)
DECLARE SUB DoShift (OS, SREG!, ADS, DEC.NUM)
DECLARE SUB DoUnM (OS, SREG, ADS, DEC.NUM, LF)
DECLARE SUB DoFEFF (OPERAND.SIZE!, SREG!, ADDRESS.LENGTH!, DEC.NUM, LF)
DECLARE SUB GoFar (OS!, selector!, offset#, JMP.NOT.CALL!)
DECLARE SUB AluMI (OS, SREG, ADS, DEC.NUM, LF)
DECLARE SUB Arpl (OS!, SREG!, ADS)
DECLARE SUB Cli ()
DECLARE SUB Enter (OS!)
DECLARE SUB Imul (OS!, segReg!, ADS, DEC.NUM!)
DECLARE SUB Lahf ()
DECLARE SUB Lea (OS!, segReg!, ADS)
DECLARE SUB Leave (OS!)
DECLARE SUB Mov (OS, SGRG, ADS, DEC.NUM)
DECLARE SUB Pusha (OS!)
DECLARE SUB Popa (OS!)
DECLARE SUB Popf (OS!)
DECLARE SUB Pushf (OS!)
DECLARE SUB Ret (OS, DEC.NUM)
DECLARE SUB Sahf ()
DECLARE SUB Sti ()
DECLARE SUB Test (OS!, SREG!, ADS!, DEC.NUM!)
DECLARE SUB Xadd (OS!, SREG!, ADS, DEC.NUM!, LF)
DECLARE SUB Xch (OS, SREG, ADS, DEC.NUM, LF)
DECLARE SUB Xlat (ADS, DEC.NUM!)
DECLARE SUB push (value#, N.BYTES!)
DECLARE FUNCTION pop# (N.BYTES!)
DECLARE SUB signal.fault (xcptype%, error.code%)
DECLARE SUB software.interrupt (intnum%)
DECLARE SUB Do87 (OS, SGRG, ADS, BYTE1)
DECLARE FUNCTION PrefixCheck% (BYTE!, OS!, ADS!, SGRG!, REPEAT!, LF!)
DECLARE FUNCTION GetEffAdd# (SREG.NUM!, DEC.NUM!)
DECLARE FUNCTION in.limits! (desc AS DESCR, Offst#, Size)
DECLARE SUB init.VM.desc ()
DECLARE SUB load.data.seg (SREG!, desc AS DESCR, FLAG%)
DECLARE SUB load.SS (sel!, desc AS ANY, FLAG%)
DECLARE SUB load.CS (sel!, desc AS ANY, FLAG%)
DECLARE SUB read.descr (sel!, desc AS DESCR, FLAG%)
DECLARE SUB load.gate.CS (gatedesc AS DESCR)
DECLARE SUB enter.new.task (sel!, desc AS DESCR, nested.task%, err.flag%, err.code%)
REM
REM Decoding Functions
REM
DECLARE FUNCTION FLAGS# (N.BYTES!)
DECLARE SUB FLAG.STORE (value#, N.BYTES!)
DECLARE FUNCTION REGZ# (REG.NUM!)
DECLARE FUNCTION REG.CODE! (REG.NUM!, Size!)
DECLARE FUNCTION WORD.SIZE! (OS!, WIDTH.BIT!)
DECLARE SUB REG.STORE (REG.NUM!, NUM#)
DECLARE FUNCTION RM.BITS! (DEC.NUM)
DECLARE FUNCTION REG.BITS (DEC.NUM!)

SUB Arpl (OS, SREG, ADS)
                                        '63
IF mode = REAL OR mode = VM86 THEN
    CALL signal.fault(INVALID.INSTR, 0)
ELSE
    LET DEC.NUM = fetch.code#(1)
   
    LET REG.NUM = REG.BITS(DEC.NUM)
    LET SOURCE = REG.CODE(REG.NUM, 2)
   
    LET src.RPL# = Bits#(REG#(SOURCE), 1, 0)
   
    IF DEC.NUM >= &HC0 THEN
        LET RM.NUM = RM.BITS(DEC.NUM)
        LET DEST = REG.CODE(RM.NUM, 2)
        LET dst# = REG#(DEST)
    ELSE
        LET offset# = GetOffset#(SREG, ADS, DEC.NUM)
        LET dst# = lg.read#(SREG, offset#, 2)
    END IF
   
    LET dst.RPL# = Bits#(X#, 1, 0)
       
    IF src.RPL > dst.RPL THEN
        LET X# = X# - dst.RPL# + src.RPL#
        IF DEC.NUM >= &HC0 THEN
            CALL REG.STORE(DEST, X#)
        ELSE
            CALL lg.write(SREG, offset#, X#, 2)
        END IF
        LET ZF = 1
    ELSE
        LET ZF = 0
    END IF
END IF
END SUB

SUB Do87 (OS, SGRG, ADS, FOP)
REM See p259
    LET N.BYTES = OS \ 8
    LET DEC.NUM = fetch.code#(1)
    
    LET EXT = REG.BITS(DEC.NUM)
    IF DEC.NUM >= &HC0 THEN
        LET FRG = RM.BITS(DEC.NUM)
    ELSE
        LET offset# = GetOffset#(SREG, ADS, DEC.NUM)
    END IF
 

IF Bits#(ctrlReg#(0), 1, 1) = 1 THEN  'MP = 1 Math Present
    REM Execute floating point
    REM The setup program is supposed to see if the co-processor
    REM is present and set this flag to zero if it's not.
ELSEIF Bits#(ctrlReg#(0), 2, 2) = 1 AND ARCHNUM < 5 THEN   ' if EM = 1
    LET INTERRUPT.RECEIVED = TRUE
    LET INTERRUPT.NUMBER = PE.UNAVAILABLE ' No Processor extension available.
    REM This interrupt is supposed to be used
    REM to do the software emulation of the 87
ELSEIF Bits#(ctrlReg#(0), 3, 3) = 1 THEN   ' if TS = 1
    LET INTERRUPT.RECEIVED = TRUE
    LET INTERRUPT.NUMBER = PE.UNAVAILABLE ' No Processor extension available.
    REM Some other task is occupying the 87
ELSE
    REM CALL 87 Simulator
END IF
END SUB

SUB DoAaCrap (DEC.NUM)
IF DEC.NUM = &H37 THEN           'AAA
    LET X& = REG#(16)
    IF AF = 1 OR (X& AND &HF > 9) THEN
        LET X& = (X& + 6) AND &HF
        CALL REG.STORE(16, CDBL(X&))
        LET Y# = REG#(20) + 1
        CALL REG.STORE(20, Y#)
        LET AF = 1
    ELSE
        LET AF = 0
    END IF
    LET CF = AF
ELSEIF DEC.NUM = &H3F THEN      'AAS
    LET X& = REG#(16)
    IF AF = 1 OR (X& AND &HF > 9) THEN
        LET X& = X& - 6
        CALL REG.STORE(16, CDBL(X&))
        LET Y# = REG#(20) - 1
        CALL REG.STORE(20, Y#)
        LET AF = 1
    ELSE
        LET AF = 0
    END IF
    LET CF = AF
ELSEIF DEC.NUM = &H27 THEN
    LET X& = REG#(16)
    IF AF = 1 OR (X& AND &HF) > 9 THEN
        LET X& = X& + 6
        CALL REG.STORE(16, CDBL(X&))
        LET AF = 1
    ELSE
        LET AF = 0
    END IF
    IF (X& AND &HF0) > &H90 OR CF = 1 THEN
        LET X& = X& + &H60
        CALL REG.STORE(16, CDBL(X&))
        LET CF = 1
    ELSE
        LET CF = 0
    END IF

    IF X& AND &H80 > 0 THEN
        LET SF = 1
    ELSE
        LET SF = 0
    END IF
    IF X& = 0 THEN
        LET ZF = 1
    ELSE
        LET ZF = 0
    END IF

   
ELSEIF DEC.NUM = &H2F THEN
    LET X& = REG#(16)
    IF AF = 1 OR (X& AND &HF) > 9 THEN
        LET X& = X& - 6
        CALL REG.STORE(16, CDBL(X&))
        LET AF = 1
    ELSE
        LET AF = 0
    END IF
    IF (X& AND &HF0) > &H90 OR CF = 1 THEN
        LET X& = X& - &H60
        CALL REG.STORE(16, CDBL(X&))
        LET CF = 1
    ELSE
        LET CF = 0
    END IF

    IF X& AND &H80 > 0 THEN
        LET SF = 1
    ELSE
        LET SF = 0
    END IF
    IF X& = 0 THEN
        LET ZF = 1
    ELSE
        LET ZF = 0
    END IF

ELSEIF DEC.NUM = &HD4 THEN
    LET N# = fetch.code#(1)
    LET M# = INT(REG#(16) / N#)
    CALL REG.STORE(20, M#)
    LET M# = CDBL(INT(REG#(16) MOD INT(N#)))
    CALL REG.STORE(16, M#)
ELSEIF DEC.NUM = &HD5 THEN
    LET N# = fetch.code#(1)
    LET M# = REG#(20) * N# + REG#(16)
    CALL REG.STORE(20, 0#)
    CALL REG.STORE(16, M#)
END IF
END SUB

SUB DoBound (OS, SREG, ADS)

    LET N.BYTES = OS \ 8
    LET DEC.NUM = fetch.code#(1)
    IF DEC.NUM >= &HC0 THEN
        CALL signal.fault(INVALID.INSTR, 0)
        EXIT SUB
    END IF
    LET REG.NUM = REG.BITS(DEC.NUM)
    LET DEST = REG.CODE(REG.NUM, N.BYTES)
    LET Z# = REG#(DEST)
    LET offset# = GetOffset#(SREG, ADS, DEC.NUM)
    LET X# = lg.read#(SREG, offset#, N.BYTES)
    LET Y# = lg.read#(SREG, offset# + CDBL(N.BYTES), N.BYTES)
    IF Z# < X# OR Y# < Z# THEN
        CALL signal.fault(BOUND, -1)
    END IF

END SUB

SUB DoCall (OS, DEC.NUM)
REM
REM          CALL  subroutine   (opcodes 9A & E8)
REM
LET N.BYTES = OS \ 8
LET COMMAND.MODE = FALSE
LET CALL.DEPTH = CALL.DEPTH + 1

IF DEC.NUM = &HE8 THEN
    LET IMM.NUM# = fetch.code#(-N.BYTES)
    LET BRANCH.EIP# = Mod2N#(EIP# + IMM.NUM#, OS)
    CALL push(EIP#, N.BYTES)
    LET EIP# = BRANCH.EIP#
ELSEIF DEC.NUM = &H9A THEN
    LET NewOffset# = fetch.code#(N.BYTES)
    LET NewSel = fetch.code#(2)
    CALL push(CDBL(segReg(CS)), N.BYTES)
    CALL push(EIP#, N.BYTES)
    CALL GoFar(OS, NewSel, NewOffset#, FALSE) 'JMP.NOT.CALL = FALSE
END IF

END SUB

SUB DoInterrupt (DEC.NUM)
REM
REM            Process Interrupt subroutine  (opcodes CC, CD & CE)
   
    LET INTERRUPT.RECEIVED = TRUE

    IF DEC.NUM = &HCC THEN
        LET INTERRUPT.NUMBER = 3
    ELSEIF mode = VM86 AND CPL > IOPL THEN
        CALL signal.fault(GP, 0)
    ELSEIF DEC.NUM = &HCE AND OF = 0 THEN
        LET INTERRUPT.RECEIVED = FALSE
        EXIT SUB
    ELSEIF DEC.NUM = &HCE THEN
        LET INTERRUPT.NUMBER = 4
    ELSEIF DEC.NUM = &HCD THEN
            LET INTERRUPT.NUMBER = fetch.code#(1)
    END IF
    CALL software.interrupt(INTERRUPT.NUMBER)
    
END SUB

SUB DoLoop (OS, ADS, DEC.NUM)
REM
REM         LOOPNE, LOOPE, LOOP and JECXZ (opcodes Ex, x < 4)
REM
LET REG.NUM = REG.CODE(ECX, ADS \ 8)
LET brad# = EIP# + fetch.code#(-1)     ' 1 byte jumps!
LET brad# = Mod2N#(brad#, OS)
IF DEC.NUM < &HE3 THEN
    CALL REG.STORE(REG.NUM, REG#(REG.NUM) - 1)
END IF
IF DEC.NUM = &HE0 AND ZF = 0 AND REG#(REG.NUM) <> 0 THEN
        LET EIP# = brad#
ELSEIF DEC.NUM = &HE1 AND ZF = 1 AND REG#(REG.NUM) <> 0 THEN
        LET EIP# = brad#
ELSEIF DEC.NUM = &HE2 AND REG#(REG.NUM) <> 0 THEN
        LET EIP# = brad#
ELSEIF DEC.NUM = &HE3 AND REG#(REG.NUM) = 0 THEN
        LET EIP# = brad#
END IF

END SUB

SUB DoMova (OS, SREG, ADS, DEC.NUM)
                                           'Ax, x < 4
LET N.BYTES = WORD.SIZE(OS, DEC.NUM MOD 2)
LET REG.NUM = REG.CODE(EAX, N.BYTES)

LET offset# = lg.read#(SREG, fetch.code#(N.BYTES), N.BYTES)
IF DEC.NUM < &HA2 THEN       ' A is destination
    LET Mem# = lg.read#(SREG, offset#, N.BYTES)
    CALL REG.STORE(REG.NUM, Mem#)
ELSE
    CALL lg.write(SREG, offset#, REG#(REG.NUM), N.BYTES)
END IF


END SUB

SUB DoMovrI (OS, DEC.NUM)

REM MOV r, imm (opcodes By)

    LET N.BYTES = WORD.SIZE(OS, (DEC.NUM - &HB0) \ 8)
    LET DEST = REG.CODE(DEC.NUM MOD 8, N.BYTES)
    LET IMM.NUM# = fetch.code#(N.BYTES)
    IF DEST = ESP THEN
        LET BOTTOM.STACK# = IMM.NUM#
    END IF
    CALL REG.STORE(DEST, IMM.NUM#)

END SUB

SUB DoMovs (OS, SGRG, ADS, DEC.NUM)
                                        '8C and 8E
    LET SEG.IS.DEST = (DEC.NUM = &H8E)
    LET N.BYTES = OS \ 8
   
    LET DEC.NUM = fetch.code#(1)
    
    LET SREG.NUM = REG.BITS(DEC.NUM)
    IF SREG.NUM > 5 OR (SEG.IS.DEST AND SREG.NUM = 1) THEN
        LET INSTRUCTION.RECOGNIZED = FALSE
        EXIT SUB
    END IF
       
    IF DEC.NUM >= &HC0 THEN
        LET GEN.REG = REG.CODE(RM.BITS(DEC.NUM), N.BYTES)
    ELSE
         LET offset# = GetOffset#(SGRG, ADS, DEC.NUM)
    END IF

    IF NOT SEG.IS.DEST THEN
        IF DEC.NUM >= &HC0 THEN
            CALL REG.STORE(GEN.REG, CDBL(segReg(SREG.NUM)))
        ELSE
            CALL lg.write(SGRG, offset#, CDBL(segReg(SREG.NUM)), N.BYTES)
        END IF
        EXIT SUB
    ELSE
        IF DEC.NUM >= &HC0 THEN
            LET sel = Bits#(REG#(GEN.REG), 15, 0)
        ELSE
            LET sel = lg.read#(SGRG, offset#, 2)
        END IF
    END IF
    
    IF mode = PROTECTED THEN
        IF SREG.NUM = SS THEN
            CALL load.SS(sel, descrVec(SREG.NUM), FALSE)
        ELSE
            CALL load.data.seg(sel, descrVec(SREG.NUM), FALSE)
        END IF
    ELSE
        LET descrVec(SREG.NUM).Valid = TRUE
        LET descrVec(SREG.NUM).BaseAdd = CDBL(sel * 16)  ' 8086 segments
        LET descrVec(SREG.NUM).Limit = 65535#              ' 64K segments
        LET descrVec(SREG.NUM).DefaultAttr = FALSE
        LET descrVec(SREG.NUM).Readable = TRUE
        LET descrVec(SREG.NUM).Writable = TRUE
        LET descrVec(SREG.NUM).ExpandDown = 0
        LET descrVec(SREG.NUM).Executable = FALSE
    END IF
    LET segReg(SREG.NUM) = sel
    IF prev.instr.loaded.SS THEN
        LET prev.instr.loaded.SS = FALSE
    ELSE
        LET prev.instr.loaded.SS = TRUE
    END IF

END SUB

SUB DoMVI (OS, SREG, ADS, DEC.NUM)

REM MOV Subroutine  (opcodes C6, C7)
  
    LET N.BYTES = WORD.SIZE(OS, DEC.NUM MOD 2)

    LET DEC.NUM = fetch.code#(1)    'ModRM byte as usual.
   
    IF REG.BITS(DEC.NUM) <> 0 THEN
        LET INSTRUCTION.RECOGNIZED = FALSE
        EXIT SUB
    END IF
    IF DEC.NUM >= &HC0 THEN
        LET DEST = REG.CODE(RM.BITS(DEC.NUM), N.BYTES)
    ELSE
        LET offset# = GetOffset#(SREG, ADS, DEC.NUM)
    END IF
    LET IMM.NUM# = fetch.code#(N.BYTES)
    IF DEC.NUM >= &HC0 THEN
        CALL REG.STORE(DEST, IMM.NUM#)
        IF DEST = ESP THEN
            LET BOTTOM.STACK# = IMM.NUM#
        END IF
    ELSE
        CALL lg.write(SREG, offset#, IMM.NUM#, N.BYTES)
    END IF


END SUB

SUB DoPop (OS, SREG, ADS, DEC.NUM)

REM
REM   POP subroutine   (opcodes 5x, x > 7, 8F, 07,17, 1F, 0FA1, 0FA9)
  
    LET N.BYTES = OS \ 8
    LET X# = pop#(N.BYTES)
  
    IF DEC.NUM > &H57 AND DEC.NUM < &H60 THEN
        LET REG.NUM = REG.CODE(DEC.NUM - &H58, N.BYTES)
        CALL REG.STORE(REG.NUM, X#)
        EXIT SUB
    ELSEIF DEC.NUM = &H8F THEN
        LET DEC.NUM = fetch.code#(1)
        LET REG.NUM = REG.CODE(REG.BITS(DEC.NUM), N.BYTES)
        IF REG.NUM <> 0 THEN
            LET INSTRUCTION.RECOGNIZED = FALSE
            EXIT SUB
        END IF
        IF DEC.NUM >= &HC0 THEN
            LET DEST = RM.BITS(DEC.NUM)
            LET DEST = REG.CODE(DEST, N.BYTES)
            CALL REG.STORE(DEST, X#)
        ELSE
            LET offset# = GetOffset#(SREG, ADS, DEC.NUM)
            CALL lg.write(SREG, offset#, X#, N.BYTES)
        END IF
        EXIT SUB
    ELSEIF DEC.NUM MOD 8 = 7 AND DEC.NUM < &H20 THEN
        LET SEG.REG = DEC.NUM \ 8
    ELSEIF DEC.NUM = &HA1 OR DEC.NUM = &HA9 THEN      '0F byte already gone
        LET SEG.REG = (DEC.NUM - &HA1) \ 8 + 4
    END IF
    REM p530
    REM Just POP segment register left.
    LET X16 = Bits#(X#, 15, 0)
    LET segReg(SEG.REG) = X16
    IF SEG.REG <> SS THEN
        IF mode = PROTECTED THEN
            CALL load.data.seg(X16, descrVec(SEG.REG), FALSE)
        ELSE
            LET descrVec(SEG.REG).BaseAdd = X16 * 16#
        END IF
    ELSEIF SEG.REG = SS THEN
        IF mode = PROTECTED THEN
            CALL load.SS(X16, descrVec(SS), FALSE)
        ELSE
            LET descrVec(SS).BaseAdd = X16 * 16#
        END IF
   
        IF prev.instr.loaded.SS THEN
            LET prev.instr.loaded.SS = FALSE
        ELSE
            LET prev.instr.loaded.SS = TRUE
        END IF
    END IF
END SUB

SUB DoPush (OS, DEC.NUM)
REM
REM  PUSH subroutine (opcodes 5x, x < 8, 68, 6A, 06, 16, 0E, 1E, 0FA0, 0FA8)
   
    IF DEC.NUM > &H20 AND DEC.NUM < &H58 THEN
        LET REG.NUM = REG.CODE(DEC.NUM - &H50, OS \ 8)
        LET src# = REG#(REG.NUM)
    ELSEIF DEC.NUM = &H68 OR DEC.NUM = &H6A THEN
        LET N.BYTES = WORD.SIZE(OS, &H6A - DEC.NUM)
        LET src# = fetch.code#(-N.BYTES)
    ELSEIF DEC.NUM MOD 8 = 6 AND DEC.NUM < &H20 THEN
        LET src# = segReg(DEC.NUM \ 8)
    ELSE  ' 0F byte already discarded, looking at A0 & A8 here.
        LET src# = segReg((DEC.NUM - &HA0) \ 8 + 4)
    END IF
   
    CALL push(src#, OS \ 8)

END SUB

SUB DoShift (OS, SREG, ADS, DEC.NUM)
REM
REM Funnel shift, SHRL & SHRD (opcodes 0F Ax, x = 4,5,C, & D)
REM
REM and ROL,ROR,RCR,SHL,SHR,SAR Subroutine (opcodes C0, C1, Dx, x < 4)

    LET DOUBLE.FLAG = (DEC.NUM < &HB0)  ' First byte 0F already discarded.
    IF DOUBLE.FLAG THEN
        LET IMM.FLAG = (DEC.NUM MOD 2 = 0)
        LET WIDTH.BIT = 1
        LET LEFT.FLAG = ((DEC.NUM AND 8) = 0)
    ELSE
        LET IMM.FLAG = (DEC.NUM < &HD0)
        LET WIDTH.BIT = DEC.NUM MOD 2
    END IF

    LET N.BYTES = WORD.SIZE(OS, WIDTH.BIT)
    LET BIT.LEN = 8 * N.BYTES
    IF NOT IMM.FLAG THEN
        IF DOUBLE.FLAG OR (DEC.NUM AND 2) THEN
            LET NUM.SHIFTS = REG#(CL)
        ELSE
            LET NUM.SHIFTS = 1
        END IF
    END IF
  
    LET DEC.NUM = fetch.code#(1)
    IF DOUBLE.FLAG THEN
        LET SOURCE = REG.BITS(DEC.NUM)
        LET PAD# = REG#(SOURCE)
        LET Y$ = BitStr$(PAD#, BIT.LEN)
        IF LEFT.FLAG THEN
            LET OP.NUM = -1
        ELSE
            LET OP.NUM = -2
        END IF
    ELSE
        LET OP.NUM = REG.BITS(DEC.NUM)
    END IF
    IF DEC.NUM >= &HC0 THEN
        LET DEST = RM.BITS(DEC.NUM)
        LET DEST = REG.CODE(DEST, N.BYTES)
        LET X$ = BitStr$(REG#(DEST), BIT.LEN)
    ELSE
        LET offset# = GetOffset#(SREG, ADS, DEC.NUM)
        LET IMM.NUM# = lg.read#(SREG, offset#, N.BYTES)
        LET X$ = BitStr$(IMM.NUM#, BIT.LEN)
    END IF
    IF IMM.FLAG THEN
        LET NUM.SHIFTS = fetch.code#(1)
    END IF
    IF NUM.SHIFTS = 0 THEN EXIT SUB

    IF OP.NUM = 0 THEN           ' ROL
        LET X$ = RIGHT$(X$, BIT.LEN - NUM.SHIFTS) + LEFT$(X$, NUM.SHIFTS)
        LET CF = VAL(RIGHT$(X$, 1))
        IF NUM.SHIFTS = 1 THEN 'Set if CF does not match most sig bit
            LET OF = 1 + (CF = VAL(LEFT$(X$, 1)))
        ELSE                   ' Agarwal p548
            LET OF = CINT(RND)
        END IF
    ELSEIF OP.NUM = 1 THEN        'ROR
        LET X$ = RIGHT$(X$, NUM.SHIFTS) + LEFT$(X$, BIT.LEN - NUM.SHIFTS)
        LET CF = VAL(LEFT$(X$, 1))
        IF NUM.SHIFTS = 1 THEN 'Set if two most sig bits differ.
           LET OF = 1 + (MID$(X$, 1, 1) = MID$(X$, 2, 1))
        ELSE
            LET OF = CINT(RND)
        END IF
    ELSEIF OP.NUM = 2 THEN         'RCL
        LET X$ = RIGHT$(STR$(CF), 1) + X$
        LET X$ = RIGHT$(X$, BIT.LEN + 1 - NUM.SHIFTS) + LEFT$(X$, NUM.SHIFTS)
        LET CF = VAL(LEFT$(X$, 1))
        LET X$ = RIGHT$(X$, BIT.LEN)
        IF NUM.SHIFTS = 1 THEN 'Set if CF does not match most sig bit
            LET OF = 1 + (CF = VAL(LEFT$(X$, 1)))
        ELSE                   ' Agarwal p548
            LET OF = CINT(RND)
        END IF
    ELSEIF OP.NUM = 3 THEN         'RCR
        LET X$ = RIGHT$(STR$(CF), 1) + X$
        LET X$ = RIGHT$(X$, NUM.SHIFTS) + LEFT$(X$, BIT.LEN + 1 - NUM.SHIFTS)
        LET CF = VAL(LEFT$(X$, 1))
        LET X$ = RIGHT$(X$, BIT.LEN)
        IF NUM.SHIFTS = 1 THEN 'Set if two most sig bits differ.
            LET OF = 1 + (MID$(X$, 1, 1) = MID$(X$, 2, 1))
        ELSE
            LET OF = CINT(RND)
        END IF
    ELSEIF OP.NUM = 4 THEN         'SHL
        LET CF = VAL(RIGHT$(LEFT$(X$, NUM.SHIFTS), 1))
        LET X$ = MID$(X$, NUM.SHIFTS + 1) + STRING$(NUM.SHIFTS, "0")
        IF NUM.SHIFTS = 1 THEN 'Set if CF does not match most sig bit
            LET OF = 1 + (CF = VAL(LEFT$(X$, 1)))
        ELSE                   ' Agarwal p548
            LET OF = CINT(RND)
        END IF
        IF INSTR(X$, "1") = 0 THEN
            LET ZF = 1
        ELSE
            LET ZF = 0
        END IF
        LET SF = VAL(LEFT$(X$, 1))
    ELSEIF OP.NUM = 5 THEN         'SHR
        LET CF = VAL(LEFT$(RIGHT$(X$, NUM.SHIFTS), 1))
        IF NUM.SHIFTS = 1 THEN '  Most sig bit before shift.
            LET OF = VAL(LEFT$(X$, 1))
        ELSE                   ' Agarwal p563
            LET OF = CINT(RND)
        END IF
        LET X$ = LEFT$(STRING$(NUM.SHIFTS, "0") + X$, BIT.LEN)
        IF INSTR(X$, "1") = 0 THEN
            LET ZF = 1
        ELSE
            LET ZF = 0
        END IF
        LET SF = VAL(LEFT$(X$, 1))
    ELSEIF OP.NUM = 7 THEN         'SAR
        IF NUM.SHIFTS = 1 THEN LET OF = 0  'Agarwal p 553
        LET CF = VAL(LEFT$(RIGHT$(X$, NUM.SHIFTS), 1))
        LET Y$ = LEFT$(X$, 1)
        LET X$ = LEFT$(STRING$(NUM.SHIFTS, Y$) + X$, BIT.LEN)
        IF INSTR(X$, "1") = 0 THEN
            LET ZF = 1
        ELSE
            LET ZF = 0
        END IF
        LET SF = VAL(LEFT$(X$, 1))
    ELSEIF OP.NUM = -1 THEN         'SHLD
        LET CF = VAL(RIGHT$(LEFT$(X$, NUM.SHIFTS), 1))
        LET Y$ = LEFT$(Y$, NUM.SHIFTS)
        LET X$ = MID$(X$, NUM.SHIFTS + 1) + Y$
        IF NUM.SHIFTS = 1 THEN 'Set if CF does not match most sig bit
            LET OF = 1 + (CF = VAL(LEFT$(X$, 1)))
        ELSE                   ' Agarwal p548
            LET OF = CINT(RND)
        END IF
        IF INSTR(X$, "1") = 0 THEN
            LET ZF = 1
        ELSE
            LET ZF = 0
        END IF
        LET SF = VAL(LEFT$(X$, 1))
    ELSEIF OP.NUM = -2 THEN         'SHRD
        LET CF = VAL(LEFT$(RIGHT$(X$, NUM.SHIFTS), 1))
        IF NUM.SHIFTS = 1 THEN '  Most sig bit before shift.
            LET OF = VAL(LEFT$(X$, 1))
        ELSE                   ' Agarwal p563
            LET OF = CINT(RND)
        END IF
        LET CF = VAL(RIGHT$(LEFT$(X$, NUM.SHIFTS), 1))
        LET Y$ = RIGHT$(Y$, NUM.SHIFTS)
        LET X$ = LEFT$(Y$ + X$, BIT.LEN)
        IF INSTR(X$, "1") = 0 THEN
            LET ZF = 1
        ELSE
            LET ZF = 0
        END IF
        LET SF = VAL(LEFT$(X$, 1))
    ELSE
        LET INSTRUCTION.RECOGNIZED = FALSE
        EXIT SUB
    END IF

    IF DEC.NUM >= &HC0 THEN
        CALL REG.STORE(DEST, BinVal#(X$))
    ELSE
        CALL lg.write(SREG, offset#, BinVal#(X$), N.BYTES)
    END IF

END SUB

SUB Enter (OS)
    LET FRAME.SIZE = fetch.code#(2)
    LET level = fetch.code#(1)
    LET level = level MOD 32
    LET N.BYTES = OS \ 8
    LET BASEP = REG.CODE(5, N.BYTES)
    LET STACKP = REG.CODE(4, N.BYTES)

    CALL push(REG#(BASEP), N.BYTES)

    LET framePtr# = REG#(STACKP)
    
        FOR I = 1 TO level - 1
            CALL REG.STORE(BASEP, REG#(BASEP) - N.BYTES)
            LET offset# = REG#(BASEP)
            CALL push(lg.read#(SS, offset#, N.BYTES), N.BYTES)
        NEXT

    CALL push(framePtr#, N.BYTES)
    CALL REG.STORE(BASEP, framePtr#)

    IF descrVec(SS).DefaultAttr THEN
        LET STACK.REG = ESP
    ELSE
        LET STACK.REG = SP
    END IF
 
    LET POINTER# = REG#(STACK.REG) - FRAME.SIZE

    CALL REG.STORE(STACK.REG, POINTER#)

END SUB

SUB Fetchexec (REPEAT, BYTE1)
REM
REM If REPEAT = 0 then fetch an instruction and commence execution.
REM    If the instruction has no repitition prefix then just execute
REM    it and return 0 in REPEAT.
REM    If the instruction is a repeated instruction, then
REM    do one iteration, and return the code 1 for REP or REPE and
REM    -1 for REPNE in REPEAT.  Return the machine code for the
REM    instruction in BYTE1.
REM
REM IF REPEAT <> 0 then do no fetching.  Execute the instruction whose
REM    machine code is in BYTE1 a single time.  If iterations remain to
REM    be done then return BYTE1 and REPEAT unchanged. If no iterations
REM    are left then return 0 in REPEAT.
REM
             
    REM
    REM Setup defaults
    REM
      
    IF descrVec(CS).DefaultAttr AND ARCHNUM > 2 THEN
        LET OPSIZE.DEFAULT = 32
    ELSE
        LET OPSIZE.DEFAULT = 16
    END IF
    LET ADSIZE.DEFAULT = OPSIZE.DEFAULT
    LET OS = OPSIZE.DEFAULT
    LET ADS = ADSIZE.DEFAULT
   
    IF REPEAT = 0 THEN
        LET BYTE1 = fetch.code#(1)
        LET SGRG = -1    ' Let -1 indicate the absense of an override prefix.
        LET LOCK.FLAG = FALSE
        WHILE PrefixCheck%(BYTE1, OS, ADS, SGRG, REPEAT, LOCK.FLAG)
            LET BYTE1 = fetch.code#(1)
        WEND
    END IF

    LET HB1 = BYTE1 \ 16               ' HB stands for half byte
    LET HB2 = BYTE1 MOD 16
REM
REM
    LET SAVE.prev.instr.loaded.SS = prev.instr.loaded.SS
    LET INSTRUCTION.RECOGNIZED = TRUE
    IF (HB1 = 6 AND HB2 > &HB) OR (HB1 = &HA AND HB2 > 3 AND HB2 \ 2 <> 4) THEN
        IF LOCK.FLAG THEN
            LET INSTRUCTION.RECOGNIZED = FALSE
        ELSE
            REM CALL DoRep(OS, ADS, BYTE1, REPEAT)
            LET N.BYTES = OS \ 8
            LET COUNTER = REG.CODE(ECX, N.BYTES)
            CALL DoString(OS, ADS, BYTE1)
            CALL REG.STORE(COUNTER, REG#(COUNTER) - 1#)
            IF REPEAT = 1 AND (BYTE1 < &H70 OR BYTE1 MOD 8 < 7) THEN 'REP prefix
                 IF REG#(COUNTER) = 0 THEN
                     LET REPEAT = 0
                 END IF
             ELSEIF REPEAT = 1 THEN   'REPE prefix
                 IF REG#(COUNTER) = 0 OR ZF = 1 THEN
                     LET REPEAT = 0
                 END IF
             ELSEIF REPEAT = -1 AND (BYTE1 < &H70 OR BYTE1 MOD 8 < 7) THEN
                     LET INSTRUCTION.RECOGNIZED = FALSE
             ELSEIF REPEAT = -1 THEN
                 IF REG#(COUNTER) = 0 OR ZF = 0 THEN
                     LET REPEAT = 0
                 END IF
             END IF
        END IF
    ELSEIF REPEAT <> 0 THEN
        LET INSTRUCTION.RECOGNIZED = FALSE
    ELSEIF HB1 < 4 AND HB2 MOD 8 < 6 THEN
        CALL DoAlu(OS, SGRG, ADS, BYTE1 MOD 8, BYTE1 \ 8, LOCK.FLAG)
    ELSEIF BYTE1 = &HF THEN
        IF ARCHNUM > 1 THEN
            CALL Do0F(OS, SGRG, ADS, LOCK.FLAG)  'Jx,  XADD   , LGDT, etc, etc
        ELSE
            CALL DoPop(OS, SGRG, ADS, BYTE1)  ' Pop CS command!
        END IF
    ELSEIF HB1 = 8 AND HB2 \ 2 = 3 THEN
        CALL Xch(OS, SGRG, ADS, BYTE1, LOCK.FLAG)
    ELSEIF HB1 = 8 AND HB2 < 4 AND HB2 <> 2 THEN
        CALL AluMI(OS, SGRG, ADS, BYTE1, LOCK.FLAG)
    ELSEIF BYTE1 = &HF6 OR BYTE1 = &HF7 THEN
        CALL DoUnM(OS, SGRG, ADS, BYTE1, LOCK.FLAG)
    ELSEIF BYTE1 = &HFE OR BYTE1 = &HFF THEN
        CALL DoFEFF(OS, SGRG, ADS, BYTE1, LOCK.FLAG)
    ELSEIF LOCK.FLAG THEN
        LET INSTRUCTION.RECOGNIZED = FALSE
    ELSEIF HB1 = 4 THEN
        CALL DoIncDec(OS, BYTE1)
    ELSEIF HB1 < 2 AND HB2 MOD 8 = 6 THEN      'Push selector.
        CALL DoPush(OS, BYTE1)
    ELSEIF HB1 < 2 AND HB2 MOD 8 = 7 THEN   'Except 0F of course
        CALL DoPop(OS, SGRG, ADS, BYTE1)
    ELSEIF (HB1 < 4 AND HB2 MOD 8 = 7) OR (HB1 = &HD AND HB2 \ 2 = 2) THEN
        CALL DoAaCrap(BYTE1)
    ELSEIF HB1 = 5 AND HB2 < 8 THEN      ' Push vword register
        CALL DoPush(OS, BYTE1)
    ELSEIF HB1 = 5 OR BYTE1 = &H8F THEN
        CALL DoPop(OS, SGRG, ADS, BYTE1)
    ELSEIF BYTE1 = &H60 THEN   ' Push all
        CALL Pusha(OS)
    ELSEIF BYTE1 = &H61 THEN   ' Pop all
        CALL Popa(OS)
    ELSEIF BYTE1 = &H62 AND ARCHNUM > 1 THEN  ' Bound
        CALL DoBound(OS, SGRG, ADS)
    ELSEIF BYTE1 = &H63 AND ARCHNUM > 1 THEN  ' Adjust Privilege Level
        CALL Arpl(OS, SREG, ADS)
    ELSEIF (BYTE1 = &H68 OR BYTE1 = &H6A) AND ARCHNUM > 1 THEN    ' Push immediate
        CALL DoPush(OS, BYTE1)
    ELSEIF (BYTE1 = &H69 OR BYTE1 = &H6B) AND ARCHNUM > 1 THEN    'IMUL 3 args
        CALL Imul(OS, SGRG, ADS, BYTE1)
    ELSEIF HB1 = 7 THEN
        CALL Jcond(OS, BYTE1)
    ELSEIF (HB1 = 8 AND HB2 < 6) OR (HB1 = &HA AND HB2 \ 2 = 4) THEN
        CALL Test(OS, SGRG, ADS, BYTE1)
    ELSEIF HB1 = 8 AND HB2 < &HC THEN
        CALL Mov(OS, SGRG, ADS, BYTE1)
    ELSEIF HB1 = 8 AND (HB2 = &HC OR HB2 = &HE) THEN
        CALL DoMovs(OS, SGRG, ADS, BYTE1)
    ELSEIF HB1 = 8 AND HB2 = &HD THEN
        CALL Lea(OS, SGRG, ADS)
    ELSEIF HB1 = 9 AND HB2 = 0 THEN
        REM Nop
    ELSEIF HB1 = 9 AND HB2 < 8 THEN
        CALL Xch(OS, SGRG, ADS, BYTE1, 0)
    ELSEIF BYTE1 = &H98 OR BYTE1 = &H99 THEN
        CALL DoConv(OS, SGRG, ADS, BYTE1)
    ELSEIF BYTE1 = &H9A OR BYTE1 = &HE8 THEN
        CALL DoCall(OS, BYTE1)
    ELSEIF BYTE1 = &H9C THEN
        CALL Pushf(OS)
    ELSEIF BYTE1 = &H9D THEN
        CALL Popf(OS)
    ELSEIF BYTE1 = &H9E THEN
        CALL Sahf
    ELSEIF BYTE1 = &H9F THEN
        CALL Lahf
    ELSEIF HB1 = &HA AND HB2 < 4 THEN
        CALL DoMova(OS, SGRG, ADS, BYTE1)
    ELSEIF HB1 = &HB THEN
        CALL DoMovrI(OS, BYTE1)
    ELSEIF (HB1 = &HC AND HB2 < 2) OR (HB1 = &HD AND HB2 < 4) THEN
        CALL DoShift(OS, SGRG, ADS, BYTE1)
    ELSEIF HB1 = &HC AND (HB2 MOD 8) \ 2 = 1 THEN
        CALL Ret(OS, BYTE1)
    ELSEIF BYTE1 = &HC4 OR BYTE1 = &HC5 THEN
        CALL DoLoadSeg(OS, SGRG, ADS, BYTE1)
    ELSEIF BYTE1 = &HC6 OR BYTE1 = &HC7 THEN
        CALL DoMVI(OS, SGRG, ADS, BYTE1)
    ELSEIF BYTE1 = &HC8 AND ARCHNUM > 1 THEN
        CALL Enter(OS)
    ELSEIF BYTE1 = &HC9 THEN
        CALL Leave(OS)
    ELSEIF BYTE1 = &HCC OR BYTE1 = &HCD OR BYTE1 = &HCE THEN
        CALL DoInterrupt(BYTE1)
    ELSEIF BYTE1 = &HCF THEN
        CALL Iret(OS)
    ELSEIF HB1 = &HD AND HB2 > 7 THEN
        CALL Do87(OS, SGRG, ADS, HB2 - 8)
    ELSEIF HB1 = &HD AND HB2 = 7 THEN
        CALL Xlat(ADS, BYTE1)
    ELSEIF HB1 = &HE AND HB2 < 4 THEN
        CALL DoLoop(OS, ADS, BYTE1)
    ELSEIF HB1 = &HE AND (HB2 MOD 8) > 3 THEN
        CALL DoInOut(OS, BYTE1)
    ELSEIF HB1 = &HE THEN     ' E8 Call already done.
        CALL Jmp(OS, BYTE1)
    ELSEIF BYTE1 = &HF4 THEN
        IF mode <> REAL OR CPL <> 0 THEN
            CALL signal.fault(GP, 0)
        ELSE
            LET HALT.FLAG = TRUE
        END IF
    ELSEIF BYTE1 = &HF5 THEN
        LET CF = 1 - CF
    ELSEIF BYTE1 = &HF8 THEN
        LET CF = 0
    ELSEIF BYTE1 = &HF9 THEN
        LET CF = 1
    ELSEIF BYTE1 = &HFA THEN
        IF mode = REAL OR CPL <= IOPL THEN
            LET IE = 0
        ELSE
            CALL signal.fault(GP, 0)
        END IF
    ELSEIF BYTE1 = &HFB THEN
        IF mode = REAL OR CPL <= IOPL THEN
            LET IE = 1
        ELSE
            CALL signal.fault(GP, 0)
        END IF
    ELSEIF BYTE1 = &HFC THEN
        LET DF = 0
    ELSEIF BYTE1 = &HFD THEN
        LET DF = 1
    ELSE
        LET INSTRUCTION.RECOGNIZED = FALSE
    END IF
    IF NOT INSTRUCTION.RECOGNIZED THEN
        CALL signal.fault(INVALID.INSTR, -1)
    END IF
    REM
    REM This flag needs to be turned off somewhere.  Why not here.
    IF SAVE.prev.instr.loaded.SS AND prev.instr.loaded.SS THEN
        LET prev.instr.loaded.SS = FALSE
    END IF

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


LET X# = 0#

IF N.BYTES = 4 THEN
    LET X# = X# + AC * 2# ^ (18)
    LET X# = X# + VM * 2# ^ (17)
    LET X# = X# + RF * 2# ^ (16)
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

FUNCTION GetOffset# (SEG.REGISTER, ADDRESS.SIZE, MODRM.NUM)

      REM
      REM       SEG.REGISTER is the register supplied by the prefix
      REM       If none was specified it has the value -1.
      REM       In this case it will be replaced by the
      REM       the default segment register
      REM       Otherwise it will be left alone.
      REM
      REM       MODRM.NUM is the numeric value of a MODRMBYTE.
      REM           It is not allowed to be as much as &HC0, since
      REM           in this case there is no memory address.
      REM       At entry EIP# is pointing to the byte following
      REM               the MODRMBYTE.
      REM

    LET DEF.SEG.REGISTER = DS
  
    LET RM.NUM = RM.BITS(MODRM.NUM)
    LET ROW.NUM = MODRM.NUM \ 64 + 1
    IF ADDRESS.SIZE = 32 THEN
        IF MODRM.NUM < &HC0 THEN
            IF RM.NUM = 4 THEN
                LET G# = GetSibAdd#(SREG, ROW.NUM)
                LET DEF.SEG.REGISTER = SREG
            ELSEIF RM.NUM = 5 AND ROW.NUM = 1 THEN
                LET IMM.NUM# = fetch.code#(-4)
                LET G# = IMM.NUM#
            ELSE
                IF ROW.NUM = 1 THEN
                    LET G# = 0#
                ELSEIF ROW.NUM = 2 THEN
                    LET G# = fetch.code#(-1)
                ELSEIF ROW.NUM = 3 THEN
                    LET G# = fetch.code#(-4)
                END IF
                IF RM.NUM = EBP THEN
                    LET DEF.SEG.REGISTER = SS
                ELSEIF RM.NUM = EDI THEN
                    LET DEF.SEG.REGISTER = ES
                END IF
                LET G# = G# + REG#(RM.NUM)
            END IF
        ELSE  'This will cause an error. This case is not supposed to occur.
            RETURN
        END IF
        LET G# = Mod2N#(G#, 32)  'This is offset wraparound.' See Agarwal p196.
    ELSEIF ADDRESS.SIZE = 16 THEN
        IF RM.NUM = 6 AND ROW.NUM = 1 THEN
            LET G# = fetch.code#(2)
        ELSE
            IF ROW.NUM > 1 THEN
                LET G# = fetch.code(ROW.NUM - 1)
            ELSE
                LET G# = 0#
            END IF
            IF RM.NUM = 0 THEN
                LET G# = G# + REG#(BX) + REG#(SI)
            ELSEIF RM.NUM = 1 THEN
                LET G# = G# + REG#(BX) + REG#(DI)
            ELSEIF RM.NUM = 2 THEN
                LET G# = G# + REG#(BP) + REG#(SI)
                LET DEF.SEG.REGISTER = SS
            ELSEIF RM.NUM = 3 THEN
                LET G# = G# + REG#(BP) + REG#(DI)
                LET DEF.SEG.REGISTER = SS
            ELSEIF RM.NUM = 4 THEN
                LET G# = G# + REG#(SI)
            ELSEIF RM.NUM = 5 THEN
                LET G# = G# + REG#(DI)
            ELSEIF RM.NUM = 6 THEN
                LET G# = G# + REG#(BP)
                LET DEF.SEG.REGISTER = SS
            ELSEIF RM.NUM = 7 THEN
                LET G# = G# + REG#(BX)
            END IF
        END IF
        LET G# = Mod2N#(G#, 16)  'This is offset wraparound.' See Agarwal p196.
    ELSE
        RETURN    'Create an error!  ADDRESS.SIZE must be 16 or 32.
    END IF

    IF SEG.REGISTER = -1 THEN
        LET SEG.REGISTER = DEF.SEG.REGISTER
    END IF

LET GetOffset# = G#

END FUNCTION

FUNCTION GetSibAdd# (SREG, ROW.NUM)

      REM
      REM            Get the contribution to the effective address
      REM               which comes from the SIB byte.
      REM       ROW.NUM is as used on p269 of Morse's 386 book.
      REM
      REM       At entry EIP# is pointing at the SIB byte.
      REM       On leaving it should point to the byte
      REM       following the SIB byte and any displacement bytes.
      REM
 
        LET SREG = DS
        LET DEC.NUM = fetch.code#(1)
        LET SCALE.NUM = DEC.NUM \ 64
        LET SCALE = 1
        FOR K = 1 TO SCALE.NUM
                LET SCALE = 2 * SCALE
        NEXT
        LET BASE.REG.NUM = RM.BITS(DEC.NUM)
        LET IND.REG.NUM = REG.BITS(DEC.NUM)
    
        IF IND.REG.NUM = 4 THEN
                IF BASE.REG.NUM = 4 AND SCALE = 1 THEN
                        LET SCALE = 0
                ELSE
                        LET ERROR.MESSAGE$ = "Invalid SIB Byte."
                        LET INSTRUCTION.RECOGNIZED = FALSE
                        EXIT FUNCTION
                END IF
        END IF
          
        IF BASE.REG.NUM = 5 AND ROW.NUM = 1 THEN
                LET BASE.ADDRESS# = fetch.code#(4)
        ELSEIF BASE.REG.NUM = 5 OR BASE.REG.NUM = 4 THEN
                LET SREG = SS
                LET BASE.ADDRESS# = REG#(BASE.REG.NUM)
        ELSE
                LET BASE.ADDRESS# = REG#(BASE.REG.NUM)
        END IF
          
        IF ROW.NUM = 1 THEN
                LET SIB.ADDRESS# = BASE.ADDRESS# + SCALE * REG#(IND.REG.NUM)
        ELSEIF ROW.NUM = 2 THEN
                LET SIB.ADDRESS# = BASE.ADDRESS# + SCALE * REG#(IND.REG.NUM) + fetch.code#(-1)
        ELSEIF ROW.NUM = 3 THEN
                LET SIB.ADDRESS# = BASE.ADDRESS# + SCALE * REG#(IND.REG.NUM) + fetch.code#(-4)
        END IF
LET GetSibAdd# = SIB.ADDRESS#
END FUNCTION

SUB GoFar (OS, selector, offset#, JMP.NOT.CALL)
REM
REM Far jumps and calls
REM
REM p457 and pp 460-461   JMP descriptions
REM p298 & p304           CALL Descriptions
DIM CSdesc AS DESCR
DIM csel AS INTEGER

LET csel = (selector AND (NOT 3))

LET segReg(CS) = selector
IF (mode = REAL OR mode = VM86) THEN
    LET descrVec(CS).BaseAdd = selector * 16#
ELSE
    CALL load.CS(selector, descrVec(CS), FALSE)
    LET CSdesc = descrVec(CS)
    IF CSdesc.Executable AND NOT JMP.NOT.CALL THEN
        LET segReg(CS) = segReg(CS) OR CPL
        IF OS = 32 THEN
            LET EIP# = offset#
        ELSE
            LET EIP# = Bits#(offset#, 15, 0)
        END IF
    END IF
    IF NOT CSdesc.CDSeg THEN
        IF CSdesc.Typ = CALLGATE16 OR CSdesc.Typ = CALLGATE32 THEN
            IF CPL > CSdesc.DPL OR CPL > (selector AND 3) THEN
                CALL signal.fault(GP, csel)
                EXIT SUB
            END IF
            IF NOT CSdesc.Valid THEN
                CALL signal.fault(NP, csel)
                EXIT SUB
            END IF
            IF JMP.NOT.CALL THEN
                LET selector = CSdesc.Selectr
                LET csel = (selector AND -4)
                LET segReg(CS) = csel
                LET offset# = CSdesc.Offst
                CALL load.CS(segReg(CS), descrVec(CS), FALSE)
                IF NOT descrVec(CS).CDSeg THEN
                    CALL signal.fault(GP, csel)
                END IF
            ELSE
                CALL load.gate.CS(CSdesc)
            END IF
        ELSEIF CSdesc.Typ = TASKGATE OR CSdesc.Typ = NOTBUSYTSS16 OR CSdesc.Typ = NOTBUSYTSS32 THEN
            LET offset# = CSdesc.Offst
            IF CPL > CSdesc.DPL OR CPL > (selector AND 3) THEN
                CALL signal.fault(GP, csel)
                EXIT SUB
            END IF
            IF NOT CSdesc.Valid THEN
                CALL signal.fault(NP, csel)
                EXIT SUB
            END IF
            IF CSdesc.Typ = TASKGATE THEN
                LET selector = CSdesc.Selectr
                IF (selector AND -4) = 0 OR (selector AND 4) > 0 THEN
                    CALL signal.fault(GP, csel)
                    EXIT SUB
                END IF
                CALL read.descr(selector, CSdesc, TRUE)
                IF CSdesc.Typ <> NOTBUSY.TSS16 AND CSdesc.Typ <> NOTBUSY.TSS32 THEN
                    CALL signal.fault(GP, csel)
                    EXIT SUB
                END IF
                IF NOT CSdesc.Valid THEN
                    CALL signal.fault(NP, csel)
                    EXIT SUB
                END IF
            END IF
            CALL enter.new.task(selector, CSdesc, FALSE, FALSE, -1)
        ELSE
            CALL signal.fault(GP, csel)
        END IF
    END IF
END IF

END SUB

SUB Jmp (OS, DEC.NUM)
REM Jump subroutine          (opcodes E9, EA, and EB)

    LET COMMAND.MODE = FALSE
    LET N.BYTES = WORD.SIZE(OS, &HEB - DEC.NUM)   '1 if EB
    
    IF DEC.NUM = &HE9 OR DEC.NUM = &HEB THEN   'near and short jumps
        LET IMM.NUM# = fetch.code#(-N.BYTES)
        LET EIP# = Mod2N#(EIP# + IMM.NUM#, 32)      'Put in wrap around!
        EXIT SUB
    ELSEIF DEC.NUM = &HEA THEN
        LET offset# = fetch.code#(N.BYTES)
        LET selector = fetch.code#(2)
        CALL GoFar(OS, selector, offset#, TRUE)  'JMP.NOT.CALL = TRUE
    END IF
       
END SUB

SUB Lahf
LET NEW.AH = 0

LET NEW.AH = 2 * NEW.AH + SF
LET NEW.AH = 2 * NEW.AH + ZF
LET NEW.AH = 2 * NEW.AH + CINT(RND)
LET NEW.AH = 2 * NEW.AH + AF
LET NEW.AH = 2 * NEW.AH + CINT(RND)
LET NEW.AH = 2 * NEW.AH + PF
LET NEW.AH = 2 * NEW.AH + CINT(RND)
LET NEW.AH = 2 * NEW.AH + CF

CALL REG.STORE(AH, CDBL(NEW.AH))

END SUB

SUB Lea (OS, SGRG, ADS)

    LET N.BYTES = OS \ 8
    LET MODRM.NUM = fetch.code#(1)
    LET DEST = REG.CODE(REG.BITS(MODRM.NUM), N.BYTES)
    LET offset# = GetOffset#(SGRG, ADS, MODRM.NUM)
    CALL REG.STORE(DEST, offset#)

END SUB

SUB Leave (OS)

    IF descrVec(SS).DefaultAttr THEN
        LET STACK.REG = ESP
        LET BASEP = EBP
    ELSE
        LET STACK.REG = SP
        LET BASEP = BP
    END IF

    CALL REG.STORE(STACK.REG, REG#(BASEP))

    LET N.BYTES = OS \ 8

    LET BASEP = REG.CODE(EBP, N.BYTES)
    CALL REG.STORE(BASEP, pop#(N.BYTES))

END SUB

SUB Mov (OS, SREG, ADS, DEC.NUM)


REM MOV  subroutine         (opcodes 8x, 7 < x < C)

    LET N.BYTES = WORD.SIZE(OS, DEC.NUM MOD 2)
    LET NMEM = (DEC.NUM > &H89)
    LET DEC.NUM = fetch.code#(1)
    LET REG.NUM = REG.BITS(DEC.NUM)
    IF NMEM THEN
        LET DEST = REG.CODE(REG.NUM, N.BYTES)
    ELSE
        LET SOURCE = REG.CODE(REG.NUM, N.BYTES)
    END IF

    IF DEC.NUM >= &HC0 THEN
        LET RM.NUM = RM.BITS(DEC.NUM)
        LET Mem.REG = REG.CODE(RM.NUM, N.BYTES)
        IF NMEM THEN
            LET X# = REG#(Mem.REG)
            CALL REG.STORE(DEST, X#)
            IF DEST = ESP THEN
                LET BOTTOM.STACK# = X#
            END IF
        ELSE
            CALL REG.STORE(Mem.REG, REG#(SOURCE))
        END IF
    ELSE
        LET offset# = GetOffset#(SREG, ADS, DEC.NUM)
        IF NMEM THEN
            LET X# = lg.read#(SREG, offset#, N.BYTES)
            CALL REG.STORE(DEST, X#)
        ELSE
            CALL lg.write(SREG, offset#, REG#(SOURCE), N.BYTES)
        END IF
    END IF

END SUB

FUNCTION pop# (N.BYTES)
                            
REM p 118
    IF descrVec(SS).DefaultAttr THEN
        LET STACK.REG = ESP
    ELSE
        LET STACK.REG = SP
    END IF
    LET stack.ptr# = REG#(STACK.REG)
    LET pop# = lg.read#(SS, stack.ptr#, N.BYTES)
    LET stack.ptr# = stack.ptr# + N.BYTES
    CALL REG.STORE(STACK.REG, stack.ptr#)

 END FUNCTION

SUB Popa (OS)
                                    '61
    LET N.BYTES = OS \ 8
    CALL REG.STORE(REG.CODE(7, N.BYTES), pop#(N.BYTES))
    CALL REG.STORE(REG.CODE(6, N.BYTES), pop#(N.BYTES))
    LET JUNK# = pop#(N.BYTES)
    CALL REG.STORE(REG.CODE(4, N.BYTES), pop#(N.BYTES))
    CALL REG.STORE(REG.CODE(3, N.BYTES), pop#(N.BYTES))
    CALL REG.STORE(REG.CODE(2, N.BYTES), pop#(N.BYTES))
    CALL REG.STORE(REG.CODE(1, N.BYTES), pop#(N.BYTES))
    CALL REG.STORE(REG.CODE(0, N.BYTES), pop#(N.BYTES))
END SUB

SUB Popf (OS)
LET N.BYTES = OS \ 8
LET X# = pop#(N.BYTES)
CALL FLAG.STORE(X#, N.BYTES)
END SUB

FUNCTION PrefixCheck% (BYTE, OS, ADS, SGRG, REPEAT, LF)
LET RETVAL% = TRUE
SELECT CASE BYTE
    CASE &H26
        LET SGRG = ES
    CASE &H2E
        LET SGRG = CS
    CASE &H36
        LET SGRG = SS
    CASE &H3E
        LET SGRG = DS
    CASE &H64
        IF ARCHNUM > 2 THEN
            LET SGRG = FS
        ELSE
            LET RETVAL% = FALSE
        END IF
    CASE &H65
        IF ARCHNUM > 2 THEN
            LET SGRG = GS
        ELSE
            LET RETVAL% = FALSE
        END IF
    CASE &H66
        IF ARCHNUM > 2 THEN
            LET OS = 48 - OS
        ELSE
            LET RETVAL% = FALSE
        END IF
    CASE &H67
        IF ARCHNUM > 2 THEN
            LET ADS = 48 - ADS
        ELSE
            LET RETVAL% = FALSE
        END IF
    CASE &HF3                   'REPE
        LET REPEAT = 1
    CASE &HF2                   'REPNE
        LET REPEAT = -1
    CASE &HF0
        LET LF = -1
    CASE ELSE
        LET RETVAL% = FALSE
END SELECT

LET PrefixCheck% = RETVAL%

END FUNCTION

SUB push (value#, N.BYTES)
REM
REM  p116
REM
    IF descrVec(SS).DefaultAttr THEN
        LET STACK.REG = ESP
    ELSE
        LET STACK.REG = SP
    END IF
 
    LET POINTER# = REG#(STACK.REG) - N.BYTES

    CALL lg.write(SS, POINTER#, value#, N.BYTES)
    IF HALT.FLAG THEN EXIT SUB
    CALL REG.STORE(STACK.REG, POINTER#)

END SUB

SUB Pusha (OS)
                            '60
    LET N.BYTES = OS \ 8
    LET STP = REG.CODE(ESP, N.BYTES)
    LET SP0# = REG#(STP)
    CALL push(REG#(REG.CODE(0, N.BYTES)), N.BYTES)
    CALL push(REG#(REG.CODE(1, N.BYTES)), N.BYTES)
    CALL push(REG#(REG.CODE(2, N.BYTES)), N.BYTES)
    CALL push(REG#(REG.CODE(3, N.BYTES)), N.BYTES)
    CALL push(REG#(REG.CODE(4, N.BYTES)), N.BYTES)
    CALL push(SP0#, N.BYTES)
    CALL push(REG#(REG.CODE(6, N.BYTES)), N.BYTES)
    CALL push(REG#(REG.CODE(7, N.BYTES)), N.BYTES)

END SUB

SUB Pushf (OS)
                                      '9C
    IF mode = VM86 AND CPO > IOPL THEN
        CALL signal.fault(GP, 0)
    ELSEIF OS = 32 THEN
        LET eflg# = FLAGS#(4)
        LET vmrf# = Bits#(eflg#, 17, 16)
        LET eflg# = elfg# - vmrf# * 2# ^ 16
        CALL push(eflg#, 4)
    ELSE
        CALL push(FLAGS#(2), 2)
    END IF

END SUB

SUB Ret (OS, DEC.NUM)

REM           RET subroutine    (opcodes C2, C3, CA, & CB)
DIM csel AS INTEGER
    LET CALL.DEPTH = CALL.DEPTH - 1
    IF CALL.DEPTH < 0 AND NOT PURE.RETS THEN
        EXIT SUB
    ELSEIF CALL.DEPTH = SKIP.DEPTH THEN
        LET SKIP.DEPTH = -1
        LET WALK.MODE = FALSE
    END IF
    LET N.BYTES = OS \ 8
    LET EIP# = pop#(N.BYTES)
    IF DEC.NUM = &HCA OR DEC.NUM = &HCB THEN
        LET segReg(CS) = Bits#(pop#(N.BYTES), 15, 0)
    END IF
    IF DEC.NUM = &HC2 OR DEC.NUM = &HCA THEN
        LET N.POPBYTES = fetch.code#(2)
    ELSE
        LET N.POPBYTES = 0
    END IF
    IF descrVec(SS).DefaultAttr THEN
        LET STACK.REG = ESP
    ELSE
        LET STACK.REG = SP
    END IF
    CALL REG.STORE(STACK.REG, REG#(STACK.REG) + N.POPBYTES)
    IF DEC.NUM = &HC2 OR DEC.NUM = &HC3 THEN
        EXIT SUB
    END IF
    LET selector = segReg(CS)
    LET csel = selector AND -4
    IF mode = REAL OR mode = VM86 THEN
        descrVec(CS).BaseAdd = selector * 16#
    ELSE
        IF (selector < 4) OR CPL > (selector AND 3) THEN
            CALL signal.fault(GP, csel)
            EXIT SUB
        END IF
        IF (selector AND 3) = CPL THEN
            CALL load.CS(selector, descrVec(CS), FALSE)
        ELSE
            LET CPL = (selector AND 3)
            CALL load.CS(selector, descrVec(CS), FALSE)
            LET segReg(SS) = Bits#(pop#(N.BYTES), 15, 0)
            CALL load.SS(segReg(SS), descrVec(SS), FALSE)
            IF descrVec(SS).DefaultAttr THEN
                LET STACK.REG = ESP
            ELSE
                LET STACK.REG = SP
            END IF
            CALL REG.STORE(STACK.REG, callerESP#)
            IF (descrVec(ES).DPL < CPL) THEN
                LET descrVec(ES).Valid = FALSE
            END IF
            IF (descrVec(DS).DPL < CPL) THEN
                LET descrVec(DS).Valid = FALSE
            END IF
            IF (descrVec(FS).DPL < CPL) THEN
                LET descrVec(FS).Valid = FALSE
            END IF
            IF (descrVec(GS).DPL < CPL) THEN
                LET descrVec(GS).Valid = FALSE
            END IF
        END IF
    END IF
END SUB

SUB Sahf
                         '9E
LET AH.COPY = REG#(AH)
LET CF = AH.COPY MOD 2     '0
LET AH.COPY = AH.COPY \ 4
LET PF = AH.COPY MOD 2     '2
LET AH.COPY = AH.COPY \ 4
LET AF = AH.COPY MOD 2      '4
LET AH.COPY = AH.COPY \ 4
LET ZF = AH.COPY MOD 2      '6
LET AH.COPY = AH.COPY \ 2
LET SF = AH.COPY MOD 2      '7
END SUB

SUB software.interrupt (intnum%)
LET INTERRUPT.RECEIVED = TRUE
LET INTERRUPT.NUMBER = intnum%
LET XH.flag = FALSE
CALL handle.intr.xcp(intnum%, FALSE, -1)
END SUB

SUB Xlat (ADS, DEC.NUM)
                                 ' D7
    LET N.BYTES = ADS \ 8
    LET POINTER = REG.CODE(EBX, N.BYTES)    '  EBX or BX
    LET X# = REG#(POINTER) + REG#(AL)
    LET Y# = lg.read#(DS, X#, 1)
    CALL REG.STORE(AL, Y#)

END SUB

