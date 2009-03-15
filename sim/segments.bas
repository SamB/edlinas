REM $INCLUDE: 'x86.bi'
DECLARE FUNCTION mode! ()
DECLARE FUNCTION MemRead# (ADDR#, N.BYTES!)
DECLARE SUB MemWrite (physaddr#, value#, N.BYTES)
DECLARE SUB check.brkpt (btype%, X#)
DECLARE FUNCTION LA.rdChk# (laddr#)
DECLARE FUNCTION LA.wrChk# (X#)
DECLARE FUNCTION IDT.read# (vecnum%, offset%, N.BYTES!)
DECLARE FUNCTION TSS.read# (sel, offset#, N.BYTES!)
DECLARE SUB TSS.write (sel, offset#, value#, N.BYTES!)
DECLARE SUB load.protected.descr (OS)
DECLARE SUB read.descr (sel, desc AS DESCR, FLAG%)
DECLARE SUB init.VM.desc ()
DECLARE FUNCTION new.stk.push# (OS!, newSS!, newdesc AS DESCR, newESP#, value#)
DECLARE SUB priv.lev.switch.CALL (OS!, desc AS DESCR, N.PARAMS%)
DECLARE SUB load.CS (sel!, desc AS DESCR, FLAG%)
DECLARE SUB load.data.seg (sel!, desc AS DESCR, FLAG%)
DECLARE SUB load.SS (sel!, desc AS DESCR, FLAG%)
DECLARE SUB load.LDT (sel, des AS DESCR)
DECLARE FUNCTION read.TSS32! (sel!, desc AS DESCR, nested.task!)
DECLARE SUB load.gate.CS (gatedesc AS DESCR)
DECLARE SUB enter.new.task (sel!, desc AS DESCR, nested.task%, err.flag%, err.code%)
DECLARE SUB exit.task ()
DECLARE SUB save.task.state (sel)
DECLARE SUB read.TSS16 (sel!, desc AS DESCR, nested.task!)
DECLARE FUNCTION TSS.read# (sel, offset#, N.BYTES!)
DECLARE SUB signal.fault (xcptype%, error.code%)
DECLARE SUB DTAB.write (sel, desc AS DESCR, offt, value#, N.BYTES!)
DECLARE FUNCTION DTAB.read# (sel, desc AS DESCR, offt, FLAG%, N.BYTES!)
DECLARE SUB mark.accessed (sel!)
DECLARE FUNCTION inBounds! (sel!)
DECLARE FUNCTION in.limits! (des AS DESCR, Offst#, N!)
REM
REM Execution Functions
REM
DECLARE FUNCTION pop# (N.BYTES!)
DECLARE SUB push (value#, N.BYTES!)
DECLARE FUNCTION fetch.code# (N.BYTES!)
DECLARE SUB FLAG.STORE (value#, N.BYTES!)
REM
REM Decoding Functions
REM
DECLARE FUNCTION FLAGS# (N.BYTES!)
DECLARE FUNCTION REG.CODE! (REG.NUM!, Size!)
DECLARE FUNCTION REG# (REG.NUM!)
DECLARE FUNCTION REG.BITS (DEC.NUM!)
DECLARE FUNCTION RM.BITS! (DEC.NUM!)
DECLARE SUB REG.STORE (REG.NUM!, NUM#)
DECLARE FUNCTION WORD.SIZE! (OS!, WIDTH.BIT!)
REM
REM Conversion Functions
REM
DECLARE FUNCTION BYTE.SWAP$ (HEX.STRING$)
DECLARE FUNCTION Mod2N# (NUM#, N!)

MemFix:
OPEN MEM.PATH$ + "MemVec.DAT" FOR RANDOM AS #2 LEN = 4
RESUME

SUB DoLoadSeg (OS, SGRG, ADS, DEC.NUM)
                            'C5, C4, 0F B2, 0F B4, 0F B5
                                ' 0F byte has been discarded
IF DEC.NUM = &HC5 THEN
    LET SREG = DS
ELSEIF DEC.NUM = &HC4 THEN
    LET SREG = ES
ELSEIF DEC.NUM = &HB2 THEN
    LET SREG = SS
ELSEIF DEC.NUM = &HB4 THEN
    LET SREG = FS
ELSEIF DEC.NUM = &HB5 THEN
    LET SREG = GS
ELSE
    LET INSTRUCTION.RECOGNIZED = FALSE
    EXIT SUB
END IF

    LET DEC.NUM = fetch.code#(1)
    LET REG.NUM = REG.BITS(DEC.NUM)
    LET N.BYTES = OS \ 8
    LET DEST = REG.CODE(REG.NUM, N.BYTES)
    IF DEC.NUM >= &HC0 THEN
        LET INSTRUCTION.RECOGNIZED = FALSE
        EXIT SUB
    ELSE
        LET offset# = GetOffset#(SGRG, ADS, DEC.NUM)
        LET X# = lg.read#(SGRG, offset#, N.BYTES)
        LET y = lg.read#(SGRG, offset# + CDBL(N.BYTES), 2)
        CALL REG.STORE(DEST, X#)
        LET segReg(SREG) = y
    END IF
 
    IF mode = REAL OR mode = VM86 THEN
        descrVec(SREG).BaseAdd = y * 16#
    ELSEIF SREG = SS THEN
        CALL load.SS(y, descrVec(SREG), FALSE)
    ELSE
        CALL load.data.seg(y, descrVec(SREG), FALSE)
    END IF


END SUB

FUNCTION DTAB.read# (err.code, desc AS DESCR, offt, TSSload AS INTEGER, N.BYTES)
REM     pp 74-75
DIM cerr AS INTEGER
LET cerr = (err.code AND -4)
IF NOT desc.Valid OR NOT desc.Readable OR NOT in.limits(desc, CDBL(offt), N.BYTES) THEN
    IF TSSload THEN
        CALL signal.fault(task, cerr)
    ELSE
        CALL signal.fault(GP, cerr)
    END IF
    EXIT FUNCTION
END IF
LET saveCPL = CPL
LET CPL = 0
LET DTAB.read# = LA.read#(desc.BaseAdd + offt, N.BYTES)
LET CPL = saveCPL
END FUNCTION

SUB DTAB.write (err.code, desc AS DESCR, offt, value#, N.BYTES)
REM  Agarwal p81.
LET saveCPL = CPL
DIM cerr AS INTEGER
LET cerr = (err.code AND -4)
IF NOT desc.Valid OR NOT in.limits(desc, CDBL(offt), N.BYTES) THEN
    CALL signal.fault(GP, cerr)
    EXIT SUB
END IF

LET CPL = 0
CALL LA.write(desc.BaseAdd + CDBL(offt), value#, N.BYTES)
LET CPL = saveCPL
END SUB

SUB enter.new.task (sel, desc AS DESCR, nested.task%, err.flag%, err.code%)
REM p 134
IF LOCKS.OK THEN LOCK #2
LET dsel = (sel AND -8) + 5

d2# = DTAB.read#(sel, descrVec(GDT), dsel, TRUE, 1)

IF Bits#(d2#, 1, 1) = 0# THEN
    LET d2# = d2# + 2#
END IF
CALL DTAB.write(sel, descrVec(GDT), dsel, d2#, 1)
IF LOCKS.OK THEN UNLOCK #2

IF NOT task.switch.in.progress THEN
    LET task.switch.in.progress = TRUE
    CALL save.task.state(sel)
END IF

LET prev.TF = 0   '  Not global at this time!!!

LET descrVec(TSS) = desc
IF nested.task THEN
    CALL TSS.write(sel, 0#, CDBL(segReg(TSS)), 2)
END IF
IF descrVec(TSS).Typ = NOTBUSY.TSS16 THEN
    CALL read.TSS16(sel, descrVec(TSS), nested.task)
    LET trapAfterSwitch = FALSE
    IF err.flag% THEN
        CALL push(CDBL(err.code%), 2)
    END IF
ELSE
    LET trapAfterSwitch = read.TSS32(sel, descrVec(TSS), nested.task)
    IF err.flag% THEN
        CALL push(CDBL(err.code%), 4)
    END IF
END IF

IF ARCHNUM > 2 THEN
    LET dr7# = debReg#(7)
    LET dr78# = Bits#(dr7#, 7, 0)
    LET dr7# = (dr7# - dr78#) + CDBL(CINT(dr78#) AND (NOT (256 + 64 + 16 + 4 + 1)))
    LET debReg#(7) = dr7#
    IF trapAfterSwitch THEN
        IF Bits#(debReg#(6), 15, 15) = 0 THEN
            LET debReg#(6) = debReg#(6) + 2# ^ 15
        END IF
        CALL signal.fault(DEBUG, -1)
        EXIT SUB
    END IF
END IF
LET XH.flag = FALSE
END SUB

SUB exit.task
DIM desc AS DESCR
REM p142
LET sel = segReg(TSS)
LET NT = 0
CALL save.task.state(sel)
LET prev.TF = 0
DIM erc AS INTEGER

LET backlink = TSS.read#(segReg(TSS), 0#, 2)
IF LOCKS.OK THEN LOCK #2
CALL read.descr(backlink, desc, TRUE)
LET erc = (backlink AND -4)
IF desc.Typ = NOTBUSY.TSS16 OR desc.Typ = NOTBUSY.TSS32 THEN
    IF NOT desc.Valid THEN
        CALL signal.fault(NP, erc)
        EXIT SUB
    END IF
ELSE
    CALL signal.fault(task, erc)
    EXIT SUB
END IF
IF LOCKS.OK THEN UNLOCK #2


LET descrVec(TSS) = desc
IF desc.Typ = NOTBUSY.TSS16 THEN
    CALL read.TSS16(sel, descrVec(TSS), FALSE)
    LET trapAfterSwitch = FALSE
ELSE
    LET trapAfterSwitch = read.TSS32(sel, descrVec(TSS), FALSE)
END IF

IF ARCHNUM > 2 THEN
    LET d78 = Bits#(debReg#(7), 7, 0)
    LET d78n = d78 AND NOT (256 + 64 + 16 + 4 + 1) ' Let bits 8,6,4,2,0 be zero.
    LET debReg#(7) = debReg#(7) + CDBL(d78n - d78)
    IF trapAfterSwitch THEN
        IF Bits#(debReg#(6), 15, 15) = 0# THEN
                 LET debReg#(6) = debReg#(6) + 2# ^ 15
        END IF
        CALL signal.fault(DEBUG, -1)
    END IF
END IF

END SUB

FUNCTION fetch.code# (N.BYTES)
REM p83
REM Note N.BYTES negative implies 2's complement representation
REM
IF EIP# < -1# THEN       ' Command is from the command mode processor
    LET fetch.code# = Mod2N#(HexVal#(BYTE.SWAP$(MID$(STORE.COMMAND$, 2 * CINT(EIP# - STORE.ORIGIN#) + 1, 2 * ABS(N.BYTES)))), 8 * N.BYTES)
ELSEIF in.limits(descrVec(CS), EIP#, N.BYTES) THEN
    LET fetch.code# = MemRead#(LA.rdChk#(descrVec(CS).BaseAdd + EIP#), N.BYTES)
ELSE
    CALL signal.fault(GP, 0)
    EXIT FUNCTION
END IF
LET EIP# = EIP# + CDBL(ABS(N.BYTES))
END FUNCTION

FUNCTION in.limits (desc AS DESCR, Offst#, siz)
REM  p78
LET Size = ABS(siz)
IF desc.ExpandDown THEN
    IF desc.DefaultAttr THEN
        LET edLimit# = 2# ^ (32) - 1#
    ELSE
        LET edLimit# = CDBL(&HFFFF)
    END IF
    LET retval = ((Offst# > desc.Limit) AND (Offst# + Size - 1 <= edLimit#))
ELSE
    LET retval = (Offst# + Size - 1 <= desc.Limit)
END IF

LET in.limits = retval
END FUNCTION

FUNCTION inBounds (sel)
    LET TI.bit = (sel MOD 8) \ 4    ' 0 Global, 1 Local
    DIM desc AS DESCR
    LET desc = descrVec(8 + TI.bit)
    LET Offst# = CDBL(sel AND -8)
    LET inBounds = desc.Valid AND desc.Readable AND in.limits(desc, Offst#, 8)
END FUNCTION

SUB init.VM.desc
REM p 140
FOR I = 0 TO 5
    LET descrVec(I).Valid = TRUE
    LET descrVec(I).BaseAdd = segReg(I) * 16#
    LET descrVec(I).Limit = CDBL(&HFFFF)
    LET descrVec(I).DPL = 3
    LET descrVec(I).DefaultAttr = FALSE
    LET descrVec(I).Readable = TRUE
    LET descrVec(I).Writable = TRUE
    LET descrVec(I).Executable = FALSE
    LET descrVec(I).ExpandDown = FALSE
NEXT
    LET descrVec(CS).Executable = TRUE

END SUB

FUNCTION lg.read# (SREG.NUM, offset#, N.BYTES)
REM This function implements Agarwal's read1, read2, and read4,
REM     found on pp 73-74.
REM      N.BYTES is 1, 2, or 4.
DIM desc AS DESCR
LET desc = descrVec(SREG.NUM)
IF desc.Valid AND desc.Readable AND in.limits(desc, offset#, N.BYTES) THEN
    LET lg.read# = LA.read#(Mod2N#(desc.BaseAdd + offset#, 32), N.BYTES)
ELSEIF SREG.NUM = SS THEN
    CALL signal.fault(stack, 0)
ELSE
    CALL signal.fault(GP, 0)
END IF
END FUNCTION

SUB lg.write (descind, offset#, value#, N.BYTES)
REM descind is the same as the SEGREG number
REM This function implements Agarwal's write1, write2, and write4,
REM    found on pp79-80.
REM    N.BYTES is 1, 2, or 4.
DIM desc AS DESCR
LET desc = descrVec(descind)

IF desc.Valid AND desc.Writable AND in.limits(desc, offset#, N.BYTES) THEN
    LET wrvalue# = Mod2N#(value#, 32)  'Why is this MOD needed?
    CALL LA.write(Mod2N#(descrVec(descind).BaseAdd + offset#, 32), wrvalue#, N.BYTES)
ELSEIF descind = SS THEN
    CALL signal.fault(stack, 0)
ELSE
    CALL signal.fault(GP, 0)
END IF

END SUB

SUB load.CS (sel, desc AS DESCR, TSSload AS INTEGER)
REM     p69

DIM csel AS INTEGER
    LET csel = (sel AND -4)
    IF csel = 0 THEN     ' NULL selector.
        IF TSSload THEN
            CALL signal.fault(task, csel)
        ELSE
            CALL signal.fault(GP, csel)
        END IF
        EXIT SUB
    END IF
    IF LOCKS.OK THEN LOCK #2
    CALL read.descr(sel, desc, TSSload)
    IF NOT desc.CDSeg AND NOT TSSload THEN       ' See the comment at the top of page 70
            EXIT SUB
    ELSE
            CALL signal.fault(task, csel)
            EXIT SUB
    END IF
    IF (desc.Executable AND ((desc.Conforming AND (CPL >= desc.DPL)) OR (NOT desc.Conforming AND (CPL = desc.DPL) AND ((sel AND 3) <= desc.DPL)))) THEN
        IF NOT desc.Valid THEN
            CALL signal.fault(NP, csel)
            EXIT SUB
        END IF
    ELSE
        IF TSSload THEN
            CALL signal.fault(task, csel)
        ELSE
            CALL signal.fault(GP, csel)
        END IF
        EXIT SUB
    END IF

    CALL mark.accessed(sel)
    IF LOCKS.OK THEN UNLOCK #2

END SUB

SUB load.data.seg (sel, desc AS DESCR, TSSload AS INTEGER)
REM  desc is written into.
REM     p 71 Agarwal
DIM dsc AS DESCR
DIM csel AS INTEGER
DIM erc AS INTEGER
LET csel = (sel AND -4)
    IF csel = 0 THEN     ' This the definition of a NULL selector.
        LET desc.Valid = FALSE     ' See p 67.
        EXIT SUB
    END IF
    IF LOCKS.OK THEN LOCK #2
    CALL read.descr(sel, dsc, TSSload)

    IF (((dsc.Readable OR dsc.Writable) AND NOT dsc.Conforming AND (dsc.DPL >= CPL) AND (dsc.DPL >= (sel AND 3))) OR (dsc.Readable AND dsc.Conforming)) THEN
        IF NOT dsc.Valid THEN
            CALL signal.fault(NP, csel)
            EXIT SUB
        END IF
    ELSE
        IF TSSload THEN
            CALL signal.fault(task, csel)
        ELSE
            CALL signal.fault(GP, erc)
        END IF
        EXIT SUB
    END IF

    CALL mark.accessed(sel)
    IF LOCKS.OK THEN UNLOCK #2
    LET desc = dsc
END SUB

SUB load.gate.CS (gatedesc AS DESCR)
DIM desc AS DESCR
DIM csel AS INTEGER
    IF (gatedesc.Selectr AND -4) = 0 THEN
        CALL signal.fault(GP, 0)
        EXIT SUB
    END IF
    LET segReg(CS) = gatedesc.Selectr
    LET csel = (segReg(CS) AND -4)
    IF LOCKS.OK THEN LOCK #2
    CALL read.descr(segReg(CS), desc, FALSE)
    IF (desc.Executable AND ((desc.Conforming AND CPL >= desc.DPL) OR (NOT desc.Conforming AND CPL = desc.DPL))) THEN
        IF NOT desc.Valid THEN
            CALL signal.fault(NP, csel)
            EXIT SUB
        END IF
        IF gatedesc.Typ = CALLGATE16 THEN
            CALL push(CDBL(segReg(CS)), 2)
            CALL push(CDBL(EIP# AND &HFFFF), 2)
        ELSE
            CALL push(CDBL(segReg(CS)), 4)
            CALL push(EIP#, 4)
        END IF
    ELSE
        IF desc.Executable AND (NOT desc.Conforming AND CPL > desc.DPL) THEN
            IF NOT desc.Valid THEN
                CALL signal.fault(NP, csel)
                EXIT SUB
            END IF
            IF gatedesc.Typ = CALLGATE16 THEN
                CALL priv.lev.switch.CALL(16, desc, gatedesc.ParamCount)
            ELSE
                CALL priv.lev.switch.CALL(32, desc, gatedesc.ParamCount)
            END IF
        ELSE
            CALL signal.fault(GP, csel)
            EXIT SUB
        END IF
        LET sel = csel
        CALL mark.accessed(sel)
        IF LOCKS.OK THEN UNLOCK #2
        LET segReg(CS) = (segReg(CS) AND -4) + CPL
        IF gatedesc.Typ = CALLGATE32 THEN
            LET EIP# = gatedesc.Offst
        ELSE
            LET EIP# = Bits#(gatedesc.Offst, 15, 0)
        END IF
    END IF






END SUB

SUB load.LDT (sel, des AS DESCR)
REM   See p141
REM
DIM erc AS INTEGER
LET erc = (sel AND -4)
IF (sel AND 4) > 0 THEN
    CALL signal.fault(task, erc)
    EXIT SUB
END IF
IF (sel AND -4) = 0 THEN
   LET des.Valid = FALSE
ELSE
    IF LOCKS.OK THEN LOCK #2
    CALL read.descr(sel, des, TRUE)
    IF des.Typ <> LDT.SEG OR NOT des.Valid THEN
        CALL signal.fault(task, erc)
        EXIT SUB
    END IF
    IF LOCKS.OK THEN UNLOCK #2
END IF

END SUB

SUB load.protected.descr (OS)
REM p140

LET CPL = (segReg(CS) AND 3)

FOR I = 0 TO 6
    LET descrVec(I).Valid = FALSE
NEXT

    CALL load.LDT(segReg(LDT), descrVec(LDT))

    CALL load.SS(segReg(SS), descrVec(SS), TRUE)
    CALL load.CS(segReg(CS), descrVec(CS), TRUE)
    CALL load.data.seg(segReg(ES), descrVec(ES), TRUE)
    CALL load.data.seg(segReg(DS), descrVec(DS), TRUE)
    IF OS = 32 THEN
        CALL load.data.seg(segReg(FS), descrVec(FS), TRUE)
        CALL load.data.seg(segReg(GS), descrVec(GS), TRUE)
    END IF

END SUB

SUB load.SS (sel, desc AS DESCR, TSSload AS INTEGER)
REM     p 70 Agarwal
DIM csel AS INTEGER
LET csel = (sel AND -4)
    IF (csel = 0) OR ((sel AND 3) <> CPL) THEN ' NULL selector or RPL <> CPL
        IF TSSload THEN
            CALL signal.fault(task, csel)
        ELSE
            CALL signal.fault(GP, csel)
        END IF
        EXIT SUB
    END IF
    IF LOCKS.OK THEN LOCK #2
    CALL read.descr(sel, desc, TSSload)

    IF desc.Writable AND (desc.DPL = CPL) THEN
        IF NOT desc.Valid THEN
            CALL signal.fault(stack, csel)
            EXIT SUB
        END IF
    ELSE
            IF TSSload THEN
                CALL signal.fault(task, csel)
            ELSE
                CALL signal.fault(GP, csel)
            END IF
            EXIT SUB
    END IF

    CALL mark.accessed(sel)
    IF LOCKS.OK THEN UNLOCK #2

END SUB

SUB mark.accessed (sel)
REM p66 Agarwal
DIM tableDesc AS DESCR
IF (sel AND 4) = 0 THEN
    LET tableDesc = descrVec(GDT)
ELSE
    LET tableDesc = descrVec(LDT)
END IF
LET sel.Index = (sel AND -8) + 5
LET d2# = DTAB.read#(sel, tableDesc, sel.Index, FALSE, 1)
IF HALT.FLAG THEN EXIT SUB
IF Bits#(2#, 0, 0) = 0# THEN
    LET d2# = d2# + 1#
END IF
CALL DTAB.write(sel, tableDesc, sel.Index, d2#, 1)
END SUB

FUNCTION mode
IF Bits#(ctrlReg#(0), 0, 0) = 0 OR ARCHNUM < 2 THEN   ' Check PE bit
    LET mode = REAL                 '086 and 186 are always in REAL mode
ELSEIF VM = 0 OR ARCHNUM = 2 THEN
    LET mode = PROTECTED            '286 has no virtual 86 mode
ELSE
    LET mode = VM86
END IF

END FUNCTION

FUNCTION new.stk.push# (OS, newSS, newdesc AS DESCR, newESP#, value#)
REM p126
LET N.BYTES = OS \ 8
DIM erc AS INTEGER
LET saveCPL = CPL
LET stack.ptr# = newESP# - CDBL(N.BYTES)
IF newdesc.DefaultAttr = 0 THEN
    LET stack.ptr# = Bits#(stack.ptr#, 15, 0)
END IF
IF newdesc.Valid AND newdesc.Writable AND in.limits(newdesc, newESP#, N.BYTES) THEN
    LET CPL = 0
    CALL LA.write(newdesc.BaseAdd + stack.ptr#, value#, N.BYTES)
    LET CPL = saveCPL
ELSE
    LET erc = newSS
    CALL signal.fault(stack, erc)
    EXIT FUNCTION
END IF
LET new.stk.push# = stack.ptr#
END FUNCTION

SUB priv.lev.switch.CALL (OS, newCSdesc AS DESCR, parmcount%)
REM p124
DIM newSSdesc AS DESCR
LET N.BYTES = OS \ 8
LET newCPL = newCSdesc.DPL
IF descrVec(TSS).Typ = TSS16 THEN
    LET newESP# = TSS.read#(segReg(TSS), CDBL(newCPL * 4 + 2), 2)
    LET newSS = TSS.read#(segReg(TSS), CDBL(newCPL * 4 + 4), 2)
    
ELSE
    LET newESP# = TSS.read#(segReg(TSS), CDBL(newCPL * 8 + 4), 4)
    LET newSS = TSS.read#(segReg(TSS), CDBL(newCPL * 8 + 8), 4)
END IF
LET saveCPL = CPL
LET CPL = newCPL
CALL load.SS(newSS, newSSdesc, TRUE)
LET CPL = saveCPL
IF mode = VM86 THEN
    LET newESP# = new.stk.push#(OS, newSS, newSSdesc, newESP#, CDBL(segReg(GS)))
    LET newESP# = new.stk.push#(OS, newSS, newSSdesc, newESP#, CDBL(segReg(FS)))
    LET newESP# = new.stk.push#(OS, newSS, newSSdesc, newESP#, CDBL(segReg(DS)))
    LET newESP# = new.stk.push#(OS, newSS, newSSdesc, newESP#, CDBL(segReg(ES)))
    LET segReg(DS) = 0
    LET segReg(ES) = 0
    LET segReg(FS) = 0
    LET segReg(GS) = 0
    LET descrVec(DS).Valid = FALSE
    LET descrVec(ES).Valid = FALSE
    LET descrVec(FS).Valid = FALSE
    LET descrVec(GS).Valid = FALSE
END IF
LET newESP# = new.stk.push#(OS, newSS, newSSdesc, newESP#, CDBL(segReg(SS)))
LET newESP# = new.stk.push#(OS, newSS, newSSdesc, newESP#, REG#(ESP))
CALL REG.STORE(ESP, REG#(ESP) + CDBL(parmcount% * N.BYTES))
IF descrVec(SS).DefaultAttr THEN
    LET stack.ptr# = REG#(ESP) + parmcount% * N.BYTES
ELSE
    LET stack.ptr# = REG#(SP) + parmcount% * N.BYTES
END IF

FOR I = 1 TO parmcount%
    LET stack.ptr# = stack.ptr# - CDBL(N.BYTES)
    
    LET tmp# = lg.read#(SS, stack.ptr#, N.BYTES)
    new.ESP# = new.stk.push#(OS, newSS, newSSdesc, newESP#, tmp#)
    
NEXT
LET newESP# = new.stk.push#(OS, newSS, newSSdesc, newESP#, CDBL(segReg(CS)))
LET newESP# = new.stk.push#(OS, newSS, newSSdesc, newESP#, EIP#)
LET VM = 0
CALL REG.STORE(SS, CDBL(newSS))
LET descrVec(SS) = newSSdesc
IF descrVec(SS).DefaultAttr THEN
    CALL REG.STORE(ESP, newESP#)
ELSE
    CALL REG.STORE(SP, newESP#)
END IF
LET CPL = newCPL
END SUB

SUB read.descr (sel, desc AS DESCR, TSSload AS INTEGER)
REM     p63
REM     Code for Special segment descriptors is wrong
REM
DIM tableDesc AS DESCR
DIM dT AS INTEGER
LET erc = (sel AND -4)
IF (sel AND 4) = 0 THEN
    LET tableDesc = descrVec(GDT)
ELSE
    LET tableDesc = descrVec(LDT)
END IF

LET sel.Index = (sel AND -8)

LET d1# = DTAB.read#(erc, tableDesc, sel.Index, TSSload, 4)
LET sel.Index = sel.Index + 4
LET d2# = DTAB.read#(erc, tableDesc, sel.Index, TSSload, 4)

LET desc.Valid = -Bits#(d2#, 15, 15)
LET desc.DPL = Bits#(d2#, 14, 13)
LET desc.CDSeg = -Bits#(d2#, 12, 12)
LET SpecialSegFlag = NOT (desc.CDSeg OR -Bits#(d2#, 10, 10))

IF desc.CDSeg OR SpecialSegFlag THEN
    desc.BaseAdd = Bits#(d2#, 31, 24) * 2# ^ 24 + Bits#(d2#, 7, 0) * 2# ^ 16 + Bits#(d1#, 31, 16)
    LET desc.Limit = Bits#(d2#, 19, 16) * 2# ^ 16 + Bits#(d1#, 15, 0)
    IF Bits#(d2#, 23, 23) = 1 THEN   'Test the granularity bit
        LET desc.Limit = desc.Limit * 2# ^ 12 + CDBL(&HFFF)
    END IF
  
    IF desc.CDSeg THEN
        LET desc.DefaultAttr = -Bits#(d2#, 22, 22)
        LET desc.Executable = -Bits#(d2#, 11, 11)
        IF desc.Executable THEN
            LET desc.Conforming = -Bits#(d2#, 10, 10)
            LET desc.Readable = -Bits#(d2#, 9, 9)
            LET desc.Writable = FALSE
            LET desc.ExpandDown = FALSE
        ELSE
            LET desc.ExpandDown = -Bits#(d2#, 10, 10)
            LET desc.Writable = -Bits#(d2#, 9, 9)
            LET desc.Readable = TRUE
            LET desc.Conforming = FALSE
        END IF
        LET desc.Typ = 0
    ELSE
    REM IF LDT.SEG OR NOTBUSY.TSS16 OR NOTBUSY.TSS32 OR BUSY.TSS32 THEN
        LET desc.Executable = FALSE        ' Just guessing here
        LET desc.Conforming = FALSE        ' Another guess
        LET desc.Typ = Bits#(d2#, 11, 8)
        LET desc.Readable = TRUE
        LET desc.Writable = TRUE
    END IF
ELSE
    LET desc.Typ = Bits#(d2#, 11, 8)
    LET desc.Selectr = Bits#(d1#, 31, 16)
    LET desc.Offst = Bits#(d2#, 31, 16) * 2# ^ 16 + Bits#(d1#, 15, 0)
    LET desc.Executable = FALSE
    LET desc.Readable = FALSE
    LET desc.Writable = FALSE
    LET desc.ParamCount = Bits#(d2#, 4, 0)
END IF

LET desc.Accessed = Bits#(d2#, 8, 8)
LET desc.GDField = Bits#(d2#, 23, 20)

END SUB

SUB read.TSS16 (sel, desc AS DESCR, nested.task)
REM p 137
DIM oldTSSDescTable AS DESCR
IF segReg(TSS) AND 4 THEN
    LET oldTSSDescTable = descrVec(LDT)
ELSE
    LET oldTSSDescTable = descrVec(GDT)
END IF
DIM csel AS INTEGER
LET csel = sel AND -4
IF (descrVec(TSS).Limit < 43) THEN
    CALL signal.fault(task, csel)
    EXIT SUB
END IF
LET EIP# = TSS.read#(sel, 14#, 2)
CALL FLAG.STORE(TSS.read#(sel, 16#, 2), 2)
CALL REG.STORE(AX, TSS.read#(sel, 18#, 2))
CALL REG.STORE(CX, TSS.read#(sel, 20#, 2))
CALL REG.STORE(DX, TSS.read#(sel, 22#, 2))
CALL REG.STORE(BX, TSS.read#(sel, 24#, 2))
CALL REG.STORE(SP, TSS.read#(sel, 26#, 2))
CALL REG.STORE(BP, TSS.read#(sel, 28#, 2))
CALL REG.STORE(SI, TSS.read#(sel, 30#, 2))
CALL REG.STORE(DI, TSS.read#(sel, 32#, 2))
LET segReg(ES) = TSS.read#(sel, 34#, 2)
LET segReg(CS) = TSS.read#(sel, 36#, 2)
LET segReg(SS) = TSS.read#(sel, 38#, 2)
LET segReg(DS) = TSS.read#(sel, 40#, 2)
LET segReg(LDT) = TSS.read#(sel, 42#, 2)
LET segReg(FS) = 0
LET segReg(GS) = 0

IF nested.task THEN LET NT = 1

REM LET prev = curr      ! Not implemented yet!!

IF NOT nested.task THEN
    IF LOCKS.OK THEN LOCK #2
    LET seltor = segReg(TSS)
    LET indel = (seltor AND -8) + 5
    LET temp = DTAB.read#(seltor, oldTSSDescTable, indel, TRUE, 1)
    LET temp = (temp AND -3)
    LET dsel = (sel AND -8) + 5
    CALL DTAB.write(seltor, oldTSSDescTable, dsel, CDBL(temp), 1)
    IF LOCKS.OK THEN UNLOCK #2
END IF

LET segReg(TSS) = sel
IF Bits#(ctrlReg#(0), 3, 3) = 0 THEN        ' LET TS = 1
    LET ctrlReg#(0) = ctrlReg#(0) + 8#
END IF
LET task.switch.in.progress = FALSE
CALL load.protected.descr(16)


END SUB

FUNCTION read.TSS32 (sel, desc AS DESCR, nested.task)
REM p 138
DIM oldTSSDescTable AS DESCR
IF segReg(TSS) AND 4 THEN
    LET oldTSSDescTable = descrVec(LDT)
ELSE
    LET oldTSSDescTable = descrVec(GDT)
END IF
DIM csel AS INTEGER
LET csel = (sel AND -4)
IF (descrVec(TSS).Limit < 100#) THEN
    CALL signal.fault(task, csel)
    EXIT FUNCTION
END IF
LET EIP# = TSS.read#(sel, 32#, 4)
CALL FLAG.STORE(TSS.read#(sel, 36#, 4), 4)
CALL REG.STORE(EAX, TSS.read#(sel, 40#, 4))
CALL REG.STORE(ECX, TSS.read#(sel, 44#, 4))
CALL REG.STORE(EDX, TSS.read#(sel, 48#, 4))
CALL REG.STORE(EBX, TSS.read#(sel, 52#, 4))
CALL REG.STORE(ESP, TSS.read#(sel, 56#, 4))
CALL REG.STORE(EBP, TSS.read#(sel, 60#, 4))
CALL REG.STORE(ESI, TSS.read#(sel, 64#, 4))
CALL REG.STORE(EDI, TSS.read#(sel, 68#, 4))
LET segReg(ES) = TSS.read#(sel, 72#, 2)
LET segReg(CS) = TSS.read#(sel, 76#, 2)
LET segReg(SS) = TSS.read#(sel, 80#, 2)
LET segReg(DS) = TSS.read#(sel, 84#, 2)
LET segReg(FS) = TSS.read#(sel, 88#, 2)
LET segReg(GS) = TSS.read#(sel, 92#, 2)
LET segReg(LDT) = TSS.read#(sel, 96#, 2)
LET trapByte = TSS.read#(sel, 100#, 1)

IF nested.task THEN LET NT = 1

REM LET prev = curr      ! Not implemented yet!!

IF NOT nested.task THEN
    IF LOCKS.OK THEN LOCK #2
    LET indel = (segReg(TSS) AND -8) + 5
    LET temp = DTAB.read#(segReg(TSS), oldTSSDescTable, indel, TRUE, 1)
    LET temp = (temp AND &HFD)
    LET dsel = (sel AND -8) + 5
    CALL DTAB.write(segReg(TSS), oldTSSDescTable, dsel, CDBL(temp), 1)
    IF LOCKS.OK THEN UNLOCK #2
END IF

LET PG = Bits#(ctrlReg#(0), 31, 31)
IF PG THEN
    LET ctrlReg#(3) = TSS.read#(sel, 28#, 4)
END IF

LET segReg(TSS) = sel
REM LET TS = 1
IF Bits#(ctrlReg#(0), 3, 3) = 0 THEN
    LET ctrlReg#(0) = ctrlReg#(0) + 8#
END IF

LET task.switch.in.progress = FALSE

IF VM THEN
    CALL init.VM.desc
    LET CPL = 3
    LET descrVec(LDT).Valid = FALSE
    CALL load.LDT(segReg(LDT), descrVec(LDT))
ELSE
    CALL load.protected.descr(32)
END IF

LET read.TSS32 = ((trapByte AND 1) = 1)

END FUNCTION

SUB save.task.state (sel)
REM p 136
REM Make sel an input parameter?  Documentation is missing here.
REM  Comments on p135 notwithstanding.
REM
IF descrVec(TSS).Typ = NOTBUSY.TSS32 THEN
    CALL TSS.write(sel, 32#, EIP#, 4)
    CALL TSS.write(sel, 36#, FLAGS#(4), 4)
    CALL TSS.write(sel, 40#, REG#(EAX), 4)
    CALL TSS.write(sel, 44#, REG#(ECX), 4)
    CALL TSS.write(sel, 48#, REG#(EDX), 4)
    CALL TSS.write(sel, 52#, REG#(EBX), 4)
    CALL TSS.write(sel, 56#, REG#(ESP), 4)
    CALL TSS.write(sel, 60#, REG#(EBP), 4)
    CALL TSS.write(sel, 64#, REG#(ESI), 4)
    CALL TSS.write(sel, 68#, REG#(EDI), 4)
    CALL TSS.write(sel, 72#, CDBL(segReg(ES)), 4)
    CALL TSS.write(sel, 76#, CDBL(segReg(CS)), 4)
    CALL TSS.write(sel, 80#, CDBL(segReg(SS)), 4)
    CALL TSS.write(sel, 84#, CDBL(segReg(DS)), 4)
    CALL TSS.write(sel, 88#, CDBL(segReg(FS)), 4)
    CALL TSS.write(sel, 92#, CDBL(segReg(GS)), 4)
END IF

END SUB

FUNCTION TSS.read# (err.code, offset#, N.BYTES)
REM pp 75-76
LET saveCPL = CPL
DIM cerr AS INTEGER
LET cerr = (err.code AND NOT 3)

IF descrVec(TSS).Valid AND in.limits(descrVec(TSS), offset#, N.BYTES) THEN
    LET CPL = 0
    LET result# = LA.read#(descrVec(TSS).BaseAdd + offset#, N.BYTES)
    LET CPL = saveCPL
ELSE
    CALL signal.fault(task, cerr)
    EXIT FUNCTION
END IF

LET TSS.read# = result#

END FUNCTION

SUB TSS.write (err.code, offset#, value#, N.BYTES)
REM pp 81,82
LET saveCPL = CPL
DIM erc AS INTEGER

IF descrVec(TSS).Valid AND in.limits(descrVec(TSS), offset#, N.BYTES) THEN
    LET CPL = 0
    CALL LA.write(descrVec(TSS).BaseAdd + offset#, value#, N.BYTES)
    LET CPL = saveCPL
ELSE
    LET erc = error.code
    CALL signal.fault(task, erc)
END IF

END SUB

