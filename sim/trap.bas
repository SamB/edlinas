REM $INCLUDE: 'x86.bi'
DECLARE SUB init.VM.desc ()
DECLARE SUB load.CS (sel!, desc AS DESCR, FLAG%)
DECLARE SUB load.SS (sel!, desc AS DESCR, FLAG%)
DECLARE SUB read.descr (sel!, desc AS DESCR, FLAG%)
DECLARE SUB mark.accessed (sel!)
DECLARE SUB exit.task ()
DECLARE SUB REG.STORE (REG.NUM!, NUM#)
DECLARE SUB FLAG.STORE (value#, N.BYTES!)
DECLARE FUNCTION G! (N!)
DECLARE FUNCTION L! (N!)
DECLARE FUNCTION RW! (N!)
DECLARE FUNCTION LN! (N!)
DECLARE SUB signal.fault (xcptype%, error.code%)
DECLARE SUB push (value#, N.BYTES!)
DECLARE FUNCTION pop# (N.BYTES)
DECLARE FUNCTION RandomBytes# (N.BYTES)
DECLARE FUNCTION IDT.read# (vecnum%, offt%, N.BYTES!)
DECLARE FUNCTION TSS.read# (sel, offset#, N.BYTES!)
DECLARE SUB clear.active.brkpts ()
DECLARE FUNCTION double.fault! ()
DECLARE SUB signal.abort (xcptype%, error.code AS INTEGER)
DECLARE SUB signal.trap (xcptype%, err.code AS INTEGER)
DECLARE SUB report.brkpts ()
DECLARE SUB check.brkpt (btype%, X#)
DECLARE SUB handle.intr.xcp (vecnum%, err.flag%, err.code AS INTEGER)
DECLARE SUB int.xcp.gate (OS!, d1#, d2#, oldEFLAGS#, err.flag%, err.cod%)
DECLARE SUB int.via.task.gate (sel!, err.flag%, err.code%)
DECLARE SUB enter.new.task (sel!, desc AS DESCR, nested.task%, err.flag%, err.cod%)
DECLARE FUNCTION new.stk.push# (OS!, newSS!, newdesc AS DESCR, newESP#, value#)
DECLARE FUNCTION brkpt.detected% ()
DECLARE FUNCTION in.limits (desc AS DESCR, X#, siz)
DECLARE FUNCTION external.interrupt! ()
DECLARE FUNCTION UserInput# (N!)
DECLARE SUB Print24 (X$)

FUNCTION brkpt.detected%
REM p 167
LET X% = FALSE
FOR K = 0 TO 3
    LET X% = X% OR brkpt.active%(K)
NEXT
LET brkpt.detected% = X%
END FUNCTION

SUB check.brkpt (btype AS INTEGER, X#)
REM p175
FOR N = 0 TO 3
IF (G(N) OR L(N)) AND NOT brkpt.active%(N) THEN
    IF RW(N) = 3 THEN
        LET brkpt.active%(N) = ((btype = 1 OR btype = 3) AND (debReg#(N) <= X#) AND (X# <= debReg#(N) + LN(N)))
    ELSE
        LET brkpt.active%(N) = ((btype = RW(N)) AND (debReg#(N) <= X#) AND (X# <= debReg#(N) + LN(N)))
    END IF
END IF
NEXT

END SUB

SUB check.interrupts
REM p151
IF NOT prev.instr.loaded.SS THEN
    IF NMI.pin.active AND NOT NMI.handler.active THEN
       LET NMI.handler.active = TRUE
       LET XH.flag = TRUE
       CALL handle.intr.xcp(NMI, FALSE, -1)
    ELSEIF INTR.pin.active THEN
        LET INTR.pin.active = FALSE
        IF IE = 1 THEN
            LET XH.flag = TRUE
            CALL Print24(" Enter interrupt number:")
            LET INTERRUPT.NUMBER = UserInput#(1)
            CALL handle.intr.xcp(CINT(INTERRUPT.NUMBER), FALSE, -1)
        ELSE
            CALL Print24(" Sorry. IE not set")
            REM LET INTR.pin.active = FALSE    ????
        END IF
    END IF
END IF
END SUB

SUB checkpoint
REM p 168
REM let prev = curr
LET prev.EIP# = EIP#
LET prev.CPL = CPL
LET exception.count = 0
LET first.page.fault.seen = FALSE
END SUB

SUB clear.active.brkpts
REM p 176
FOR N = 0 TO 3
    LET brkpt.active%(N) = FALSE
NEXT
END SUB

FUNCTION double.fault
REM See p 153
    LET exception.count = exception.count + 1
    IF exception.count = 2 THEN
        LET double.fault = TRUE
    ELSEIF exception.count >= 3 THEN
        LET double.fault = TRUE
        REM CALL shutdown  (Next two lines)
        LET HALT.FLAG = TRUE
        LET INTR.pin.active = FALSE 'Avoid making the main loop go infinite.
    ELSE
        LET double.fault = FALSE
    END IF
END FUNCTION

FUNCTION external.interrupt
REM p 166
LET X% = NOT prev.instr.loaded.SS
LET Y% = (NOT NMI.handler.active AND NMI.pin.active)
LET Y% = (Y% OR (IE = 1 AND INTERRUPT.RECEIVED))
IF X% AND Y% THEN
    LET RF = 1
    LET external.interrupt = TRUE
ELSE
    LET external.interrupt = FALSE
END IF

END FUNCTION

FUNCTION G (N)
REM p 173
LET G = -Bits#(debReg#(7), 2 * N + 1, 2 * N + 1)
END FUNCTION

SUB handle.intr.xcp (vecnum AS INTEGER, err.flag%, err.code AS INTEGER)
REM p160
REM err.code = -1 means there is no error code.
REM
DIM erc AS INTEGER
IF LOCKS.OK THEN UNLOCK #2
IF mode = REAL THEN
    CALL push(FLAGS#(2), 2)
    IF HALT.FLAG THEN EXIT SUB
    CALL push(CDBL(segReg(CS)), 2)
    IF HALT.FLAG THEN EXIT SUB
    LET IP# = Bits#(EIP#, 15, 0)
    CALL push(IP#, 2)
    IF HALT.FLAG THEN EXIT SUB
    LET IE = 0
    LET TF = 0
    LET prev.TF = 0
    LET EIP# = IDT.read#(vecnum, vecnum * 4, 2)
    IF HALT.FLAG THEN EXIT SUB
    LET segReg(CS) = IDT.read#(vecnum, vecnum * 4 + 2, 2)
    IF HALT.FLAG THEN EXIT SUB
    IF LOCKS.OK THEN UNLOCK #2
    REM The "longjump goto" will have to be taken care of
    REM using ordinary exits.
    LET INTERRUPT.RECEIVED = TRUE
    LET INTERRUPT.NUMBER = vecnum
ELSE
    
    IF LOCKS.OK THEN LOCK #2
    xd1# = IDT.read#(vecnum, vecnum * 8, 4)
    IF HALT.FLAG THEN EXIT SUB
    d2# = IDT.read#(vecnum, vecnum * 8 + 4, 4)
    IF HALT.FLAG THEN EXIT SUB
    IF LOCKS.OK THEN UNLOCK #2
    IF NOT XH.flag AND CPL > Bits#(d2#, 14, 13) THEN
        LET erc = vecnum * 8 + 2
        CALL signal.fault(GP, erc)
    END IF
    LET xcpEFLAGS# = FLAGS#(4)

    LET Typefield = Bits#(d2#, 11, 8)
    LET erc = vecnum * 8 + 2
    SELECT CASE Typefield
        CASE TRAPGATE16, INTERRUPTGATE16
            IF Typefield = INTERRUPTGATE16 THEN
                LET IE = 0
            END IF
            IF Bits#(d2#, 15, 15) = 0 THEN
                CALL signal.fault(NP, erc)
            ELSE
                CALL int.xcp.gate(16, d1#, d2#, xcpEFLAGS#, err.flag%, err.code)
            END IF
        CASE TRAPGATE32, INTERRUPTGATE32
            IF Typefield = INTERRUPTGATE32 THEN
                LET IE = 0
            END IF
            IF Bits#(d2#, 15, 15) = 0 THEN
                CALL signal.fault(NP, erc)
            ELSE
                CALL int.xcp.gate(32, d1#, d2#, xcpEFLAGS#, err.flag%, err.code)
            END IF
        CASE TASKGATE
            IF Bits#(d2#, 15, 15) = 0 THEN
                CALL signal.fault(NP, erc)
            END IF
            LET sel = Bits#(d1#, 31, 16)
            CALL int.via.task.gate(sel, err.flag%, err.code)
        CASE ELSE
            CALL signal.fault(GP, erc)
    END SELECT
END IF
END SUB

FUNCTION IDT.read# (vecnum AS INTEGER, offt AS INTEGER, N.BYTES)
REM p77
DIM err.code AS INTEGER
LET saveCPL = CPL
IF in.limits(descrVec(IDT), CDBL(offt), N.BYTES) THEN
    LET CPL = 0
    LET IDT.read# = LA.read#(descrVec(IDT).BaseAdd + offt, N.BYTES)
    LET CPL = saveCPL
ELSE
    LET err.code = vecnum * 8 + 2
    CALL signal.fault(GP, err.code)
END IF
END FUNCTION

SUB int.via.task.gate (sel, err.flag AS INTEGER, err.code AS INTEGER)
REM p133
DIM csel AS INTEGER
LET csel = sel AND -4
LET sel.TI = (sel AND 4) \ 4
IF csel = 0 OR sel.TI = 1 THEN
    CALL signal.fault(GP, csel)
    EXIT SUB
END IF
DIM desc AS DESCR
CALL read.descr(sel, desc, FALSE)
IF desc.Typ <> NOTBUSY.TSS16 AND desc.Typ <> NOTBUSY.TSS32 THEN
    CALL signal.fault(GP, csel)
    EXIT SUB
END IF
IF NOT desc.Valid THEN
    CALL signal.fault(NP, csel)
    EXIT SUB
END IF
LET newsel = desc.Selectr
CALL enter.new.task(newsel, desc, TRUE, err.flag, err.code)

END SUB

SUB int.xcp.gate (OS, d1#, d2#, xcpEFLAGS#, err.flag AS INTEGER, err.code%)
REM p162
REM oldEFLAGS is unused in the text.
REM It is presumably the same as xcpEFLAGS
DIM csel AS INTEGER
    LET IDT.sel = Bits#(d1#, 31, 16)
    LET IDT.offset# = Bits#(d2#, 31, 16) * 2# ^ 16 + Bits#(d1#, 15, 0)

    LET TF = 0        ' These two setting appear to make the later
    LET prev.TF = 0   ' ones redundant.
    LET NT = 0
    LET csel = (IDT.sel AND -4)
    IF csel = 0 THEN
        CALL signal.fault(GP, csel)
        EXIT SUB
    END IF
    IF LOCKS.OK THEN LOCK #2
    DIM desc AS DESCR
    DIM newSSdesc AS DESCR
    
    CALL read.descr(IDT.sel, desc, FALSE)
    IF NOT desc.Executable THEN
        IF NOT desc.Valid THEN
            CALL signal.fault(NP, csel)
            EXIT SUB
        END IF
        IF mode <> VM86 AND ((desc.Conforming AND CPL >= desc.DPL) OR (NOT desc.Conforming AND CPL = desc.DPL)) THEN
            CALL mark.accessed(IDT.sel)
            IF LOCKS.OK THEN UNLOCK #2
REM
REM     SAME_LEVEL_INTR:
REM
            LET newCS = csel + CPL
            LET RF = 0
            LET N.BYTES = OS \ 8
            CALL push(xcpEFLAGS#, N.BYTES)
            IF HALT.FLAG THEN EXIT SUB
            CALL push(CDBL(segReg(CS)), N.BYTES)
            IF HALT.FLAG THEN EXIT SUB
            CALL push(EIP#, N.BYTES)
            IF HALT.FLAG THEN EXIT SUB
            IF err.flag THEN
                CALL push(CDBL(err.code), N.BYTES)
            END IF
            LET XH.flag = FALSE
            LET segReg(CS) = newCS
            LET EIP# = IDT.offset#
            LET INTERRUPT.RECEIVED = TRUE
            LET INTERRUPT.NUMBER = xcptype
REM
REM end SAME_LEVEL_INTR
REM

        ELSEIF (desc.Executable AND NOT desc.Conforming AND ((mode = VM86 AND desc.DPL = 0) OR (mode <> VM86 AND desc.DPL < CPL))) THEN
            CALL mark.accessed(IDT.sel)
            IF LOCKS.OK THEN UNLOCK #2
REM
REM     INTER_LEVEL_INTR:
REM
            LET newCPL = desc.DPL
            LET newCS = csel + newCPL
            IF descrVec(TSS).Typ = TSS16 THEN
                CALL REG.STORE(ESP, RandomBytes#(4))
                CALL REG.STORE(SS, TSS.read#(segReg(TSS), CDBL(newCPL * 4 + 2), 2))
                LET new.SS = TSS.read#(segReg(TSS), CDBL(newCPL * 4 + 4), 2)
            ELSEIF descrVec(TSS).Typ = TSS32 THEN
                LET new.ESP# = TSS.read#(segReg(TSS), CDBL(newCPL * 8 + 4), 4)
                LET new.SS = TSS.read#(segReg(TSS), CDBL(newCPL * 8 + 8), 2)
            END IF

            LET saveCPL = CPL
            LET CPL = newCPL
            CALL load.SS(new.SS, newSSdesc, TRUE)
            LET CPL = saveCPL
            IF mode = VM86 THEN
                LET new.ESP# = new.stk.push#(OS, new.SS, newSSdesc, new.ESP#, CDBL(segReg(GS)))
                LET new.ESP# = new.stk.push#(OS, new.SS, newSSdesc, new.ESP#, CDBL(segReg(FS)))
                LET new.ESP# = new.stk.push#(OS, new.SS, newSSdesc, new.ESP#, CDBL(segReg(DS)))
                LET new.ESP# = new.stk.push#(OS, new.SS, newSSdesc, new.ESP#, CDBL(segReg(ES)))
                LET segReg(GS) = 0
                LET segReg(FS) = 0
                LET segReg(DS) = 0
                LET segReg(ES) = 0
                LET descrVec(GS).Valid = FALSE
                LET descrVec(FS).Valid = FALSE
                LET descrVec(DS).Valid = FALSE
                LET descrVec(ES).Valid = FALSE
            END IF

            LET new.ESP# = new.stk.push#(OS, new.SS, newSSdesc, new.ESP#, CDBL(segReg(SS)))
            LET new.ESP# = new.stk.push#(OS, new.SS, newSSdesc, new.ESP#, REG#(ESP))
            LET new.ESP# = new.stk.push#(OS, new.SS, newSSdesc, new.ESP#, CDBL(segReg(CS)))
            LET new.ESP# = new.stk.push#(OS, new.SS, newSSdesc, new.ESP#, EIP#)
            IF err.flag THEN
                LET new.ESP# = new.stk.push#(OS, new.SS, newSSdesc, new.ESP#, CDBL(err.code))
            END IF
            LET RF = 0
            LET VM = 0
            LET segReg(SS) = newSS
            LET descrVec(SS) = newSSdesc
            IF descrVec(SS).DefaultAttr THEN
                CALL REG.STORE(ESP, new.ESP#)
            ELSE
                CALL REG.STORE(SP, Bits#(new.ESP#, 15, 0))
            END IF
            LET CPL = newCPL
            LET segReg(CS) = newCS
            LET EIP# = IDT.offset#
            LET XH.flag = FALSE
            LET INTERRUPT.RECEIVED = TRUE
            LET INTERRUPT.NUMBER = xcptype
        ELSE
            CALL signal.fault(GP, csel)
        END IF
    END IF

END SUB

SUB Iret (OS)
REM p447
LET N.BYTES = OS \ 8
DIM csel AS INTEGER
IF mode = PROTECTED THEN
    LET NMI.handler.active = FALSE
    IF NT THEN
        CALL exit.task
    ELSE
        LET EIP# = pop#(N.BYTES)
        LET segReg(CS) = Bits#(pop#(N.BYTES), 15, 0)
        LET flg# = pop#(N.BYTES)
        CALL FLAG.STORE(flg#, N.BYTES)
    END IF
    IF VM THEN
        LET tmpESP# = pop#(4)
        LET segReg(SS) = Bits#(pop#(4), 15, 0)
        LET segReg(ES) = Bits#(pop#(4), 15, 0)
        LET segReg(DS) = Bits#(pop#(4), 15, 0)
        LET segReg(FS) = Bits#(pop#(4), 15, 0)
        LET segReg(GS) = Bits#(pop#(4), 15, 0)
        CALL REG.STORE(ESP, tmpESP#)
        LET CPL = 3
        CALL init.VM.desc
    ELSE
        IF (segReg(CS) AND -4) = 0 OR CPL > (segReg(CS) AND 3) THEN
            LET csel = (segReg(CS) AND NOT 3)
            CALL signal.fault(GP, csel)
            EXIT SUB
        ELSEIF CPL = (segReg(CS) AND 3) THEN
            CALL load.CS(segReg(CS), descrVec(CS), FALSE)
        ELSE
            LET CPL = (segReg(CS) AND 3)
            CALL load.CS(segReg(CS), descrVec(CS), FALSE)
            LET callerESP# = pop#(N.BYTES)
            LET segReg(SS) = (pop#(N.BYTES) AND &HFFFF)
            CALL load.SS(segReg(SS), descrVec(SS), FALSE)
            IF (descrVec(SS).DefaultAttr) THEN
                CALL REG.STORE(ESP, callerESP#)
            ELSE
                CALL REG.STORE(ESP, (callerESP# AND &HFFFF))
            END IF
            IF descrVec(ES).DPL < CPL THEN
                LET descrVec(ES).Valid = FALSE
            END IF
            IF descrVec(DS).DPL < CPL THEN
                LET descrVec(DS).Valid = FALSE
            END IF
            IF descrVec(FS).DPL < CPL THEN
                LET descrVec(FS).Valid = FALSE
            END IF
            IF descrVec(GS).DPL < CPL THEN
                LET descrVec(GS).Valid = FALSE
            END IF
        END IF

    END IF
ELSEIF mode = VM86 AND CPL > IOPL THEN 'Put in real mode
    CALL signal.fault(GP, 0)
    EXIT SUB
ELSE
    LET NMI.handler.active = FALSE
    LET EIP# = pop#(N.BYTES)
    LET segReg(CS) = Bits#(pop#(N.BYTES), 15, 0)
    LET flg# = pop#(N.BYTES)
    IF CPL > 0 THEN
        LET X# = Bits#(flg#, 13, 12)
        LET Y# = IOPL * 2# ^ 12
        LET flg# = flg# - X# + Y#
    END IF
    CALL FLAG.STORE(flg#, N.BYTES)

END IF

END SUB

FUNCTION L (N)
REM p 173
LET L = -Bits#(debReg#(7), 2 * N, 2 * N)
END FUNCTION

FUNCTION LN (N)
REM p 173
LET LN = Bits#(debReg#(7), 4 * N + 19, 4 * N + 18)

END FUNCTION

SUB report.brkpts
REM p175
LET linEIP# = descrVec(CS).BaseAdd + EIP#
DIM B(4) AS INTEGER    ' Use B(4) for BS.  Also use +1 not -1
FOR N = 0 TO 3
    REM X uses -1 not +1
    LET X = -(1 - RF) AND (G(N) OR L(N)) AND (RW(N) = 0) AND (LN(N) = 1)
    LET X = X AND (debReg#(N) = linEIP#)
    LET B(N) = -(brkpt.active%(N) OR X)
NEXT
CALL clear.active.brkpts
LET RF = 0
IF prev.TF THEN 'LET BS = 1
    LET B(4) = 1
END IF
LET Y# = Bits#(debReg#(6), 31, 4) * 16#
IF Bits#(debReg#(6), 14, 14) > 0 THEN
    LET Y# = Y# - 2# ^ 14
END IF
LET Y# = Y# + B(4) * 2# ^ 14
LET debReg#(6) = Y# + B(3) * 8# + B(2) * 4# + B(1) * 2# + B(0)
LET prev.TF = TF
REM
REM Presume that there is an error on p176.
REM Use BS instead of prev.TF in the following condition:
REM
IF B(0) OR B(1) OR B(2) OR B(3) OR B(4) THEN
    CALL signal.trap(DEBUG, -1)
END IF

END SUB

SUB reset.cpu
REM p 182-3
LET prev.TF = 0
LET prev.instr.loaded.SS = FALSE
LET task.switch.in.progress = FALSE
LET NMI.handler.active = FALSE
FOR K = 0 TO 3
    LET brkpt.active%(K) = FALSE
NEXT
END SUB

FUNCTION RW (N)
REM p 173
LET RW = Bits#(debReg#(7), 4 * N + 17, 4 * N + 16)

END FUNCTION

SUB signal.abort (xcptype AS INTEGER, error.code%)
REM p158
   
    LET CPL = prev.CPL
    CALL clear.active.brkpts
    IF xcptype = DOUBLEFAULT THEN
        CALL handle.intr.xcp(xcptype, TRUE, 0)
    END IF
END SUB

SUB signal.fault (xcptype AS INTEGER, error.code AS INTEGER)
REM p155
REM  Use -1 to indicate that there is no error.code for this exception type
REM curr = prev
LET EIP# = prev.EIP#
LET CPL = prev.CPL
    DIM erc AS INTEGER
    CALL clear.active.brkpts
    IF xcptype <> DEBUG THEN
        LET RF = 1
    END IF
    SELECT CASE xcptype
        CASE DIVIDE
            IF NOT double.fault THEN
                LET XH.flag = TRUE
                CALL handle.intr.xcp(DIVIDE, FALSE, -1)
            END IF
        CASE DEBUG
            IF Bits#(debReg#(7), 13, 13) > 0# THEN  ' Clear GD flag
                LET debReg#(7) = debReg#(7) - 2# ^ 13
            END IF
            LET XH.flag = TRUE
            CALL handle.intr.xcp(DEBUG, FALSE, -1)
        CASE BOUND, INVALID.INST, PE.UNAVAILABLE
            LET XH.flag = TRUE
            CALL handle.intr.xcp(xcptype, FALSE, -1)
        CASE task, NP, STACK
            IF NOT double.fault THEN
                IF XH.flag THEN
                    LET erc = error.code OR 1
                    LET XH.flag = TRUE
                END IF
                CALL handle.intr.xcp(xcptype, TRUE, erc)
            END IF
        CASE GP
            IF NOT double.fault THEN
                IF XH.flag THEN
                    LET error.code = error.code OR 1
                END IF
                CALL handle.intr.xcp(xcptype, TRUE, erc)
            END IF
        CASE PAGE
            IF exception.count = 1 OR first.page.fault.seen THEN
               LET exception.count = exception.count + 1
            END IF
            IF exception.count = 2 THEN
                CALL signal.abort(DOUBLEFAULT, 0)
            ELSEIF exception.count >= 3 THEN
                REM CALL shutdown  (Next two lines)
                LET HALT.FLAG = TRUE
                LET INTR.pin.active = FALSE 'Avoid making the main loop go infinite.
            ELSE
                LET XH.flag = TRUE
                LET first.page.fault.seen = TRUE
                LET erc = error.code AND -4
                CALL handle.intr.xcp(PAGE, TRUE, erc) ' Surely it's not 13?
            END IF
        CASE ALIGN
            LET XH.flag = TRUE
            CALL handle.intr.xcp(ALIGN, FALSE, -1)
    END SELECT
END SUB

SUB signal.trap (xcptype AS INTEGER, err.code%)
REM p 157
LET CPL = prev.CPL
CALL clear.active.brkpts
IF xcptype = DEBUG THEN
    IF Bits#(debReg#(7), 13, 13) > 0 THEN
        LET debReg#(7) = debReg#(7) - 2# ^ 13
    END IF
    LET XH.flag = TRUE
    CALL handle.intr.xcp(DEBUG, FALSE, 0)
ELSE
    LET INTERRUPT.RECEIVED = TRUE
    LET INTERRUPT.NUMBER = xcptype

END IF
END SUB

