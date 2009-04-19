REM $INCLUDE: 'x86.bi'
REM
REM Display Functions
REM
DECLARE SUB ScreenInit ()
DECLARE SUB RefreshScreen (SHOW.FLAGS)
DECLARE SUB RegDisplay (SHOW.FLAGS)
DECLARE SUB DisplayCode ()
DECLARE SUB DisplayStack ()
DECLARE SUB DisplayMem (ADDR#)
DECLARE SUB SegmentDisplay ()
DECLARE SUB MiscDisplay (PLACE)
DECLARE SUB DisplayEnv (PLACE)
DECLARE SUB HelpDisplay (PLACE)
REM
DECLARE SUB Getlines (FIRST.LIN%, LAST.LIN%, VB%)
DECLARE SUB PrintMsg (NUM!)
DECLARE SUB Print24 (X$)
REM
REM Edlin Mode Functions
REM
DECLARE SUB CommandMode ()
DECLARE SUB InsertMode ()
REM
REM Editor Command Functions
REM
DECLARE SUB InstDelete ()
DECLARE SUB InstInsert (RAW.CODE$, PACKED.CODE$, MNEMONIC$, LABEL$, DISPLABEL$, DISPCODE, COMMENT.FIELD$, HX$, NB)
DECLARE SUB Merge (SOURCE$)
DECLARE SUB ShortenJump (INST.K%, ERM$)
DECLARE SUB WriteDotO (X$)
DECLARE SUB ProcessLine (RAW.CODE$, IFL%, AGAIN%)
DECLARE FUNCTION GetCorrection$ (PR$, X$)
DECLARE SUB STUFF (Y$)
REM
REM Object File Manipulations
REM
DECLARE SUB DotOInit (orig#)
DECLARE SUB DotOLoad (X$, orig#)
DECLARE SUB HeaderRead (orig#, a.text#, a.data#, a.bss#, a.syms#, a.entry#, a.trsize#, a.drsize#)
REM
REM  Parsing Functions
REM
DECLARE SUB ParseInit ()
DECLARE SUB LineParse (MNEMONIC$, INDEX!, LABEL$, PF$, CODE$, COMMENT.FIELD$, ERM$)
DECLARE FUNCTION COUNT.CHAR (X$, C$)
REM
REM Execution Functions
REM
DECLARE SUB Fetchexec (REPEAT, BYTE)
DECLARE SUB reset.cpu ()
DECLARE SUB checkpoint ()
DECLARE SUB check.interrupts ()
DECLARE SUB check.brkpt (btype%, X#)
DECLARE SUB report.brkpts ()
DECLARE SUB reset.cpu ()
REM
REM
REM           This program simulates an x86 processor.
REM    Its top level is an "edlin" style source code editor.
REM    Code may be executed either interactively, or as a
REM    program.  Object code can be written in a.out or ELF
REM    format which may be linked to Linux system calls or
REM    other C programs.
REM
REM                    Preliminaries
     
REM
REM The config file EDL.INI is opened twice, once just to check
REM the first line for a data directory, DAT.PATH$.
REM The second time it is opened it reads all lines as instructions
REM and executes them, except the first if it happens to be DAT.PATH$.
REM
REM EDL.INI, if it exists, must be in the working directory.
REM If there is no EDL.INI then X86.DAT is assumed to be there.
REM ParseInit requires X86.DAT
REM
LET BADFILE.FLAG = FALSE
ON ERROR GOTO BadFileName
OPEN "EDL.INI" FOR INPUT AS #1
ON ERROR GOTO 0
IF BADFILE.FLAG THEN
    LET DAT.PATH$ = ""
    LET BADFILE.FLAG = FALSE
ELSE
    INPUT #1, DAT.PATH$          ' Tentative Data path
    CLOSE #1
END IF

ON ERROR GOTO BadFileName       '  Check Data path
OPEN DAT.PATH$ + "X86.DAT" FOR INPUT AS #1
ON ERROR GOTO 0
IF BADFILE.FLAG THEN
    LET BADFILE.FLAG = FALSE
    CLOSE #1
    LET DAT.PATH$ = ""
    ON ERROR GOTO NoDat
    OPEN "X86.DAT" FOR INPUT AS #1
    ON ERROR GOTO 0
END IF
   
CLOSE #1

REM
REM   Parser Initialization
REM

CALL ParseInit

REM
REM  Simulator Defaults
REM
LET INSTTIM = .6   '  Number of seconds for each instruction when walking.
REM
REM  Assembler Defaults
REM
LET USE.SIZE = 32   ' assemble using 32 bit default.
LET USE.INSTSET = 3 ' 386  instruction set.
REM USE.O = 2       ' a.out file.
LET USE.O = 3       ' ELF file.
REM USE.O = 4       ' .OBJ file
REM USE.O = 5       ' .COM file
REM
REM  Establish Hardware Defaults
REM
LET ARCHNUM = 3
REM Initialize Hardware

LET ADDR.BITS = 20      ' 1 Meg Memory

REM     Initialize Segment registers.
CALL reset.cpu

FOR I = 0 TO 5
    LET segReg(I) = 0
    LET descrVec(I).Valid = TRUE
    LET descrVec(I).BaseAdd = 0
    LET descrVec(I).Limit = 65535
    LET descrVec(I).DPL = 0
    LET descrVec(I).DefaultAttr = TRUE
    LET descrVec(I).Readable = TRUE
    LET descrVec(I).Writable = TRUE
    LET descrVec(I).Executable = FALSE
    LET descrVec(I).ExpandDown = FALSE
NEXT
    LET ctrlReg#(0) = 5#  'Set PE = 1, MP = 0, EM = 1, TS = 0, ET = 0, PG = 0
    LET descrVec(CS).Executable = TRUE
    LET descrVec(DS).Limit = 1048575#
    LET descrVec(ES).Limit = 1048575#
    LET descrVec(IDT).Valid = TRUE
    LET descrVec(GDT).Valid = TRUE
    LET descrVec(LDT).Valid = TRUE
    LET descrVec(TSS).Valid = TRUE
    LET descrVec(GDT).Readable = TRUE

    LET BOTTOM.STACK# = 8192#

     REM                Scramble Data Registers and Flags
    
        RANDOMIZE TIMER
        FOR KCOUNT = 0 TO 7
            LET genReg(KCOUNT) = HexStr$(RandomBytes#(4), -8)
        NEXT
        LET CF = CINT(RND * RND)    '2nd power discourages letters.
        LET PF = CINT(RND * RND)
        LET AF = CINT(RND * RND)
        LET ZF = CINT(RND * RND)
        LET SF = CINT(RND * RND)
        LET IE = 1
        LET DF = CINT(RND * RND)
        LET OF = CINT(RND * RND)
        LET IOPL = 0
        LET CPL = 0
        LET VM = 0

        LET NMI.pin.active = FALSE
REM
REM Read the config file.  (Override the defaults.)
REM
LET PROG.LEN = 0  '  Prevents searching for labels!
LET BADFILE.FLAG = FALSE
ON ERROR GOTO BadFileName
OPEN "EDL.INI" FOR INPUT AS #1
ON ERROR GOTO 0
IF NOT BADFILE.FLAG THEN
    WHILE NOT EOF(1)
        LINE INPUT #1, RAW.CODE$
        IF RAW.CODE$ <> DAT.PATH$ THEN
            CALL ProcessLine(RAW.CODE$, FALSE, JUNK%)
        END IF
    WEND
CLOSE #1
ELSE
    LET BADFILE.FLAG = FALSE
END IF

REM
REM  Assembler Array Initialization
REM
FOR K = 1 TO MAXNUM.INST
    LET INST.ADR#(K) = 0#
    LET LABEL.LIST$(K) = ""          '    Initialize the arrays
    LET DISPLABEL.LIST$(K) = ""          '    Initialize the arrays
    LET DISPLABEL.CODE(K) = 0
NEXT

REM
REM Initialize Editor
REM
CONST N.SAVES = 10          'Delete stack
DIM EDIT.STACK$(N.SAVES)
REM
REM Initialize Display
REM
    FOR K = 1 TO 7
        LET PLACE.MARK(K) = 1
    NEXT
    LET PLACE.MARK(3) = (CINT(LOAD.ORIGIN#)) \ 2
    LET PLACE.MARK(5) = 3
    LET PLACE.MARK(4) = 3

    DIM VERBOSE AS INTEGER
    REM    *********************************************************
     
    REM              Main part of the program.
    
    REM    *********************************************************
   
    REM          Get and Verify the File Name

    LET A86$ = COMMAND$

    IF A86$ = "" THEN   ' This is EDLIN behavior.
        PRINT "File name must be specified"
        SYSTEM
    END IF
  
    ON ERROR GOTO NewFilename
    LET NEWFILE.FLAG = FALSE
    OPEN A86$ FOR INPUT AS #1
    ON ERROR GOTO 0
    CLOSE #1
   
    LET N.SAVED = 0
    LET SKIP.DEPTH = -1
    LET CALL.DEPTH = 0
    LET LINE.NUM = 0
    LET PROG.LEN = 0
    LET FIRST.LINE = 1
    LET CODE.WINDOW.FLAG = TRUE
    LET CURRENT.INST = 1
    LET SRC.LINE(1) = 1
    LET DOT.SPOT = INSTR(A86$, ".")
    IF DOT.SPOT = 0 THEN
        LET BASE.NAME$ = A86$
        LET EXT.NAME$ = ""
    ELSEIF DOT.SPOT = LEN(A86$) THEN
        LET BASE.NAME$ = LEFT$(A86$, DOT.SPOT - 1)
        LET EXT.NAME$ = ""
    ELSE
        LET BASE.NAME$ = LEFT$(A86$, DOT.SPOT - 1)
        LET EXT.NAME$ = MID$(A86$, DOT.SPOT + 1)
    END IF
    LET SCREEN.FLAG = FALSE
    CALL ScreenInit
    IF EXT.NAME$ = "O" AND NEWFILE.FLAG THEN
        PRINT ""
        SYSTEM
    ELSEIF EXT.NAME$ = "O" THEN
        CALL DotOLoad(BASE.NAME$, LOAD.ORIGIN#)
        CALL HeaderRead(LOAD.ORIGIN#, a.text#, a.data#, a.bss#, a.syms#, a.entry#, a.trsize#, a.drsize#)
        LET INST.ADR#(1) = LOAD.ORIGIN# + a.entry#
        LET DISPLAY.MODE = 3
        CALL RefreshScreen(FALSE)
    ELSE
        CALL DotOInit(LOAD.ORIGIN#)
        LET INST.ADR#(1) = LOAD.ORIGIN#
        IF NOT NEWFILE.FLAG THEN
            CALL Merge(A86$)
        END IF
        LET DISPLAY.MODE = 1
    END IF

LET EIP# = -1
LET WALK.MODE = FALSE
LET REPEAT = 0          'Allow main loop to interrupt string repeats
LET LOST.FLAG = FALSE
LET CONTINUE.FLAG = TRUE
LET FRESH.FLAG = TRUE
LET VERBOSE = -1
LET YN.AWAIT = FALSE
IF NEWFILE.FLAG THEN  'Edlin behavior :\)
    CALL Print24(" New File")
ELSE
    CALL Print24(" End of input file")
END IF

CONST TRAP.REP.NUM = 100

WHILE CONTINUE.FLAG
   
    LET TRAP$ = ""
    LET NN = 0
    WHILE TRAP$ = "" AND (NOT WALK.MODE OR NN < TRAP.REP.NUM)
        IF WALK.MODE THEN
            LET NN = NN + 1
        END IF
        LET TRAP$ = INKEY$
    WEND
    LET TRAP$ = UCASE$(TRAP$)
   
    IF NN >= TRAP.REP.NUM THEN
        LET TRAP$ = CHR$(0) + CHR$(66)   'Manufactured F8 keystroke
        SLEEP INSTTIM
    ELSEIF WALK.MODE AND LEN(TRAP$) = 1 AND (TRAP$ < "F" OR TRAP$ > "L") AND TRAP$ <> CHR$(9) THEN
        LET WALK.MODE = FALSE             'Arrow keys shouldn't stop walk.
        LET SKIP.DEPTH = -1
        LET TRAP$ = "Do nothing."
        CALL PrintMsg(5)
    END IF
    IF YN.AWAIT THEN
        IF TRAP$ = "Y" THEN
            LET CONTINUE.FLAG = FALSE
        ELSEIF TRAP$ = "N" THEN
            IF EIP# > -1 THEN
                CALL PrintMsg(5)
            ELSE
                CALL PrintMsg(6)
            END IF
            LET YN.AWAIT = FALSE
        END IF
    ELSEIF TRAP$ = CHR$(3) THEN               'Control C
        REM Use this to allow user to signal an interrupt.
        LET INTR.pin.active = TRUE
    ELSEIF TRAP$ = CHR$(13) THEN              'Enter key
        IF CURRENT.INST = PROG.LEN + 1 OR LOST.FLAG THEN
            LET LOST.FLAG = FALSE
            LET CURRENT.INST = 1
            LET HALT.FLAG = FALSE
            LET EIP# = LOAD.ORIGIN# + a.entry#
            LET CALL.DEPTH = 0
            IF FRESH.FLAG THEN
                CALL Print24(" Ok Ready!     Space=Step   Enter=SetIP ")
                LET FRESH.FLAG = FALSE
            ELSE
                CALL Print24(" Program restarted")
            END IF
        ELSEIF INST.ADR#(CURRENT.INST) <> EIP# THEN
            LET EIP# = INST.ADR#(CURRENT.INST)
            CALL PrintMsg(5)
        END IF
        LET REPEAT = 0
        LET HALT.FLAG = FALSE
        CALL RefreshScreen(FALSE)
    ELSEIF TRAP$ = CHR$(27) OR TRAP$ = CHR$(0) + CHR$(64) THEN  ' Escape key
        CALL CommandMode               ' or the F6 key
        IF EIP# > -1 THEN
            CALL PrintMsg(5)
        ELSE
            CALL PrintMsg(6)
        END IF
    ELSEIF TRAP$ = CHR$(0) + CHR$(62) THEN     'F4
        LET SCREEN.FLAG = NOT SCREEN.FLAG
        IF SCREEN.FLAG THEN
            CALL RefreshScreen(FALSE)
        ELSEIF EIP# > -1 THEN
            CALL ScreenInit
            CALL PrintMsg(5)
        ELSE
            CALL ScreenInit
            CALL PrintMsg(6)
        END IF
    ELSEIF TRAP$ = CHR$(0) + CHR$(63) THEN     'F5
        IF EIP# <> -1# THEN
            LET WALK.MODE = TRUE
            CALL Print24(" Walking ...")
        END IF
    ELSEIF TRAP$ = CHR$(0) + CHR$(66) OR TRAP$ = CHR$(32) THEN  'F8 key
        IF EIP# = -1# THEN
            CALL Print24(" IP not set.                 Enter=SetIP")
        ELSE            ' p11
            LET FRESH.FLAG = FALSE
            IF INTERRUPT.RECEIVED THEN
                LET INTERRUPT.RECEIVED = FALSE
            END IF
            LET REP.BROKEN = FALSE
            IF REPEAT > 0 THEN
                IF external.interrupt THEN
                    LET EIP# = prev.EIP#
                    LET REP.BROKEN = TRUE
                ELSEIF brkpt.detected THEN
                    LET REP.BROKEN = TRUE
                END IF
            END IF
            IF REP.BROKEN OR REPEAT = 0 THEN
                CALL report.brkpts
            END IF
            IF NOT INTERRUPT.RECEIVED THEN
                IF REP.BROKEN OR REPEAT = 0 THEN
                    CALL check.interrupts
                END IF
            END IF
            IF HALT.FLAG THEN
                LET WALK.MODE = FALSE
            ELSE
                IF REPEAT = 0 AND NOT INTERRUPT.RECEIVED THEN
                    CALL checkpoint
                END IF
                CALL Fetchexec(REPEAT, BYTE)
            END IF
            LET K = 1
            WHILE INST.ADR#(K) <> EIP# AND K <= PROG.LEN
                LET K = K + 1
            WEND
            IF HALT.FLAG THEN
                CALL Print24(" Processor halted")
            ELSEIF INTERRUPT.RECEIVED THEN
                LET MSG$ = " Interrupt #" + STR$(INTERRUPT.NUMBER) + ","
                CALL Print24(MSG$)
                LET WALK.MODE = FALSE
            ELSEIF INST.ADR#(K) <> EIP# THEN
                LET LOST.FLAG = TRUE
                LET ERM$ = " CRASH!    EIP = " + HexStr$(EIP#, 6) + "    Enter=Restart"
                CALL Print24(ERM$)
            ELSE
                LET CURRENT.INST = K
                IF WALK.MODE AND SKIP.DEPTH > CALL.DEPTH THEN
                    CALL Print24(" Skipping ...")
                ELSEIF WALK.MODE THEN
                    CALL Print24(" Walking ...")
                ELSEIF REPEAT > 0 THEN
                    CALL Print24(" Repeating ...")
                ELSE
                    CALL PrintMsg(5)
                END IF
            END IF
            IF CALL.DEPTH < 0 AND NOT RETS.PURE THEN
                LET EIP# = -1
                CALL PrintMsg(7)
                LET WALK.MODE = FALSE
            END IF
            CALL RefreshScreen(FALSE)
        END IF
    ELSEIF TRAP$ = CHR$(0) + CHR$(68) THEN     'F10 key
        IF EIP# <> -1# THEN
            LET SKIP.DEPTH = CALL.DEPTH
            LET WALK.MODE = TRUE
            CALL Print24(" Skipping ...")
        END IF
    ELSEIF TRAP$ = "C" THEN
        LET CODE$ = RAW.SRCE$(SRC.LINE(CURRENT.INST))
        CALL LineParse(MNEMONIC$, INDEX, LABEL$, PF$, CODE$, COMMENT.FIELD$, ERM$)
        LET FIXIT$ = LABEL$ + SPACE$(1) + MNEMONIC$ + SPACE$(1) + CODE$
        CALL InstDelete
        CALL Print24(" Edit instruction:")
        LET PMPT$ = "*"
        LET CODE$ = GetCorrection$(PMPT$, FIXIT$)
        COLOR 15, 1
        CALL ProcessLine(CODE$, TRUE, JUNK%)
        CALL RefreshScreen(FALSE)
        IF EIP# > -1 THEN
            CALL PrintMsg(5)
        ELSE
            CALL PrintMsg(6)
        END IF
    ELSEIF TRAP$ = "D" OR TRAP$ = "Y" OR TRAP$ = CHR$(0) + CHR$(83) THEN   ' Delete key
        IF CURRENT.INST >= 1 AND CURRENT.INST <= PROG.LEN THEN
            IF N.SAVED < N.SAVES THEN
                LET N.SAVED = N.SAVED + 1
            ELSE
                FOR K = 1 TO N.SAVES - 1
                    LET EDIT.STACK$(K) = EDIT.STACK$(K + 1)
                NEXT
            END IF
            LET EDIT.STACK$(N.SAVED) = RAW.SRCE$(SRC.LINE(CURRENT.INST))
            IF TRAP$ = "Y" THEN  'Just a yank.
                CALL Print24(" Line yanked")
            ELSE
                CALL InstDelete
            END IF
            CALL RefreshScreen(FALSE)
        ELSE
            CALL Print24(" Delete what?")
        END IF
    ELSEIF TRAP$ = "E" OR TRAP$ = "W" THEN
        CALL Print24(" Writing ...")
        IF NOT NEWFILE.FLAG THEN
            ON ERROR GOTO NoBak
            KILL BASE.NAME$ + ".BAK"
            ON ERROR GOTO 0
            NAME A86$ AS BASE.NAME$ + ".BAK"
        END IF
        OPEN A86$ FOR OUTPUT AS #1
            FOR K = 1 TO SRC.LINE(PROG.LEN + 1) - 1
                PRINT #1, RAW.SRCE$(K)
            NEXT
        CLOSE #1
        IF TRAP$ = "E" THEN
            LET CONTINUE.FLAG = FALSE
        ELSEIF EIP# > -1 THEN
            CALL PrintMsg(5)
        ELSE
            CALL PrintMsg(6)
        END IF
    ELSEIF TRAP$ = "F" THEN
        CALL RefreshScreen(TRUE)
        SLEEP 2 * INSTTIM
        CALL RefreshScreen(FALSE)
    ELSEIF TRAP$ = "H" OR TRAP$ = "?" OR TRAP$ = CHR$(0) + CHR$(59) THEN        'F1
        LET DISPLAY.MODE = 7
        LET CODE.WINDOW.FLAG = FALSE
        CALL RefreshScreen(FALSE)
        CALL Print24(SPACE$(25) + "More help: " + CHR$(25) + CHR$(24))
    ELSEIF TRAP$ = "I" OR TRAP$ = CHR$(0) + CHR$(82) THEN
        CALL Print24(" Enter instruction:")
        CALL InsertMode
        IF EIP# > -1 THEN
            CALL PrintMsg(5)
        ELSE
            CALL PrintMsg(6)
        END IF
    ELSEIF TRAP$ = "O" THEN
        CALL Print24(" Writing ...")
        CALL WriteDotO(BASE.NAME$)
        IF EIP# > -1 THEN
            CALL PrintMsg(5)
        ELSE
            CALL PrintMsg(6)
        END IF
    ELSEIF TRAP$ = "P" OR TRAP$ = "U" THEN
        IF N.SAVED = 0 THEN
            CALL Print24(" Buffer empty")
        ELSE
            CALL ProcessLine(EDIT.STACK$(N.SAVED), TRUE, JUNK%)
            LET CURRENT.INST = CURRENT.INST - 1
            IF TRAP$ = "U" THEN LET N.SAVED = N.SAVED - 1
        END IF
        CALL RefreshScreen(FALSE)
        IF EIP# > -1 THEN
            CALL PrintMsg(5)
        ELSE
            CALL PrintMsg(6)
        END IF
    ELSEIF TRAP$ = "Q" THEN
        CALL Print24(" Abort edit (Y/N)?")
        LET YN.AWAIT = TRUE
    ELSEIF TRAP$ = "R" OR TRAP$ = "T" THEN
        CALL Print24("Enter file name")
        LOCATE 25, 1
        PRINT ":";
        LINE INPUT ; "", MFILE$
        CALL Merge(MFILE$)
        CALL RefreshScreen(FALSE)
        IF EIP# > -1 THEN
            CALL PrintMsg(5)
        ELSE
            CALL PrintMsg(6)
        END IF
    ELSEIF TRAP$ = "S" THEN
        IF CURRENT.INST <= PROG.LEN THEN
            CALL ShortenJump(CURRENT.INST, ERM$)
        ELSE
            LET ERM$ = "Shorten what?"
        END IF
        IF ERM$ <> "" THEN
            CALL Print24(ERM$)
        ELSE
            CALL RefreshScreen(FALSE)
        END IF
    ELSEIF TRAP$ = CHR$(0) + CHR$(72) OR TRAP$ = "K" THEN   'Up arrow
        IF CODE.WINDOW.FLAG THEN
            IF CURRENT.INST > 1 THEN
                LET CURRENT.INST = CURRENT.INST - 1
            END IF
            CALL PrintMsg(5)
        ELSE
            LET PLACE.MARK(DISPLAY.MODE) = PLACE.MARK(DISPLAY.MODE) - 1
        END IF
        CALL RefreshScreen(FALSE)
    ELSEIF TRAP$ = CHR$(0) + CHR$(80) OR TRAP$ = "J" THEN  'Down arrow
        IF CODE.WINDOW.FLAG THEN
            IF CURRENT.INST <= PROG.LEN THEN
                LET CURRENT.INST = CURRENT.INST + 1
            END IF
            IF FRESH.FLAG AND CURRENT.INST = PROG.LEN + 1 AND DISPLAY.MODE < 3 THEN
                CALL Print24(" End of input file")
            ELSE
                CALL PrintMsg(5)
            END IF
        ELSE
            LET PLACE.MARK(DISPLAY.MODE) = PLACE.MARK(DISPLAY.MODE) + 1
        END IF
        CALL RefreshScreen(FALSE)
    ELSEIF TRAP$ = CHR$(0) + CHR$(77) OR TRAP$ = "L" THEN    'Right arrow
        LET CODE.WINDOW.FLAG = FALSE
        CALL RefreshScreen(FALSE)
    ELSEIF TRAP$ = CHR$(9) THEN    'Tab Key
        IF DISPLAY.MODE = 7 THEN
            IF EIP# > -1 THEN
                CALL PrintMsg(5)
            ELSE
                CALL PrintMsg(6)
            END IF
        END IF
        LET DISPLAY.MODE = (DISPLAY.MODE MOD 7) + 1
        IF DISPLAY.MODE = 7 THEN
            LET MSG$ = SPACE$(25) + "More help: " + CHR$(25) + CHR$(24)

            CALL Print24(MSG$)
        END IF
        CALL RefreshScreen(FALSE)
    ELSEIF TRAP$ = CHR$(0) + CHR$(75) THEN     'Left arrow  H???
        LET CODE.WINDOW.FLAG = TRUE
        CALL RefreshScreen(FALSE)
    ELSEIF TRAP$ = CHR$(0) + CHR$(15) THEN    'Left Tab
        IF DISPLAY.MODE = 7 THEN
            IF EIP# > -1 THEN
                CALL PrintMsg(5)
            ELSE
                CALL PrintMsg(6)
            END IF
        END IF
        LET DISPLAY.MODE = (DISPLAY.MODE + 5) MOD 7 + 1
        IF DISPLAY.MODE = 7 THEN
            LET MSG$ = SPACE$(25) + "More help: " + CHR$(25) + CHR$(24)
            CALL Print24(MSG$)
        END IF
        CALL RefreshScreen(FALSE)
    ELSEIF TRAP$ = CHR$(0) + CHR$(71) THEN    'Home key
        IF CODE.WINDOW.FLAG THEN
            LET CURRENT.INST = FIRST.LINE
            CALL RefreshScreen(FALSE)
        END IF
    ELSEIF TRAP$ = CHR$(0) + CHR$(79) THEN    'End key
        IF CODE.WINDOW.FLAG THEN
            IF LAST.LINE <= PROG.LEN + 1 AND LAST.LINE > 2 THEN
                LET CURRENT.INST = LAST.LINE - 1
            END IF
            CALL RefreshScreen(FALSE)
        END IF
    ELSEIF TRAP$ = CHR$(0) + CHR$(73) THEN    'Page Up
        IF CODE.WINDOW.FLAG THEN
            IF FIRST.LINE > 1 THEN
                CALL Getlines(FIRST.LINE, LAST.LINE, VERBOSE)
                LET NEW.LAST.LINE = FIRST.LINE - 1
                DO
                    LET SAVE.FIRST.LINE = FIRST.LINE
                    LET FIRST.LINE = FIRST.LINE - 1
                    LET CURRENT.INST = FIRST.LINE
                    CALL Getlines(FIRST.LINE, LAST.LINE, VERBOSE)
                LOOP WHILE LAST.LINE >= NEW.LAST.LINE AND SAVE.FIRST.LINE <> 1
                IF SAVE.FIRST.LINE > 0 THEN
                    LET FIRST.LINE = SAVE.FIRST.LINE
                ELSE
                    LET FIRST.LINE = 1
                END IF
                LET CURRENT.INST = FIRST.LINE
                CALL Getlines(FIRST.LINE, LAST.LINE, VERBOSE)
            END IF
        ELSEIF DISPLAY.MODE = 2 THEN
        ELSEIF DISPLAY.MODE = 3 THEN
            LET PLACE.MARK(DISPLAY.MODE) = PLACE.MARK(DISPLAY.MODE) - 16
        ELSE
            LET PLACE.MARK(DISPLAY.MODE) = PLACE.MARK(DISPLAY.MODE) - 1
        END IF
        CALL PrintMsg(6)
        CALL RefreshScreen(FALSE)
    ELSEIF TRAP$ = CHR$(0) + CHR$(81) THEN    'Page Down
        IF CODE.WINDOW.FLAG THEN
                CALL Getlines(FIRST.LINE, LAST.LINE, VERBOSE)
                LET CURRENT.INST = LAST.LINE + 1
                LET FIRST.LINE = CURRENT.INST
                CALL Getlines(FIRST.LINE, LAST.LINE, VERBOSE)
                LET CURRENT.INST = LAST.LINE
        ELSEIF DISPLAY.MODE = 2 THEN
        ELSEIF DISPLAY.MODE = 3 THEN
            LET PLACE.MARK(DISPLAY.MODE) = PLACE.MARK(DISPLAY.MODE) + 16
        ELSE
            LET PLACE.MARK(DISPLAY.MODE) = PLACE.MARK(DISPLAY.MODE) + 1
        END IF
        IF FRESH.FLAG AND CURRENT.INST = PROG.LEN + 1 AND DISPLAY.MODE < 3 THEN
            CALL Print24(" End of input file")
        END IF
        CALL RefreshScreen(FALSE)
    END IF
   
WEND

END
   
BadFileName:
CLOSE #1
LET BADFILE.FLAG = TRUE
RESUME NEXT

NewFilename: ' file must be a new file
LET NEWFILE.FLAG = TRUE
CALL Print24(" New File")
RESUME NEXT

NoBak:
RESUME NEXT

NoDat:
PRINT "Can't find X86.DAT file"
SYSTEM

SUB CommandMode
    LET COMMAND.MODE = TRUE
    LET SAVE.EIP# = EIP#
    LET EIP# = -1#               'Take the yellow line out of the program.
    CALL RefreshScreen(FALSE)
    LET SAVE.CALL.DEPTH = CALL.DEPTH
    LET CALL.DEPTH = -1
    WHILE COMMAND.MODE
        CALL Print24(" Enter command:")
        COLOR 14
        LOCATE 25, 1
        PRINT ">" + SPACE$(38);
        LOCATE 25, 2
        LINE INPUT ; "", CODE$
        COLOR 15, 1
        CALL ProcessLine(CODE$, FALSE, PR.RETURN%)
            REM The value of COMMAND.MODE may be changed by ProcessLine
        LET COMMAND.MODE = COMMAND.MODE AND PR.RETURN%
        CALL RefreshScreen(FALSE)
        IF INTERRUPT.RECEIVED THEN
            LET MSG$ = " Interrupt #" + STR$(INTERRUPT.NUMBER) + ","
            CALL Print24(MSG$)
            LET INTERRUPT.RECEIVED = FALSE
            SLEEP 2 * INSTTIM
        END IF
    WEND
    IF NOT PR.RETURN% THEN     ' COMMAND.MODE changed in the usual way.
        LET EIP# = SAVE.EIP#      ' retrieve the saved EIP
    END IF
    LET CALL.DEPTH = SAVE.CALL.DEPTH
    CALL RefreshScreen(FALSE)
    CALL Print24(SPACE$(29) + "Enter=SetIP")
    COLOR 15, 1
    LOCATE 25, 1
    PRINT SPACE$(39);
END SUB

SUB DisplayCode
SHARED DISPLAY.MODE

IF FIRST.LINE < 1 THEN LET FIRST.LINE = 1

LET VERBOSE% = -1
REM
REM       subroutine to display source and machine code
REM
IF DISPLAY.MODE = 1 THEN
    LOCATE 3, 30
    PRINT "Object Code";
    LOCATE 4, 30
    PRINT "  Adrs Hex ";
END IF

LOCATE 4, 30
IF NOT CODE.WINDOW.FLAG THEN
    PRINT "*";
ELSE
    PRINT " ";
END IF

LOCATE 5, 30
PRINT SPACE$(11);
CALL Getlines(FIRST.LINE, LAST.LINE, VERBOSE%)
LOCATE 6
DIM PART$(5)
FOR I = FIRST.LINE TO LAST.LINE
    FOR N = 0 TO 5
        LET PART$(N) = ""
    NEXT
    LOCATE , 14
    IF EIP# = INST.ADR#(I) AND EIP# >= 0# THEN COLOR 14
    LET TAIL$ = SHOW.SRCE$(I)
    LET N = 0
    WHILE TAIL$ <> ""
        LET N = N + 1
        LET SEMI.SPOT = INSTR(TAIL$, ";")
        LET PART$(N) = LEFT$(TAIL$, SEMI.SPOT - 1)
        LET TAIL$ = MID$(TAIL$, SEMI.SPOT + 1)
    WEND
    IF DISPLAY.MODE = 1 THEN
        PRINT PART$(1); TAB(32);
    ELSE
        PRINT PART$(1); TAB(40);
    END IF
    LOCATE , 17
    IF LEN(LABEL.LIST$(I)) > 3 AND INST.ADR#(I) = INST.ADR#(I + 1) THEN
        REM Don't overwrite the label
    ELSEIF I = CURRENT.INST AND CODE.WINDOW.FLAG THEN
        PRINT "*";
    ELSE
        PRINT SPACE$(1);
    END IF
    LOCATE , 32
    IF DISPLAY.MODE = 1 THEN
        IF PART$(2) <> "" THEN
            LET PART$(2) = LEFT$(SPACE$(4) + PART$(2) + SPACE$(10), 14)
        END IF
        IF PART$(3) <> "" THEN
            LET PART$(3) = LEFT$(SPACE$(4) + PART$(2) + SPACE$(10), 14)
        END IF
    END IF
    IF DISPLAY.MODE = 1 THEN
        LET HXN = LEN(HEX$(INST.ADR#(I)))
        IF HXN < 3 THEN PRINT SPACE$(3 - HXN);
        PRINT HEX$(INST.ADR#(I));  '  print machine address in hex
        IF INST.ADR#(I + 1) > INST.ADR#(I) + 15# THEN
            LET TOP.AD = INST.ADR#(I) + 15#
        ELSE
            LET TOP.AD = INST.ADR#(I + 1) - 1#
        END IF
        FOR HX = INST.ADR#(I) TO TOP.AD
            IF HX = INST.ADR#(I) + 4 THEN
                LOCATE , 14
                LET PART$(3) = LEFT$(PART$(3) + SPACE$(20), 20)
                PRINT SPACE$(1) + PART$(3);
            ELSEIF HX = INST.ADR#(I) + 2 THEN
                LOCATE , 14
                LET PART$(2) = LEFT$(PART$(2) + SPACE$(20), 20)
                PRINT SPACE$(1) + PART$(2);
            ELSEIF HX = INST.ADR#(I) THEN
            ELSEIF (HX - INST.ADR#(I)) MOD 2 = 0 THEN
                LOCATE , 14
                PRINT SPACE$(21);
            END IF
            LET POINTER# = HX
            LET X# = LA.read(POINTER#, 1)
            LET CODE$ = HexStr$(X#, 2)
            PRINT " "; CODE$;
            IF (HX + INST.ADR#(I)) MOD 2 = 0 AND HX = TOP.AD THEN
                PRINT "   ";
            END IF
            IF (HX + INST.ADR#(I)) MOD 2 = 1 AND HX <> TOP.AD THEN
                PRINT
            END IF
        NEXT
    END IF
    IF INST.ADR#(I + 1) <= INST.ADR#(I) THEN PRINT SPACE$(6);
    PRINT
    COLOR 15
NEXT

J = CSRLIN
FOR I = J TO 23
    LOCATE I, 13
    PRINT SPACE$(28);
NEXT

END SUB

SUB DisplayMem (ADDR#)
   
        LET ADDR# = ADDR# * 2
        LOCATE 3, 30
        PRINT "Memory     ";
        LOCATE 4, 32
        LET Y# = Bits#(ADDR#, 7, 0)
        LET X$ = HexStr(ADDR# - Y#, 8)
        PRINT LEFT$(X$, 6);
        COLOR 8, 1
        PRINT RIGHT$(X$, 2) + SPACE$(1);
        COLOR 15, 1

    
   
    LET START.LINE# = INT(ADDR# / 2) * 2#
    LOCATE 6, 32
    FOR I = 0 TO 32 STEP 2
        LOCATE , 32
        LET POINTER# = START.LINE# + CDBL(I)
        PRINT HexStr$(POINTER#, 2);
        LET X# = LA.read#(POINTER#, 1)
        LET CODE$ = HexStr$(X#, 2)
        IF EIP# = POINTER# THEN COLOR 14
        PRINT " "; CODE$;
        COLOR 15
        LET POINTER# = POINTER# + 1#
        LET X# = LA.read#(POINTER#, 1)
        LET CODE$ = HexStr$(X#, 2)
        IF EIP# = POINTER# THEN COLOR 14
        PRINT " "; CODE$
        COLOR 15
    NEXT
END SUB

SUB DisplayStack
REM
     
    LOCATE 3, 28
    PRINT "  Stack      ";
    LOCATE 4, 32
    PRINT "         ";
    LET LOCAL.ESP# = REG#(4)
    LET DISPLAY.VALID = (LOCAL.ESP# > 0 AND LOCAL.ESP# < 2# ^ ADDR.BITS AND BOTTOM.STACK# < 2# ^ ADDR.BITS)
   
    LOCATE 6, 30

    IF NOT DISPLAY.VALID THEN
        FOR K = 6 TO 23
            LOCATE K, 32
            PRINT SPACE$(9);
        NEXT
        EXIT SUB
    END IF

    LET v = (BOTTOM.STACK# - LOCAL.ESP#) \ 4 - 1
   
    IF v - 5 >= 0 AND PLACE.MARK(2) > v - 5 THEN
        LET PLACE.MARK(2) = v - 5
    ELSEIF PLACE.MARK(2) < 0 THEN
        LET PLACE.MARK(2) = 0
    END IF
    
      FOR K = 0 TO 5
        FOR I = 0 TO 3
                LOCATE 6 + 3 * K + (I + 1) \ 2, 32
                IF v < 5 THEN
                    LET w = 5 - v
                ELSE
                    LET w = -PLACE.MARK(2)
                END IF
                LET u = 4 * (K + PLACE.MARK(2))
                IF K < (5 - v) THEN
                    PRINT SPACE$(9);
                ELSE
REM             IF LOCAL.ESP# + I + u < BOTTOM.STACK# THEN
                        IF I = 0 THEN
                                PRINT LEFT$(HEX$(LOCAL.ESP# + 4 * (K - w)) + SPACE$(9), 9)
                                LOCATE , 32
                                PRINT "   ";
                        ELSEIF I = 2 THEN
                                LOCATE 6 + 3 * K + 2, 32
                                PRINT "   ";
                        ELSE
                                LOCATE , 38
                        END IF
                        LET BYTE# = LA.read#(descrVec(SS).BaseAdd + LOCAL.ESP# + 4 * (K - w) + I, 1)
                        PRINT RIGHT$("0" + HEX$(BYTE#), 2); " ";
                        IF I = 1 OR I = 3 THEN PRINT
REM                ELSE
REM                    PRINT SPACE$(9);
                END IF
               
        NEXT
      NEXT
END SUB

FUNCTION GetCorrection$ (PMPT$, X$)
LOCATE 25, 1
PRINT PMPT$;      ' Should be one character
CALL STUFF(X$)
LINE INPUT ; "", C$
LOCATE 25, 1
PRINT SPACE$(39);
LET GetCorrection$ = C$
END FUNCTION

SUB Getlines (FIRST.LINE%, LAST.LINE%, VERBOSITY.FLAG%)
   
    IF FIRST.LINE% > CURRENT.INST THEN
        LET FIRST.LINE% = CURRENT.INST
    END IF

    DO
        LET TOTAL.LINES% = 0
        LET LAST.LINE% = FIRST.LINE% - 2
        LET NEW.LINES = 0
        DO
            LET LAST.LINE% = LAST.LINE% + 1
            LET TOTAL.LINES% = TOTAL.LINES% + NEW.LINES
            IF NOT VERBOSITY.FLAG% THEN
                LET NEW.LINES = 1
            ELSEIF LAST.LINE% >= PROG.LEN THEN
                LET NEW.LINES = 1
            ELSE
                LET NEW.LINES = (INST.ADR#(LAST.LINE% + 2) - INST.ADR#(LAST.LINE% + 1) + 1) \ 2
            END IF
            LET NUM.SEMIS = COUNT.CHAR(SHOW.SRCE$(LAST.LINE% + 1), ";")
            IF NEW.LINES < NUM.SEMIS THEN
                LET NEW.LINES = NUM.SEMIS
            END IF
        LOOP WHILE TOTAL.LINES% + NEW.LINES <= 17 AND LAST.LINE% <= PROG.LEN AND NEW.LINES >= 0

        IF LAST.LINE% < FIRST.LINE% THEN
            LET LAST.LINE% = FIRST.LINE%
        END IF
       
        IF LAST.LINE% = CURRENT.INST AND EIP# > 0# AND TOTAL.LINES% > 15 THEN
            LET FIRST.LINE% = (FIRST.LINE% + LAST.LINE% + 1) \ 2
            LET LAST.LINE% = FIRST.LINE%
        ELSEIF LAST.LINE% < CURRENT.INST THEN
            IF LAST.LINE% <= PROG.LEN THEN
                LET FIRST.LINE% = FIRST.LINE% + 1
                LET LAST.LINE% = FIRST.LINE%
            END IF
            IF TOTAL.LINES% < 17 AND LAST.LINE% = PROG.LEN THEN
                LET LAST.LINE% = LAST.LINE% + 1
            END IF
        END IF

    LOOP WHILE LAST.LINE% < CURRENT.INST AND LAST.LINE% <= PROG.LEN
END SUB

SUB InsertMode
LET INSERT.MODE% = TRUE
WHILE INSERT.MODE%
    CALL Print24(" Enter instruction:")
    LOCATE 25, 1
    PRINT "* ";
    LINE INPUT ; "", CODE$
    LOCATE 25, 2
    PRINT SPACE$(38);
    LOCATE , 2
    CALL ProcessLine(CODE$, TRUE, INSERT.MODE%)
    CALL RefreshScreen(FALSE)
WEND
CALL PrintMsg(5)  'F5, F8, F10
LOCATE 25, 1
PRINT SPACE$(39);
END SUB

SUB Merge (SOURCE$)
SHARED BADFILE.FLAG, DISPLAY.MODE

DO  'Get file name from user
    LET BADFILE.FLAG = FALSE
    ON ERROR GOTO BadFileName
    OPEN SOURCE$ FOR INPUT AS #1
    ON ERROR GOTO 0
   
    IF SOURCE$ = "" THEN
        EXIT SUB
    ELSEIF BADFILE.FLAG THEN
        CALL Print24(" File not found")
        LET AGAIN$ = GetCorrection$(":", SOURCE$)
        LET SOURCE$ = AGAIN$
        COLOR 15, 1
    END IF
LOOP WHILE BADFILE.FLAG
   
LET LINE.NUM = SRC.LINE(CURRENT.INST)
WHILE NOT EOF(1) AND PROG.LEN < MAXNUM.INST
    LINE INPUT #1, RAW.CODE$        ' Unprocessed source code.
    CALL ProcessLine(RAW.CODE$, TRUE, JUNK%)
    LET DISPLAY.MODE = 1
    CALL RefreshScreen(FALSE)
WEND

IF NOT EOF(1) THEN
    LET ERROR.MESSAGE$ = "The limit on program length is " + STR$(MAXNUM.INST) + " instructions."
    SYSTEM
END IF
CLOSE #1
     
END SUB

SUB MiscDisplay (PLACE)
    LET PLACE = ((PLACE + 359) MOD 3) + 1
    LOCATE 3, 29
    LET X$ = RIGHT$("   " + REGISTER.LIST$(SREG.NUM + 24), 3)
    IF PLACE = 3 THEN
        PRINT " Control    "
    ELSEIF PLACE = 1 THEN
        PRINT " Debug      "
    ELSEIF PLACE = 2 THEN
        PRINT " Test       "
    END IF
    LOCATE 4, 32
    PRINT "Registers";
    IF PLACE = 3 AND ARCHNUM > 1 THEN
        FOR K = 0 TO 4
            LOCATE 6 + 2 * K, 32
            IF K = 0 AND ARCHNUM = 2 THEN
                PRINT "MSW" + SPACE$(6);
                LOCATE 7 + 2 * K, 32
                LET X$ = HexStr(ctrlReg#(K), 8)
                LET X$ = SPACE$(2) + RIGHT$(X$, 4) + SPACE$(2)
                PRINT " " + X$;
            ELSEIF K = 1 OR ARCHNUM = 2 OR (K = 4 AND ARCHNUM < 6) THEN
                PRINT SPACE$(9);
                LOCATE 7 + 2 * K, 32
                PRINT SPACE$(9);
            ELSE
                PRINT CHR$(K + 48) + SPACE$(8);
                LOCATE 7 + 2 * K, 32
                LET X$ = HexStr(ctrlReg#(K), 8)
                PRINT " " + X$;
            END IF
        NEXT
        LOCATE 17, 32
        IF ARCHNUM < 3 THEN
            PRINT " FLAGS:  "
            LOCATE 18, 32
            PRINT SPACE$(5) + RIGHT$(HexStr$(FLAGS#(4), 8), 4);
        ELSE
            PRINT "EFLAGS:  "
            LOCATE 18, 32
            PRINT " " + HexStr$(FLAGS#(4), 8);
        END IF
        LOCATE 19, 32
        PRINT SPACE$(9);
        LOCATE 20, 32
        IF ARCHNUM < 2 THEN
            PRINT SPACE$(9);
        ELSE
            PRINT "CPL: " + CHR$(48 + CPL);
        END IF
        FOR K = 21 TO 23
            LOCATE K, 32
            PRINT SPACE$(9);
        NEXT
    ELSEIF PLACE = 1 AND ARCHNUM > 2 THEN
        FOR K = 0 TO 7
            LOCATE 6 + 2 * K, 32
            IF K = 4 OR K = 5 THEN
                PRINT SPACE$(9);
            ELSE
                PRINT CHR$(K + 48) + SPACE$(8);
            END IF
            LOCATE 7 + 2 * K, 32
            IF K = 4 OR K = 5 THEN
                PRINT SPACE$(9);
            ELSE
                LET X$ = HexStr(debReg#(K), 8)
                PRINT " " + X$;
            END IF
        NEXT
        FOR K = 22 TO 23
            LOCATE K, 30
            PRINT SPACE$(11);
        NEXT
    ELSEIF PLACE = 2 AND ARCHNUM > 2 THEN
        FOR K = 0 TO 7
            LOCATE 6 + 2 * K, 32
            IF (K > 2 AND ARCHNUM > 3) OR K > 5 THEN
                PRINT CHR$(K + 48) + SPACE$(8);
                LOCATE 7 + 2 * K, 32
                LET X$ = HexStr(testReg#(K), 8)
                PRINT " " + X$
            ELSE
                PRINT SPACE$(9);
                LOCATE 7 + 2 * K, 32
                PRINT SPACE$(9);
            END IF
        NEXT
        FOR K = 22 TO 23
            LOCATE K, 32
            PRINT SPACE$(9);
        NEXT
    ELSE
        FOR K = 8 TO 23
            LOCATE K, 32
            PRINT SPACE$(9);
        NEXT
    END IF
END SUB

SUB Print24 (X$)
COLOR 1, 15
LOCATE 24, 1
PRINT LEFT$(X$ + SPACE$(40), 40);
COLOR 15, 1
END SUB

SUB PrintMsg (NUM)
DIM MSG$(7)
    
MSG$(5) = "                Space=Step   Enter=SetIP"  'Default message
MSG$(6) = "                             Enter=SetIP"
MSG$(7) = " Ok! All Done!"
     
LOCATE 24, 1
      IF NUM = 7 THEN
        COLOR 14, 15
      ELSE
        COLOR 1, 15
      END IF
LET X$ = LEFT$(MSG$(NUM) + SPACE$(40), 40)
PRINT X$;
COLOR 15, 1

END SUB

SUB RefreshScreen (SHOW.FLAGS)
SHARED DISPLAY.MODE
REM

IF SCREEN.FLAG THEN
    LET VBUFF# = CDBL(&HB8000)
    DEF SEG = &HB800
    FOR K& = 0& TO 2004&                ' Check the upper value!!
        LET Y# = MemRead#(VBUFF#, 1)
        LET VBUFF# = VBUFF# + 1#
        B% = CINT(Y#)
        POKE K&, B%
    NEXT
    EXIT SUB
END IF
CALL RegDisplay(SHOW.FLAGS)

CALL DisplayCode

IF DISPLAY.MODE = 2 THEN
    CALL DisplayStack
ELSEIF DISPLAY.MODE = 3 THEN
    IF PLACE.MARK(3) < 0 THEN
        LET PLACE.MARK(3) = 0
    END IF
    CALL DisplayMem(CDBL(PLACE.MARK(3)))
ELSEIF DISPLAY.MODE = 4 THEN
    CALL SegmentDisplay
ELSEIF DISPLAY.MODE = 5 THEN
    CALL MiscDisplay(PLACE.MARK(5))
ELSEIF DISPLAY.MODE = 6 THEN
    CALL DisplayEnv(PLACE.MARK(6))
ELSEIF DISPLAY.MODE = 7 THEN
    CALL HelpDisplay(PLACE.MARK(7))
END IF
    
END SUB

SUB RegDisplay (SHOW.FLAGS)
      REM      subroutine to display general registers and 6 Flags
LOCATE 1, 16
PRINT "(" + ARCHNAME$(ARCHNUM) + ")";
FOR I = 0 TO 7
    LOCATE 6 + 2 * I, 5
    IF I > 0 AND I < 4 THEN
        LET J = 1 + ((I + 1) MOD 3)
    ELSE
        LET J = I
    END IF
    LET hex.val$ = genReg(J)
    IF ARCHNUM < 3 THEN
        LET hex.val$ = SPACE$(2) + RIGHT$(hex.val$, 4) + SPACE$(2)
    END IF
    PRINT hex.val$
NEXT
LOCATE 22, 1
      COLOR 6
      PRINT CHR$(244);
      COLOR 10
      IF OF OR SHOW.FLAGS THEN PRINT "o";  ELSE PRINT CHR$(247);
      COLOR 6
      PRINT CHR$(244);
      COLOR 11
      IF DF OR SHOW.FLAGS THEN PRINT "d";  ELSE PRINT CHR$(247);
      COLOR 6
      PRINT CHR$(244);
      COLOR 4
      IF SF OR SHOW.FLAGS THEN PRINT "s";  ELSE PRINT CHR$(247);
      COLOR 6
      PRINT CHR$(244);
      COLOR 3
      IF ZF OR SHOW.FLAGS THEN PRINT "z";  ELSE PRINT CHR$(247);
      COLOR 6
      PRINT CHR$(244);
      COLOR 10
      IF AF OR SHOW.FLAGS THEN PRINT "a";  ELSE PRINT CHR$(247);
      COLOR 6
      PRINT CHR$(244);
      COLOR 3
      IF CF OR SHOW.FLAGS THEN PRINT "c" ELSE PRINT CHR$(247);
      LOCATE 23, 1
      COLOR 6
      FOR K = 1 TO 6
        PRINT CHR$(179) + SPACE$(1);
      NEXT
      COLOR 15, 1
END SUB

SUB ScreenInit

REM             subroutine to initialize the screen
      
      WIDTH 40
      COLOR 15, 1, 1
      CLS
     
      PRINT "  EDLINAS 0.92                          "
      PRINT
      PRINT "General      Source Code                "
      PRINT "Registers    Lbl Mnm Oprnds             "

IF ARCHNUM < 3 THEN
      PRINT
      PRINT " AX "
      PRINT
      PRINT " BX "
      PRINT
      PRINT " CX "
      PRINT
      PRINT " DX "
      PRINT
      PRINT " SP "
      PRINT
      PRINT " BP "
      PRINT
      PRINT " SI "
      PRINT
      PRINT " DI "
      PRINT
ELSE
      PRINT
      PRINT "EAX "
      PRINT
      PRINT "EBX "
      PRINT
      PRINT "ECX "
      PRINT
      PRINT "EDX "
      PRINT
      PRINT "ESP "
      PRINT
      PRINT "EBP "
      PRINT
      PRINT "ESI "
      PRINT
      PRINT "EDI "
      PRINT
END IF
     

    LET N = LEN(A86$)
    IF N < 18 THEN
        LOCATE 1, 23
        PRINT A86$
    ELSEIF N < 40 THEN
        LOCATE 2, 41 - N
        PRINT A86$;
    ELSE
        LOCATE 1, 23
        PRINT "Too long!";
    END IF

    CALL RefreshScreen(FALSE)
END SUB

SUB SegmentDisplay
    IF ARCHNUM < 2 THEN
        LET NUM.REGS% = 4
    ELSE
        LET NUM.REGS% = 10
    END IF
    LET SRNUM = PLACE.MARK(4) + NUM.REGS%
    LET SRNUM = (SRNUM MOD NUM.REGS%)
    IF ARCHNUM = 2 THEN    ' The 286 has no FS or GS registers
        IF SRNUM = 4 THEN
            LET SRNUM = 6
        ELSEIF SRNUM = 5 THEN
            LET SRNUM = 3
        END IF
    END IF
    LET PLACE.MARK(4) = SRNUM
    IF SRNUM < 8 THEN
        LET Y$ = REGISTER.LIST$(SRNUM + 24)
    ELSEIF SRNUM = 8 THEN
        LET Y$ = "GDT"
    ELSEIF SRNUM = 9 THEN
        LET Y$ = "IDT"
    END IF
    LOCATE 3, 28
    LET X$ = LEFT$(Y$ + ":    ", 5)
    PRINT "  Segment    ";
    LOCATE 4, 32
    PRINT "Registers";
    LOCATE 5, 32
    PRINT SPACE$(9);
    LOCATE 6, 32
    PRINT X$
    LOCATE 6, 36
    IF SRNUM < 8 THEN
        PRINT HexStr$(CDBL(segReg(SRNUM)), 4) + " ";
    ELSE
        PRINT SPACE$(5);
    END IF
    LOCATE 7, 32
    PRINT SPACE$(9);

    IF NOT descrVec(SRNUM).Valid THEN
        LOCATE 8, 32
        PRINT "Invalid  "
    ELSE
        LOCATE 8, 32
        PRINT "Base:    "
        LOCATE 9, 32
        PRINT HexStr$(descrVec(SRNUM).BaseAdd, 8) + " ";
        LOCATE 10, 32
        PRINT SPACE$(9);
        LOCATE 11, 32
        PRINT "Limit:   "
        LOCATE 12, 32
        PRINT HexStr$(descrVec(SRNUM).Limit, 8) + " ";
        LOCATE 13, 32
        PRINT SPACE$(9);
        IF SRNUM < 6 THEN
            LOCATE 14, 32
            PRINT "DPL:  " + CHR$(48 + descrVec(SRNUM).DPL) + SPACE$(2);
            LOCATE 15, 32
            PRINT "Type: " + HEX$(descrVec(SRNUM).DPL) + SPACE$(2);
            LOCATE 16, 32
            PRINT "Bits:";
            IF descrVec(SRNUM).DefaultAttr THEN
                PRINT "32  ";
            ELSE
                PRINT "16  ";
            END IF
        ELSE
            FOR K = 14 TO 16
                LOCATE K, 32
                PRINT SPACE$(9);
            NEXT
        END IF
        FOR K = 17 TO 23
            LOCATE K, 31
            PRINT SPACE$(10);
        NEXT

    END IF
END SUB

