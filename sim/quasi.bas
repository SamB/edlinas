DECLARE SUB DefCheck (LABEL$, Y$, PACK$, ERM$)
REM $INCLUDE: 'x86.bi'
DECLARE SUB DefRead (LABEL$, Y$)
DECLARE FUNCTION LblMatch$ (LBL$)
DECLARE SUB ExpEval (ARG$, value#, ERM$)
DECLARE SUB Immparse (ARG$, PACKED.SOURCE$, LABEL$, ONE.B!, value#, ERM$)
DECLARE SUB NumParse (RAW.SOURCE$, PACK$, NUM.VALUE#, ERM$)
DECLARE FUNCTION StringCheck! (X$, ERM$)
DECLARE FUNCTION MbCharCheck! (X$, ERM$)
DECLARE FUNCTION MemCheck! (Z$, ERM$, N.BYTES!)
DECLARE SUB StringStore (POINTER#, CODE$)
DECLARE FUNCTION MemStore! (POINTER#, MemExp$, N.BYTES!)
DECLARE SUB ScreenInit ()
DECLARE FUNCTION CUT$ (BBBX$)
DECLARE FUNCTION LCUT$ (BBX$)
DECLARE FUNCTION RCUT$ (BBX$)
BadPath:
LET MEM.PATH$ = ""
RESUME

NoLocks:
LET LOCKS.OK = FALSE
RESUME NEXT

SUB DefCheck (LABEL$, Y$, PACK$, ERM$)
    LET Z = VAL(Y$)
    SELECT CASE UCASE$(LABEL$)
        CASE "ARCH", "INSTSET"
            LET Z = -1
            FOR K = 0 TO ARCH.DIM
                IF ARCHNAME$(K) = Y$ THEN
                    LET Z = K
                END IF
            NEXT
            IF Z = -1 THEN
                LET ERM$ = "Unknown Architecture"
                EXIT SUB
            END IF
        CASE "MEM"
            LET ERM$ = ""
            CALL ExpEval(Y$, value#, ERM$)
        CASE "MEMPATH"
            LET MEM.PATH$ = Y$
            CLOSE #2
            ON ERROR GOTO BadPath 'On failure reverts to Current directory
            OPEN MEM.PATH$ + "Memvec.dat" FOR RANDOM SHARED AS #2 LEN = 4
            LET LOCKS.OK = TRUE      'Test Dos for availability of SHARE
            ON ERROR GOTO NoLocks
            LOCK #2
            ON ERROR GOTO 0
            IF LOCKS.OK THEN UNLOCK #2
        CASE "INSTTIM"
            IF Z > 0 THEN
                LET INSTTIM = Z
            END IF
        CASE "USE"
            IF Z = 16 OR Z = 32 THEN
            ELSE
                LET ERM$ = "Invalid size"
                EXIT SUB
            END IF
        CASE "DOTO"
            IF Y$ = "AOUT" OR Y$ = "ELF" OR Y$ = "OBJ" OR Y$ = "COM" THEN
            ELSE
                LET ERM$ = "Unsupported type"
            END IF
    END SELECT
    IF N.DEFS = ENV.DIM THEN
        LET K = 0
        LET LOC.FOUND = FALSE
        LET MATCH.FOUND = FALSE
        WHILE K < N.DEFS AND NOT LOC.FOUND
            LET K = K + 1
            LET SPOT = INSTR(ENV$(K), ";")
            IF LBL$ <= LEFT$(ENV$(K), SPOT - 1) THEN
                LET LOC.FOUND = TRUE
                IF LBL$ = LEFT$(ENV$(K), SPOT - 1) THEN
                    LET MATCH.FOUND = TRUE
                END IF
            END IF
        WEND
        IF NOT MATCH.FOUND AND CODE$ <> "" THEN
            LET ERM$ = "Env is full"
        END IF
    END IF
END SUB

SUB DefInsert (LBL$, CODE$)

CALL DefRead(LBL$, CODE$)
LET K = 0
LET LOC.FOUND = FALSE
LET MATCH.FOUND = FALSE
WHILE K < N.DEFS AND NOT LOC.FOUND
    LET K = K + 1
    LET SPOT = INSTR(ENV$(K), ";")
    IF LBL$ <= LEFT$(ENV$(K), SPOT - 1) THEN
        LET LOC.FOUND = TRUE
        IF LBL$ = LEFT$(ENV$(K), SPOT - 1) THEN
            LET MATCH.FOUND = TRUE
        END IF
    END IF
WEND
IF MATCH.FOUND AND CODE$ = "" THEN
    LET N.DEFS = N.DEFS - 1
    FOR J = K TO N.DEFS
        LET ENV$(J) = ENV$(J + 1)
    NEXT
ELSEIF MATCH.FOUND THEN
    LET ENV$(K) = LBL$ + ";" + CODE$
ELSEIF CODE$ <> "" AND N.DEFS = ENV.DIM THEN
    REM This is supposed to be caught in DefCheck!
    REM Create an error here.
    RETURN
ELSEIF LOC.FOUND THEN
    FOR J = N.DEFS TO K STEP -1
        LET ENV$(J + 1) = ENV$(J)
    NEXT
    LET N.DEFS = N.DEFS + 1
    LET ENV$(K) = LBL$ + ";" + CODE$
ELSEIF CODE$ <> "" THEN
    LET N.DEFS = N.DEFS + 1
    LET ENV$(N.DEFS) = LBL$ + ";" + CODE$
END IF
END SUB

SUB DefRead (LABEL$, Y$)
REM
REM Set the variables which control the behavior of
REM either the simulator or the assembler.
REM
    LET Z = VAL(Y$)
    SELECT CASE UCASE$(LABEL$)
        CASE "ARCH"
            LET Z = -1
            FOR K = 0 TO ARCH.DIM
                IF ARCHNAME$(K) = Y$ THEN
                    LET Z = K
                END IF
            NEXT
            IF Z > -1 THEN
                LET ARCHNUM = Z
                CALL ScreenInit
            ELSEIF Y$ = "" THEN
                LET ARCHNUM = 3    'the default
            ELSE
                RETURN
            END IF
        CASE "MEM"
            LET ERM$ = ""
            CALL ExpEval(Y$, value#, ERM$)
            IF ERM$ = "" THEN
                LET PLACE.MARK(3) = FIX(value# / 2#)
            END IF
        CASE "MEMPATH"
            LET MEM.PATH$ = Y$
            CLOSE #2
            ON ERROR GOTO BadPath 'On failure reverts to Current directory
            OPEN MEM.PATH$ + "Memvec.dat" FOR RANDOM SHARED AS #2 LEN = 4
            LET LOCKS.OK = TRUE      'Test Dos for availability of SHARE
            ON ERROR GOTO NoLocks
            LOCK #2
            ON ERROR GOTO 0
            IF LOCKS.OK THEN UNLOCK #2
        CASE "INSTTIM"
            IF Z > 0 THEN
                LET INSTTIM = Z
            ELSEIF Y$ = "" THEN
                LET INSTTIM = 1.5
            END IF
        CASE "USE"
            IF Z = 16 OR Z = 32 THEN
                LET USE.SIZE = Z
            ELSEIF Y$ = "" THEN
                LET USE.SIZE = 32
            ELSE
                RETURN
            END IF
        CASE "DOTO"
            IF Y$ = "ELF" THEN
                LET USE.O = 3
            ELSEIF Y$ = "AOUT" THEN
                LET USE.O = 2
            ELSEIF Y$ = "COM" THEN
                LET USE.O = 4
            END IF

        CASE "INSTSET"
            LET Z = -1
            FOR K = 0 TO ARCH.DIM
                IF ARCHNAME$(K) = Y$ THEN
                    LET Z = K
                END IF
            NEXT
            IF Z > -1 THEN
                LET USE.INSTSET = Z
                CALL ScreenInit
            ELSE
                RETURN
            END IF
    END SELECT
END SUB

SUB DirectiveParse (LABEL$, CODE$, INDEX, PACK$, N.HX, ERM$)
LET PACK$ = CODE$
IF INDEX = -1 OR INDEX = -2 THEN
    CALL DefCheck(LABEL$, CODE$, PACK$, ERM$)
ELSEIF INDEX = -4 THEN
    LET PACK$ = LTRIM$(RTRIM$(CODE$))
    IF PACK$ = "" THEN
        LET ERM$ = "Symbol missing"
    END IF
    LET N.HX = 0
ELSEIF INDEX = -7 THEN
    LET N.HX = MemCheck(CODE$, ERM$, 1)
ELSEIF INDEX = -8 THEN
    LET N.HX = MemCheck(CODE$, ERM$, 2)
ELSEIF INDEX = -9 THEN
    LET N.HX = MemCheck(CODE$, ERM$, 4)
END IF
END SUB

SUB DisplayEnv (PLACE)
IF N.DEFS > 0 THEN
    LET PLACE = (PLACE + 7 * 360 - 1) MOD N.DEFS + 1
END IF

LOCATE 3, 28
PRINT "  Definitions";
LOCATE 4, 32
PRINT "         ";

    LET J = 6
    IF PLACE > 0 THEN
        LET K = PLACE
    ELSE
        LET K = 1
    END IF
    DO
        IF K <= N.DEFS THEN
            LET X$ = ENV$(K)
            LET SPOT = INSTR(X$, ";")
            LET PART1$ = LEFT$(X$, SPOT - 1)
            LET PART1L$ = LEFT$(PART1$ + SPACE$(13), 11)
            LET PART2$ = MID$(X$, SPOT + 1)
            LET PART2L$ = LEFT$(SPACE$(1) + PART2$ + SPACE$(13), 11)
            IF LEN(PART1$ + PART2$) < 7 THEN
                LET PART1S$ = PART1$ + "=" + PART2$ + SPACE$(10)
                LET PART1S$ = LEFT$(PART1S$, 9)
                LET NEW.LINES = 1
            ELSE
                LET PART1L$ = LEFT$(PART1L$, 8) + SPACE$(7)
                LET PART1L$ = LEFT$(PART1L$, 9)
                LET PART2L$ = LEFT$(SPACE$(1) + "=" + PART2L$ + SPACE$(7), 9)
                LET NEW.LINES = 2
            END IF
        ELSE
            LET NEW.LINES = 1
            LET PART1S$ = SPACE$(9)
        END IF
        IF NEW.LINES = 2 THEN
            IF J < 23 THEN
                LOCATE J, 32
                PRINT PART1L$;
                LET J = J + 1
                LOCATE J, 32
                PRINT PART2L$;
                LET J = J + 1
            END IF
        ELSEIF NEW.LINES = 1 THEN
            IF J < 24 THEN
                LOCATE J, 32
                PRINT PART1S$;
                LET J = J + 1
            END IF
        END IF
        IF J < 24 THEN
            LOCATE J, 32
            PRINT SPACE$(9);
            LET J = J + 1
        END IF
        LET K = K + 1
    LOOP WHILE J < 24

         

END SUB

SUB DoQuasi (INDEX)
        SELECT CASE INDEX
            CASE -10
                LET USE.INSTSET = 4
            CASE -11
                LET USE.INSTSET = 3
            CASE -12
                LET USE.INSTSET = 2
            CASE -13
                LET USE.INSTSET = 0
        END SELECT
END SUB

SUB ExpEval (ARG$, value#, ERM$)
REM Initialize the operator set.
DIM PREC(10)
DIM LEFT.ASSOC(10)
LET PREC(3) = 1     '+
LET LEFT.ASSOC(3) = TRUE
LET PREC(5) = 2     '*
LET LEFT.ASSOC(3) = TRUE
LET PREC(2) = 1     '-
LET LEFT.ASSOC(3) = TRUE

LET N = LEN(ARG$)
LET PAREN.DEPTH = 0
LET ARG.BEGIN = 0
LET OP.SPOT = 0
LET PRECEDENCE.MIN = 100
LET PARENS.OCCUR = FALSE
FOR K = 1 TO N
    LET C = ASC(MID$(ARG$, K, 1))
    IF C = 40 THEN             '(
        IF ARG.BEGIN = 0 THEN
            LET ARG.BEGIN = K
        END IF
        LET PAREN.DEPTH = PAREN.DEPTH + 1
        LET PARENS.OCCUR = TRUE
    ELSEIF C = 41 THEN         ')
        LET PAREN.DEPTH = PAREN.DEPTH - 1
        LET LAST.RIGHT = K
    END IF

    IF PAREN.DEPTH = 0 THEN
        IF C = 43 OR C = 45 OR C = 42 THEN        '+, * , -
            IF PREC(C - 40) < PRECEDENCE.MIN THEN
                LET PRECEDENCE.MAX = PREC(C - 40)
                LET OP.SPOT = K
                LET OP.ASC = C
            ELSEIF PREC(C - 40) = PRECEDENCE.MIN AND LEFT.ASSOC(C - 40) THEN
                LET OP.SPOT = K
                LET OP.ASC = C
            END IF
        ELSEIF ARG.BEGIN = 0 AND NOT (C = 32 OR C = 9) THEN   'Space or Tab
            LET ARG.BEGIN = K
        END IF
    END IF
NEXT
LET Y$ = LblMatch$(ARG$)
   
LET K1 = INSTR(ARG$, "(")
IF PAREN.DEPTH <> 0 THEN
    LET ERM$ = "Bad () s"
    REM EXIT DEF
ELSEIF PARENS.OCCUR AND OP.SPOT = 0 THEN
    IF K1 > LAST.RIGHT - 2 THEN
        LET ERM$ = "Bad () s"
        EXIT SUB
    ELSE
        LET X$ = MID$(ARG$, K1 + 1, LAST.RIGHT - K1 - 1)
        LET X$ = LCUT$(X$)
        LET X$ = RCUT$(X$)
        CALL ExpEval(X$, value#, ERM$)
    END IF
ELSEIF OP.SPOT > 0 AND ARG.BEGIN < OP.SPOT THEN
    LET X$ = LEFT$(ARG$, OP.SPOT - 1)
    LET X$ = LCUT$(X$)
    LET X$ = RCUT$(X$)
    LET Y$ = MID$(ARG$, OP.SPOT + 1)
    CALL ExpEval(X$, valuex#, ERM$)
    IF ERM$ <> "" THEN
        EXIT SUB
    END IF
    LET Y$ = LCUT$(Y$)
    LET Y$ = RCUT$(Y$)
    CALL ExpEval(Y$, valuey#, ERM$)
    IF ERM$ <> "" THEN
        EXIT SUB
    END IF
    LET OP.ASC = ASC(MID$(ARG$, OP.SPOT, 1))

    SELECT CASE OP.ASC
        CASE 43   '+
            LET value# = valuex# + valuey#
        CASE 45
            LET value# = valuex# - valuey#
        CASE 42
            LET value# = valuex# * valuey#
    END SELECT
    LET PACK$ = PACKX$ + PACKY$
ELSEIF OP.SPOT > 0 THEN
    LET Y$ = MID$(ARG$, OP.SPOT + 1)
    CALL ExpEval(Y$, valuey#, ERM$)
    SELECT CASE OP.ASC
        CASE 45  '-
            LET value# = -valuey#
    END SELECT
    LET PACK$ = Y$
ELSEIF ARG$ >= "0" AND ARG$ < ":" THEN
    CALL NumParse(ARG$, PACKED.SOURCE$, value#, ERM$)
    LET LABEL$ = ""
    IF ERM$ <> "" THEN
        LET ONE.B = -1
    ELSEIF value# < -2# ^ 31 OR value# >= 2# ^ 32 THEN
        LET ERM$ = "Exceeds 32 bits!"
    ELSEIF value# < -128# OR value# > 255# THEN
        LET ONE.B = 0
        LET ARG$ = PACKED.SOURCE$
    ELSE
        LET ONE.B = 1
        LET ARG$ = PACKED.SOURCE$
    END IF
ELSEIF Y$ <> "" THEN
    CALL ExpEval(Y$, value#, ERM$)
ELSE
    LET ERM$ = "Label?"
END IF


END SUB

SUB Immparse (ARG$, PACKED.SOURCE$, LABEL$, ONE.B, value#, ERM$)
REM
REM ONE.B = -1 for no value, 1 for 1 byte, 0 otherwise
REM
    LET X$ = ARG$
    LET LABEL$ = ""
    LET value# = 0#
    LET ONE.B = 0
    DO
        LET SPOT = INSTR(X$, "+")
        IF SPOT > 1 THEN
            LET Y$ = LEFT$(X$, SPOT - 1)
            LET Y$ = LTRIM$(RTRIM$(Y$))
            LET X$ = MID$(X$, SPOT + 1)
            LET X$ = LTRIM$(RTRIM$(X$))
        ELSE
            LET Y$ = X$
            LET X$ = ""
        END IF
        LET ERM$ = ""
        CALL ExpEval(Y$, v#, ERM$)
        IF LABEL$ <> "" AND ERM$ = "Label?" THEN
            ERM$ = "One label only!"
            EXIT SUB
        ELSEIF ERM$ = "Label?" THEN
            LET LABEL$ = Y$
        ELSEIF ERM$ <> "" THEN
            EXIT SUB
        ELSE
            LET value# = value# + v#
        END IF
    LOOP WHILE X$ <> ""
    LET PACKED.SOURCE$ = ARG$
    IF LABEL$ <> "" THEN
        EXIT SUB
    END IF

    IF value# < -2# ^ 31 OR value# >= 2# ^ 32 THEN
        LET ERM$ = "Exceeds 32 bits!"
        LET ONE.B = -1
    ELSEIF value# < -128# OR value# > 255# THEN
        LET ONE.B = 0
        LET LABEL$ = ""
    ELSE
        LET ONE.B = 1
        LET LABEL$ = ""
    END IF

END SUB

FUNCTION LblMatch$ (LBL$)
REM
REM Returns the definiendum in ENV$() of LBL$ if there is one.
REM If there is not it returns the empty string.
REM
LET K = 0
LET LOC.FOUND = FALSE
WHILE K < N.DEFS AND NOT LOC.FOUND
    LET K = K + 1
    LET SPOT = INSTR(ENV$(K), ";")
    IF LBL$ <= LEFT$(ENV$(K), SPOT - 1) THEN
        LET LOC.FOUND = TRUE
    END IF
WEND
IF LOC.FOUND THEN
    IF LBL$ = LEFT$(ENV$(K), SPOT - 1) THEN
        LET X$ = MID$(ENV$(K), SPOT + 1)
    ELSE
        LET X$ = ""
    END IF
ELSE
    LET X$ = ""
END IF
LET LblMatch$ = X$
END FUNCTION

FUNCTION MbCharCheck (X$, ERM$)
LET MbCharCheck = LEN(X$)
END FUNCTION

FUNCTION MemCheck (Z$, ERM$, N.BYTES)
LET X$ = CUT$(Z$)
LET K = 1
LET N = LEN(X$)
LET PAREN.DEPTH = 0
LET IN.QUOTES = FALSE
LET IN.DOUBLES = FALSE
LET IN.SINGLES = FALSE
LET PLUS.SPOT = 0
LET TIMES.SPOT = 0
WHILE K < N AND PLUS.SPOT = 0
    LET C$ = MID$(X$, K, 1)
    IF NOT IN.QUOTES AND C$ = "(" THEN
        LET PAREN.DEPTH = PAREN.DEPTH + 1
    ELSEIF NOT IN.QUOTES AND C$ = ")" THEN
        LET PAREN.DEPTH = PAREN.DEPTH - 1
    ELSEIF NOT IN.QUOTES AND C$ = CHR$(34) THEN
        LET IN.QUOTES = TRUE
        LET IN.DOUBLES = TRUE
    ELSEIF NOT IN.QUOTES AND C$ = CHR$(39) THEN
        LET IN.QUOTES = TRUE
        LET IN.SINGLES = TRUE
    ELSEIF IN.DOUBLES AND C$ = CHR$(34) AND D$ <> "\" THEN
        LET IN.QUOTES = FALSE
        LET IN.DOUBLES = FALSE
    ELSEIF NOT IN.QUOTES AND PAREN.DEPTH = 0 AND (C$ = "+" OR C$ = ",") THEN
        LET PLUS.SPOT = K
    ELSEIF NOT IN.QUOTES AND PAREN.DEPTH = 0 AND C$ = "*" THEN
        LET TIMES.SPOT = K
    END IF
    LET D$ = C$
    LET K = K + 1
WEND
IF PLUS.SPOT > 0 THEN
    IF 1 = PLUS.SPOT OR PLUS.SPOT = N THEN
        LET ERM$ = "Invalid Concat"
    ELSE
        LET a$ = LEFT$(X$, PLUS.SPOT - 1)
        LET a$ = CUT$(a$)
        LET B$ = MID$(X$, PLUS.SPOT + 1)
        LET B$ = CUT$(B$)
        LET RETVAL = MemCheck(a$, ERM$, N.BYTES)
        IF ERM$ = "" THEN
            LET RETVAL = RETVAL + MemCheck(B$, ERM$, N.BYTES)
        END IF
    END IF
ELSEIF TIMES.SPOT > 0 THEN
    IF 1 = TIMES.SPOT OR TIMES.SPOT = N THEN
        LET ERM$ = "Invalid Repeat"
    ELSE
        LET a$ = LEFT$(X$, TIMES.SPOT - 1)
        LET B$ = MID$(X$, TIMES.SPOT + 1)
        LET a$ = CUT$(a$)
        LET B$ = CUT$(B$)
        CALL Immparse(a$, PS$, LB$, OB, value#, ERM$)
        IF ERM$ <> "" THEN
            EXIT FUNCTION
        ELSEIF LB$ <> "" THEN
            LET ERM$ = LB$ + " undefined"
            EXIT FUNCTION
        ELSE
            LET RETVAL = MemCheck(B$, ERM$, N.BYTES)
        END IF
 
        LET RETVAL = RETVAL * value#

    END IF
ELSEIF LEFT$(X$, 1) = "(" THEN
    IF ERM$ = "" THEN
        LET X$ = RTRIM$(X$)
        IF RIGHT$(X$, 1) <> ")" THEN
            LET ERM$ = "Bad parens"
        ELSE
            LET Y$ = MID$(X$, 2, LEN(X$) - 2)
            LET RETVAL = MemCheck(Y$, ERM$, N.BYTES)
        END IF
    END IF
ELSEIF LEFT$(X$, 1) = "?" THEN
    LET X$ = RTRIM$(X$)
    IF X$ <> "?" THEN
        LET ERM$ = "? means 1 byte"
    END IF
    LET RETVAL = N.BYTES
ELSEIF LEFT$(X$, 1) = CHR$(39) THEN
    LET X$ = RTRIM$(X$)
    IF RIGHT$(X$, 1) <> CHR$(39) THEN
        LET ERM$ = "Unbalanced quotes"
    END IF
    LET X$ = MID$(X$, 2, LEN(X$) - 2)
    LET RETVAL = MbCharCheck(X$, ERM$)
ELSEIF LEFT$(X$, 1) = CHR$(34) THEN
    LET X$ = RTRIM$(X$)
    IF RIGHT$(X$, 1) <> CHR$(34) THEN
        LET ERM$ = "Unbalanced quotes"
    END IF
    LET X$ = MID$(X$, 2, LEN(X$) - 2)
    LET RETVAL = StringCheck(X$, ERM$)
ELSE
    LET X$ = RTRIM$(X$)
    CALL ExpEval(X$, value#, ERM$)
    LET RETVAL = N.BYTES
END IF
LET MemCheck = RETVAL

END FUNCTION

FUNCTION MemStore (POINTER#, MemExp$, N.BYTES)
LET N = LEN(MemExp$)
LET K = 1
LET PAREN.DEPTH = 0
LET IN.QUOTES = FALSE
LET IN.SINGLES = FALSE
LET IN.DOUBLES = FALSE
LET PLUS.SPOT = 0
LET TIMES.SPOT = 0
WHILE K < N AND PLUS.SPOT = 0
    LET C$ = MID$(MemExp$, K, 1)
    IF NOT IN.QUOTES AND C$ = "(" THEN
        LET PAREN.DEPTH = PAREN.DEPTH + 1
    ELSEIF NOT IN.QUOTES AND C$ = ")" THEN
        LET PAREN.DEPTH = PAREN.DEPTH - 1
    ELSEIF NOT IN.QUOTES AND C$ = CHR$(34) THEN
        LET IN.QUOTES = TRUE
        LET IN.DOUBLES = TRUE
    ELSEIF NOT IN.QUOTES AND C$ = CHR$(39) THEN
        LET IN.QUOTES = TRUE
        LET IN.SINGLES = TRUE
    ELSEIF IN.DOUBLES AND C$ = CHR$(34) AND D$ <> "\" THEN
        LET IN.QUOTES = FALSE
        LET IN.DOUBLES = FALSE
    ELSEIF IN.SINGLES AND C$ = CHR$(39) AND D$ <> "\" THEN
        LET IN.QUOTES = FALSE
        LET IN.SINGLES = FALSE
    ELSEIF NOT IN.QUOTES AND PAREN.DEPTH = 0 AND (C$ = "+" OR C$ = ",") THEN
        LET PLUS.SPOT = K
    ELSEIF NOT IN.QUOTES AND PAREN.DEPTH = 0 AND C$ = "*" THEN
        LET TIMES.SPOT = K
    END IF
    LET D$ = C$
    LET K = K + 1
WEND
IF PLUS.SPOT > 0 THEN
    LET X$ = RTRIM$(LEFT$(MemExp$, PLUS.SPOT - 1))
    LET Y$ = LTRIM$(MID$(MemExp$, PLUS.SPOT + 1))
    LET N = MemStore(POINTER#, X$, N.BYTES)
    LET POINTER2# = POINTER# + N
    LET M = MemStore(POINTER2#, Y$, N.BYTES)
    LET RETVAL = M + N
ELSEIF TIMES.SPOT > 0 THEN
    LET X$ = LEFT$(MemExp$, TIMES.SPOT - 1)
    LET Y$ = MID$(MemExp$, TIMES.SPOT + 1)
    LET X$ = LTRIM$(RTRIM$(X$))
    LET Y$ = LTRIM$(RTRIM$(Y$))
    CALL ExpEval(X$, value#, ERM$)
    LET N = value#
    LET POINTER2# = POINTER#
    FOR K = 1 TO N
        LET M = MemCheck(Y$, ERRR$, N.BYTES)
        LET MM = MemStore(POINTER2#, Y$, N.BYTES)
        LET POINTER2# = POINTER2# + M
    NEXT
    LET RETVAL = POINTER2# - POINTER#
ELSEIF LEFT$(LTRIM$(MemExp$), 1) = "(" THEN
    LET X$ = LTRIM$(RTRIM$(MemExp$))
    LET Y$ = MID$(X$, 2, LEN(X$) - 2)
    LET M = MemStore(POINTER#, Y$, N.BYTES)
    LET RETVAL = M
ELSEIF LEFT$(LTRIM$(MemExp$), 1) = CHR$(34) THEN
    LET X$ = LTRIM$(RTRIM$(MemExp$))
    LET Y$ = MID$(X$, 2, LEN(X$) - 2)
    CALL StringStore(POINTER#, Y$)
    LET RETVAL = M
ELSEIF LEFT$(LTRIM$(MemExp$), 1) = "?" THEN
    LET RETVAL = N.BYTES
ELSE
    LET MemExp$ = RTRIM$(MemExp$)
    CALL ExpEval(MemExp$, value#, ERM$)
    CALL LA.write(POINTER#, value#, -N.BYTES)
    LET RETVAL = N.BYTES
END IF
LET MemStore = RETVAL
END FUNCTION

SUB NumParse (RAW.SOURCE$, PACK$, NUM.VALUE#, ERM$)

     REM        Number finding subroutine

     REM                Return a double precision real which is within
     REM                the range of valid unsigned 32 bit integers.

     REM                        HEX$ works on these numbers.

    LET HEX.VALUE# = 0
    LET DEC.VALUE# = 0
    IF LEFT$(RAW.SOURCE$, 1) = "-" THEN
        LET END.POS = 1
        LET MFLAG = TRUE
    ELSE
        LET END.POS = 0
        LET MFLAG = FALSE
    END IF

    LET N = LEN(RAW.SOURCE$)
    DO
        LET END.POS = END.POS + 1
        LET T = ASC(MID$(RAW.SOURCE$, END.POS, 1))
        IF T >= ASC("a") AND T <= ASC("f") THEN LET T = T - 32
                IF T < ASC("A") AND T > ASC("9") THEN LET T = T + 100
                IF T >= ASC("A") AND T <= ASC("F") THEN
                    LET HEX.VALUE# = 16# * HEX.VALUE# + CDBL(T - 55)
                    LET DEC.VALUE# = -1#
                ELSEIF T >= ASC("0") AND T <= ASC("9") THEN
                    LET HEX.VALUE# = 16# * HEX.VALUE# + CDBL(T - 48)
                    LET DEC.VALUE# = 10# * DEC.VALUE# + CDBL(T - 48)
                END IF
          LOOP WHILE END.POS < N AND T >= ASC("0") AND T <= ASC("F")
          LET PACK$ = UCASE$(LEFT$(RAW.SOURCE$, END.POS - 1))
          LET RAW.SOURCE$ = MID$(RAW.SOURCE$, END.POS)
          IF T >= ASC("0") AND T <= ASC("F") THEN
            IF DEC.VALUE# < 0# THEN
                LET NUM.VALUE# = HEX.VALUE#
            ELSE
                LET NUM.VALUE# = DEC.VALUE#
            END IF
            LET PACK$ = PACK$ + CHR$(T)
            LET RAW.SOURCE$ = MID$(RAW.SOURCE$, 2)
          ELSEIF (T = ASC("H") OR T = ASC("h")) AND N = END.POS THEN
            LET NUM.VALUE# = HEX.VALUE#
            LET PACK$ = PACK$ + CHR$(T)
            LET RAW.SOURCE$ = MID$(RAW.SOURCE$, 2)
          ELSE
            LET ERM$ = MID$(RAW.SOURCE$, END.POS, 1) + " in num?"
          END IF
          IF MFLAG THEN LET NUM.VALUE# = -NUM.VALUE#
      
END SUB

FUNCTION StringCheck (X$, ERM$)
LET K = 0
LET BYTE.COUNT = 0
LET N = LEN(X$)
LET STATE = 1   ' 1 Normal, 2 Slash, 3 Hex, 4 Octal, 5 Catchup
WHILE K < N OR STATE = 5
    IF STATE = 5 THEN
        LET STATE = 1
        LET BYTE.COUNT = BYTE.COUNT + 1
    ELSE
        LET K = K + 1
    END IF
    LET C$ = MID$(X$, K, 1)
    IF STATE = 1 THEN
        IF C$ = "\" THEN
            LET STATE = 2
        ELSEIF C$ = CHR$(34) THEN
            LET ERM$ = "Needs escape"
        END IF
    ELSEIF STATE = 2 THEN
        SELECT CASE C$
        CASE "\", "'", "?", "a", "r", "t", "f", "v", "b", "n", CHR$(34)
            LET STATE = 1
        CASE "x"
            LET STATE = 3
        CASE "0", "1", "2", "3", "4", "5", "6", "7"
            LET STATE = 4
        CASE ELSE
            LET ERM$ = "Invalid escape"
        END SELECT
    ELSEIF STATE = 3 THEN
        IF (C$ >= "0" AND C$ <= "9") OR (C$ >= "A" AND C$ <= "F") OR (C$ >= "a" AND C$ <= "f") THEN
        ELSE
            LET STATE = 5
        END IF
    ELSEIF STATE = 4 THEN
        IF C$ >= "0" AND C$ <= "7" THEN
        ELSE
            LET STATE = 5
        END IF
    END IF
    IF STATE = 1 THEN
        LET BYTE.COUNT = BYTE.COUNT + 1
    END IF
WEND
LET StringCheck = BYTE.COUNT
END FUNCTION

SUB StringStore (POINTER#, CODE$)
LET K = 0
LET X$ = CODE$
LET NUM.VAL = 0
LET N = LEN(X$)
LET P# = POINTER#
LET STATE = 1   ' 1 Normal, 2 Slash, 3 Hex, 4 Octal, 5 Catchup
WHILE K < N OR STATE = 5
    IF STATE = 5 THEN
        LET STATE = 1
        CALL LA.write(P#, CDBL(NUM.VAL), 1)
        LET P# = P# + 1
        LET NUM.VAL = 0
    ELSE
        LET K = K + 1
    END IF
    LET C$ = MID$(X$, K, 1)
    IF STATE = 1 THEN
        IF C$ = "\" THEN
            LET STATE = 2
        ELSE
            LET CHAR.VAL = ASC(C$)
        END IF
    ELSEIF STATE = 2 THEN
        LET STATE = 1
        SELECT CASE C$
        CASE "\"
            LET CHAR.VAL = ASC("\")
        CASE "?"
            LET CHAR.VAL = ASC("?")
        CASE "'"
            LET CHAR.VAL = ASC("'")
        CASE CHR$(34)
            LET CHAR.VAL = 34
        CASE "a"
            LET CHAR.VAL = 7
        CASE "r"
            LET CHAR.VAL = 13
        CASE "t"
            LET CHAR.VAL = 9
        CASE "f"
            LET CHAR.VAL = 12
        CASE "n"
            LET CHAR.VAL = 10
        CASE "v"
            LET CHAR.VAL = 11
        CASE "b"
            LET CHAR.VAL = 8
        CASE "x"
            LET STATE = 3
        CASE "0", "1", "2", "3", "4", "5", "6", "7"
            LET NUM.VAL = ASC(C$) - 48
            LET STATE = 4
        END SELECT
    ELSEIF STATE = 3 THEN
        IF C$ >= "0" AND C$ <= "9" THEN
            LET NUM.VAL = 16 * NUM.VAL + ASC(C$) - 48
        ELSEIF C$ >= "A" AND C$ <= "F" THEN
            LET NUM.VAL = 16 * NUM.VAL + ASC(C$) - 55
        ELSEIF C$ >= "a" AND C$ <= "f" THEN
            LET NUM.VAL = 16 * NUM.VAL + ASC(C$) - 87
        ELSE
            LET STATE = 5
        END IF
    ELSEIF STATE = 4 THEN
        IF C$ >= "0" AND C$ <= "7" THEN
            LET NUM.VAL = NUM.VAL * 8 + ASC(C$) - 48
        ELSE
            LET STATE = 5
        END IF
    END IF
    IF STATE = 1 THEN
        CALL LA.write(P#, CDBL(CHAR.VAL), 1)
        LET P# = P# + 1#
    END IF
WEND
END SUB

