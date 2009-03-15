REM $INCLUDE: 'x86.bi'
'***
' QB.BI - Assembly Support Include File
'
'       Copyright <C> 1987 Microsoft Corporation
'
' Purpose:
'      This include file defines the types and gives the DECLARE
'       statements for the assembly language routines ABSOLUTE,
'       INTERRUPT, INTERRUPTX, INT86OLD, and INT86XOLD.
'
'***************************************************************************
'
' Define the type needed for INTERRUPT
'
TYPE RegType
	 axx    AS INTEGER
	 bxx    AS INTEGER
	 cxx    AS INTEGER
	 dxx    AS INTEGER
	 bpp    AS INTEGER
	 sii    AS INTEGER
	 dii    AS INTEGER
	 flagss AS INTEGER
END TYPE
'
'
' Generate a software interrupt, loading all but the segment registers
'
DECLARE SUB INTERRUPT (intnum AS INTEGER, inreg AS RegType, outreg AS RegType)
'
' Generate a software interrupt, loading all registers
'
'
' Call a routine at an absolute address.
' NOTE: If the routine called takes parameters, then they will have to

SUB STUFF (Z$)

DIM passreg AS RegType
DIM dummyreg AS RegType


LET N = LEN(Z$)
FOR K = 1 TO N

LET passreg.axx = &H500
LET passreg.cxx = ASC(MID$(Z$, K, 1))

CALL INTERRUPT(&H16, passreg, dummyreg)

NEXT

END SUB

