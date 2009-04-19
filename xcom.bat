COPY SIM\X86.DAT X86.DAT < UTIL\NIL2
COPY SIM\X86.BI X86.BI < UTIL\NIL2
bc /e/x/o/t/c:512 sim\x86.bas;
bc /e/x/o/t/c:512 sim\keyhelp.bas,kh.obj;
bc /e/x/o/t/c:512 sim\comutil.bas,cu.obj;
bc /e/x/o/t/c:512 sim\alu.bas;
bc /e/x/o/t/c:512 sim\parse.bas;
bc /e/x/o/t/c:512 sim\execute.bas,xx.obj;
bc /e/x/o/t/c:512 sim\exec0f.bas,0f.obj;
bc /e/x/o/t/c:512 sim\segments.bas,sg.obj;
bc /e/x/o/t/c:512 sim\doto.bas;
bc /e/x/o/t/c:512 sim\quasi.bas;
bc /e/x/o/t/c:512 sim\trap.bas;
bc /e/x/o/t/c:512 stufstub.bas,st.obj;
link x86.obj+st.obj+kh.obj+cu.obj+alu.obj+parse.obj+quasi.obj+xx.obj+0f.obj+sg.obj+trap.obj+doto.obj,edl.exe,,qb.lib;
