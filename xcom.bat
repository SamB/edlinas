COPY SIM\X86.DAT X86.DAT < \UTIL\NIL2
COPY SIM\X86.BI X86.BI < \UTIL\NIL2
bc /e/x/o/t/c:512 sim\x86.bas < \util\nil2 
bc /e/x/o/t/c:512 sim\keyhelp.bas < kh.nil 
bc /e/x/o/t/c:512 sim\comutil.bas < cu.nil
bc /e/x/o/t/c:512 sim\alu.bas < \util\nil2 
bc /e/x/o/t/c:512 sim\parse.bas < \util\nil2 
bc /e/x/o/t/c:512 sim\execute.bas < xx.nil 
bc /e/x/o/t/c:512 sim\exec0f.bas < 0f.nil 
bc /e/x/o/t/c:512 sim\segments.bas < sg.nil 
bc /e/x/o/t/c:512 sim\doto.bas < \util\nil2 
bc /e/x/o/t/c:512 sim\quasi.bas < \util\nil2 
bc /e/x/o/t/c:512 sim\trap.bas < \util\nil2 
bc /e/x/o/t/c:512 stufstub.bas < st.nil
link x86.obj st.obj kh.obj cu.obj alu.obj parse.obj quasi.obj xx.obj 0f.obj sg.obj trap.obj doto.obj < link.in

