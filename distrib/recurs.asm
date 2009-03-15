	MOV esp, 2000h; This is the end of the simulator's memory
	MOV edx, 0; This is the simulator's fictional port number.
	IN eax, dx
	PUSH eax; Our first argument
	IN eax, dx
	PUSH eax; Our second argument
	CALL pks; This is the top call to the recursive function.
	ADD esp, 8; Put the stack pointer back to where it was.
	MOV edx,1
	OUT dx, eax; edx should still hold port number zero.
	RET 
pks	PUSH ebp
	MOV ebp, esp
	ADD ebp, 12; Base pointer now points to arg #1.
	MOV eax, 1
	SUB eax, [ebp]; See if arg#1 is equal to 1
	JNZ non; If it's not try for one other base case.
	SUB ebp, 4; If it is then return arg#2.
	MOV eax, [ebp]; Since the base pointer is now on arg#2 
	JMP bak;   we are now returning arg#2 in eax.
non	MOV eax, [ebp]; Base pointer is still on arg #1
	SUB ebp, 4; Move it to arg #2
	SUB eax, [ebp]; Compare arg#1 and arg#2
	JNZ nen; If they are not equal, then recurse
	MOV eax, 1; If they are equal then return 1.
	JMP bak
nen	ADD ebp, 4; Put the base pointer back to arg#1
	MOV eax, [ebp]; Get ready to recurse with both args
	DEC eax; decremented.
	PUSH eax; Push the new first argument
	SUB ebp, 4; Go for the second one.
	MOV eax, [ebp]; 
	DEC eax; Decrement it also
	PUSH eax; Now both arguments are on the stack
	CALL pks; Recurse!
	ADD esp, 8; Clean the arguments off the stack.
	PUSH eax; Save the result of this recursion on the stack.
	ADD ebp, 4; Change the base pointer from arg #2 to arg #1
	MOV eax, [ebp]; Get ready to recurse with only the
	PUSH eax;       second argument decremented.
	SUB ebp, 4; Go back to arg #2.
	MOV eax, [ebp]; 
	DEC eax; The second argument is now decremented.
	PUSH eax; Now both arguments have been pushed.
	CALL pks; Recurse again!
	ADD esp, 8; Clean off the stack.
	POP ecx; Retrieve the first value returned by the prvious CALL.                                                     
	ADD eax, ecx; The two return values are added here, and the result is where we want it.                
bak	POP ebp; Put back the previous ebp.  
	RET 
