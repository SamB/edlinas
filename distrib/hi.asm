	.rodata 
	.db "ABC"
LC0:      
	.db "hello, %d\n\n"
	.data        
LC1:    .db "goodbye, %d\n\n\0"
	.text 
	.globl main
main     
	PUSH ebp
	MOV ebp,esp
	PUSH 5
	PUSH LC0
	CALL printf
	ADD esp,8
	PUSH 2
	PUSH LC1
	CALL printf
	ADD esp,8
	MOV esp, ebp
	POP ebp
	RET 
