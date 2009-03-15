.globl main
LC0:   .db "\nHello world!  %d\n\x0"
main:   push 5
	push LC0
	call printf
	add esp,8
	ret
