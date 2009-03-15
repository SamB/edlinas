.globl pks       
       pks:     push ebp
                mov ebp, esp
                add ebp, 12     ; Base pointer now points to arg #1.
                mov eax, 1
                sub eax, [ebp]  ; See if arg#1 is equal to 1
                jnz non         ; If it's not try for one other base case.
                sub ebp, 4      ; If it is then return arg#2.
                mov eax, [ebp]  ; Since the base pointer is now on arg#2 
                jmp bak         ;   we are now returning arg#2 in eax.
        non     mov eax, [ebp]  ; Base pointer is still on arg #1
                sub ebp, 4      ; Move it to arg #2
                sub eax, [ebp]  ; Compare arg#1 and arg#2
                jnz nen         ; If they are not equal, then recurse
                mov eax, 1      ; If they are equal then return 1.
                jmp bak                         
        nen     add ebp, 4      ; Put the base pointer back to arg#1
                mov eax, [ebp]  ; Get ready to recurse with both args
                dec eax         ; decremented.
                push eax        ; Push the new first argument
                sub ebp, 4      ; Go for the second one.
                mov eax, [ebp]  ; 
                dec eax         ; Decrement it also
                push eax        ; Now both arguments are on the stack
                call pks       ; Recurse!
                add esp, 8      ; Clean the arguments off the stack.
                push eax        ; Save the result of this recursion on the stack.
                add ebp, 4      ; Change the base pointer from arg #2 to arg #1
                mov eax, [ebp]  ; Get ready to recurse with only the
                push eax        ;       second argument decremented.
                sub ebp, 4      ; Go back to arg #2.
                mov eax, [ebp]  ; 
                dec eax         ; The second argument is now decremented.
                push eax        ; Now both arguments have been pushed.
                call pks       ; Recurse again!
                add esp, 8      ; Clean off the stack.
                pop ecx         ; Retrieve the first value returned by the prvious CALL.                                                     
                add eax, ecx    ; The two return values are added here, and the result is where we want it.                
        bak     pop ebp         ; Put back the previous ebp.  
                ret
