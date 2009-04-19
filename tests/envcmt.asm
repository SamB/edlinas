;;; Test parsing of 'LABEL .= value' with a comment after it

ARA .= 100h
ARR .= 100h                      ; Address of array

        mov byte ptr [100h], 0
        mov byte ptr [ARA], 0
        mov byte ptr [ARR], 0
