
;;; If JNC is not working correctly, this program will output 666.
;;; If JNC is working correctly, this program will just halt.

        CLC
        JNC end
        MOV ax, 666
        OUT [1], ax
end:    HLT
