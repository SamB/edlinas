DECLARE FUNCTION LEread& (N!, FILENUM!)
DECLARE FUNCTION HexRead$ (N!, FILENUM!)

REM/*
REM/*    This is a .o file parsing program.
REM/*    It parses a.out and ELF ".o" files.
REM/*
REM*/

'    int obfile,i, j ,k, g.strtab, f.strtab;
'    int c, d, a.text, a.data, a.bss, a.syms, a.entry, a.trsize, a.drsize;
'    int e.type, e.machine, e.version, e.entry, e.phoff, e.shoff, e.flags;
'    int e.ehsize, e.phentsize, e.phnum, e.shentsize, e.shnum, e.shstrndx;
'    int st.name , st.value, st.size, st.info, st.other, st.shndx;
'    int r.info, r.offset, r.addend;
'    int shtab[MAX.shnum][10],symtnd[100];
'    char s[100], t[100], symtab[100][100];
'   
'    int offset, a.strings;
'    obfile = open(argv[1], O.RDONLY);
	LET X$ = COMMAND$
	OPEN X$ FOR BINARY AS #1
	LET C& = LEread&(2, 1)

IF C& = &H107 THEN
	LET C& = LEread&(2, 1)
	IF (C& > 0) THEN
		PRINT " Miscellaneous = "; HEX$(C&)
	END IF
  
	LET C& = LEread&(4, 1)
	IF (C& > 0) THEN
		PRINT " Text size = "; HEX$(C&)
	END IF
	LET a.text = C&
  
	LET C& = LEread&(4, 1)
	IF (C& > 0) THEN
		PRINT " Data size = "; HEX$(C&)
	END IF
	LET a.data = C&
   
	LET C& = LEread&(4, 1)
	IF (C& > 0) THEN
		PRINT " Bss size = "; HEX$(C&)
	END IF
	LET a.bss = C&

	LET C& = LEread&(4, 1)
	IF (C& > 0) THEN
		PRINT "Symbol Table size = "; HEX$(C&)
	END IF
	LET a.syms = C&

	LET C& = LEread&(4, 1)
	IF (C& > 0) THEN
		PRINT "Entry point = "; HEX$(C&)
	END IF
	LET a.entry = C&
   
	LET C& = LEread&(4, 1)
	IF (C& > 0) THEN
		PRINT "Text rellocation table size = "; HEX$(C&)
	END IF
	LET a.trsize = C&

	LET C& = LEread&(4, 1)
	IF (C& > 0) THEN
		PRINT "Data rellocation table size = "; HEX$(C&)
	END IF
	LET a.drsize = C&

	LET offset = 32
	PRINT
	PRINT
	PRINT " Text:"
	LET i = 0
	WHILE (i < a.text)

		IF (i MOD 16 = 0) THEN
			LET X$ = RIGHT$(SPACE$(13) + HEX$(offset + i), 13) + ":"
			PRINT
			PRINT X$;
		END IF

		IF (i MOD 4 = 0) THEN
			PRINT SPACE$(2);
		END IF
		LET BYTE$ = HexRead$(1, 1)
		PRINT BYTE$;
		LET i = i + 1
   
	WEND
   
	IF a.data > 0 THEN
		PRINT
		PRINT
		PRINT " Data:"
	END IF
	LET i = 0
	WHILE (i < a.data)

		IF (i MOD 16 = 0) THEN
			LET X$ = RIGHT$(SPACE$(13) + HEX$(offset + i), 13) + ":"
			PRINT
			PRINT X$;
		END IF

		IF (i MOD 4 = 0) THEN
			PRINT SPACE$(2);
		END IF
		LET BYTE$ = HexRead$(1, 1)
		PRINT BYTE$;
		LET i = i + 1
  
	WEND
 
	LET offset = offset + a.data
   
   
	IF a.trsize > 0 THEN
		PRINT
		PRINT
		PRINT " Text Relocation Table:"
	END IF
	LET i = 0
	WHILE (i < a.trsize)

		LET X$ = RIGHT$(SPACE$(13) + HEX$(offset + i), 13) + ":  "
		PRINT
		PRINT X$;
	  
		LET NUM$ = HexRead$(1, 4)
		PRINT NUM$ + SPACE$(2);
		LET NUM$ = HexRead$(1, 3)
		PRINT NUM$ + SPACE$(4);
		LET NUM$ = HexRead$(1, 1)
		PRINT NUM$ + SPACE$(2);
	  
		LET i = i + 8
	WEND

	LET offset = offset + a.trsize
   
	IF a.drsize > 0 THEN
		PRINT
		PRINT
		PRINT " Data Relocation Table:"
	END IF
	LET i = 0
	WHILE (i < a.drsize)

		LET X$ = RIGHT$(SPACE$(13) + HEX$(offset + i), 13) + ":  "
		PRINT
		PRINT X$;
	  
		LET NUM$ = HexRead$(1, 4)
		PRINT NUM$ + SPACE$(2);
		LET NUM$ = HexRead$(1, 3)
		PRINT NUM$ + SPACE$(4);
		LET NUM$ = HexRead$(1, 1)
		PRINT NUM$ + SPACE$(2);
	  
		LET i = i + 8
	WEND

	LET offset = offset + a.drsize
   
	PRINT
	PRINT
	PRINT " Symbol Table:"

	LET i = 0
	WHILE (i < a.syms)
	
		LET NUM$ = RIGHT$("0000" + HEX$(i \ 12), 4)
		PRINT SPACE$(1) + NUM$ + SPACE$(1);
		LET NUM$ = HexRead$(1, 4)
		LET NUM$ = RIGHT$("0000" + HEX$(offset + i), 4)
		PRINT NUM$ + ":  ";
	   
		LET NUM$ = HexRead$(1, 4)
		PRINT NUM$ + SPACE$(2);
		LET NUM$ = HexRead$(1, 4)
		PRINT NUM$ + SPACE$(2);
		LET NUM$ = HexRead$(1, 4)
		PRINT NUM$ + SPACE$(2);

		LET i = i + 12

	WEND
	LET offset = offset + a.syms
	
	LET i = 0
	LET a.strings = LEread&(4, 1)
	
	LET i = i + 4

	PRINT
	PRINT
	PRINT " String Table:"
	WHILE (i < a.strings)
		LET NUM$ = RIGHT$("0000" + HEX$(i), 4)
		PRINT SPACE$(1) + NUM$ + SPACE$(1);
		LET NUM$ = HexRead$(1, 4)
		LET NUM$ = RIGHT$("00000000" + HEX$(offset + i), 8)
		PRINT NUM$ + ":  ";

		LET C$ = "XXX"
		WHILE (ASC(C$) > 0)
			LET C$ = INPUT$(1, 1)
			PRINT C$;
			LET i = i + 1
		WEND
	WEND
	PRINT
	PRINT
	PRINT " Past End:";
	PRINT RIGHT$("0000" + HEX$(offset + i), 4)
	PRINT
	PRINT
ELSE
'    d = 0;
'    read(obfile, &d, 2);
'    if(c != 0x457f || d != 0x464c)
'    {
'        printf("Not an a.out or an ELF file.\n");
'        printf("\n Info  = %x,%x\n\n", c,d);
'        return 0;
'    }
'    for (i = 0; i < 3; i = i + 1) read(obfile, &c, 4);
'   
'     e.type = 0;
'     e.machine = 0;
'     read(obfile, &e.type, 2);
'     read(obfile, &e.machine, 2);
'     read(obfile, &e.version, 4);
'     read(obfile, &e.entry, 4);
'     read(obfile, &e.phoff, 4);
'     read(obfile, &e.shoff, 4);
'     read(obfile, &e.flags, 4);
'     e.ehsize = 0;
'     e.phentsize = 0;
'     e.shentsize = 0;
'     e.shnum = 0;
'     e.shstrndx = 0;
'     read(obfile, &e.ehsize,2);
'     read(obfile, &e.phentsize,2);
'     read(obfile, &e.phnum,2);
'     read(obfile, &e.shentsize,2);
'     read(obfile, &e.shnum,2);
'     read(obfile, &e.shstrndx,2);
'
'    if (!(e.shnum < MAX.shnum))
'    {
'        printf("Increase MAX.shnum!\n\n");
'        return 0;
'    }
'   
'    d = e.shoff;
'    lseek(obfile, d, SEEK.SET);
'    printf("\n %04x:%04x  Section Header Table\n\n",
'                d, d + e.shnum * e.shentsize);
'    for(j = 0; j < e.shnum; j = j + 1)
'    {
'        printf("%02x:  ", j);
'        for(i = 0; i < e.shentsize / 4; i = i + 1)
'        {
'            c = 0;
'            read(obfile, &c, 4);
'            shtab[j][i] = c;
'            printf(" %04x", c);
'        }
'        printf("\n");
'        if(shtab[j][1] == 3 && j != e.shstrndx) f.strtab = j;
'        if(shtab[j][1] == 3 && j == e.shstrndx) g.strtab = j;
'    }
'
'    for(j = 0; j < e.shnum; j = j + 1)
'    {
'        d = shtab[j][0]+ shtab[g.strtab][4];
'        lseek(obfile, d, SEEK.SET);
'        k = 0;
'        DO
'        {
'            c = 0;
'            read(obfile, &c, 1);
'            t[k] = c;
'            k = k + 1;
'        } while (!(c == 0));
'           
'        if(shtab[j][1] == 1)
'        {
'            d =  shtab[j][4];
'            lseek(obfile, d, SEEK.SET);
'            if (shtab[j][5] > 0)
'            {
'                printf("\n %04x:%04x  Program Bits ", d, d + shtab[j][5]);
'                printf(" %s \n\n    ", t);
'            }
'            for(i = 0; i < shtab[j][5] ; i = i + 1)
'            {
'                c = 0;
'                read(obfile, &c, 1);
'                printf("%02x", c);
'                if ((i+1) % 4 == 0) printf(" ");
'                if ((i+1) % 16 == 0) printf("\n    ");
'            }
'        printf("\n");
'        }
'   
'        if(shtab[j][1] == 2)
'        {
'            d =  shtab[j][4];
'            lseek(obfile, d, SEEK.SET);
'            if (shtab[j][5] > 0)
'            {
'                printf("\n %04x:%04x  Symbol Table", d, d + shtab[j][5]);
'                printf(" %s \n\n", t);
'            }
'            for(i = 0; i < shtab[j][5] /16; i = i + 1)
'            {
'                read(obfile, &st.name, 4);
'                symtnd[i] = st.name;
'                read(obfile, &st.value , 4);
'                read(obfile, &st.size, 4);
'                st.info = 0;
'                read(obfile, &st.info, 1);
'                st.other = 0;
'                read(obfile, &st.other, 1);
'                st.shndx = 0;
'                read(obfile, &st.shndx, 2);
'               
'                sprintf(symtab[i], "%08x %08x %08x %02x %02x %04x\n",
'                    st.name ,
'                    st.value,
'                    st.size,
'                    st.info,
'                    st.other,
'                    st.shndx                      );
'            }
'
'            for(i = 0; i < shtab[j][5] / 16; i = i + 1)
'            {
'                d =  symtnd[i] + shtab[f.strtab][4];
'                lseek(obfile, d, SEEK.SET);
'                k = 0;
'                DO
'                {
'                    c = 0;
'                    read(obfile, &c, 1);
'                    s[k] = c;
'                    k = k + 1;
'                } while (!(c == 0));
'                printf("%15s", s);
'                printf(" %s", symtab[i]);
'            }
'        }
'       
'        if(shtab[j][1] == 3)
'        {
'            d = shtab[j][4];
'            lseek(obfile, d, SEEK.SET);
'            if (shtab[j][5] > 0)
'            {
'                printf("\n %04x:%04x  String Table", d, d + shtab[j][5]);
'                printf(" %s \n  ", t);
'            }
'            i = 0;
'            c = 0;
'            while(i < shtab[j][5])
'            {
'                if(c == 0) printf("\n%04x:  ", i);
'                c= 0;
'                read(obfile, &c, 1);
'                if(c != 0) printf("%c", c);
'                i = i + 1;
'            }
'        printf("\n");
'        }
'
'        if(shtab[j][1] == 4)
'        {
'            d =  shtab[j][4];
'            lseek(obfile, d, SEEK.SET);
'            if (shtab[j][5] > 0)
'            {
'                printf("\n %04x:%04x  Relocation with Addends Table",
'                    d, d + shtab[j][5]);
'                printf(" %s \n", t);
'            }
'            for(i = 0; i < shtab[j][5] /12; i = i + 1)
'            {
'                r.offset= 0;
'                read(obfile, &r.offset, 4);
'                r.info = 0; 
'                read(obfile, &r.info , 4);
 '               r.addend= 0;
'                read(obfile, &r.addend, 4);
'                printf("%08x %08x %08x\n",
'                    r.offset,
'                    r.info,
'                    r.addend        );
'            }
'        }
'
'        if(shtab[j][1] == 7)
'        {
'            d =  shtab[j][4];
'            lseek(obfile, d, SEEK.SET);
'            if (shtab[j][5] > 0)
'            {
 '               printf("\n %04x:%04x  Note     ", d, d + shtab[j][5]);
 '               printf(" %s \n\n    ", t);
 '           }
  '          for(i = 0; i < shtab[j][5] ; i = i + 1)
  '          {
'                c = 0;
'                read(obfile, &c, 1);
'                printf("%02x", c);
'                if ((i+1) % 4 == 0) printf(" ");
'                if ((i+1) % 16 == 0) printf("\n    ");
'            }
'        printf("\n");
'        }
'
'        if(shtab[j][1] == 9)
'        {
'            d =  shtab[j][4];
'            lseek(obfile, d, SEEK.SET);
'            if (shtab[j][5] > 0)
'            {
'                printf("\n %04x:%04x  Relocation table", d, d + shtab[j][5]);
'                printf(" %s \n\n", t);
'            }
'            for(i = 0; i < shtab[j][5]/8; i = i + 1)
'            {
'                read(obfile, &r.offset, 4);
'                read(obfile, &r.info, 4);
'                printf("    %08x %08x\n", r.offset, r.info);
'            }
'        }
 '   }
  '  printf("\n\n");
  '  return 0;
'} '
END IF

FUNCTION HexRead$ (N, FILENUM)
LET T$ = ""
FOR K = 1 TO N
	LET C$ = HEX$(ASC(INPUT$(1, #FILENUM)))
	LET C$ = RIGHT$("00" + C$, 2)
	LET T$ = C$ + T$
NEXT
LET HexRead$ = T$
END FUNCTION

FUNCTION LEread& (N, FILENUM)
LET TOTAL& = ASC(INPUT$(1, #FILENUM))
LET POWER& = 1&
FOR K = 1 TO N - 1
	LET POWER& = POWER& * 256&
	LET TOTAL& = TOTAL& + POWER& * CINT(ASC(INPUT$(1, #FILENUM)))
NEXT
LET LEread& = TOTAL&
END FUNCTION

