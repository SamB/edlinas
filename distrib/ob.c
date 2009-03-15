#include <stdio.h>
#include <fcntl.h>
#include <sys/types.h>
#include <sys/stat.h>
#define MAX_shnum 20

/*
/*    This is a .o file parsing program.  
/*    It parses a.out and ELF ".o" files.
/* 
*/

main(argc, argv)
int argc;
char *argv[];
{
	int obfile,i, j ,k, g_strtab, f_strtab;
	int c, d, a_text, a_data, a_bss, a_syms, a_entry, a_trsize, a_drsize;
	int e_type, e_machine, e_version, e_entry, e_phoff, e_shoff, e_flags;
	int e_ehsize, e_phentsize, e_phnum, e_shentsize, e_shnum, e_shstrndx;
	int st_name , st_value, st_size, st_info, st_other, st_shndx;
	int r_info, r_offset, r_addend;
	int shtab[MAX_shnum][10],symtnd[100];
	char s[100], t[100], symtab[100][100];
	
	int offset, a_strings;
	obfile = open(argv[1], O_RDONLY);
	if(obfile < 0) 
	{
		printf("\nFile not found.\n.");
		return 0;
	}
	c = 0;
	read(obfile, &c, 2);
	if(c == 0x107)
	{
	read(obfile,&c,  2);
	if(c) printf(" Miscellaneous= %x", c);
	read(obfile,&c,  4);
	printf("\n Text size = %x",c);
	a_text = c;
	read(obfile,&c,  4);
	if (c) printf("\n Data size = %x  ",c);
	a_data = c;
	read(obfile,&c,  4);
	if (c) printf("\n Bss  size = %x ", c);
	a_bss = c;
	read(obfile,&c,  4);
	printf("\n Symbol Table size = %x  ", c);
	a_syms = c;
	read(obfile,&c,  4);
	if (c) printf("\n Entry pnt = %x", c);
	a_entry = c;
	read(obfile,&c,  4);
	if (c) printf("\n Text rellocation table size = %x",c);
	a_trsize = c;
	read(obfile,&c,  4);
	if (c) printf("\n Data rellocation table size = %x",c);
	a_drsize = c;

	offset = 32;

	printf("\n\n Text:\n");
	i = 0;
	while (i < a_text)
	{
	if (i % 16 == 0) printf("\n %13x:", offset + i);
	if (i % 4 == 0) printf("  ");
	c = 0;
	read(obfile, &c, 1);
	printf("%02x", c);
	i = i + 1;
	}

	offset = offset + a_text;

	if(a_data)	printf("\n\n Data:\n");
	i = 0;
	while (i < a_data)
	{
	if (i % 16 == 0) printf("\n %13x:", offset + i);
	if (i % 4 == 0) printf("  ");
	c = 0;
	read(obfile, &c, 1);
	printf("%02x", c);
	i = i + 1;
	}

	offset = offset + a_data;

	if(a_trsize)	printf("\n\n Text Relocation Table:\n");
	i = 0;
	while (i < a_trsize)
	{
		printf("\n %13x:  ", offset + i);
		c = 0;
		read(obfile, &c, 4);
		printf("%08x  ", c);
		c = 0;
		read(obfile, &c, 3);
		printf("%06x    ", c);
		c = 0;
		read(obfile, &c, 1);
		printf("%02x   ", c);

		i = i + 8;
	}

	offset = offset + a_trsize;

	if(a_drsize)
		printf("\n\n Data Relocation Table:\n");
	i = 0;
	while (i < a_drsize)
	{
		printf("\n %13x:  ", offset + i);
		c = 0;
		read(obfile, &c, 4);
		printf("%08x  ", c);
		c = 0;
		read(obfile, &c, 3);
		printf("%06x    ", c);
		c = 0;
		read(obfile, &c, 1);
		printf("%02x   ", c);

		i = i + 8;
	}

	offset = offset + a_drsize;

	printf("\n\n Symbol Table:\n");
	i = 0;
	while (i < a_syms)
	{
		printf("\n %4x %8x:  ", i / 12, offset + i);
		c = 0;
		read(obfile, &c, 4);
		printf("%08x  ", c);
		c = 0;
		read(obfile, &c, 4);
		printf("%08x  ", c);
		c = 0;
		read(obfile, &c, 4);
		printf("%08x  ", c);

		i = i + 12;

	}


	offset = offset + a_syms;
	
	i = 0;
	read(obfile, &a_strings, 4);
	i = i + 4;

	printf("\n\n String Table:\n");
	while (i < a_strings)
	{
		printf("\n %4x %8x:  ", i, offset + i);
		c = 1;
		while (c !=0)
		{
			c = 0;
			read(obfile, &c, 1);
			printf("%c", c);
			i = i + 1;
		}
	}

	c = offset + i;
	printf("\n\n Past End:%4x \n\n",c);
	return 0;
	}

	d = 0;
	read(obfile, &d, 2);
	if(c != 0x457f || d != 0x464c) 
	{
		printf("Not an a.out or an ELF file.\n");
		printf("\n Info  = %x,%x\n\n", c,d);
		return 0;
	}
	for (i = 0; i < 3; i = i + 1) read(obfile, &c, 4);
	
	 e_type = 0;
	 e_machine = 0;
	 read(obfile, &e_type, 2);
	 read(obfile, &e_machine, 2);
	 read(obfile, &e_version, 4);
	 read(obfile, &e_entry, 4);
	 read(obfile, &e_phoff, 4);
	 read(obfile, &e_shoff, 4);
	 read(obfile, &e_flags, 4);
	 e_ehsize = 0;
	 e_phentsize = 0;
	 e_shentsize = 0;
	 e_shnum = 0;
	 e_shstrndx = 0;
	 read(obfile, &e_ehsize,2);
	 read(obfile, &e_phentsize,2);
	 read(obfile, &e_phnum,2);
	 read(obfile, &e_shentsize,2);
	 read(obfile, &e_shnum,2);
	 read(obfile, &e_shstrndx,2);

	if (!(e_shnum < MAX_shnum)) 
	{
		printf("Increase MAX_shnum!\n\n");
		return 0;
	}
	
	d = e_shoff;
	lseek(obfile, d, SEEK_SET);
	printf("\n %04x:%04x  Section Header Table\n\n", 
				d, d + e_shnum * e_shentsize); 
	for(j = 0; j < e_shnum; j = j + 1)
	{
		printf("%02x:  ", j);
		for(i = 0; i < e_shentsize / 4; i = i + 1)
		{
			c = 0;
			read(obfile, &c, 4);
			shtab[j][i] = c;
			printf(" %04x", c);
		}
		printf("\n");
		if(shtab[j][1] == 3 && j != e_shstrndx) f_strtab = j;
		if(shtab[j][1] == 3 && j == e_shstrndx) g_strtab = j;
	}

	for(j = 0; j < e_shnum; j = j + 1)
	{
		d = shtab[j][0]+ shtab[g_strtab][4]; 
		lseek(obfile, d, SEEK_SET);
		k = 0;
		do
		{
			c = 0;
			read(obfile, &c, 1);
			t[k] = c;
			k = k + 1;
		} while (!(c == 0));
			
		if(shtab[j][1] == 1)
		{
			d =  shtab[j][4];
			lseek(obfile, d, SEEK_SET);
			if (shtab[j][5] > 0)
			{
				printf("\n %04x:%04x  Program Bits ", d, d + shtab[j][5]);
				printf(" %s \n\n    ", t);
			}
			for(i = 0; i < shtab[j][5] ; i = i + 1)
			{
				c = 0;
				read(obfile, &c, 1);
				printf("%02x", c);
				if ((i+1) % 4 == 0) printf(" ");
				if ((i+1) % 16 == 0) printf("\n    ");
			}
		printf("\n");
		}
	
		if(shtab[j][1] == 2)
		{
			d =  shtab[j][4];
			lseek(obfile, d, SEEK_SET);
			if (shtab[j][5] > 0)
			{
				printf("\n %04x:%04x  Symbol Table", d, d + shtab[j][5]);
				printf(" %s \n\n", t);
			}
			for(i = 0; i < shtab[j][5] /16; i = i + 1)
			{
				read(obfile, &st_name, 4);
				symtnd[i] = st_name;
				read(obfile, &st_value , 4);
				read(obfile, &st_size, 4);
				st_info = 0;
				read(obfile, &st_info, 1);
				st_other = 0;
				read(obfile, &st_other, 1);
				st_shndx = 0;
				read(obfile, &st_shndx, 2);
				
				sprintf(symtab[i], "%08x %08x %08x %02x %02x %04x\n", 
					st_name ,
					st_value,
					st_size,
					st_info,
					st_other,
					st_shndx                      );
			}

			for(i = 0; i < shtab[j][5] / 16; i = i + 1)
			{
				d =  symtnd[i] + shtab[f_strtab][4]; 
				lseek(obfile, d, SEEK_SET);
				k = 0;
				do
				{
					c = 0;
					read(obfile, &c, 1);
					s[k] = c;
					k = k + 1;
				} while (!(c == 0));
				printf("%15s", s);
				printf(" %s", symtab[i]);
			}
		}
		
		if(shtab[j][1] == 3)
		{
			d = shtab[j][4];
			lseek(obfile, d, SEEK_SET);
			if (shtab[j][5] > 0)
			{
				printf("\n %04x:%04x  String Table", d, d + shtab[j][5]);
				printf(" %s \n  ", t);
			}
			i = 0;
			c = 0;
			while(i < shtab[j][5])
			{
				if(c == 0) printf("\n%04x:  ", i);
				c= 0;
				read(obfile, &c, 1);
				if(c != 0) printf("%c", c);
				i = i + 1;
			}
		printf("\n");
		}

		if(shtab[j][1] == 4)
		{
			d =  shtab[j][4];
			lseek(obfile, d, SEEK_SET);
			if (shtab[j][5] > 0)
			{
				printf("\n %04x:%04x  Relocation with Addends Table", 
					d, d + shtab[j][5]);
				printf(" %s \n", t);
			}
			for(i = 0; i < shtab[j][5] /12; i = i + 1)
			{
				r_offset= 0;
				read(obfile, &r_offset, 4);
				r_info = 0;	
				read(obfile, &r_info , 4);
				r_addend= 0;
				read(obfile, &r_addend, 4);
				printf("%08x %08x %08x\n", 
					r_offset,
					r_info,
					r_addend	);
			}
		}

		if(shtab[j][1] == 7)
		{
			d =  shtab[j][4];
			lseek(obfile, d, SEEK_SET);
			if (shtab[j][5] > 0)
			{
				printf("\n %04x:%04x  Note     ", d, d + shtab[j][5]);
				printf(" %s \n\n    ", t);
			}
			for(i = 0; i < shtab[j][5] ; i = i + 1)
			{
				c = 0;
				read(obfile, &c, 1);
				printf("%02x", c);
				if ((i+1) % 4 == 0) printf(" ");
				if ((i+1) % 16 == 0) printf("\n    ");
			}
		printf("\n");
		}

		if(shtab[j][1] == 9)
		{
			d =  shtab[j][4];
			lseek(obfile, d, SEEK_SET);
			if (shtab[j][5] > 0)
			{
				printf("\n %04x:%04x  Relocation table", d, d + shtab[j][5]);
				printf(" %s \n\n", t);
			}
			for(i = 0; i < shtab[j][5]/8; i = i + 1)
			{
				read(obfile, &r_offset, 4);
				read(obfile, &r_info, 4);
				printf("    %08x %08x\n", r_offset, r_info);
			}
		}
	}
	printf("\n\n");
	return 0;
}

	
