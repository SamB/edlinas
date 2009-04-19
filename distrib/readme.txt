                 Edlinas Installation Notes

     Edlinas is a DOS program, EDL.EXE.  In order to run it
requires the file X86.DAT.  This file may be located in the
current directory.  To locate this file somewhere else, a
directory name may be the first line of an EDL.INI file
located in the current directory.  All other lines of
EDL.INI are either assembler language instructions,
assembler directives, or comment lines beginning with "#".
The simulated machine's memory is a file MEMVEC.DAT.  This
file will be created in a directory specified by the
simulator control variable MEMPATH.  The default value is
the current directory.  This variable may be set in the
EDL.INI file with an assembler directive such as:

     MEMPATH .= F:\SIMULATOR\

Since memory is by default 1 Meg, you may not want to locate
this file on a floppy disk.  The memory file is not erased
when the program terminates.  It may be safely erased, or it
may be left if it contains useful data.

To run the program it should be called with a file name as
an argument:

     C:> EDL GCD.ASM

If this file does not exist it will be created.  On this
disk are two programs: GCD.ASM and RECURS.ASM.

The GCD program illustrates how IN and OUT work on the
simulator.  It finds the greatest common divisor of two
inputs.  When the program is loaded and the message "End of
Input File" appears, hit Enter to set the instruction
pointer to the beginning of the program (if the asterisk is
at the end of the file then the pointer defaults to the
beginning).  Then hit the space bar to step through the
program.  Respond to the two flashing input requests by
entering a positive integer.  Respond to the flashing output
by acknowledging it with an Enter.  When the program
finishes it may be restarted again by hitting enter.  To
leave hit "q".

The program RECURS.ASM calculates binomial coefficients
using the algorithm:

     C(k, n) = n              if k = 1
             = 1              if k = n
             = C(k, n-1) + C(k-1, n-1) otherwise.

Hence entering the values first 2 then 4 should yield the
output C(2,4) = 6.  To watch the stack during this
computation hit the Tab key once to bring the stack into
view.

The programs HI.ASM, HI5.ASM, and PKS.ASM are all programs
which run under Linux (ELF).  The first two illustrate
calling C library functions from assembler.  To use them,
create object files by assembling them:

     C:\X86> EDL HI.ASM

Hit "o" to save an object file.  Go into Linux and copy the
object code:

     linuxbox% mcopy a:HI.O hi.o


Link it using gcc:

     linuxbox% gcc hi.o

This produces an executable.  PKS.ASM is written to be
called by the program EXAMPLE.C.  Compile this first with

     linuxbox% gcc -c example.c

Then link with:

     linuxbox% gcc example.o pks.o


The OB.C program parses ".o" object files.  It is useful for
debugging.


SEE ALSO:
For more detailed documentation, see edlinas.txt.
