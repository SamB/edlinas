                 Edlinas Installation Notes

     Edlinas is a DOS program, EDL.EXE.  In order to run, it
requires the presence of the file, X86.DAT.  The following
setup is suggested:

     C:\> MKDIR X86

     C:\> CD X86

     C:\X86> COPY A:\*.*

To run the program it should be called with a file name as
an argument:

     C:> EDL GCD.ASM

  When the program finishes it may be restarted again by
hitting enter.  To leave hit "q".


Configuration File

     A file stored in the current directory and named
EDL.INI can be used as an initialization file.

If the location of the X86.DAT is not the same as the
directory from which the program is being run this directory
may be given as the first line of EDL.INI.  This may be
convenient for the sake of EDL.EXE and X86.DAT stored on a
network directory.

     Any additional lines in EDL.INI will executed as though
there were entered from the command prompt ">".

Memory File

     A file MEMVEC.DAT is created by EDL.  It is the
simulated machine's memory.  This file may be deleted or
modified at will.

