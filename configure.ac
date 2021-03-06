#                                               -*- Autoconf -*-
# Process this file with autoconf to produce a configure script.

AC_PREREQ(2.60)
AC_INIT([POY], [4.0 Beta], poy4@groups.google.com)

# Checks for programs.
AC_PROG_MAKE_SET
AC_PROG_MKDIR_P
AC_PROG_CC
AC_PROG_INSTALL
AC_ARG_PROGRAM

AC_SUBST(USEGRAPHICS)
AC_SUBST(USEPARALLEL)
AC_SUBST(DOCSTRING)
AC_SUBST(OCAMLFLAGS)
AC_SUBST(OCAMLC)
AC_SUBST(OCAML)
AC_SUBST(OCAMLBUILD)

AC_CHECK_PROGS([OCAMLC], [ocamlc.opt ocamlc])
AC_CHECK_PROGS([OCAMLBUILD], [ocamlbuild.native ocamlbuild])
AC_CHECK_PROGS([OCAML], [ocaml])

AC_ARG_ENABLE([graphics], [AS_HELP_STRING([--enable-graphics],
              [Force POY to support graphical trees on screen. Valid options are none, ocaml, and tk. The default is none.])], [USEGRAPHICS=$enableval], [USEGRAPHICS=none])

AC_SUBST(USEINTERFACE)
AC_ARG_ENABLE([interface], [AS_HELP_STRING([--enable-interface=interface],
              [Force POY's user interface. Valid options are flat, ncurses, readline, and html. The default is flat. NOTE: poyd master and servant require the flat interface.])],
              [USEINTERFACE=$enableval],
              [USEINTERFACE=flat])

AC_ARG_WITH([ocaml-flags], [Add the following flags to the ocaml compiler.],
            [OCAMLFLAGS=$withval],[])

AC_ARG_WITH([extras], [Add the following object files to the final linking rules (needed for windows).],
            [EXTRAS=$withval],[])

AC_ARG_WITH(ocaml-flags, [Add the following flags to the ocaml compiler.],
            [OCAMLFLAGS=$withval],[])

AC_ARG_ENABLE([mpi], [AS_HELP_STRING([--enable-mpi], 
              [Turns on parallel support using MPI. When using MPI remember to set CC to the appropriate mpicc compiler.])], 
              [USEPARALLEL=true], 
              [USEPARALLEL=false])

AC_SUBST(USESHAREDHD)
AC_ARG_ENABLE([shared-hd], [AS_HELP_STRING([--enable-shared-hd],
              [Force POY to support live broadcast of file contents when running in parallel. The default is false.])], [USESHAREDHD=true], [USESHAREDHD=false])


AC_ARG_ENABLE([long-sequences], [AS_HELP_STRING([--enable-long-sequences], 
              [Enable the analysis of sequences longer than 16383 elements (The limit would be  2^31 or 2^63 for 32 bit and 64 bit architectures respectively). When using this option, more memory will be needed for the default search strategies. If you are analyzing fragments shorted than, say 12.000 bp, you are unlikely to need this option.])], 
              [USE_LONG_SEQUENCES=true], 
              [USE_LONG_SEQUENCES=false])

AC_ARG_ENABLE([xslt], [AS_HELP_STRING([--enable-xslt],
              [Enable the postprocessing of XML output using XSLT templates.  This option requires libxml2.])],
               [USE_XSLT=true],
               [USE_XSLT=false])

AC_ARG_ENABLE([large-alphabets], [AS_HELP_STRING([--enable-large-alphabets],
              [Enable the analysis of alphabets larger than 255 elements. When using this option, more memory will be needed to store each tree (4 or 8 times more for 32 and 64 bit computers respectively). This option is only needed for large alphabets if you use custom alphabets. Normal nucleotide, aminoacid, or morphological analyses are nor affected by this option.])],
              [USE_LARGE_ALPHABETS=true],
              [USE_LARGE_ALPHABETS=false])

AC_ARG_ENABLE([cost-verification], [AS_HELP_STRING([--enable-cost-verification],
              [Check every pairwise sequence alignment cost. This is only intended for development and debugging purposes. This option will make the program run really slow.])],
              [USE_VERIFY_COSTS=true],
              [USE_VERIFY_COSTS=false])

# Check how the documentation string should be 
CONFIG_GUESS_SYS=`./gnu/config.guess` 
case ${CONFIG_GUESS_SYS} in
    *darwin9*)
        DOCSTRING='\\newcommand{\\buildnumber}{'
        ;;
    *)
        DOCSTRING='\newcommand{\buildnumber}{'
        ;;
esac

# System dependent options
case `./gnu/config.guess` in
    x86_64*)
        OCAMLFLAGS="-fno-PIC $OCAMLFLAGS" 
        ;;
    *-apple-*)
        CFLAGS="-I /usr/include/malloc $CFLAGS"
        ;;
    *-cygwin*)
        USEWIN32=true
        CFLAGS="-mwin32 -mno-cygwin $CFLAGS"
	
        ;;
    *)
        ;;
esac

# Checks for libraries.

# Checks for header files.

# Checks for typedefs, structures, and compiler characteristics.

# Checks for library functions.

AC_CONFIG_FILES([Makefile])
AC_CONFIG_SUBDIRS([src])
AC_OUTPUT
