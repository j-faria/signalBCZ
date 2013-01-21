# no console output during compilation:
#.SILENT:

# *******************************************************
# ***          Comecar por limpar os sufixos          ***
# *******************************************************
.SUFFIXES:

# *******************************************************
# ***     Especificar os sufixos para .f .o .do       ***
# *******************************************************
#.SUFFIXES: .f90 .o

#
# *******************************************************
# ***                       Macros                    ***
# *******************************************************
#FC = /usr/bin/f77
#FC	= g77 -Wall
FC	= gfortran
FFLAGS = -O0 -ffree-form
#dbx     = -O5 -r8 -g
#profil  = -p -O5 -r8 
#samedir = .
#FTN     = ftnchek

#
# *******************************************************
# *** Regra que por defeito produz os ficheiros .o **
# *******************************************************
%.o: %.f
	$(FC) $(FFLAGS) -c -o $@ $*.f
%.o: %.f90
	$(FC) $(FFLAGS) -c -o $@ $*.f90

#
# *******************************************************
# ***   Especificar as directorias com as subrotinas  ***
# *******************************************************
JF = /home/joao/Programs/CODE_freqFit/sig_bcz
JFA = /home/joao/Programs/CODE_freqFit/common
JFB = /home/joao/Programs/CODE_freqFit/subroutines
JFC = /home/joao/Programs/CODE_freqFit/nl2sol_fit/subroutines/nl2sol
# plot library:
JFp = /home/joao/Utils/gnuFor2
# spline library:
JFspl = /home/joao/Programs/CODE_freqFit/dierckx

#----------------------------------------------------------

FILES = \
$(JF)/commonvar.o \
$(JF)/commonarray.o \
$(JF)/teste.o \
$(JF)/sig_bcz.o \
$(JF)/interfaces.o \
$(JF)/get_chi_square.o \
$(JF)/deffreq.o \
$(JF)/parameters.o \
$(JF)/openfiles.o \
$(JF)/output.o \
$(JF)/writeout2.o \
$(JF)/fun.o \
$(JF)/fun2.o 

FCOM = \
   $(JFA)/minimize.o \
   $(JFA)/nl2sol.o \
   $(JFA)/resid.o \
   $(JFA)/fit.o \
   $(JFA)/init.o \
   $(JFA)/fitlamb.o \
   $(JFA)/sec.o \
   $(JFA)/funk.o

FBASIC = \
$(JFB)/amoeba/amoeba.o \
$(JFB)/num_to_text.o \
$(JFB)/skpcom.o \
$(JFB)/length.o \
$(JFB)/poly_smooth.o \
$(JFB)/order.o

Fnl2sol = \
$(JFC)/interfaces.o \
$(JFC)/minimize.o \
$(JFC)/nl2sol.o \
$(JFC)/resid.o



FPLOT = \
$(JFp)/gnufor2.o

#FSPLINE = \
#$(JFspl)/concur.o \
#$(JFspl)/curev.o \
#$(JFspl)/fpadpo.o \
#$(JFspl)/fpback.o \
#$(JFspl)/fpbspl.o \
#$(JFspl)/fpched.o \
#$(JFspl)/fpcons.o \
#$(JFspl)/fpdisc.o \
#$(JFspl)/fpgivs.o \
#$(JFspl)/fpinst.o \
#$(JFspl)/fpknot.o \
#$(JFspl)/fppocu.o \
#$(JFspl)/fprati.o \
#$(JFspl)/fprota.o 


# **********************************************************
# ***             Compilar os programas                  *** 
# **********************************************************

sig_bcz: $(FILES) $(FCOM) $(FBASIC) $(FPLOT)
	$(FC) $(FILES) $(FCOM) $(FBASIC) $(FPLOT) -o sig_bcz_run

clean:
	rm -f $(FILES) $(FCOM) $(FBASIC) $(FPLOT) *~ *.mod sig_bcz_run
