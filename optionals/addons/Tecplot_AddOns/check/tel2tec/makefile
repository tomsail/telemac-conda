# Copyright (C) 2004-2005 JP Renaud - University of Bristol

#-------------------------------------------------------------------------------
# Makefile for tel2tec
#-------------------------------------------------------------------------------

# This program is free software; you can redistribute it and/or modify it
# under the terms of the GNU General Public License as published by the
# Free Software Foundation; either version 2 of the License, or any later
# version.

# This program is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
# or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for
# more details.

# You should have received a copy of the GNU General Public License along
# with this program, in the text file 'gpl.txt'; if not, write to:
# Free Software Foundation, Inc., 59 Temple Place, Suite 330, Boston,
# MA 02111-1307 USA
# or get it from http://www.gnu.org/licenses/gpl.html

#-------------------------------------------------------------------------------
# Important note: when the Tecplot binary library tecio.a exists on your
# system, this software will use this library to produce binary files. The
# tecio.a library belongs to Tecplot Inc and is in no way covered by this
# license.
#-------------------------------------------------------------------------------

# Fortran 90 compiler
# Attention it needs to have the iargc extension!!!
FC = ifort
FFLAGS = -warn all -check bounds -I./tecplot/

# Fortran linker and linker flag
LK = ifort
LKFLAG = -lm -lstdc++ -o 

# Destination for the executable
DEST=$(HOME)/bin

############ IMPORTANT #############
# If you have access to the tecplot library tecio.a (or equivalent)
# you can specify its location here using LKLIB. In that case, VPATH
# need to include output_binary/ and *not* output_ascii/

# No support for binary output
#VPATH = ./ ; output_ascii/
#LKLIB = 

# Binary Tecplot support
VPATH = ./ ; output_binary/
LKLIB =  $(TEC110HOME)/lib/tecio.a 
########## END IMPORTANT ###########

OBJS = \
m_help.o \
m_output_functions.o \
m_output_util.o \
m_output.o \
m_processing.o \
p_tel2tec.o

EXE = tel2tec

BINEXE=$(DEST)/$(EXE)

$(EXE): $(OBJS)
	@echo "Linking the objects."
	@$(LK) $(LKFLAG) $(EXE) $(OBJS) $(LKLIB)
	@echo "Done."

$(BINEXE): $(EXE)
	@echo "Installing tel2tec in $(DEST)"
	@cp $(EXE) $(DEST)/.

# rule to create object files
$(OBJS): %.o: %.f90
	@echo "Compiling $<"
	@$(FC) $(FFLAGS) -c $< -o $@

install: $(BINEXE)

.PHONY: clean
clean:
	@echo "Cleaning objects, executable and modules"
	@rm -f $(EXE) $(OBJS)
	@rm -f *.mod *.MOD 


