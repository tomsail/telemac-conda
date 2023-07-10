SHELL=/bin/sh
LIB=$(TECADDONDEVDIR)/lib/$(PLATFORM)/$(OBJSUBDIR)/lib$(LIBBASENAME).$(LIBEXT)
OBJDIR=$(OBJBUILDDIR)/obj/$(PLATFORM)/$(OBJSUBDIR)
EXTLIBDIR=$(EXTBASEDIR)/$(PLATFORM)

shlib: $(LIB)

#
# list of obj files....
#
OBJS=\
     $(OBJDIR)/M_ESTEL3D_UTIL.o \
     $(OBJDIR)/M_SERAFIN2_DEF.o \
     $(OBJDIR)/M_READ_SERAFIN2.o \
     $(OBJDIR)/DoConversion.o \
     $(OBJDIR)/main.o \
     $(OBJDIR)/engine.o

EHEADER=
GHEADER=
EHEADER=ENGINE.h


#
# Main link command:
#
$(LIB) : $(OBJS)
	cd $(OBJDIR); pgf90 $(LDFLAGS) *.o -o $(LIB) 

#
# Duplicate the next line for additional source files...
#
$(OBJDIR)/M_ESTEL3D_UTIL.o : M_ESTEL3D_UTIL.f90
	cd $(OBJDIR); $(FCOMP) $(FFLAGS) $(SOURCEDIR)/M_ESTEL3D_UTIL.f90
$(OBJDIR)/M_SERAFIN2_DEF.o : M_SERAFIN2_DEF.f90
	cd $(OBJDIR); $(FCOMP) $(FFLAGS) $(SOURCEDIR)/M_SERAFIN2_DEF.f90
$(OBJDIR)/M_READ_SERAFIN2.o : M_READ_SERAFIN2.f90
	cd $(OBJDIR); $(FCOMP) $(FFLAGS) $(SOURCEDIR)/M_READ_SERAFIN2.f90
$(OBJDIR)/DoConversion.o : DoConversion.c ADDGLBL.h ENGINE.h
	cd $(OBJDIR); $(CCOMP) $(CFLAGS) $(SOURCEDIR)/DoConversion.c
$(OBJDIR)/main.o : main.c ADDGLBL.h $(EHEADER) $(GHEADER)
	cd $(OBJDIR); $(CCOMP) $(CFLAGS) $(SOURCEDIR)/main.c 
$(OBJDIR)/engine.o : engine.c ADDGLBL.h ENGINE.h
	cd $(OBJDIR); $(CCOMP) $(CFLAGS) $(SOURCEDIR)/engine.c


 
clean:
	rm obj/*/*/*.o > /dev/null 2>&1

