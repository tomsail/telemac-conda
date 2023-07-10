SHELL=/bin/sh
LIB=$(TECADDONDEVDIR)/lib/$(PLATFORM)/$(OBJSUBDIR)/lib$(LIBBASENAME).$(LIBEXT)
OBJDIR=$(OBJBUILDDIR)/obj/$(PLATFORM)/$(OBJSUBDIR)
EXTLIBDIR=$(EXTBASEDIR)/$(PLATFORM)

shlib: $(LIB)

#
# list of obj files....
#
OBJS=$(OBJDIR)/main.o \
     $(OBJDIR)/TecUtilSet.o \
     $(OBJDIR)/TecUtilTime.o 

EHEADER=
GHEADER=


#
# Main link command:
#
$(LIB) : $(OBJS)
	cd $(OBJDIR); $(LD) $(LDFLAGS) *.o -o $(LIB)

#
# Duplicate the next line for additional source files...
#
$(OBJDIR)/main.o : main.cpp ADDGLBL.h $(EHEADER) $(GHEADER)
	cd $(OBJDIR); $(CCOMP) $(CFLAGS) $(SOURCEDIR)/main.cpp 
$(OBJDIR)/TecUtilSet.o : TecUtilSet.cpp TecUtilSet.h $(EHEADER) $(GHEADER)
	cd $(OBJDIR); $(CCOMP) $(CFLAGS) $(SOURCEDIR)/TecUtilSet.cpp 
$(OBJDIR)/TecUtilTime.o : TecUtilTime.cpp TecUtilTime.h $(EHEADER) $(GHEADER)
	cd $(OBJDIR); $(CCOMP) $(CFLAGS) $(SOURCEDIR)/TecUtilTime.cpp 


 
clean:
	rm obj/*/*/*.o > /dev/null 2>&1

