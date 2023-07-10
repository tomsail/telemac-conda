SHELL=/bin/sh
LIB=$(TECADDONDEVDIR)/lib/$(PLATFORM)/$(OBJSUBDIR)/lib$(LIBBASENAME).$(LIBEXT)
OBJDIR=$(OBJBUILDDIR)/obj/$(PLATFORM)/$(OBJSUBDIR)
EXTLIBDIR=$(EXTBASEDIR)/$(PLATFORM)

shlib: $(LIB)

#
# list of obj files....
#
OBJS=\
     $(OBJDIR)/processing.o \
     $(OBJDIR)/main.o \
     $(OBJDIR)/guicb.o \
     $(OBJDIR)/guidefs.o

EHEADER=
GHEADER=
GHEADER=GUIDEFS.h


#
# Main link command:
#
$(LIB) : $(OBJS)
	cd $(OBJDIR); $(LD) $(LDFLAGS) *.o -o $(LIB)

#
# Duplicate the next line for additional source files...
#
$(OBJDIR)/processing.o : processing.c ADDGLBL.h $(EHEADER) $(GHEADER)
	cd $(OBJDIR); $(CCOMP) $(CFLAGS) $(SOURCEDIR)/processing.c 
$(OBJDIR)/main.o : main.c ADDGLBL.h $(EHEADER) $(GHEADER)
	cd $(OBJDIR); $(CCOMP) $(CFLAGS) $(SOURCEDIR)/main.c 
$(OBJDIR)/guidefs.o : guidefs.c guibld.c $(GHEADER)
	cd $(OBJDIR); $(CCOMP) $(CFLAGS) $(SOURCEDIR)/guidefs.c 
$(OBJDIR)/guicb.o : guicb.c guibld.c ADDGLBL.h $(GHEADER)
	cd $(OBJDIR); $(CCOMP) $(CFLAGS) $(SOURCEDIR)/guicb.c


 
clean:
	rm obj/*/*/*.o > /dev/null 2>&1

