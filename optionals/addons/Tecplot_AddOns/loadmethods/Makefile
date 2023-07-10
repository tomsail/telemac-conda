SHELL=/bin/sh
LIB=$(TECADDONDEVDIR)/lib/$(PLATFORM)/$(OBJSUBDIR)/lib$(LIBBASENAME).$(LIBEXT)
OBJDIR=$(OBJBUILDDIR)/obj/$(PLATFORM)/$(OBJSUBDIR)
EXTLIBDIR=$(EXTBASEDIR)/$(PLATFORM)
ADKUTILLIB=$(EXTLIBDIR)/libadkutil.a

shlib: $(LIB)

#
# list of obj files....
#
OBJS=$(OBJDIR)/main.o \
     $(OBJDIR)/engine.o \
     $(OBJDIR)/guicb.o \
     $(OBJDIR)/guidefs.o

EHEADER=
GHEADER=
EHEADER=ENGINE.h
GHEADER=GUIDEFS.h


#
# Main link command:
#
$(LIB) : $(OBJS)
	cd $(OBJDIR); $(LD) $(LDFLAGS) *.o $(ADKUTILLIB) -o $(LIB) >/dev/null 2>&1

#
# Duplicate the next line for additional source files...
#
$(OBJDIR)/main.o : main.c $(EHEADER) $(GHEADER) ADDGLBL.h
	cd $(OBJDIR); $(CCOMP) $(CFLAGS) $(SOURCEDIR)/main.c 
$(OBJDIR)/guidefs.o : guidefs.c guibld.c $(GHEADER) ADDGLBL.h
	cd $(OBJDIR); $(CCOMP) $(CFLAGS) $(SOURCEDIR)/guidefs.c 
$(OBJDIR)/guicb.o : guicb.c guibld.c $(GHEADER) ADDGLBL.h
	cd $(OBJDIR); $(CCOMP) $(CFLAGS) $(SOURCEDIR)/guicb.c
$(OBJDIR)/engine.o : engine.c ENGINE.h ADDGLBL.h
	cd $(OBJDIR); $(CCOMP) $(CFLAGS) $(SOURCEDIR)/engine.c

clean:
	rm obj/*/*/*.o > /dev/null 2>&1

