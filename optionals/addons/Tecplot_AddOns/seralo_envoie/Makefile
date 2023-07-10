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
     $(OBJDIR)/readfile.o \
     $(OBJDIR)/add_delwaq.o \
     $(OBJDIR)/LoadFileSera.o \
     $(OBJDIR)/LoadFileSera2.o \
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
	cd $(OBJDIR); $(LD) $(LDFLAGS) *.o $(ADKUTILLIB) -o $(LIB)

#
# Duplicate the next line for additional source files...
#
$(OBJDIR)/main.o : main.c ADDGLBL.h $(EHEADER) $(GHEADER)
	cd $(OBJDIR); $(CCOMP) $(CFLAGS) $(SOURCEDIR)/main.c 
$(OBJDIR)/guidefs.o : guidefs.c guibld.c $(GHEADER)
	cd $(OBJDIR); $(CCOMP) $(CFLAGS) $(SOURCEDIR)/guidefs.c 
$(OBJDIR)/guicb.o : guicb.c guibld.c ADDGLBL.h $(GHEADER)
	cd $(OBJDIR); $(CCOMP) $(CFLAGS) $(SOURCEDIR)/guicb.c
$(OBJDIR)/add_delwaq.o : add_delwaq.c ADDGLBL.h
	cd $(OBJDIR); $(CCOMP) $(CFLAGS) $(SOURCEDIR)/add_delwaq.c
$(OBJDIR)/LoadFileSera.o : LoadFileSera.c ADDGLBL.h
	cd $(OBJDIR); $(CCOMP) $(CFLAGS) $(SOURCEDIR)/LoadFileSera.c
$(OBJDIR)/LoadFileSera2.o : LoadFileSera2.c ADDGLBL.h
	cd $(OBJDIR); $(CCOMP) $(CFLAGS) $(SOURCEDIR)/LoadFileSera2.c
$(OBJDIR)/readfile.o : readfile.c ADDGLBL.h
	cd $(OBJDIR); $(CCOMP) $(CFLAGS) $(SOURCEDIR)/readfile.c
$(OBJDIR)/engine.o : engine.c ADDGLBL.h ENGINE.h
	cd $(OBJDIR); $(CCOMP) $(CFLAGS) $(SOURCEDIR)/engine.c


 
clean:
	rm obj/*/*/*.o > /dev/null 2>&1

