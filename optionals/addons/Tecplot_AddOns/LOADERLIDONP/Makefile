SHELL=/bin/sh
LIB=$(TECADDONDEVDIR)/lib/$(PLATFORM)/$(OBJSUBDIR)/lib$(LIBBASENAME).$(LIBEXT)
OBJDIR=$(OBJBUILDDIR)/obj/$(PLATFORM)/$(OBJSUBDIR)
EXTLIBDIR=$(EXTBASEDIR)/$(PLATFORM)

shlib: $(LIB)

#
# list of obj files....
#
OBJS= \
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
	cd $(OBJDIR); $(LD) $(LDFLAGS) *.o -o $(LIB)

#
# Duplicate the next line for additional source files...
#
$(OBJDIR)/DoConversion.o : DoConversion.c ADDGLBL.h $(EHEADER) $(GHEADER)
	cd $(OBJDIR); $(CCOMP) $(CFLAGS) $(SOURCEDIR)/DoConversion.c 
$(OBJDIR)/main.o : main.c ADDGLBL.h $(EHEADER) $(GHEADER)
	cd $(OBJDIR); $(CCOMP) $(CFLAGS) $(SOURCEDIR)/main.c 
$(OBJDIR)/engine.o : engine.c ADDGLBL.h ENGINE.h
	cd $(OBJDIR); $(CCOMP) $(CFLAGS) $(SOURCEDIR)/engine.c


 
clean:
	rm obj/*/*/*.o > /dev/null 2>&1

