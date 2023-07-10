SHELL=/bin/sh
LIB=$(TECADDONDEVDIR)/lib/$(PLATFORM)/$(OBJSUBDIR)/lib$(LIBBASENAME).$(LIBEXT)
OBJDIR=$(OBJBUILDDIR)/obj/$(PLATFORM)/$(OBJSUBDIR)
EXTLIBDIR=$(EXTBASEDIR)/$(PLATFORM)

IOUTILLIB=$(EXTLIBDIR)/libioutil.a

shlib: $(LIB)

#
# list of obj files....
#
OBJS=$(OBJDIR)/main.o \
     $(OBJDIR)/guicb.o \
     $(OBJDIR)/extprec.o\
     $(OBJDIR)/guidefs.o

EHEADER=
GHEADER=
GHEADER=GUIDEFS.h


#
# Main link command:
#
$(LIB) : $(OBJS)
	cd $(OBJDIR); $(LD) $(LDFLAGS) *.o $(EXTLIBDIR)/libadkutil.a -o $(LIB) >/dev/null 2>&1


#
# Duplicate the next line for additional source files...
#
$(OBJDIR)/main.o : main.c $(EHEADER) $(GHEADER)
	cd $(OBJDIR); $(CCOMP) $(CFLAGS) $(SOURCEDIR)/main.c 
$(OBJDIR)/guidefs.o : guidefs.c guibld.c $(GHEADER)
	cd $(OBJDIR); $(CCOMP) $(CFLAGS) $(SOURCEDIR)/guidefs.c 
$(OBJDIR)/guicb.o : guicb.c guibld.c $(GHEADER)
	cd $(OBJDIR); $(CCOMP) $(CFLAGS) $(SOURCEDIR)/guicb.c
$(OBJDIR)/extprec.o :  extprec.c EXTPREC.h  $(EHEADER) $(GHEADER)
	cd $(OBJDIR); $(CCOMP) $(CFLAGS) $(SOURCEDIR)/extprec.c


 
clean:
	rm obj/*/*/*.o > /dev/null 2>&1

