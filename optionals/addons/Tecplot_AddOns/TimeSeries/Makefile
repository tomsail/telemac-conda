SHELL=/bin/sh
LIB=$(TECADDONDEVDIR)/lib/$(PLATFORM)/$(OBJSUBDIR)/lib$(LIBBASENAME).$(LIBEXT)
OBJDIR=$(OBJBUILDDIR)/obj/$(PLATFORM)/$(OBJSUBDIR)
EXTLIBDIR=$(EXTBASEDIR)/$(PLATFORM)

shlib: $(LIB)

#
# list of obj files....
#
OBJS=$(OBJDIR)/main.o

EHEADER=
GHEADER=



#
# Main link command:
#
$(LIB) : $(OBJS)
	cd $(OBJDIR); $(CPPLD) $(CPPLDFLAGS) *.o -o $(LIB)

#
# Duplicate the next line for additional source files...
#
$(OBJDIR)/main.o : main.cpp $(EHEADER) $(GHEADER)
	cd $(OBJDIR); $(CPPCOMP) $(CPPFLAGS) $(SOURCEDIR)/main.cpp

 
clean:
	rm obj/*/*/*.o > /dev/null 2>&1

