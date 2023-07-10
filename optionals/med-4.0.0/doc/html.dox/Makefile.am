include $(top_srcdir)/config/automake.common

#On utilise trois fichiers pour éviter des listes trop grandes
#que le shell n'arriverait pas à gérer
include $(srcdir)/htmllistfile1.am
include $(srcdir)/htmllistfile2.am
include $(srcdir)/htmllistfile3.am

#docdir = $(datadir)/doc/html

#doc_DATA = 
EXTRA_DIST = $(srcdir)/htmllistfile1.am $(srcdir)/htmllistfile2.am $(srcdir)/htmllistfile3.am
EXTRA_DIST += CMakeLists.txt

#La règle suivante permet de générer la doc à inclure dans la distribution
#La documentation générée n'est pas versionnée.
#Le fichier $(srcdir)/htmllistfile.am est également généré,
#il contient la liste des fichiers à installer et est inclus dans ce Makefile.am
#Seuls le distributeur et les developpeurs ont besoin de l'outil doxygen
#La commande find crée une liste des fichiers de documentation sous forme
#d'une colomne où chaque fin de ligne est constituée du caractère de continuation  '\'
#La commande sed supprime le caractère '\' de la dernière ligne

html-local: $(builddir)/../dox/Doxyfile.cfg
	echo "Running doxygen in directory: "`pwd`; \
	doxygen $< ; \
	echo "Generate $(srcdir)/htmllistfile1.am $(srcdir)/htmllistfile2.am $(srcdir)/htmllistfile3.am" ; \
	echo "htmllistfile1= \\" > $(builddir)/htmllistfile1.am.tmp ; \
	echo "htmllistfile2= \\" > $(builddir)/htmllistfile2.am.tmp ; \
	echo "htmllistfile3= \\" > $(builddir)/htmllistfile3.am.tmp ; \
	find . \( -name "*[0-5].html" \) -exec echo {}\\ \;  | @SED@ '$$s,^\(.*\)\\$$,\1,g' >> $(builddir)/htmllistfile1.am.tmp ; \
	find . \( -name "*[^0-9].html" \) -exec echo {}\\ \;  | @SED@ '$$s,^\(.*\)\\$$,\1,g' >> $(builddir)/htmllistfile2.am.tmp ; \
	find . \( -name "*[6-9].html" -o -name "*.jpg"  -o -name "*.gif" -o -name "*.png"  -o -name "*.svg" -o -name "*.map" -o -name "*.css" -o -name "*.js" -o -name "*.dot" \) -exec echo {}\\ \; | @SED@ '$$s,^\(.*\)\\$$,\1,g' >> $(builddir)/htmllistfile3.am.tmp ; \
	diff $(builddir)/htmllistfile1.am.tmp $(srcdir)/htmllistfile1.am || @CP@ $(builddir)/htmllistfile1.am.tmp $(srcdir)/htmllistfile1.am ; \
	diff $(builddir)/htmllistfile3.am.tmp $(srcdir)/htmllistfile3.am || @CP@ $(builddir)/htmllistfile3.am.tmp $(srcdir)/htmllistfile3.am ; \
	diff $(builddir)/htmllistfile2.am.tmp $(srcdir)/htmllistfile2.am || @CP@ $(builddir)/htmllistfile2.am.tmp $(srcdir)/htmllistfile2.am

install-data-local: $(htmllistfile1) $(htmllistfile2) $(htmllistfile3)
	test -z "$(DESTDIR)$(docdir)" || $(MKDIR_P) "$(DESTDIR)$(docdir)"
	for i in $(htmllistfile1) ; do \
	  $(INSTALL_DATA) \
	     `test -f $$i || echo $(srcdir)/`$$i \
	     $(DESTDIR)$(docdir) ; \
	done 
	for i in $(htmllistfile2) ; do \
	  $(INSTALL_DATA) \
	     `test -f $$i || echo $(srcdir)/`$$i \
	     $(DESTDIR)$(docdir) ; \
	done
	for i in $(htmllistfile3) ; do \
	  $(INSTALL_DATA) \
	     `test -f $$i || echo $(srcdir)/`$$i \
	     $(DESTDIR)$(docdir) ; \
	done

uninstall-local: $(htmllistfile1) $(htmllistfile2) $(htmllistfile3)
	rm -rf $(DESTDIR)$(docdir); 

.PHONY: htmlclean

htmlclean:
	rm -f *.html *.jpg *.gif *.png *.map *.css *.md5 *.dot *.svg *.js htmllistfile1.am.tmp htmllistfile2.am.tmp htmllistfile3.am.tmp med.tag

maintainer-clean-local: htmlclean
	rm $(srcdir)/htmllistfile1.am && touch $(srcdir)/htmllistfile1.am ; \
	rm $(srcdir)/htmllistfile2.am && touch $(srcdir)/htmllistfile2.am ; \
	rm $(srcdir)/htmllistfile3.am && touch $(srcdir)/htmllistfile3.am 

#dist-hook: $(htmllistfile1) $(htmllistfile2) $(htmllistfile3) html-local
dist-hook: $(htmllistfile) $(htmllistfile2) $(htmllistfile3)
	for i in $(htmllistfile1) ; do \
	   $(INSTALL_DATA) \
	     `test -f $$i || echo $(srcdir)/`$$i \
	      $(distdir) ; \
	done
	for i in $(htmllistfile2) ; do \
	$(INSTALL_DATA) \
	     `test -f $$i || echo $(srcdir)/`$$i \
	     $(distdir) ; \
        done
	for i in $(htmllistfile3) ; do \
	 $(INSTALL_DATA) \
	     `test -f $$i || echo $(srcdir)/`$$i \
             $(distdir) ; \
	done

