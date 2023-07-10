include $(top_srcdir)/config/automake.common

#docdir = $(datadir)/doc/html
#doc_DATA =

#Oblige la reconstruction pour la règle make (all)
#du fichier Doxyfile.cfg en cas de modification d'une de ses dépendances
all-am: Doxyfile.cfg aliases.als

html-local: all-am
#MEDfileC2Fref.dox

aliases.als: $(srcdir)/MEDlibrary.als $(srcdir)/MEDfile.als  \
$(srcdir)/MEDmesh.als  $(srcdir)/MEDsupportMesh.als $(srcdir)/MEDfield.als  $(srcdir)/MEDfamily.als $(srcdir)/MEDequivalence.als \
$(srcdir)/MEDlink.als $(srcdir)/MEDjoint.als $(srcdir)/MEDlocalization.als $(srcdir)/MEDinterp.als $(srcdir)/MEDfilter.als $(srcdir)/MEDparameter.als $(srcdir)/MEDstructElement.als $(srcdir)/MEDprofile.als

Doxyfile.cfg: $(srcdir)/customDoxygenLayout.xml $(builddir)/aliases.als $(srcdir)/toc_reference.dox

# Construit la liste des exemples à documenter (cf EXAMPLE_PATH du doxyconfig)
examples_dox.tmp:
	find $(srcdir)/../../tests -name '*.c' -o -name '*.f' > $(builddir)/examples.tmp; \
	echo "/*!" > $(builddir)/examples_dox.tmp && sed 's,^.*/tests/,\\example ,g' $(builddir)/examples.tmp >> $(builddir)/examples_dox.tmp && echo "*/" >> $(builddir)/examples_dox.tmp; \
	rm -f $(builddir)/examples.tmp

examples.dox: examples_dox.tmp
	diff $(builddir)/examples_dox.tmp $(srcdir)/examples.dox || @CP@ $(builddir)/examples_dox.tmp $(srcdir)/examples.dox ; \
	rm examples_dox.tmp

all-local: examples_dox.tmp examples.dox

EXTRA_DIST = Doxyfile.cfg.in customDoxygenLayout.xml \
aliases.als.in MEDlibrary.als MEDfile.als MEDmesh.als MEDsupportMesh.als MEDfield.als MEDfamily.als MEDlink.als MEDjoint.als MEDlocalization.als MEDinterp.als MEDfilter.als MEDstructElement.als  MEDequivalence.als MEDparameter.als MEDprofile.als toc_reference.dox \
MEDfileFortran.dox MEDlibraryFortran.dox MEDmeshFortran.dox MEDfieldFortran.dox MEDequivalenceFortran.dox \
MEDfamilyFortran.dox MEDlinkFortran.dox MEDfilterFortran.dox MEDparameterFortran.dox MEDlocalizationFortran.dox MEDprofileFortran.dox MEDsupportFortran.dox MEDjointFortran.dox MEDinterpFortran.dox MEDstructElementFortran.dox \
examples.dox

EXTRA_DIST+= CMakeLists.txt Doxyfile.cfg.cmake.in aliases.als.cmake.in 

htmlclean:
	rm -f example.dox

maintainer-clean-local: htmlclean

