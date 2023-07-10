## For having included files in DISTribution & INSTallation (public headers)
include_HEADERS =  med.hf med.hf77 med.hf90 med_proto.h \
		 medlibrary.h medfile.h medmesh.h medfield.h \
                 medfamily.h medequivalence.h medprofile.h \
		 medlocalization.h medlink.h medparameter.h \
	         medfilter.h medsubdomain.h medinterp.h medstructelement.h \
		 medC_win_dll.h MEDerreur.hxx med_exit_if.h med_err.h

## For having included files in  INSTallation (public headers)
nodist_include_HEADERS = med.h med_utils.h med_parameter.hf

## For having included files in DISTribution only (private headers)
noinst_HEADERS = med_outils.h  med_misc.h  \
	med_hdfi.h med_versioned_proto.h med_versioned_proto_en.h \
	med_versioned_hdfi.h med_versioned_misc.h med_versioned_cfi.h med_versioned.h \
	MEDversionedApi3.hxx MEDgetGeometryTypeFromName.hxx MEDmodeAcces.hxx \
	MEDfileVersion.hxx MEDfieldChecked.hxx med_vargs.h

# Ce fichier ne doit pas être dans la distribution, il doit être généré
#nobase_include_HEADERS = med/med_config.h

#swig python : Il est plus facile de générer les fichiers  _exception.i ds le répertoire
# où sont les .h correspondants (pour ne pas utiliser les VPATH)
SUFFIXES = _exception.i

BUILT_SOURCES = medlibrary_exception.i medfile_exception.i medmesh_exception.i medfield_exception.i medfamily_exception.i medequivalence_exception.i medprofile_exception.i medlocalization_exception.i medlink_exception.i medparameter_exception.i medfilter_exception.i medsubdomain_exception.i medinterp_exception.i medstructelement_exception.i

.h_exception.i:
	sed  -n 's/^.*\(MED[^(]*\)(.*$$/%MedException(\1)/p' $(top_srcdir)/include/$*.h > $@

SUBDIRS = 2.3.6 . 

CLEANFILES = med_config.h med.h med_utils.h med_parameter.hf
MAINTAINERCLEANFILES= medlibrary_exception.i medfile_exception.i medmesh_exception.i medfield_exception.i medfamily_exception.i medequivalence_exception.i medprofile_exception.i medlocalization_exception.i medlink_exception.i medparameter_exception.i medfilter_exception.i medsubdomain_exception.i medinterp_exception.i medstructelement_exception.i
MAINTAINERCLEANFILES += med_config.h.in Makefile.in
EXTRA_DIST = medlibrary_exception.i medfile_exception.i medmesh_exception.i medfield_exception.i medfamily_exception.i medequivalence_exception.i medprofile_exception.i medlocalization_exception.i medlink_exception.i medparameter_exception.i medfilter_exception.i medsubdomain_exception.i medinterp_exception.i medstructelement_exception.i
EXTRA_DIST += CMakeLists.txt
