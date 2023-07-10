include $(top_srcdir)/config/automake.common

SUFFIXES = .med .dump

.med.dump:
	@H5DUMP@ $< > $@

check-local: \
./UsesCase_MEDfield_1.dump \
./UsesCase_MEDfield_4.dump \
./UsesCase_MEDmesh_10.dump \
./UsesCase_MEDmesh_1.dump \
./UsesCase_MEDmesh_4.dump \
./UsesCase_MEDmesh_6.dump \
./UsesCase_MEDmesh_9.dump \
./UsesCase_MEDmesh_13.dump

CLEANFILES = \
./UsesCase_MEDfield_1.dump \
./UsesCase_MEDfield_4.dump \
./UsesCase_MEDmesh_10.dump \
./UsesCase_MEDmesh_1.dump \
./UsesCase_MEDmesh_4.dump \
./UsesCase_MEDmesh_6.dump \
./UsesCase_MEDmesh_9.dump \
./UsesCase_MEDmesh_13.dump


EXTRA_DIST = \
./UsesCase_MEDfield_1.med \
./UsesCase_MEDfield_4.med \
./UsesCase_MEDmesh_10.med \
./UsesCase_MEDmesh_1.med \
./UsesCase_MEDmesh_4.med \
./UsesCase_MEDmesh_6.med \
./UsesCase_MEDmesh_9.med \
./UsesCase_MEDmesh_13.med


