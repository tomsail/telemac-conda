include $(top_srcdir)/config/automake.common

SUFFIXES = .med .dump

.med.dump:
	@H5DUMP@ $< > $@

#test2.dump: test2.med
#	@H5DUMP@ -a /ENS_MAA/maa1/DIM -a /ENS_MAA/maa1/DES -a /ENS_MAA/maa1/TYP -g /ENS_MAA/maa2 -g /ENS_MAA/maa3 -g /INFOS_GENERALES $< > $@

#BUILT_SOURCES = 

check-local: \
./Test_MEDmeshStructElementVarAttWr.dump \
./Test_MEDstructElementConstAttWithProfileWr.dump \
./Test_MEDstructElementCr.dump \
./Test_MEDstructElementVarAttCr.dump \
./Test_MEDsupportMesh.dump \
./Unittest_MEDfile_1.dump \
./Unittest_MEDparameter_1.dump


CLEANFILES = \
./Test_MEDmeshStructElementVarAttWr.dump \
./Test_MEDstructElementConstAttWithProfileWr.dump \
./Test_MEDstructElementCr.dump \
./Test_MEDstructElementVarAttCr.dump \
./Test_MEDsupportMesh.dump \
./Unittest_MEDfile_1.dump \
./Unittest_MEDparameter_1.dump

EXTRA_DIST = \
./Test_MEDmeshStructElementVarAttWr.med \
./Test_MEDstructElementConstAttWithProfileWr.med \
./Test_MEDstructElementCr.med \
./Test_MEDstructElementVarAttCr.med \
./Test_MEDsupportMesh.med \
./Unittest_MEDfile_1.med \
./Unittest_MEDparameter_1.med

