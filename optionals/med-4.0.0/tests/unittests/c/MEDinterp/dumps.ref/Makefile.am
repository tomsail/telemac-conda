include $(top_srcdir)/config/automake.common

SUFFIXES = .med .dump

.med.dump:
	@H5DUMP@ $< > $@

#test2.dump: test2.med
#	@H5DUMP@ -a /ENS_MAA/maa1/DIM -a /ENS_MAA/maa1/DES -a /ENS_MAA/maa1/TYP -g /ENS_MAA/maa2 -g /ENS_MAA/maa3 -g /INFOS_GENERALES $< > $@

#BUILT_SOURCES = 

check-local: \
./Test_MEDinterpCr.dump \
./Test_MEDinterpBaseFunctionWr.dump \
./Test_MEDfieldInterpWr.dump


CLEANFILES = \
./Test_MEDinterpCr.dump \
./Test_MEDinterpBaseFunctionWr.dump \
./Test_MEDfieldInterpWr.dump

EXTRA_DIST = \
./Test_MEDinterpCr.med \
./Test_MEDinterpBaseFunctionWr.med \
./Test_MEDfieldInterpWr.med

