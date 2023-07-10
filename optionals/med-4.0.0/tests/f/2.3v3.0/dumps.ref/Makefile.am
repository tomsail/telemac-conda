include $(top_srcdir)/config/automake.common

SUFFIXES = .med .dump

.med.dump:
	@H5DUMP@ $< > $@

#test2.dump: test2.med
#	@H5DUMP@ -a /ENS_MAA/maa1/DIM -a /ENS_MAA/maa1/DES -a /ENS_MAA/maa1/TYP -g /ENS_MAA/maa2 -g /ENS_MAA/maa3 -g /INFOS_GENERALES $< > $@

#test31.dump: test31.med
#	@H5DUMP@  -a /ENS_MAA/maa1/DIM -a /ENS_MAA/maa1/DES -a /ENS_MAA/maa1/TYP -d /ENS_MAA/maa1/NOE/COO -d /ENS_MAA/maa1/NOE/FAM -d /ENS_MAA/maa1/NOE/NOM -d /ENS_MAA/maa1/NOE/NUM -g /INFOS_GENERALES $< > $@ && ( @H5DUMP@ -d /ENS_MAA/maa1/NOE/GLB $< | $(AWK) 'BEGIN {i=0;p=0} i==0 && $$0 ~ /ATTRIBUTE "NBR"/ {i=1;} i==1 && $$0 ~ /\{/ {p=p+1;} i==1 && $$0 ~ /\}/ {p=p-1; if (p==0) {i=0;next;} } i==0 {print;}' >> $@ )

#BUILT_SOURCES =
check-local:\
./test20.dump

CLEANFILES = \
./test20.dump

EXTRA_DIST = \
./test20.med

