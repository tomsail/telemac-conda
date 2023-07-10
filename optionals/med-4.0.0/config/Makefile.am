include $(top_srcdir)/config/automake.common

AUX_DIST = \
	config.guess \
	config.sub \
	install-sh \
	ltconfig \
	ltmain.sh \
	mdate.sh \
	missing \
	mkinstalldirs \
	depcomp \
	libtool.m4 \
	lt~obsolete.m4 \
	ltsugar.m4 \
	ltversion.m4 \
	ltoptions.m4 \
	ltsugar.m4


MAINTAINERCLEANFILES += $(AUX_DIST)
CLEANFILES	     = package.m4

EXTRA_DIST = package.m4 automake.common automake.common.2.3.6 \
cmake_files/deprecated \
cmake_files/FindMedfileHDF5.cmake \
cmake_files/FindMedfileMPI.cmake \
cmake_files/MEDFileConfig.cmake.in \
cmake_files/medMacros.cmake

all-local: package.m4

package.m4: Makefile
	@{ \
	  echo '# Signature of the current package.'; \
	  echo 'm4_define([AT_PACKAGE_NAME],	  [@PACKAGE_NAME@])'; \
	  echo 'm4_define([AT_PACKAGE_TARNAME],	  [@PACKAGE_TARNAME@])'; \
	  echo 'm4_define([AT_PACKAGE_VERSION],	  [@PACKAGE_VERSION@])'; \
	  echo 'm4_define([AT_PACKAGE_STRING],	  [@PACKAGE_STRING@])'; \
	  echo 'm4_define([AT_PACKAGE_BUGREPORT], [@PACKAGE_BUGREPORT@])'; \
	} > package.m4
	@echo "Creating package.m4"
