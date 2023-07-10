/* include/med_config.h.  Generated from med_config.h.in by configure.  */
/* include/med_config.h.in.  Generated from configure.ac by autoheader.  */

/* Define this to an absolute name of <H5Ipublic.h>. */
#define ABSOLUTE_H5IPUBLIC_H "///home/tomsail/apps/telemac-mascaret/optionals/hdf5-1.10.3/include/H5Ipublic.h"

/* Define this to an absolute name of <H5public.h>. */
#define ABSOLUTE_H5PUBLIC_H "///home/tomsail/apps/telemac-mascaret/optionals/hdf5-1.10.3/include/H5public.h"

/* Define to dummy `main' function (if any) required to link to the Fortran
   libraries. */
/* #undef F77_DUMMY_MAIN */

/* Define to a macro mangling the given C identifier (in lower and upper
   case), which must not contain underscores, for linking with Fortran. */
#define F77_FUNC(name,NAME) name ## _

/* As F77_FUNC, but for C identifiers containing underscores. */
#define F77_FUNC_(name,NAME) name ## _

/* Define if F77 and FC dummy `main' functions are identical. */
/* #undef FC_DUMMY_MAIN_EQ_F77 */

/* The C99 capabilities of C compiler. */
#define HAVE_CC_C99 1

/* Define to 1 if you have the `cuserid' function. */
#define HAVE_CUSERID 1

/* Define to 1 if you have the <dlfcn.h> header file. */
#define HAVE_DLFCN_H 1

/* The size of a Fortran integer for hid_t C type, as computed by sizeof. */
#define HAVE_F77HIDT64 /**/

/* The size of a Fortran integer, as computed by sizeof. */
/* #undef HAVE_F77INT64 */

/* Define to 1 if you have the `ftime' function. */
#define HAVE_FTIME 1

/* Define to 1 if you have the `geteuid' function. */
#define HAVE_GETEUID 1

/* Define to 1 if you have the `getpwuid' function. */
#define HAVE_GETPWUID 1

/* Define to 1 if you have the `gettimeofday' function. */
#define HAVE_GETTIMEOFDAY 1

/* Define to 1 if you have the <H5Ipublic.h> header file. */
#define HAVE_H5IPUBLIC_H 1

/* Define to 1 if you have the `H5Pset_fapl_mpio' function. */
#define HAVE_H5PSET_FAPL_MPIO 1

/* Define to 1 if you have the <H5public.h> header file. */
#define HAVE_H5PUBLIC_H 1

/* The size of a int64_t, as computed by sizeof. */
#define HAVE_INT64 /**/

/* Define to 1 if you have the <inttypes.h> header file. */
#define HAVE_INTTYPES_H 1

/* Define to 1 if you have the `hdf5' library (-lhdf5). */
#define HAVE_LIBHDF5 1

/* Define to 1 if you have the <malloc.h> header file. */
#define HAVE_MALLOC_H 1

/* Define to 1 if you have the <memory.h> header file. */
#define HAVE_MEMORY_H 1

/* Define if you have the MPI library. */
#define HAVE_MPI 1

/* Define to 1 if you have the <pwd.h> header file. */
#define HAVE_PWD_H 1

/* If available, contains the Python version number currently in use. */
/* #undef HAVE_PYTHON */

/* Define to 1 if stdbool.h conforms to C99. */
#define HAVE_STDBOOL_H 1

/* Define to 1 if you have the <stdint.h> header file. */
#define HAVE_STDINT_H 1

/* Define to 1 if you have the <stdlib.h> header file. */
#define HAVE_STDLIB_H 1

/* Define to 1 if you have the <strings.h> header file. */
#define HAVE_STRINGS_H 1

/* Define to 1 if you have the <string.h> header file. */
#define HAVE_STRING_H 1

/* Define to 1 if you have the <sys/stat.h> header file. */
#define HAVE_SYS_STAT_H 1

/* Define to 1 if you have the <sys/timeb.h> header file. */
#define HAVE_SYS_TIMEB_H 1

/* Define to 1 if you have the <sys/time.h> header file. */
#define HAVE_SYS_TIME_H 1

/* Define to 1 if you have the <sys/types.h> header file. */
#define HAVE_SYS_TYPES_H 1

/* Define to 1 if you have the <unistd.h> header file. */
#define HAVE_UNISTD_H 1

/* Define to 1 if the system has the type `_Bool'. */
#define HAVE__BOOL 1

/* Define to the sub-directory where libtool stores uninstalled libraries. */
#define LT_OBJDIR ".libs/"

/* Define to 1 if the complete 2.3.6 API is provided. */
#define MED_API_23 1

/* Return an error if a MED file contains at least one object of a previous
   data model which is not be managable by a MED3.0 function. */
#define MED_CHECK_23FORMAT 1

/* The hdf type to use for 64bits signed integer type. */
#define MED_H5T_INT64 H5T_NATIVE_LONG

/* Define if the Fortran part of MED library must be compiled. */
#define MED_HAVE_FORTRAN 1

/* Define if the Python part of MED library must be installed. */
#define MED_HAVE_PYTHON 0

/* The size of a long imply the size of python integer via swig. */
#define MED_SWIG_INT64 /**/

/* MED library will not display error messages. */
#define MESGERR 1

/* Name of package */
#define PACKAGE "med"

/* Define to the address where bug reports for this package should be sent. */
#define PACKAGE_BUGREPORT "eric.fayolle@edf.fr"

/* Define to the full name of this package. */
#define PACKAGE_NAME "MED Fichier"

/* Define to the full name and version of this package. */
#define PACKAGE_STRING "MED Fichier 4.0.0"

/* Define to the one symbol short name of this package. */
#define PACKAGE_TARNAME "med"

/* Define to the home page for this package. */
#define PACKAGE_URL ""

/* Define to the version of this package. */
#define PACKAGE_VERSION "4.0.0"

/* The size of a Fortran `integer', as computed by sizeof. */
#define SIZEOF_FORTRAN_INTEGER 4

/* The size of a Fortran `integer*4', as computed by sizeof. */
/* #undef SIZEOF_FORTRAN_INTEGERp4 */

/* The size of a Fortran `integer*8', as computed by sizeof. */
#define SIZEOF_FORTRAN_INTEGERp8 8

/* The size of `hid_t', as computed by sizeof. */
#define SIZEOF_HID_T 8

/* The size of `int', as computed by sizeof. */
#define SIZEOF_INT 4

/* The size of `long', as computed by sizeof. */
#define SIZEOF_LONG 8

/* The size of `long long', as computed by sizeof. */
#define SIZEOF_LONG_LONG 8

/* Define to 1 if you have the ANSI C header files. */
#define STDC_HEADERS 1

/* Define to 1 if you can safely include both <sys/time.h> and <time.h>. */
#define TIME_WITH_SYS_TIME 1

/* Version number of package */
#define VERSION "4.0.0"

/* Define to empty if `const' does not conform to ANSI C. */
/* #undef const */

/* Define to the type of a signed integer type of width exactly 32 bits if
   such a type exists and the standard includes do not define it. */
/* #undef int32_t */

/* Define to the type of a signed integer type of width exactly 64 bits if
   such a type exists and the standard includes do not define it. */
/* #undef int64_t */

/* Define to `unsigned int' if <sys/types.h> does not define. */
/* #undef size_t */
