INCLUDE(CheckIncludeFile)
IF(CMAKE_Fortran_COMPILER_WORKS)
  INCLUDE(FortranCInterface)
ENDIF(CMAKE_Fortran_COMPILER_WORKS)
INCLUDE(CheckFunctionExists)
INCLUDE(CheckTypeSize)

###############################################################################
##Define constants
###############################################################################

SET(PACKAGE_NAME "\"MED Fichier\"")
SET(MED_CHECK_23FORMAT 1)
SET(MED_HAVE_FORTRAN 1)
SET(MESGERR 1)
SET(PACKAGE \"${PROJECT_NAME}\")
SET(PACKAGE_BUGREPORT \"eric.fayolle@edf.fr\")
SET(PACKAGE_STRING "\"MED Fichier ${MED_STR_VERSION}\"")
SET(PACKAGE_TARNAME "${PROJECT_NAME}")
SET(PACKAGE_URL \"\")
SET(PACKAGE_VERSION \"${MED_STR_VERSION}\")
SET(VERSION \"${MED_STR_VERSION}\")

###############################################################################
## TEMPORARY HARDCODED FLAGS, BECAUSE CMAKE HAS NO TOOLS TO 
## THEIR DETERMINATION.
##  !!! SHOULD BE REMOVED AFTER FUTURE IMPROVEMENTS !!!
###############################################################################
IF(NOT WINDOWS)
   SET(HAVE_CC_C99 1)
   SET(STDC_HEADERS 1)
ENDIF(NOT WINDOWS)
SET(HAVE__BOOL 1)

MACRO(MED_CREATE_CONFIG_FILES)
    SET(f_content_new "")
    FILE(READ ${PROJECT_SOURCE_DIR}/include/med_config.h.in f_content)
    STRING(REPLACE "\n" ";" list_f_content ${f_content})
    FOREACH(ln ${list_f_content})
        STRING(REGEX MATCH "#undef.*" flag_name "${ln}")
        STRING(REPLACE "#undef" "" flag_name "${flag_name}")
        STRING(STRIP "${flag_name}" flag_name)
        STRING(LENGTH "${flag_name}" len)
        IF(${len} GREATER 0)
            STRING(STRIP "${flag_name}" flag_name)
            IF(flag_name MATCHES "^F77_FUNC.*")
               SET(ln "#cmakedefine ${flag_name}@${flag_name}@")
            ELSE()
               SET(ln "#cmakedefine ${flag_name} @${flag_name}@")
            ENDIF()
        ENDIF(${len} GREATER 0)
        SET(f_content_new "${f_content_new}${ln}\n")
    ENDFOREACH(ln ${list_f_content})

    FILE(WRITE ${PROJECT_BINARY_DIR}/include/med_config.h.cmake "${f_content_new}")
    FILE(WRITE ${PROJECT_BINARY_DIR}/include/2.3.6/med_config.h.cmake "${f_content_new}")
ENDMACRO(MED_CREATE_CONFIG_FILES)

##
## Macro to extract version numbers from Autotools file
##
MACRO(MED_EXTRACT_AUTOTOOLS_VERSION filename major minor release)
  FILE(READ ${filename} _f_content)
  STRING(REGEX MATCH "MED_NUM_MAJEUR=([0-9]+)" _output_unused ${_f_content})
  SET(${major} ${CMAKE_MATCH_1})
  
  STRING(REGEX MATCH "MED_NUM_MINEUR=([0-9]+)" _output_unused ${_f_content})
  SET(${minor} ${CMAKE_MATCH_1})
  
  STRING(REGEX MATCH "MED_NUM_RELEASE=([0-9]+)" _output_unused ${_f_content})
  SET(${release} ${CMAKE_MATCH_1})
ENDMACRO(MED_EXTRACT_AUTOTOOLS_VERSION)

###############################################################################
## Macro to set compilation flag for target
###############################################################################
MACRO(MED_SET_DEFINITIONS lib)
   STRING(LENGTH "${ARGN}" len)
   IF(${len})
     # Doc says:
     # COMPILE_DEFINITIONS property may be set to a semicolon-separated list 
     # of preprocessor definitions using the syntax VAR or VAR=value.
     SET_TARGET_PROPERTIES(${lib} PROPERTIES COMPILE_DEFINITIONS "${ARGN}")
     #MESSAGE("bli ${ARGN}")
   ENDIF(${len})
ENDMACRO(MED_SET_DEFINITIONS)

###############################################################################
## Macro to check header files
###############################################################################
MACRO(MED_CHECK_HEADERS)

#Define list of headers
SET(H_LIST "H5public;dlfcn;inttypes;malloc;memory;pwd;stdbool;stdint;stdlib")
SET(H_LIST "${H_LIST};stdbool;stdint;stdlib;strings;string;sys/stat")
SET(H_LIST "${H_LIST};sys/timeb;sys/time;sys/types;unistd;stdio")

#Check each header
FOREACH(H_FILE ${H_LIST})
   SET(STR_FILE "${H_FILE}")  
   STRING(REPLACE "/" "_"  STR_FILE ${STR_FILE})
   STRING(TOUPPER  "${STR_FILE}" STR_FILE)
   SET(H_FILE_FLAG "HAVE_${STR_FILE}_H")
   CHECK_INCLUDE_FILE("${H_FILE}.h" ${H_FILE_FLAG})
ENDFOREACH(H_FILE ${H_LIST})

ENDMACRO(MED_CHECK_HEADERS)

###############################################################################
## Macro to check methods files
###############################################################################
MACRO(MED_CHECK_FUNCTIONS)

#Define list of functions
SET(F_LIST "cuserid;ftime;geteuid;getpwuid;gettimeofday")

#Check each header
FOREACH(F_FUNC ${F_LIST})
   STRING(TOUPPER   "${F_FUNC}" STR_F_FUNC)
   SET(F_FUNC_FLAG "HAVE_${STR_F_FUNC}")
   CHECK_FUNCTION_EXISTS("${F_FUNC}" ${F_FUNC_FLAG})
ENDFOREACH(F_FUNC ${F_LIST})

ENDMACRO(MED_CHECK_FUNCTIONS)

###############################################################################
## Macro to get sizeof Fortran INTEGER
###############################################################################
MACRO(CHECK_FORTRAN_INTEGER_SIZE)

SET(SIZEOF_FORTRAN_INTEGER 4 CACHE INTERNAL "Size of the default INTEGER type" FORCE)

IF(CMAKE_Fortran_COMPILER_SUPPORTS_F90)
    FOREACH(_TEST_SIZE 1 2 4 8 16 32)
       SET(_TEST_FILE ${PROJECT_BINARY_DIR}/testFortranIntegerSize${_TEST_SIZE}.f90)
       FILE( WRITE ${_TEST_FILE}
       "
       PROGRAM check_size
       INTEGER*${_TEST_SIZE}, TARGET :: a
       INTEGER, POINTER :: pa
       pa => a
       END PROGRAM
       ")
       TRY_COMPILE( SIZEOF_INTEGER ${PROJECT_BINARY_DIR} ${_TEST_FILE} )
       IF(SIZEOF_INTEGER)
          MESSAGE(STATUS "Testing default INTEGER*${_TEST_SIZE} - found")
          SET(SIZEOF_FORTRAN_INTEGER ${_TEST_SIZE})
          BREAK()
       ENDIF(SIZEOF_INTEGER)
    ENDFOREACH(_TEST_SIZE 1 2 4 8 16 32)
ENDIF(CMAKE_Fortran_COMPILER_SUPPORTS_F90)
ENDMACRO(CHECK_FORTRAN_INTEGER_SIZE)

###############################################################################
## Macro to check sizeof
###############################################################################
MACRO(MED_CHECK_SIZE)
#int
CHECK_TYPE_SIZE(int SIZEOF_INT)
#long
CHECK_TYPE_SIZE(long SIZEOF_LONG)
#Fortran INTEGER 
CHECK_FORTRAN_INTEGER_SIZE()

IF(${SIZEOF_FORTRAN_INTEGER} EQUAL 8)
    SET(HAVE_F77INT64 1)
    IF(${SIZEOF_LONG} EQUAL 8)
        SET(LONG_OR_INT long)
    ELSE(${SIZEOF_LONG} EQUAL 8)
        MESSAGE(FATAL_ERROR "Size of C type long expected to be eight bytes")
    ENDIF(${SIZEOF_LONG} EQUAL 8)
ELSE(${SIZEOF_FORTRAN_INTEGER} EQUAL 8)
    IF(${SIZEOF_FORTRAN_INTEGER} EQUAL 4)
        IF(${SIZEOF_INT} EQUAL 4)
           SET(LONG_OR_INT int)
        ELSE(${SIZEOF_INT} EQUAL 4)
           MESSAGE(FATAL_ERROR "Size of C type int expected to be four bytes")
        ENDIF(${SIZEOF_INT} EQUAL 4)
    ENDIF(${SIZEOF_FORTRAN_INTEGER} EQUAL 4)
ENDIF(${SIZEOF_FORTRAN_INTEGER} EQUAL 8)

##
## The below is a light porting of what the M4 macro config/med_check_typeof_int64.m4 does
##

# Try to find 64 and 32 bits types on the platform: 
SET(HAVE_INT64 0)
SET(MED_INT64 0)
SET(MED_INT32 0)
CHECK_TYPE_SIZE(int64_t SIZEOF_INT64T)
IF (${SIZEOF_INT64T} EQUAL 8)
    MESSAGE(STATUS "Type 'int64_t' exists and is 8 bytes")
    SET(MED_INT64 "int64_t")
    SET(HAVE_INT64 1)
ELSE()
    MESSAGE(WARNING "Type 'int64_t' has WRONG size or is UNDEFINED (${SIZEOF_INT64T})!! 64bits fields will be unavailable")
    SET(MED_INT64 "med_int")
ENDIF()

CHECK_TYPE_SIZE(int32_t SIZEOF_INT32T)
IF (${SIZEOF_INT32T} EQUAL 4)
    MESSAGE(STATUS "Type 'int32_t' exists and is 4 bytes")
    SET(MED_INT32 "int32_t")
ELSE()
    MESSAGE(FATAL_ERROR "Type 'int32_t' has WRONG size or is UNDEFINED (${SIZEOF_INT32T})!! Unable to pursue.")
ENDIF()

# In the HDF5 world we have to come back to native types ('long' or 'long long') since int64_t or int32_t
# won't be understood
SET(MED_H5T_INT64)
CHECK_TYPE_SIZE("long long" SIZEOF_LONGLONG)
CHECK_TYPE_SIZE("long" SIZEOF_LONG)
IF (${SIZEOF_LONG} EQUAL 8)
    SET(MED_H5T_INT64 "H5T_NATIVE_LONG")
    MESSAGE(STATUS "MED_H5T_INT64 will map to H5T_NATIVE_LONG")
ELSE()
    IF (${SIZEOF_LONGLONG} EQUAL 8)
        SET(MED_H5T_INT64 "H5T_NATIVE_LLONG")
        MESSAGE(STATUS "MED_H5T_INT64 will map to H5T_NATIVE_LLONG")
    ELSE()
        MESSAGE(FATAL_ERROR "Neither 'long' or 'long long' are 8 bytes long!! Unable to pursue.")
    ENDIF()
ENDIF()

ENDMACRO(MED_CHECK_SIZE)

###############################################################################
## Macro to get C <-> Fortran mangling
###############################################################################
MACRO(MED_C_FORTRAN_INTERFACE)
IF(CMAKE_Fortran_COMPILER_WORKS)
  FortranCInterface_HEADER(${PROJECT_BINARY_DIR}/include/FC.h)
  FILE(READ ${PROJECT_BINARY_DIR}/include/FC.h f_content)
  STRING(REPLACE "\n" ";" list_f_content ${f_content})
  
  FOREACH(ln ${list_f_content})
    IF(${ln} MATCHES "#define FortranCInterface_GLOBAL[^_].*")
       STRING(REPLACE "#define FortranCInterface_GLOBAL" ""  F77_FUNC "${ln}")
    ENDIF()

    IF("${ln}" MATCHES "#define FortranCInterface_GLOBAL_.*")
       STRING(REPLACE "#define FortranCInterface_GLOBAL_" "" F77_FUNC_ "${ln}")
    ENDIF()
  ENDFOREACH(ln ${list_f_content})

ELSE(CMAKE_Fortran_COMPILER_WORKS)

    SET(F77_FUNC "(name,NAME) NAME")

ENDIF(CMAKE_Fortran_COMPILER_WORKS)

ENDMACRO(MED_C_FORTRAN_INTERFACE)

###############################################################################
## Macro to check possibility include time.h and sys/time.h headers together
###############################################################################
MACRO(MED_TIME_SYS_TIME)

SET(_TEST_FILE ${PROJECT_BINARY_DIR}/testTimeSysTime.c)
FILE( WRITE ${_TEST_FILE}
"
#include <sys/types.h>
#include <sys/time.h>
#include <time.h>

#  ifdef __cplusplus
     extern \"C\"
#  endif

int
main ()
{
if ((struct tm *) 0)
return 0;
  ;
  return 0;
}
"
)

TRY_COMPILE( TIME_WITH_SYS_TIME ${PROJECT_BINARY_DIR} ${_TEST_FILE} )
IF(${TIME_WITH_SYS_TIME})
  SET(TIME_WITH_SYS_TIME 1)
ELSE(${TIME_WITH_SYS_TIME})
  SET(TIME_WITH_SYS_TIME 0)
ENDIF(${TIME_WITH_SYS_TIME})

ENDMACRO(MED_TIME_SYS_TIME)

###########################
# SALOME_CHECK_EQUAL_PATHS(result path1 path2)
#  Check if two paths are identical, resolving links. If the paths do not exist a simple
#  text comparison is performed.
#  result is a boolean.
###########################
MACRO(SALOME_CHECK_EQUAL_PATHS varRes path1 path2)  
  SET("${varRes}" OFF)
  IF(EXISTS "${path1}")
    GET_FILENAME_COMPONENT(_tmp1 "${path1}" REALPATH)
  ELSE()
    SET(_tmp1 "${path1}")
  ENDIF() 

  IF(EXISTS "${path2}")
    GET_FILENAME_COMPONENT(_tmp2 "${path2}" REALPATH)
  ELSE()
    SET(_tmp2 "${path2}")
  ENDIF() 

  IF("${_tmp1}" STREQUAL "${_tmp2}")
    SET("${varRes}" ON)
  ENDIF()
#  MESSAGE(${${varRes}})
ENDMACRO()

########################################################################
# SALOME_FIND_PACKAGE(englobingPackageName standardPackageName modus)
# Encapsulate the call to the standard FIND_PACKAGE(standardPackageName) passing all the options
# given when calling the command FIND_PACKAGE(customPackageName)
# Modus is either MODULE or CONFIG (cf standard FIND_PACKAGE() documentation).
# This macro is to be called from within the FindCustomPackage.cmake file.
########################################################################
MACRO(SALOME_FIND_PACKAGE englobPkg stdPkg mode)
  # Only bother if the package was not already found:
  # Some old packages use the lower case version - standard should be to always use
  # upper case:
  STRING(TOUPPER ${stdPkg} stdPkgUC)
  IF(NOT (${stdPkg}_FOUND OR ${stdPkgUC}_FOUND))
    IF(${englobPkg}_FIND_QUIETLY)
      SET(_tmp_quiet "QUIET")
    ELSE()
      SET(_tmp_quiet)
    ENDIF()  
    IF(${englobPkg}_FIND_REQUIRED)
      SET(_tmp_req "REQUIRED")
    ELSE()
      SET(_tmp_req)
    ENDIF()  
    IF(${englobPkg}_FIND_VERSION_EXACT)
      SET(_tmp_exact "EXACT")
    ELSE()
      SET(_tmp_exact)
    ENDIF()
    IF(${englobPkg}_FIND_COMPONENTS)
      STRING(REPLACE ";" " " _tmp_compo ${${englobPkg}_FIND_COMPONENTS})
    ELSE()
      SET(_tmp_compo)
    ENDIF()

    # Call the root FIND_PACKAGE():
    IF(_tmp_compo)
      FIND_PACKAGE(${stdPkg} ${${englobPkg}_FIND_VERSION} ${_tmp_exact} ${mode} ${_tmp_quiet} ${_tmp_req} COMPONENTS ${_tmp_compo})
    ELSE()
      FIND_PACKAGE(${stdPkg} ${${englobPkg}_FIND_VERSION} ${_tmp_exact} ${mode} ${_tmp_quiet} ${_tmp_req})
    ENDIF()
  ENDIF()
ENDMACRO()

###############################################################################
## Macro to find MPI and most of all set all relevant flags
###############################################################################
MACRO(MED_FIND_MPI)
  SET (MEDFILE_USE_MPI OFF CACHE BOOL "Use MPI to compile MED-file")  
  IF (MEDFILE_USE_MPI)
    MESSAGE(STATUS "Check for MPI ...")
  
    FIND_PACKAGE(MedfileMPI REQUIRED)

    # Library configuration:
    IF(WIN32)
      # Win specific stuff
      FIND_LIBRARY(MPI_LIB_THREAD libboost_thread-vc90-mt-gd-1_35 ${MPI_ROOT_DIR_EXP}/lib)
      FIND_LIBRARY(MPI_LIB_DATE_TIME libboost_date_time-vc90-mt-gd-1_35 ${MPI_ROOT_DIR_EXP}/lib)
    ELSE(WIN32)
      # set flags for both OpenMPI or MPICH:
      ADD_DEFINITIONS(-DOMPI_IGNORE_CXX_SEEK -DMPICH_IGNORE_CXX_SEEK)
    ENDIF(WIN32)
    # Gather all MPI libs in one place:
    SET(MPI_LIBS ${MPI_CXX_LIBRARIES})
    LIST(APPEND MPI_LIBS ${MPI_C_LIBRARIES})
    IF(CMAKE_Fortran_COMPILER_WORKS)
       LIST(APPEND MPI_LIBS ${MPI_Fortran_LIBRARIES})
    ENDIF()
    MESSAGE(STATUS "MPI libs: ${MPI_LIBS}")
  ELSE(MEDFILE_USE_MPI)
    MESSAGE(STATUS "Configuring without MPI (set MEDFILE_USE_MPI to True to change this)")
  ENDIF(MEDFILE_USE_MPI)
ENDMACRO(MED_FIND_MPI)

###############################################################################
## Macro to find HDF5 and most of all set all relevant flags
###############################################################################
MACRO(MED_FIND_HDF5)
    MESSAGE(STATUS "Check for HDF5 ...")
    
    FIND_PACKAGE(MedfileHDF5 REQUIRED)

    ##
    ## Requires 1.10.x version
    ##
    IF (NOT HDF_VERSION_MAJOR_REF EQUAL 1 OR NOT HDF_VERSION_MINOR_REF EQUAL 10 OR NOT HDF_VERSION_RELEASE_REF GREATER 1)
        MESSAGE(FATAL_ERROR "HDF5 version is ${HDF_VERSION_REF}. Only versions >= 1.10.2 are supported.")
    ENDIF()
    ##
    ##

    ADD_DEFINITIONS(-DH5_USE_16_API)  
    IF(WIN32 AND MEDFILE_BUILD_SHARED_LIBS)
      ADD_DEFINITIONS(-D_HDF5USEDLL_ -DH5_BUILT_AS_DYNAMIC_LIB=1)   
    ENDIF()
    
    # Take what is exposed by the standard FIND_PACKAGE()
    
    SET(HDF5_LIBS ${HDF5_LIBRARIES})

    IF(HDF5_IS_PARALLEL OR HDF5_ENABLE_PARALLEL)
      MESSAGE(STATUS "HDF5 is parallel")
    ENDIF()
    
    # Extract some stuff from headers
    FIND_FILE(_H5PUB_ABS_PATH H5public.h PATHS ${HDF5_INCLUDE_DIRS} NO_DEFAULT_PATH)
    FIND_FILE(_H5_I_PUB_ABS_PATH H5Ipublic.h PATHS ${HDF5_INCLUDE_DIRS} NO_DEFAULT_PATH)
    IF(NOT _H5PUB_ABS_PATH OR NOT _H5_I_PUB_ABS_PATH)
       MESSAGE(FATAL_ERROR "Could not find H5public.h or H5Ipublic.h!")
    ELSE()
      FILE(STRINGS "${_H5PUB_ABS_PATH}" _h5pub_contents)
      FOREACH(_line IN LISTS _h5pub_contents)
        SET(_match)
        STRING(REGEX MATCH "^[ \t]*typedef .*herr_t.*" _match "${_line}")
        IF(_match)
          SET(HDF5_TYPEDEF_HERR_T "${_match}")    
        ENDIF() 
        STRING(REGEX MATCH "^[ \t]*typedef .*hsize_t.*" _match "${_line}")
        IF(_match)
          SET(HDF5_TYPEDEF_HSIZE_T "${_match}")    
        ENDIF()  
      ENDFOREACH()
      
      FILE(STRINGS "${_H5_I_PUB_ABS_PATH}" _h5_i_pub_contents)
      FOREACH(_line IN LISTS _h5_i_pub_contents)
        SET(_match)
        STRING(REGEX MATCH "^[ \t]*typedef .*hid_t.*" _match "${_line}")
        IF(_match)
          SET(HDF5_TYPEDEF_HID_T "${_match}")
          BREAK()    
        ENDIF() 
      ENDFOREACH()
    ENDIF()
ENDMACRO(MED_FIND_HDF5)

#
# Install and compile a Python file - needs Python!
#
MACRO(INSTALL_AND_COMPILE_PYTHON_FILE PYFILE2COMPINST PYFILELOC)
  INSTALL(CODE "SET(PYTHON_FILE ${f})")
  FOREACH(input ${PYFILE2COMPINST})
    GET_FILENAME_COMPONENT(inputname ${input} NAME)
    INSTALL(FILES ${input} DESTINATION ${CMAKE_INSTALL_PREFIX}/${PYFILELOC})
    INSTALL(CODE "MESSAGE(STATUS \"py compiling ${CMAKE_INSTALL_PREFIX}/${PYFILELOC}/${inputname}\")")
    INSTALL(CODE "SET(CMD \"import py_compile ; py_compile.compile('${CMAKE_INSTALL_PREFIX}/${PYFILELOC}/${inputname}')\")")
    INSTALL(CODE "EXECUTE_PROCESS(COMMAND ${PYTHON_EXECUTABLE} -c \"\${CMD}\")")
    INSTALL(CODE "EXECUTE_PROCESS(COMMAND ${PYTHON_EXECUTABLE} -O -c \"\${CMD}\")")
  ENDFOREACH(input ${PYFILE2COMPINST})
ENDMACRO(INSTALL_AND_COMPILE_PYTHON_FILE PYFILE2COMPINST PYFILELOC)
