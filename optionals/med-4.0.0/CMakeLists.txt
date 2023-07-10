CMAKE_MINIMUM_REQUIRED(VERSION 2.8.10 FATAL_ERROR)
PROJECT(MEDFile C CXX)

## The following signature is still buggy as of CMake 2.8.11 - 
## the OPTIONAL is not considered.
## See http://www.cmake.org/Bug/view.php?id=10227
#ENABLE_LANGUAGE(Fortran OPTIONAL)

INCLUDE(CheckLanguage)
CHECK_LANGUAGE(Fortran)
IF(CMAKE_Fortran_COMPILER)
  ENABLE_LANGUAGE(Fortran)
ELSE()
  MESSAGE(STATUS "No Fortran support")
ENDIF()

## Version number 
SET(MED_NUM_MAJEUR  4)
SET(MED_NUM_MINEUR  0)
SET(MED_NUM_RELEASE 0)

## Previous version
SET(MED_3_LATEST_MINOR 3)

SET(MED_STR_VERSION "${MED_NUM_MAJEUR}.${MED_NUM_MINEUR}.${MED_NUM_RELEASE}")

SET(LONG_OR_INT int)

## User options:
OPTION (MEDFILE_BUILD_TESTS "Build MED-file tests" ON)
IF (MEDFILE_BUILD_TESTS)
  ENABLE_TESTING()
ENDIF()
OPTION (MEDFILE_BUILD_SHARED_LIBS "Build MED-file shared libraries" ON)
OPTION (MEDFILE_BUILD_STATIC_LIBS "Build MED-file static libraries" OFF)
OPTION (MEDFILE_INSTALL_DOC "Install pre-built documentation" ON)
OPTION (MEDFILE_BUILD_PYTHON "Build Python bindings (needs SWIG)" OFF)

#-- Add an Option to toggle the generation of the API documentation
#-- Embedded documentation contains patched doxygen generated fortran documention
#-- If you rebuild it with your own doxygen Fortran documentation will differ
option(MEDFILE_BUILD_DOC "Use Doxygen to create the HTML based API documentation" OFF)
if(MEDFILE_BUILD_DOC)
  FIND_PACKAGE(Doxygen)
  if (NOT DOXYGEN_FOUND)
    message(FATAL_ERROR
      "Doxygen is needed to build the documentation. Please install it correctly")
  endif()

  ADD_SUBDIRECTORY(doc)
endif()

# Option checking.
IF(MEDFILE_BUILD_SHARED_LIBS AND MEDFILE_BUILD_STATIC_LIBS)
  MESSAGE(FATAL_ERROR "You can not build shared and static libraries at the same time! ")
  SET(MEDFILE_BUILD_STATIC_LIBS OFF)
ENDIF()
IF(NOT (MEDFILE_BUILD_SHARED_LIBS OR MEDFILE_BUILD_STATIC_LIBS))
  MESSAGE(FATAL_ERROR "You must select to build either STATIC or SHARED libs")
  SET(MEDFILE_BUILD_SHARED_LIBS ON)
ENDIF()

## Useful macros
LIST(APPEND CMAKE_MODULE_PATH "${PROJECT_SOURCE_DIR}/config/cmake_files")
INCLUDE(medMacros)

## Version number
#MED_EXTRACT_AUTOTOOLS_VERSION(${PROJECT_SOURCE_DIR}/configure.ac MED_NUM_MAJEUR MED_NUM_MINEUR MED_NUM_RELEASE)
#SET(MED_STR_VERSION "${MED_NUM_MAJEUR}.${MED_NUM_MINEUR}.${MED_NUM_RELEASE}")

## Platform specific flags (windows)
IF(WIN32)
  ADD_DEFINITIONS(-D_CRT_SECURE_NO_WARNINGS)  # To disable windows warnings for strcpy, fopen, ...
  ADD_DEFINITIONS(-D_SCL_SECURE_NO_WARNINGS)  # To disable windows warnings std::copy, std::transform, ...
  ADD_DEFINITIONS(-DWNT -DWIN32)
  ADD_DEFINITIONS(-D_WIN32_WINNT=0x0500) # Windows 2000 or later API is required
  ADD_DEFINITIONS(-DPPRO_NT_CALL) # For windows system functions call

  IF(NOT MINGW)
    # Needed to avoid a seg fault at runtime!
    # The variable is not very well documented but seems to exist
    SET(CMAKE_Fortran_FLAGS  "/iface:mixed_str_len_arg ${CMAKE_Fortran_FLAGS}")
    ADD_DEFINITIONS(-DPPRO_NT) # For windows medfile function nammes
  ENDIF()

ELSE()
  # Ensure intermediate OBJECT libraries and static libs are built with -fPIC:
  SET(CMAKE_POSITION_INDEPENDENT_CODE ON)
ENDIF(WIN32)

## Directories
IF(WIN32 AND NOT CYGWIN)
   SET(INSTALL_CMAKE_CONFIG_DIR cmake)
ELSE()
   SET(INSTALL_CMAKE_CONFIG_DIR share/cmake/med-fichier-${MED_STR_VERSION})
ENDIF()
SET(INSTALL_INCLUDE_DIR include)

## Pre-requisites detection
# Detect HDF5 first to potentially retrieve MPI automatically:
MED_FIND_HDF5()
MED_FIND_MPI()

IF(MEDFILE_BUILD_PYTHON)
  FIND_PACKAGE(SWIG REQUIRED)
  INCLUDE(${SWIG_USE_FILE})
  FIND_PACKAGE(PythonLibs REQUIRED)
  FIND_PACKAGE(PythonInterp REQUIRED)  # to get version string ...
ENDIF()

## Creation of files med_config.h.cmake
MED_CREATE_CONFIG_FILES()

## Check for various functions
MED_CHECK_HEADERS()
MED_CHECK_FUNCTIONS()
MED_CHECK_SIZE()
MED_C_FORTRAN_INTERFACE()
MED_TIME_SYS_TIME()

## Configure the files created above
CONFIGURE_FILE("${PROJECT_BINARY_DIR}/include/med_config.h.cmake" "${PROJECT_BINARY_DIR}/include/med_config.h")
CONFIGURE_FILE("${PROJECT_BINARY_DIR}/include/2.3.6/med_config.h.cmake" "${PROJECT_BINARY_DIR}/include/2.3.6/med_config.h")

## Sources 
ADD_SUBDIRECTORY(include)
ADD_SUBDIRECTORY(src)
ADD_SUBDIRECTORY(tools)
IF(MEDFILE_BUILD_TESTS)
  ADD_SUBDIRECTORY(tests)
ENDIF()
IF(MEDFILE_BUILD_PYTHON)
  ADD_SUBDIRECTORY(python)
ENDIF()

## Installation of the doc
IF(MEDFILE_INSTALL_DOC  OR MEDFILE_BUILD_DOC)
  INSTALL(DIRECTORY doc/html.dox/ DESTINATION ${CMAKE_INSTALL_PREFIX}/share/doc/med-fichier-${MED_STR_VERSION} REGEX "Makefile*" EXCLUDE PATTERN "*.am" EXCLUDE)
  INSTALL(DIRECTORY doc/gif doc/jpg doc/png DESTINATION ${CMAKE_INSTALL_PREFIX}/share/doc/med-fichier-${MED_STR_VERSION} FILES_MATCHING PATTERN "*.gif" PATTERN "*.jpg" PATTERN "*.svg" PATTERN "*.png")
ENDIF()

#######################
## Configuration export
#######################
INCLUDE(CMakePackageConfigHelpers)

# Project name, upper case
STRING(TOUPPER ${PROJECT_NAME} PROJECT_NAME_UC)

# Detect if we are built as a sub-project in a bigger source tree:
IF("${CMAKE_SOURCE_DIR}" STREQUAL "${PROJECT_SOURCE_DIR}")
  SET(_BUILT_AS_SUB FALSE)
  MESSAGE(STATUS "Configuring ${PROJECT_NAME} standalone.")
ELSE()
  MESSAGE(STATUS "Configuring ${PROJECT_NAME} as a sub-project. No conf files will be installed.")
  SET(_BUILT_AS_SUB TRUE)
ENDIF()

# Add all targets to the build-tree export set if we are *not*  
# built as a sub-folder of a bigger project.
IF(NOT _BUILT_AS_SUB)
  IF(CMAKE_Fortran_COMPILER_WORKS)
    SET(_fortLib med medfwrap)
    SET(_fortLibS med_static medfwrap_static)
  ELSE()
    SET(_fortLib "")
    SET(_fortLibS "")
  ENDIF()
  
  IF(MEDFILE_BUILD_STATIC_LIBS)
    EXPORT(TARGETS medC_static ${_fortLibS}
      FILE ${PROJECT_BINARY_DIR}/${PROJECT_NAME}Targets.cmake)
  ENDIF()
  IF(MEDFILE_BUILD_SHARED_LIBS) 
    EXPORT(TARGETS medC ${_fortLib}
      APPEND FILE ${PROJECT_BINARY_DIR}/${PROJECT_NAME}Targets.cmake)
  ENDIF()
  
ENDIF()

# Create the configuration files:
#   - in the build tree:
SET(HDF5_ROOT_DIR "${HDF5_ROOT_DIR}")  # ensure the var exists, even if blank
SET(MPI_ROOT_DIR "${MPI_ROOT_DIR}")    # ensure the var exists, even if blank
SET(CONF_INCLUDE_DIRS "${PROJECT_SOURCE_DIR}/include" "${PROJECT_BINARY_DIR}/include")
CONFIGURE_PACKAGE_CONFIG_FILE(config/cmake_files/${PROJECT_NAME}Config.cmake.in 
    ${PROJECT_BINARY_DIR}/${PROJECT_NAME}Config.cmake
    INSTALL_DESTINATION "${INSTALL_CMAKE_CONFIG_DIR}"
    PATH_VARS CONF_INCLUDE_DIRS INSTALL_CMAKE_CONFIG_DIR CMAKE_INSTALL_PREFIX HDF5_ROOT_DIR MPI_ROOT_DIR)

#   - in the install tree:
#       Get the relative path of the include directory so 
#       we can register it in the generated configuration files:
SET(CONF_INCLUDE_DIRS "${CMAKE_INSTALL_PREFIX}/${INSTALL_INCLUDE_DIR}")
CONFIGURE_PACKAGE_CONFIG_FILE(config/cmake_files/${PROJECT_NAME}Config.cmake.in 
    ${PROJECT_BINARY_DIR}/to_install/${PROJECT_NAME}Config.cmake
    INSTALL_DESTINATION "${INSTALL_CMAKE_CONFIG_DIR}"
    PATH_VARS CONF_INCLUDE_DIRS INSTALL_CMAKE_CONFIG_DIR CMAKE_INSTALL_PREFIX HDF5_ROOT_DIR MPI_ROOT_DIR)

WRITE_BASIC_PACKAGE_VERSION_FILE(${PROJECT_BINARY_DIR}/${PROJECT_NAME}ConfigVersion.cmake
    VERSION ${MED_STR_VERSION}
    COMPATIBILITY AnyNewerVersion)
  
# Install the CMake configuration files:
INSTALL(FILES
  "${PROJECT_BINARY_DIR}/to_install/${PROJECT_NAME}Config.cmake"
  "${PROJECT_BINARY_DIR}/${PROJECT_NAME}ConfigVersion.cmake"
  DESTINATION "${INSTALL_CMAKE_CONFIG_DIR}")

# Install the export set for use within the install-tree
IF(NOT _BUILT_AS_SUB)
  INSTALL(EXPORT medfileTargets DESTINATION "${INSTALL_CMAKE_CONFIG_DIR}" FILE ${PROJECT_NAME}Targets.cmake)
ENDIF()

