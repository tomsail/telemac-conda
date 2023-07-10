INCLUDE_DIRECTORIES(
  ${HDF5_INCLUDE_DIRS}
  ${MPI_INCLUDE_DIRS})

# On Win platform, we need to make sure symbols are properly exported in
# the intermediary OBJECT targes built in the sub-directories:
# The definition "-D<target>_EXPORTS" is normally added automatically by CMake
# when building Win DLL,
# but this doesn't work when using intermediary targets (ADD_LIBRARY(... OBJECT ...)
IF(WIN32)
  ADD_DEFINITIONS(-DmedC_EXPORTS)
ENDIF()
  
# Specific include logic - see CMakeLists.txt inside:
ADD_SUBDIRECTORY("2.3.6")

# Important: the following include comes AFTER the subdirectory 2.3.6
# to ensure the include works properly (2.3.6 includes are specific)
INCLUDE_DIRECTORIES(${PROJECT_BINARY_DIR}/include 
    ${PROJECT_SOURCE_DIR}/include)

# Get pure C intermediary targets:
ADD_SUBDIRECTORY(hdfi)
ADD_SUBDIRECTORY(ci)
ADD_SUBDIRECTORY(misc)

IF(CMAKE_Fortran_COMPILER_WORKS)
  # Build intermediate targets _cfi, _cfi_static and _fi:
  ADD_SUBDIRECTORY(cfi)
  ADD_SUBDIRECTORY(fi)

  SET(medfort_wrap_SOURCES
    MEDiteratorsF.f 
    $<TARGET_OBJECTS:_fi236> 
    $<TARGET_OBJECTS:_fi>
    $<TARGET_OBJECTS:_cfi236> 
    $<TARGET_OBJECTS:_cfi>
  )
  
  SET(medfort_static_wrap_SOURCES
    MEDiteratorsF.f 
    $<TARGET_OBJECTS:_fi236> 
    $<TARGET_OBJECTS:_fi>
    $<TARGET_OBJECTS:_cfi236_static> 
    $<TARGET_OBJECTS:_cfi_static>
  )
ENDIF()

SET(medC_SOURCES
  MEDiterators.c
  $<TARGET_OBJECTS:_ci236>
  $<TARGET_OBJECTS:_hdfi236>
  $<TARGET_OBJECTS:_misc236>
  $<TARGET_OBJECTS:_med236>
  $<TARGET_OBJECTS:_ci>
  $<TARGET_OBJECTS:_misc>
  $<TARGET_OBJECTS:_hdfi>  
)

SET(medC_static_SOURCES
  MEDiterators.c
  $<TARGET_OBJECTS:_ci236_static>
  $<TARGET_OBJECTS:_hdfi236_static>
  $<TARGET_OBJECTS:_misc236_static>
  $<TARGET_OBJECTS:_med236_static>
  $<TARGET_OBJECTS:_ci_static>
  $<TARGET_OBJECTS:_misc_static>
  $<TARGET_OBJECTS:_hdfi_static>  
)

########################### Pure C stuff ###################################
############################################################################

### Installation preparation:
SET(_export_group "medfileTargets")

######### Shared Libraries ##########
IF(MEDFILE_BUILD_SHARED_LIBS)
  ADD_LIBRARY(medC SHARED ${medC_SOURCES})
  SET_TARGET_PROPERTIES(medC PROPERTIES
    VERSION   11.0.0
    SOVERSION 11.0.0)
  TARGET_LINK_LIBRARIES(medC ${HDF5_LIBS} ${MPI_LIBS})
  MED_SET_DEFINITIONS(medC NOGDI)
  
  INSTALL(TARGETS medC EXPORT ${_export_group} DESTINATION lib)  
ENDIF()

######### Static Libraries ##########
##
## Target names are suffixed with "_static" but file 
## names remain the same.
 
IF(MEDFILE_BUILD_STATIC_LIBS)
  ADD_LIBRARY(medC_static STATIC ${medC_SOURCES})
  SET_TARGET_PROPERTIES(medC_static PROPERTIES OUTPUT_NAME medC)
  TARGET_LINK_LIBRARIES(medC_static ${HDF5_LIBS} ${MPI_LIBS})
  MED_SET_DEFINITIONS(medC_static NOGDI)
  
  INSTALL(TARGETS medC_static EXPORT ${_export_group} DESTINATION lib)
ENDIF()

########################### Fortran stuff ###################################
#############################################################################
IF(CMAKE_Fortran_COMPILER_WORKS)
  
  ######### Shared Libraries ##########
  IF(MEDFILE_BUILD_SHARED_LIBS)
    ## Intermediate library - this is unavoidable 
    ## if we want the Fortran wrapper to build correctly under win.
    ADD_LIBRARY(medfwrap SHARED ${medfort_wrap_SOURCES})
    SET_TARGET_PROPERTIES(medfwrap PROPERTIES
      VERSION   11.0.0
      SOVERSION 11.0.0)
    TARGET_LINK_LIBRARIES(medfwrap medC)
    INSTALL(TARGETS medfwrap EXPORT medfileTargetsF DESTINATION lib)

    # Add Shared MED library
    ADD_LIBRARY(med SHARED MEDiterators.c)
    SET_TARGET_PROPERTIES(med PROPERTIES
      VERSION   11.0.0
      SOVERSION 11.0.0)
    TARGET_LINK_LIBRARIES(medfwrap medC)
    TARGET_LINK_LIBRARIES(med medfwrap)    
   
    # Install only the resulting library:
    INSTALL(TARGETS med EXPORT medTargetsF DESTINATION lib)
  ENDIF()

  ######### Static Libraries ##########
  ##
  ## Target names are suffixed with "_static" but file 
  ## names remain the same.
  IF(MEDFILE_BUILD_STATIC_LIBS)
    ## Intermediate library - this is unavoidable 
    ## if we want the Fortran wrapper to build correctly under win.
    ADD_LIBRARY(medfwrap_static STATIC ${medfort_wrap_SOURCES})
    SET_TARGET_PROPERTIES(medfwrap_static PROPERTIES OUTPUT_NAME medfwrap)
    TARGET_LINK_LIBRARIES(medfwrap_static medC_static)
    INSTALL(TARGETS medfwrap_static EXPORT medfileTargetsF DESTINATION lib)
    
    # Add Static MED library
    ADD_LIBRARY(med_static STATIC MEDiterators.c)
    SET_TARGET_PROPERTIES(med_static PROPERTIES OUTPUT_NAME med)
    TARGET_LINK_LIBRARIES(med_static medfwrap_static)

    # Install only the resulting library:  
    INSTALL(TARGETS med_static EXPORT medfileTargetsF DESTINATION lib)
  ENDIF()
  
ENDIF(CMAKE_Fortran_COMPILER_WORKS)
