INCLUDE_DIRECTORIES(
  ${HDF5_INCLUDE_DIRS}
  ${MPI_INCLUDE_DIRS}
)
 
SET(med236_SOURCES MEDaffiche.c)

# Definitions
ADD_DEFINITIONS(-DMED_API_23)

# Include are specific for the 2.3.6 version - make sure they get incl first:
INCLUDE_DIRECTORIES(BEFORE 
    "${PROJECT_BINARY_DIR}/include/2.3.6"
    "${PROJECT_SOURCE_DIR}/include/2.3.6")

# Build intermediate targets _cfi236, _hdfi236, _misc236 and _ci236 
# (and their static counterpart):
ADD_SUBDIRECTORY(cfi)
ADD_SUBDIRECTORY(ci)
ADD_SUBDIRECTORY(hdfi)
ADD_SUBDIRECTORY(misc)

# Build intermediate Fortran target _fi236:
IF(CMAKE_Fortran_COMPILER_WORKS)
  ADD_SUBDIRECTORY(fi)
ENDIF()

# Intermediary target:
ADD_LIBRARY(_med236  OBJECT ${med236_SOURCES})
