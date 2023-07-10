# Copyright (C) 2013  CEA/DEN, EDF R&D, OPEN CASCADE
#
# This library is free software; you can redistribute it and/or
# modify it under the terms of the GNU Lesser General Public
# License as published by the Free Software Foundation; either
# version 2.1 of the License.
#
# This library is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
# Lesser General Public License for more details.
#
# You should have received a copy of the GNU Lesser General Public
# License along with this library; if not, write to the Free Software
# Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307 USA
#
# See http://www.salome-platform.org/ or email : webmaster.salome@opencascade.com
#
# Author: Adrien Bruneton
#

# MPI detection for med-file

# 1. Load environment or any previously detected MPI
IF(DEFINED ENV{MPI_ROOT_DIR})
  FILE(TO_CMAKE_PATH "$ENV{MPI_ROOT_DIR}" _MPI_ROOT_DIR_ENV)
  SET(_dflt_value "${_MPI_ROOT_DIR_ENV}")
ELSE()
  # will be blank if no MPI was previously loaded
  SET(_dflt_value "${MPI_ROOT_DIR_EXP}")
ENDIF()

#   Make cache entry 
SET(MPI_ROOT_DIR "${_dflt_value}" CACHE PATH "Path to MPI directory")

# 2. Find package - config mode is impossible as MPI is compiled with autotools
IF(EXISTS "${MPI_ROOT_DIR}")
  SET(CMAKE_PREFIX_PATH "${MPI_ROOT_DIR}")
ENDIF()
INCLUDE(medMacros)
SALOME_FIND_PACKAGE(MedfileMPI MPI MODULE)
SET(MEDFILEMPI_FOUND ${MPI_FOUND})

IF(MPI_FOUND OR MPIEXEC)  # MPI_FOUND is declared as deprecated in CMake doc
  MESSAGE(STATUS "Found MPI!")

  # 3. Set the root dir which was finally retained 
  # Extract it from MPIEXEC:
  GET_FILENAME_COMPONENT(_tmp "${MPIEXEC}" REALPATH)  # dereference symlinks
  GET_FILENAME_COMPONENT(_tmp "${_tmp}" PATH)         # go up one level in the path 
  GET_FILENAME_COMPONENT(_tmp_ROOT_DIR "${_tmp}" PATH)

  # 4. Warn if CMake found something not located under ENV(XYZ_ROOT_DIR)
  IF(DEFINED ENV{MPI_ROOT_DIR})
    SALOME_CHECK_EQUAL_PATHS(_res "${_tmp_ROOT_DIR}" "${_MPI_ROOT_DIR_ENV}")
    IF(NOT _res)
      MESSAGE(WARNING "MPI was found, but not at the path given by the "
"environment MPI_ROOT_DIR! Is the variable correctly set?")
    ELSE()
      MESSAGE(STATUS "MPI found directory matches what was specified in the MPI_ROOT_DIR, all good!")    
    ENDIF()
  ENDIF()
      
  # Check for potential conflicts with previously configured MPI:  
  IF(MPI_ROOT_DIR_EXP)
      SALOME_CHECK_EQUAL_PATHS(_res "${_tmp_ROOT_DIR}" "${MPI_ROOT_DIR_EXP}") 
      IF(NOT _res)
         MESSAGE(WARNING "Warning: MPI: detected version conflicts with a previously found MPI!"
                          " The two paths are " ${_tmp_ROOT_DIR} " vs " ${MPI_ROOT_DIR_EXP})
      ELSE()
          MESSAGE(STATUS "MPI directory matches what was previously exposed by another prereq, all good!")
      ENDIF()        
  ENDIF()

  # 6. Include directories:  
  SET(MPI_INCLUDE_DIRS ${MPI_CXX_INCLUDE_PATH})
  LIST(APPEND MPI_INCLUDE_DIRS ${MPI_C_INCLUDE_DIR})
  LIST(APPEND MPI_INCLUDE_DIRS ${MPI_Fortran_INCLUDE_DIR})

  MARK_AS_ADVANCED(MPI_EXTRA_LIBRARY MPI_LIBRARY)

  SET(MPI_ROOT_DIR ${_tmp_ROOT_DIR})

  SET(MED_DEFINE_MED_HAVE_MPI "#define MED_HAVE_MPI")

ELSE(MPI_FOUND OR MPIEXEC)
  MESSAGE(STATUS "MPI was not found.")  
ENDIF(MPI_FOUND OR MPIEXEC)
