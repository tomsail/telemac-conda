/*  This file is part of MED.
 *
 *  COPYRIGHT (C) 1999 - 2019  EDF R&D, CEA/DEN
 *  MED is free software: you can redistribute it and/or modify
 *  it under the terms of the GNU Lesser General Public License as published by
 *  the Free Software Foundation, either version 3 of the License, or
 *  (at your option) any later version.
 *
 *  MED is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU Lesser General Public License for more details.
 *
 *  You should have received a copy of the GNU Lesser General Public License
 *  along with MED.  If not, see <http://www.gnu.org/licenses/>.
 */

/*
 * Unitary tests for MED library module
 */

#include <med.h>
#define MESGERR 1
#include <med_utils.h>

#include <string.h>

int main (int argc, char **argv)
{
  med_int major,minor,release;
  med_int majorFromStr, minorFromStr, releaseFromStr;
  char version[11];

  /* Get library version numbers */
  if (MEDlibraryNumVersion(&major,&minor,&release) < 0) {
    MESSAGE("Error : library version numbers");
    return -1;
  }

  /* Get library version numbers in a string */
  if (MEDlibraryStrVersion(version) < 0) {
    MESSAGE("Error : library version numbers (in a string)");
    return -1;
  }
  sscanf(version,"MED-"IFORMAT"."IFORMAT"."IFORMAT,
	 &majorFromStr,&minorFromStr,&releaseFromStr);
  if ((major != majorFromStr) ||
      (minor != minorFromStr) ||
      (release != releaseFromStr)) {
    MESSAGE("ERROR : The MED num version is not the good one");
    SSCRUTE(version);
    return -1;
  }

  /* Get Hdf library version numbers */
  if (MEDlibraryHdfNumVersion(&major,&minor,&release) < 0) {
    MESSAGE("Error : Hdf library version numbers");
    return -1;
  }

  /* Get Hdf library version numbers in a string */
  if (MEDlibraryHdfStrVersion(version) < 0) {
    MESSAGE("Error : Hdf library version numbers (in a string)");
    return -1;
  }
  sscanf(version,"HDF5-"IFORMAT"."IFORMAT"."IFORMAT,
	 &majorFromStr,&minorFromStr,&releaseFromStr);
  if ((major != majorFromStr) ||
      (minor != minorFromStr) ||
      (release != releaseFromStr)) {
    MESSAGE("ERROR : The HDF num version is not the good one");
    SSCRUTE(version);ISCRUTE(major);ISCRUTE(minor);ISCRUTE(release);
    return -1;
  }

  /* flush all data and clean memory */
  if (MEDlibraryClose() < 0) {
    MESSAGE("ERROR : Med library close");
    return -1;
  }

  return 0;

}
