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
 * Field use case 1 : write a field on mesh vertices and elements
 */

#include <med.h>
#define MESGERR 1
#include <med_utils.h>

#include <string.h>


int main (int argc, char **argv) {
  med_idt fid;
  const char meshname[MED_NAME_SIZE+1] = "2D unstructured mesh";
  const char fieldname[MED_NAME_SIZE+1] = "TEMPERATURE_FIELD";
  const med_int ncomponent = 1;
  const char componentname[MED_SNAME_SIZE+1] = "TEMPERATURE";
  const char componentunit[MED_SNAME_SIZE+1] = "C";
  const med_float verticesvalues[15] = {  0.,   100., 200.,  300.,  400.,
					  500.,   600., 700.,  800.,  900,
					  1000., 1100, 1200., 1300., 1500. };
  const med_int nnodes = 15;
  const med_float tria3values[8] = {1000., 2000., 3000., 4000.,
				    5000., 6000., 7000., 8000.};
  const med_int ntria3 = 8;
  const med_float quad4values[4] = {10000., 20000., 30000., 4000.};
  const med_int nquad4 = 4;
  int ret=-1;

  /* file creation */
  fid = MEDfileOpen("UsesCase_MEDfield_1.med",MED_ACC_CREAT);
  if (fid < 0) {
    MESSAGE("ERROR : file creation ...");
    goto ERROR;
  }

  /* field creation : temperature field  : 1 component in celsius degree
   *                  the mesh is the 2D unstructured mesh of UsescaseMEDmesh_1.c
   */

  /* create mesh link */
  if (MEDlinkWr(fid,meshname,"./UsesCase_MEDmesh_1.med") < 0) {
    MESSAGE("ERROR : create mesh link ...");
    goto ERROR;
  }

  if (MEDfieldCr(fid, fieldname, MED_FLOAT64,
		 ncomponent, componentname, componentunit,"",
		 meshname) < 0) {
    MESSAGE("ERROR : create field");
    goto ERROR;
  }

  /* write field values at vertices */
  if (MEDfieldValueWr(fid, fieldname, MED_NO_DT, MED_NO_IT, 0.0, MED_NODE,
		      MED_NONE, MED_FULL_INTERLACE, MED_ALL_CONSTITUENT,
		      nnodes,(unsigned char*) verticesvalues) < 0) {
    MESSAGE("ERROR : write field values on vertices");
    goto ERROR;
  }

  /* write values at cell centers : 8 MED_TRIA3 and 4 MED_QUAD4 */
  /* MED_TRIA3 */
  if (MEDfieldValueWr(fid, fieldname, MED_NO_DT, MED_NO_IT, 0.0, MED_CELL,
		      MED_TRIA3, MED_FULL_INTERLACE, MED_ALL_CONSTITUENT,
		      ntria3, (unsigned char*) tria3values) < 0) {
    MESSAGE("ERROR : write field values on MED_TRIA3");
    goto ERROR;
  }
  /* MED_QUAD4 */
  if (MEDfieldValueWr(fid, fieldname, MED_NO_DT, MED_NO_IT, 0.0, MED_CELL,
		      MED_QUAD4, MED_FULL_INTERLACE, MED_ALL_CONSTITUENT,
		      nquad4, (unsigned char*) quad4values) < 0) {
    MESSAGE("ERROR : write field values on MED_QUAD4 ");
    goto ERROR;
  }

  ret=0;
 ERROR :

  /* close file */
  if (MEDfileClose(fid) < 0) {
    MESSAGE("ERROR : close file ...");
    ret = -1;
  }

  return ret;
}

