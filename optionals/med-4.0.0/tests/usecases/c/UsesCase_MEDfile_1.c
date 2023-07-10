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

#include <med.h>
#define MESGERR 1
#include <med_utils.h>

#include <string.h>

int main (int argc, char **argv) {

 
 /* how to mount a MED file with a mesh and a MED file with a field in a 
    root MED file */

  med_idt fid;

  /* file creation */
  fid = MEDfileOpen("UsesCase_MEDfile_1.med",MED_ACC_CREAT);
  if (fid < 0) {
    MESSAGE("ERROR : file creation");
    return -1;
  }

  /* comment writing */
  if (MEDfileCommentWr(fid,"A root MED file") < 0) {
    MESSAGE("ERROR : file comment writing");
    return -1;
  }

  /* file closing */
  if (MEDfileClose(fid) < 0) {
    MESSAGE("ERROR : file closing");             
    return -1; 
  } 

  return 0;
}

