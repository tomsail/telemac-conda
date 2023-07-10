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
 * Unitary tests to create, open, close MED files
 */

#include <med.h>
#define MESGERR 1
#include <med_utils.h>

#include <string.h>

int main (int argc, char **argv)
{
  med_idt fid;
  char filename[] = "Unittest_MEDfile_1.med";
  char comment[] = "My first comment";
  char comment2[] = "My second comment";
  char commentToRead[MED_COMMENT_SIZE+1];
  med_bool hdfok, medok;
  med_int major,minor,release;
  char medversion[10];
  med_int majorFromStr, minorFromStr, releaseFromStr;
  char filenameFromId[MED_PATHNAME_SIZE+1] = "";
  char* filenameFromIdPtr = NULL;
  med_int filenamesize = 0;

  /* file creation */
  fid = MEDfileOpen(filename,MED_ACC_CREAT);
  if (fid < 0) {
    MESSAGE("ERROR : file creation");
    return -1;
  }

  /* write a comment */
  if (MEDfileCommentWr(fid,comment) < 0) {
    MESSAGE("ERROR : file comment writing");
    return -1;
  }

  /* Get filename */
  if ( (MEDfileName(fid,filenameFromId,MED_PATHNAME_SIZE+1) < 0 ) || 
       ( strncmp(filenameFromId,filename,MED_PATHNAME_SIZE) )) {
    MESSAGE("ERROR : file getting filename");
    return -1;
  }

  /* file closing */
  if (MEDfileClose(fid) < 0) {
    MESSAGE("ERROR : file closing");
    return -1;
  }

  /* file opening in READ ONLY access mode */
  fid = MEDfileOpen(filename,MED_ACC_RDONLY);
  if (fid < 0) {
    MESSAGE("ERROR : file opening in READ ONLY ACCESS mode");
    return -1;
  }

  /* med library version is read in the file */
  if (MEDfileNumVersionRd(fid,&major,&minor,&release) < 0) {
    MESSAGE("ERROR : MED version reading");
    ISCRUTE(major);
    ISCRUTE(minor);
    ISCRUTE(release);
    return -1;
  }
  if ((major != MED_MAJOR_NUM) ||
      (minor != MED_MINOR_NUM) ||
      (release != MED_RELEASE_NUM)) {
    MESSAGE("ERROR : The MED num version is not the good one");
    ISCRUTE(major);
    ISCRUTE(minor);
    ISCRUTE(release);
    return -1;
  }

  if (MEDfileStrVersionRd(fid,medversion) < 0) {
    MESSAGE("ERROR : MED str version reading");
    SSCRUTE(medversion);
    return -1;
  }
  sscanf(medversion,"MED-"IFORMAT"."IFORMAT"."IFORMAT,
	 &majorFromStr,&minorFromStr,&releaseFromStr);
  if ((major != majorFromStr) ||
      (minor != minorFromStr) ||
      (release != releaseFromStr)) {
    ISCRUTE(majorFromStr);
    ISCRUTE(minorFromStr);
    ISCRUTE(releaseFromStr);
    MESSAGE("ERROR : The MED num version is not the good one");
    SSCRUTE(medversion);
    return -1;
  }

  /* file comment reading */
  if (MEDfileCommentRd(fid,commentToRead) < 0) {
    MESSAGE("ERROR : file comment reading");
    return -1;
  }
  if (strcmp(comment,commentToRead)) {
    MESSAGE("ERROR : file comment is not the good one");
    SSCRUTE(comment);
    SSCRUTE(commentToRead);
    return -1;
  }

  /* Get filename */
  if ( (filenamesize=MEDfileName(fid,NULL,0)) < 0 ) { 
    MESSAGE("ERROR : file getting filename");
    return -1;
  } else 
    filenameFromIdPtr = (char * ) malloc((filenamesize+1)*sizeof(char));

  if ( (MEDfileName(fid,filenameFromIdPtr,filenamesize) < 0) || 
       ( strncmp(filenameFromIdPtr,filename,filenamesize) )) {
    MESSAGE("ERROR : file getting filename");
    free(filenameFromIdPtr);
    return -1;
  }
  free(filenameFromIdPtr);
  
  /* file closing */
  if (MEDfileClose(fid) < 0) {
    MESSAGE("ERROR : file closing");
    return -1;
  }

  /* file opening in READ and WRITE access mode */
  fid = MEDfileOpen(filename,MED_ACC_RDWR);
  if (fid < 0) {
    MESSAGE("ERROR : file opening in read and write access mode");
    return -1;
  }

  /* comment writing */
  if (MEDfileCommentWr(fid,comment2) < 0) {
    MESSAGE("ERROR : file comment writing");
    return -1;
  }

  /* file closing */
  if (MEDfileClose(fid) < 0) {
    MESSAGE("ERROR : file closing");
    return -1;
  }

  /* file opening in READ and EXTENSION access mode */
  fid = MEDfileOpen(filename,MED_ACC_RDEXT);
  if (fid < 0) {
    MESSAGE("ERROR : file opening in READ and EXTENSION access mode");
    return -1;
  }

  /* write a comment has to be impossible */
  printf("Un message d'erreur est attendu :\n");
  if (MEDfileCommentWr(fid,comment) == 0) {
    MESSAGE("ERROR : write comment has to be impossible");
    return -1;
  }
  printf("Fin du message d'erreur attendu.\n");

  /* file closing */
  if (MEDfileClose(fid) < 0) {
    MESSAGE("ERROR : file closing");
    return -1;
  }

  /* file compatibility test with hdf5
     and med library version */
  if (MEDfileCompatibility(filename,&hdfok,&medok) < 0) {
    MESSAGE("ERROR : file compatibility test");
    return -1;
  }

  if (! hdfok) {
    MESSAGE("ERROR : the file must be in hdf5 format");
    ISCRUTE(hdfok);
    return -1;
  }

  if (! medok) {
    MESSAGE("ERROR : the file must be compatible");
    ISCRUTE(medok);
    return -1;
  }

  return 0;

}
