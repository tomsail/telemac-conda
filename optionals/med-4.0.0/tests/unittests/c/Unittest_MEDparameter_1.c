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
 * Unitary tests to create parameters in MED files
 */

#include <med.h>
#define MESGERR 1
#include <med_utils.h>

#include <string.h>

int main (int argc, char **argv)
{
  med_idt fid;
  const char filename[] = "Unittest_MEDparameter_1.med";
  const char comment[] = "Parameters unit tests";
  const char p1name[] = "FLOAT_PARAMETER";
  const char p2name[] = "INT_PARAMETER_1";
  const char p3name[] = "INT_PARAMETER_2";
  const med_parameter_type p1type = MED_FLOAT64;
  const med_parameter_type p2type = MED_INT;
  const med_parameter_type p3type = MED_INT;
  const char p1description[] = "PARAMETER 1 DESCRIPTION";
  const char p2description[] = "PARAMETER 2 DESCRIPTION";
  const char p3description[] = "PARAMETER 3 DESCRIPTION";
  const char dtunit1[] = "DT UNIT P1";
  const char dtunit2[] = "DT UNIT P2";
  const char dtunit3[] = "DT UNIT P3";
  med_int np;
  char nameToRead[MED_NAME_SIZE+1];
  char descriptionToRead[MED_COMMENT_SIZE+1];
  med_parameter_type typeToRead;
  char dtunitToRead[MED_SNAME_SIZE+1];
  med_int nstepToRead;
  int i,j;
  med_float p1v1 = 6.98;
  med_float p1v2 = 19.07;
  med_float p1v3 = 78.0;
  med_int p1numdt1 = MED_NO_DT;
  med_int p1numdt2 = 1;
  med_int p1numdt3 = 1;
  med_int p1numit1 = MED_NO_IT;
  med_int p1numit2 = 3;
  med_int p1numit3 = 2;
  med_float p1dt1 = 0.0;
  med_float p1dt2 = 0.1;
  med_float p1dt3 = 5.5;
  const med_int p1ncpst = 3;
  med_int p2v1 = 5;
  med_int p2v2 = 6;
  med_int p2numit1 = MED_NO_IT;
  med_int p2numdt1 = MED_NO_DT;
  med_float p2dt1 = 0.0;
  const med_int p2ncpst = 1;
  med_int p3v1 = 77;
  med_int p3v2 = 89;
  med_int p3numdt1 = 1;
  med_int p3numit1 = 1;
  med_float p3dt1 = 18.9;
  med_int p3numdt2 = MED_NO_DT;
  med_int p3numit2 = MED_NO_IT;
  med_float p3dt2 = 0.0;
  const med_int p3ncpst = 2;
  med_int numdtToRead;
  med_int numitToRead;
  med_float dtToRead; 
  med_float fvalueToRead;
  med_int ivalueToRead;

  /* file creation */
  fid = MEDfileOpen(filename,MED_ACC_CREAT);
  if (fid < 0) {
    MESSAGE("ERROR : file creation ...");
    SSCRUTE(filename);
    return -1;
  }

  if (MEDfileCommentWr(fid,comment) < 0) {
    MESSAGE("ERROR : write comment ...");
    SSCRUTE(comment);
    return -1;
  }

  /* create a MED_FLOAT64 parameter */
  if (MEDparameterCr(fid, p1name, p1type, p1description, dtunit1) < 0) {
        MESSAGE("ERROR : parameter p1 creation ...");
	SSCRUTE(p1name);
	return -1;
  }

  if (MEDparameterValueWr(fid, p1name, p1numdt1, p1numit1, p1dt1, (unsigned char*) &p1v1) < 0) {
        MESSAGE("ERROR : write p1v1   ...");
	SSCRUTE(p1name);
	return -1;
  }
  if (MEDparameterValueWr(fid, p1name, p1numdt2, p1numit2, p1dt2, (unsigned char*) &p1v2) < 0) {
        MESSAGE("ERROR : write p1v2   ...");
	SSCRUTE(p1name);
	return -1;
  }
  if (MEDparameterValueWr(fid, p1name, p1numdt3, p1numit3, p1dt3, (unsigned char*) &p1v3) < 0) {
        MESSAGE("ERROR : write p1v3   ...");
	SSCRUTE(p1name);
	return -1;
  }

  /* create a MED_INT parameter with different values */
  if (MEDparameterCr(fid, p2name, p2type, p2description, dtunit2) < 0) {
        MESSAGE("ERROR : paramter p2 creation ...");
	SSCRUTE(p2name);
	return -1;
  }
  if (MEDparameterValueWr(fid, p2name, p2numdt1, p2numit1, p2dt1, (unsigned char*) &p2v1) < 0) {
        MESSAGE("ERROR : write p2v1   ...");
	SSCRUTE(p2name);
	return -1;
  }
  if (MEDparameterValueWr(fid, p2name, p2numdt1, p2numit1, p2dt1, (unsigned char*) &p2v2) < 0) {
        MESSAGE("ERROR : write p2v2 on same computation step  ...");
	SSCRUTE(p2name);
	return -1;
  }

  /* create another MED_INT parameter */
  if (MEDparameterCr(fid, p3name, p3type, p3description, dtunit3) < 0) {
        MESSAGE("ERROR : paramter p3 creation ...");
	SSCRUTE(p3name);
	return -1;
  }
  if (MEDparameterValueWr(fid, p3name, p3numdt1, p3numit1, p3dt1, (unsigned char*) &p3v1) < 0) {
        MESSAGE("ERROR : write p3v1   ...");
	SSCRUTE(p3name);
	return -1;
  }
  if (MEDparameterValueWr(fid, p3name, p3numdt2, p3numit2, p3dt2, (unsigned char*) &p3v2) < 0) {
        MESSAGE("ERROR : write p3v2   ...");
	SSCRUTE(p3name);
	return -1;
  }

  /* close file */
  if (MEDfileClose(fid) < 0) {
    MESSAGE("ERROR : file closing ...");             
    return -1; 
  } 

  /* open file in READ ONLY access mode */
  fid = MEDfileOpen(filename,MED_ACC_RDONLY);
  if (fid < 0) {
    MESSAGE("ERROR : open in READ ONLY ACCESS mode ...");
    SSCRUTE(filename);
    return -1;
  }

  /* direct access to parameters */
  if (MEDparameterInfoByName(fid, p1name, &typeToRead, descriptionToRead,
			     dtunitToRead, &nstepToRead) < 0) {
    MESSAGE("ERROR : read information for parameter p1 ...");
    SSCRUTE(p1name);
    ISCRUTE(typeToRead);
    SSCRUTE(descriptionToRead);
    SSCRUTE(dtunitToRead);
    ISCRUTE(nstepToRead);
    return -1;
  }
  if ((typeToRead != p1type) || strcmp(descriptionToRead, p1description) ||
      strcmp(dtunitToRead,dtunit1) || (nstepToRead != p1ncpst)) {
    MESSAGE("ERROR : read information for parameter p1 : attributes ...");
    SSCRUTE(p1name);
    ISCRUTE(typeToRead);
    SSCRUTE(descriptionToRead);
    SSCRUTE(dtunitToRead);
    ISCRUTE(nstepToRead);
    return -1;
  }

  if (MEDparameterInfoByName(fid, p2name, &typeToRead, descriptionToRead,
			     dtunitToRead, &nstepToRead) < 0) {
    MESSAGE("ERROR : read information for parameter p2 ...");
    SSCRUTE(p2name);
    ISCRUTE(typeToRead);
    SSCRUTE(descriptionToRead);
    SSCRUTE(dtunitToRead);
    ISCRUTE(nstepToRead);
    return -1;
  }
  if ((typeToRead != p2type) || strcmp(descriptionToRead, p2description) ||
      strcmp(dtunitToRead,dtunit2) || (nstepToRead != p2ncpst)) {
    MESSAGE("ERROR : read information for parameter p2 : attributes ...");
    SSCRUTE(p2name); 
    ISCRUTE(typeToRead); ISCRUTE(p2type);
    SSCRUTE(descriptionToRead); SSCRUTE(p2description);
    SSCRUTE(dtunitToRead); SSCRUTE(dtunit2);
    ISCRUTE(nstepToRead); ISCRUTE(p2ncpst);
    return -1;
  }

  if (MEDparameterInfoByName(fid, p3name, &typeToRead, descriptionToRead,
			     dtunitToRead, &nstepToRead) < 0) {
    MESSAGE("ERROR : read information for parameter p3 ...");
    SSCRUTE(p3name);
    ISCRUTE(typeToRead);
    SSCRUTE(descriptionToRead);
    SSCRUTE(dtunitToRead);
    ISCRUTE(nstepToRead);
    return -1;
  }
  if ((typeToRead != p3type) || strcmp(descriptionToRead, p3description) ||
      strcmp(dtunitToRead,dtunit3) || (nstepToRead != p3ncpst)) {
    MESSAGE("ERROR : read information for parameter p2 : attributes ...");
    SSCRUTE(p3name);
    ISCRUTE(typeToRead);
    SSCRUTE(descriptionToRead);
    SSCRUTE(dtunitToRead);
    ISCRUTE(nstepToRead);
    return -1;
  }
  
  /* how many parameter(s) in the file ? */
  if ((np = MEDnParameter(fid)) < 0) {
    MESSAGE("ERROR : read number of paremeter ...");
    ISCRUTE(np);
    return -1;
  }
  if (np != 3) {
    MESSAGE("The number of parameter is 3 !");
    ISCRUTE(np);
    return -1;
  }
  /* TODO : A SUPPRIMER */
  np=0;

  /* read informations for each parameter */ 
  for (i=0;i<np;i++) {

    if (MEDparameterInfo(fid, i+1, nameToRead, &typeToRead, descriptionToRead,
			 dtunitToRead, &nstepToRead) < 0) {
      MESSAGE("ERROR : read information for parameter p3 ...");
      SSCRUTE(nameToRead);
      ISCRUTE(typeToRead);
      SSCRUTE(descriptionToRead);
      SSCRUTE(dtunitToRead);
      ISCRUTE(nstepToRead);
      return -1;
    }

    if (i == 0)
      if (strcmp(nameToRead, p1name) || (typeToRead != p1type) || 
	  strcmp(descriptionToRead, p1description) || strcmp(dtunitToRead,dtunit1) 
	  || (nstepToRead != p1ncpst)) {
	MESSAGE("ERROR : read information for parameter p1 : attributes ...");
	SSCRUTE(nameToRead);
	ISCRUTE(typeToRead);
	SSCRUTE(descriptionToRead);
	SSCRUTE(dtunitToRead);
	ISCRUTE(nstepToRead);
	return -1;
      }

    if (i == 1)
      if ( strcmp(nameToRead, p2name) || (typeToRead != p2type) || 
	   strcmp(descriptionToRead, p2description) || strcmp(dtunitToRead,dtunit2) 
	   || (nstepToRead != p2ncpst)) {
	MESSAGE("ERROR : read information for parameter p2 : attributes ...");
	SSCRUTE(nameToRead);
	ISCRUTE(typeToRead);
	SSCRUTE(descriptionToRead);
	SSCRUTE(dtunitToRead);
	ISCRUTE(nstepToRead);
	return -1;
      }

    if (i == 3)
      if (strcmp(nameToRead,p3name) || (typeToRead != p3type) || 
	  strcmp(descriptionToRead, p3description) || strcmp(dtunitToRead,dtunit3) || 
	  (nstepToRead != p3ncpst)) {
	MESSAGE("ERROR : read information for parameter p2 : attributes ...");
	SSCRUTE(nameToRead);
	ISCRUTE(typeToRead);
	SSCRUTE(descriptionToRead);
	SSCRUTE(dtunitToRead);
	ISCRUTE(nstepToRead);
	return -1;
      }

    /* read informations about each computation step
       and then read value for each parameter */
    for (j=0; j<nstepToRead; j++) {

      if (MEDparameterComputationStepInfo(fid, nameToRead, j+1, 
					  &numdtToRead, &numitToRead, &dtToRead) < 0) {
	MESSAGE("ERROR : read information about computation step ...");
	SSCRUTE(nameToRead);
	ISCRUTE(j);
	ISCRUTE(numdtToRead);
	ISCRUTE(numitToRead);
	return -1;
      }

      if (typeToRead == MED_FLOAT64)
	if (MEDparameterValueRd(fid, nameToRead, numdtToRead, numitToRead,
				(unsigned char *) &fvalueToRead) < 0) {
	  MESSAGE("ERROR : read parameter value ...");
	  SSCRUTE(nameToRead);
	  ISCRUTE(j);
	  ISCRUTE(numdtToRead);
	  ISCRUTE(numitToRead);
	  return -1;
	}

      if (typeToRead != MED_FLOAT64)
	if (MEDparameterValueRd(fid, nameToRead, numdtToRead, numitToRead,
				(unsigned char *) &ivalueToRead) < 0) {
	  MESSAGE("ERROR : read parameter value ...");
	  SSCRUTE(nameToRead);
	  ISCRUTE(j);
	  ISCRUTE(numdtToRead);
	  ISCRUTE(numitToRead);
	  ISCRUTE(ivalueToRead);
	  return -1;
	}

      /* data verifications */
      if ((i==0) && (j==0))
	if ((numdtToRead != p1numdt1) || (numitToRead != p1numit1) || (dtToRead != p1dt1) ||
	    (fvalueToRead != p1v1)) {
	  MESSAGE("ERROR : false informations for computation step : attributes ...");
	  SSCRUTE(nameToRead);
	  ISCRUTE(j);
	  ISCRUTE(numdtToRead); ISCRUTE(p1numdt1);
	  ISCRUTE(numitToRead); ISCRUTE(p1numit1);
	  return -1;
	}

      if ((i==0) && (j==1))
	if ((numdtToRead != p1numdt3) || (numitToRead != p1numit3) || (dtToRead != p1dt3) ||
	    (fvalueToRead != p1v3)) {
	  MESSAGE("ERROR : false informations for computation step : attributes ...");
	  SSCRUTE(nameToRead);
	  ISCRUTE(j);
	  ISCRUTE(numdtToRead); ISCRUTE(p1numdt3);
	  ISCRUTE(numitToRead); ISCRUTE(p1numit3);
	  return -1;
	}

      if ((i==0) && (j==2))
	if ((numdtToRead != p1numdt2) || (numitToRead != p1numit2) || (dtToRead != p1dt2) ||
	    (fvalueToRead != p1v2)) {
	  MESSAGE("ERROR : false informations for computation step : attributes ...");
	  SSCRUTE(nameToRead);
	  ISCRUTE(j);
	  ISCRUTE(numdtToRead); ISCRUTE(p1numdt2);
	  ISCRUTE(numitToRead); ISCRUTE(p1numit2);
	  return -1;
	}

      if (i==1)
	if ((numdtToRead != p2numdt1) || (numitToRead != p2numit1) || (dtToRead != p2dt1) ||
	    (ivalueToRead != p2v2)) {
	  MESSAGE("ERROR : false informations for computation step : attributes ...");
	  SSCRUTE(nameToRead);
	  ISCRUTE(j);
	  ISCRUTE(numdtToRead); ISCRUTE(p2numdt1);
	  ISCRUTE(numitToRead); ISCRUTE(p2numit1);
	  return -1;
	}

      if ((i == 2) && (j == 0))
	if ((numdtToRead != p3numdt2) || (numitToRead != p3numit2) || (dtToRead != p3dt2) ||
	    (ivalueToRead != p3v2)) {
	  MESSAGE("ERROR : false informations for computation step : attributes ...");
	  SSCRUTE(nameToRead);
	  ISCRUTE(j);
	  ISCRUTE(numdtToRead); ISCRUTE(p3numdt2);
	  ISCRUTE(numitToRead); ISCRUTE(p3numit2);
	  return -1;
	}

      if ((i == 2) && (j == 1))
	if ((numdtToRead != p3numdt1) || (numitToRead != p3numit1) || (dtToRead != p3dt1) ||
	    (ivalueToRead != p3v1)) {
	  MESSAGE("ERROR : false informations for computation step : attributes ...");
	  SSCRUTE(nameToRead);
	  ISCRUTE(j);
	  ISCRUTE(numdtToRead); ISCRUTE(p3numdt1);
	  ISCRUTE(numitToRead); ISCRUTE(p3numit1);
	  return -1;
	}

    }

  }

  /* close file */
  if (MEDfileClose(fid) < 0) {
    MESSAGE("ERROR : close file ...");
    return -1;
  }
  
  return 0;
}
