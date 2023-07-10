/*********************************************************************
 *  AddOn 
 **********************************************************************/


#include "TECADDON.h"
#include "ADDGLBL.h"
#include "TASSERT.h"
#include "UTIL.h"
#include "ADKUTIL.h"


/************************************************************************/
Boolean_t ReadDelwaqHeader( FILE *f,
	                   char Title[160],
			   int *npoin_delwaq,
			   int *nvar_delwaq)
/************************************************************************ 
 * Read the Header section and the title in a Delwaq data
 * file.
 ************************************************************************/
{
  // Four strings for reading the header information
  Boolean_t IsOk;
  char str4[20];
/************************************************************************/

  // Read the first lines : 
  // 4 times 40 characters.
  if(fread(&Title[0],160,1,f) != 1)
  {
      TRACE("Reading Delwaq file header failed : \n");
      IsOk = FALSE;
  }
  TRACE(Title);
  TRACE("\n");


  if(fread(nvar_delwaq,4,1,f) != 1)
  {
      TRACE("Read number of Delwaq variables - failed. \n");
      IsOk = FALSE;
  }

  if(fread(npoin_delwaq,4,1,f) != 1)
  {
      TRACE("Error Reading the number of points in delwaq. \n");
      TRACE("\n");
      IsOk = FALSE;
      return(IsOk);
  }
  sprintf(str4,"number of variables delw : %d \n",*nvar_delwaq);
  TRACE(str4);
  sprintf(str4,"number of points delw : %d \n",*npoin_delwaq);
  TRACE(str4);

  return(IsOk);

}

/************************************************************************/


/************************************************************************/
Boolean_t GetVarNamesDelwaq ( FILE *f,
	                      int NbVarDelwaq,
			      StringList_pa  *VarNamesDelwaq)
/************************************************************************ 
 * Function : Read the variable names in the serafin/volfin file.
 * Each variable name is 32 bytes long.
 * Concatenate these names, separeted by a "," as requested by tecplot
 * and add the coordinates X and Y.
 ************************************************************************/
{
  int i;
  Boolean_t IsOk  ;  // return value
  char VarName[20];  // String for the variable names

/************************************************************************/

  IsOk = TRUE;
  // read the variable names and add them to the current data set :
  for (i=0;i < NbVarDelwaq; i++)
  {
      if(fread(&VarName,20,1,f) != 1)
      {
          TRACE("Error reading variable names ");
	  IsOk = FALSE;
          break;
      }
      else
      {
	  // a variable name should end with a 0.
	  VarName[19] = 0;
	  TRACE("Variable name : \n");
	  TRACE(VarName);
	  TRACE("\n");
	  TecUtilStringListAppendString(*VarNamesDelwaq,VarName);
      }
  }
  
  return(IsOk) ;
}
/************************************************************************/
