/*********************************************************************
 *  AddOn for loading files of the telemac Serfin or Volfin data
 *  format
 *  into tecplot.
 *  written by R Nebauer EDF R&D LNHE, may 2007
 *  regina.nebauer@edf.fr
 *  readfile.c - reading the input data file
 **********************************************************************/


#include "TECADDON.h"
#include "ADDGLBL.h"
#include "TASSERT.h"
#include "ENGINE.h"
#include "UTIL.h"

/************************************************************************/
Boolean_t ReadFHead(
	FILE *f,
	Boolean_t Invert,
	int RefVal,
	int *FHeader,
	Boolean_t Compare,
	char *MessageString,
	Boolean_t *EndOfFile)
{
/************************************************************************ 
 *  Function : Read the fortran header of the block in the file.
 *  Check the header value with a reference value (expected block
 *  size). If bad block size, exit with error.
 ************************************************************************/
 Boolean_t IsOk ;
 int dummy;

/************************************************************************/
 *EndOfFile = FALSE;

 if(fread(FHeader,sizeof(int),1,f) != 1)
 {
     /*
  sprintf(*MessageString,"Reached end of file.");
  TecUtilDialogMessageBox(*MessageString,MessageBox_Information);
  */
     *EndOfFile = TRUE; IsOk = TRUE;  return(IsOk); 
 }

 if(Invert) {dummy = *FHeader;swapbytes32(dummy);*FHeader=dummy;}

 if (Compare  && (*FHeader != RefVal))
 {
      sprintf(MessageString,"Bad block size. Read %d \n ",*FHeader);
      TRACE(MessageString);
      IsOk = FALSE ;
      return(IsOk);
 }
 IsOk = TRUE;
 return(IsOk);
}

/************************************************************************/
Boolean_t CheckReadBlock(
	        FILE *f, 
               	Boolean_t Invert,
		int FHeader,
		char *MessageString)
/************************************************************************ 
 * Read the trail bytes of a block in the file. Check this against the
 * provided Header value. If they are not equal, error.
 ************************************************************************/
{
 Boolean_t IsOk;
 int FTrail;
/************************************************************************/

 if(fread(&FTrail,sizeof(int),1,f) != 1)
 {
     strcpy(*MessageString,"Unexpected End Of File (while reading Trail)");
     return FALSE;
 }

 if(Invert) swapbytes32(FTrail);

 if(FHeader != FTrail )
 {
     strcpy(MessageString,"Error while reading file!");
     IsOk = FALSE;
 }
 return(IsOk);
}
/************************************************************************/

