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
	char **MessageString,
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
 IsOk = FALSE;

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
      sprintf(*MessageString,"Bad block size. Read %d ",*FHeader);
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
		char **MessageString)
/************************************************************************ 
 * Read the trail bytes of a block in the file. Check this against the
 * provided Header value. If they are not equal, error.
 ************************************************************************/
{
 Boolean_t IsOk;
 int FTrail;
/************************************************************************/

 IsOk = FALSE;
 if(fread(&FTrail,sizeof(int),1,f) != 1)
 {
     strcpy(*MessageString,"Unexpected End Of File (while reading Trail)");
     TRACE(*MessageString);
     return FALSE;
 }

 if(Invert) swapbytes32(FTrail);

 if(FHeader != FTrail )
 {
     strcpy(*MessageString,"Error while reading file!");
     TRACE(*MessageString);
     IsOk = FALSE;
 }
 else
     IsOk = TRUE;
 return(IsOk);
}
/************************************************************************/


/************************************************************************/
Boolean_t GetVarNames (
	FILE *f,
	Boolean_t Invert,
	int NumVar,
	char *VarNames ,
	char **MessageString)
/************************************************************************ 
 * Function : Read the variable names in the serafin/volfin file.
 * Each variable name is 32 bytes long.
 * Concatenate these names, separeted by a "," as requested by tecplot
 * and add the coordinates X and Y.
 ************************************************************************/
{
  Boolean_t IsOk = TRUE ;
  Boolean_t EndOfFile ;
  char str[32] ;
  char str33[33] ;
  int i;
  int FHeader, RefVal;


/************************************************************************/

  strcpy(VarNames,"X, Y, ");
  /* Read the Variable names.Each Variable is 32 caracters long, they
   * are written one after the other.
   * so before and after the variable name, we have the 4byte control
   * value.
   */

  RefVal =  32;

  for (i=0;i<NumVar;i++)
  {

      IsOk = ReadFHead(f,Invert,RefVal,&FHeader,TRUE,MessageString,&EndOfFile);
      if(!IsOk) return(IsOk);
      if(EndOfFile)
      {
          strcpy(*MessageString,"Unexpected End Of File");
          return FALSE ;
      }
      if(fread(&str,sizeof(str),1,f) != 1)
      {
          strcpy(*MessageString,"Error reading variable names");
          IsOk = FALSE;
          return(IsOk);
      }
      strcpy(str33,str);
      str33[32]=0;
      strcat(VarNames,str33);

      if( i < NumVar-1 ) strcat(VarNames,",");
      /* Check trailing fortran value */
      
      IsOk = CheckReadBlock(f,Invert,FHeader,MessageString);
      if(!IsOk) return FALSE ;
  }
  
  /*
  strcpy(*MessageString ,"variables detected : ");
  strcat(*MessageString ,VarNames);
  TecUtilDialogMessageBox(*MessageString,MessageBox_Information);
  strcpy(*MessageString ,"exiting GetVarNames ");
  TecUtilDialogMessageBox(*MessageString,MessageBox_Information);
  */
 

  IsOk = TRUE ;

  return(IsOk) ;
}
/************************************************************************/

/************************************************************************/
/************************************************************************/
Boolean_t ReadDataSet(
	FILE  *f          ,
	Boolean_t Invert,
	int    NumVars    ,
	int    NDataPoints,
	float *PtrValues  ,
	float *time       ,
	char  **MessageString,
	Boolean_t *EndOfFile)
/************************************************************************ 
 * Function : Read a data set. The data set is described by the
 * associated time and a table for each variable.
 ************************************************************************/
{
  Boolean_t IsOk = TRUE ;
  int  i, j             ;
  float *PtrTemp        ;
  int RefVal, FHeader;

/************************************************************************/

  /* The data set is written in two steps : first the solution time,
   * and than the data.
   * The data points for each variable represent a block.
   */
  
/*
  strcpy(*MessageString ,"entered ReadDataSet");
  TecUtilDialogMessageBox(*MessageString,MessageBox_Information);
  */
 
 

  /* Read header for the time ... */
  RefVal = sizeof(float);
  IsOk = ReadFHead(f,Invert,RefVal,&FHeader,TRUE,MessageString,EndOfFile);
  if(!IsOk) return(IsOk);
  if(*EndOfFile)
  {
      return TRUE  ;
  }

  /* Read the time ... */
  if(fread(time,sizeof(float),1,f)!=1)
  {
      strcpy(*MessageString,"Error reading time");
      IsOk = FALSE;
      return(IsOk);
  }
  if(Invert){ swapbytes32(*time);  }
  
/*
  sprintf(*MessageString,"Reading solution time  : %f ",*time);
  TecUtilDialogMessageBox(*MessageString,MessageBox_Information);

  sprintf(*MessageString,"NumVars =  : %d ",NumVars);
  TecUtilDialogMessageBox(*MessageString,MessageBox_Information);
  */

  /* Read trailer for the time ... */
  IsOk = CheckReadBlock(f,Invert,FHeader,MessageString);
  if(!IsOk) return FALSE ;


  PtrTemp = PtrValues ;
  RefVal = NDataPoints * sizeof(float);

  for(i=0;i<NumVars;i++)
  {
      /* Read header for datapoints of one variable ... */
      IsOk = ReadFHead(f,Invert,RefVal,&FHeader,TRUE,MessageString,EndOfFile);
      if(!IsOk) return(IsOk);
      if(*EndOfFile)
      {
          strcpy(*MessageString,"Unexpected End Of File");
          return FALSE ;
      }
      /* Read datapoints of one variable ... */
      if(fread(PtrTemp,NDataPoints*sizeof(float),1,f)!=1)
      {
          strcpy(*MessageString,"Unexpected End Of File");
          return FALSE ;
      }
      if(Invert) {
	  for(j=0;j<NDataPoints;j++) { swapbytes32(PtrTemp[j]); } }
      /* Read trailer for datapoints of one variable ... */
      IsOk = CheckReadBlock(f,Invert,FHeader,MessageString);
      if(!IsOk) return FALSE ;
      PtrTemp += NDataPoints ;
  }

  
/* 
  strcpy(*MessageString ,"exit ReadDataSet");
  TecUtilDialogMessageBox(*MessageString,MessageBox_Information);
  */
  
 
  IsOk = TRUE;

  return(IsOk);
}
/************************************************************************/
