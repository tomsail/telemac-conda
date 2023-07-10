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
      sprintf(MessageString,"Bad block size. Read %d ",*FHeader);
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

 IsOk = TRUE;
 if(fread(&FTrail,sizeof(int),1,f) != 1)
 {
     strcpy(MessageString,"Unexpected End Of File (while reading Trail)");
     TRACE(MessageString);
     return FALSE;
 }

 if(Invert) swapbytes32(FTrail);

 if(FHeader != FTrail )
 {
     strcpy(MessageString,"Error while reading file!");
     TRACE(MessageString);
     IsOk = FALSE;
 }
 return(IsOk);
}
/************************************************************************/


/************************************************************************/
Boolean_t GetVarNames (
	FILE *f,
	Boolean_t Invert,
	int NumVar,
	StringList_pa *VarNames ,
	char *MessageString)
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

  strcpy(str33,"X");
  TecUtilStringListAppendString(*VarNames,str33);
  strcpy(str33,"Y");
  TecUtilStringListAppendString(*VarNames,str33);
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
          strcpy(MessageString,"Unexpected End Of File");
          return FALSE ;
      }
      if(fread(&str,sizeof(str),1,f) != 1)
      {
          strcpy(MessageString,"Error reading variable names");
          IsOk = FALSE;
          return(IsOk);
      }
      strcpy(str33,str);
      str33[32]=0;
      TecUtilStringListAppendString(*VarNames,str33);

      IsOk = CheckReadBlock(f,Invert,FHeader,MessageString);
      if(!IsOk) return FALSE ;
  }
  
  IsOk = TRUE ;

  return(IsOk) ;
}
/************************************************************************/


/************************************************************************/
Boolean_t GetMeshInfo(
	FILE *f,    /* File to read */
	Boolean_t Invert,
	int *nelem,  /* number of mesh elements */
	int *npoin,  /* number of mesh nodes */
	int *ndp,    /* number of points per element */
	int *nplan,  /* number of prism layers */
	char *MessageString /* error message */
	)
/************************************************************************
 * Function : Get the mesh information data.
 * this are the number of elements and the number of nodes, and - for
 * 3D prism data - the number of layers.
 * ndp is the number of points in an element.
 ************************************************************************/
{
  Boolean_t IsOk = TRUE ;
  int Int10[10] ;
  int Int6[6] ;
  int Int4[4] ;
  int FHeader, RefVal;
  Boolean_t EndOfFile;
  int i;

/************************************************************************/
  
 

/* Reading the first 10 integers. */
 
  RefVal = 10*sizeof(int);
  IsOk = ReadFHead(f,Invert,RefVal,&FHeader,TRUE,MessageString,&EndOfFile);
  if(!IsOk)
  {
       strcpy(MessageString,"Error reading header of file");
       return(IsOk);
  }

  if(EndOfFile)
  {
      strcpy(MessageString,"Unexpected End Of File");
      return FALSE ;
  }
  if( fread(&Int10,sizeof(Int10),1,f) != 1)
  {
      strcpy(MessageString,"Error Reading Mesh Info section 1");
      IsOk = FALSE ; return(IsOk);
  }
  IsOk = CheckReadBlock(f,Invert,FHeader,MessageString);
  if(!IsOk) return FALSE ;
  if(Invert) for(i=0;i<10;i++) swapbytes32(Int10[i]);


  *nplan = Int10[6] ; /* Number of Prism layers */

/* If Int10[10] is not zero, we have to read the date and the time
 * in the following 6 integers. */

  if( Int10[9] != 0 ) 
  {
      RefVal = 6*sizeof(int);
      IsOk = ReadFHead(f,Invert,RefVal,&FHeader,TRUE,MessageString,&EndOfFile);
      if(!IsOk) return(IsOk);
      if(EndOfFile)
          if( fread(&Int6,sizeof(Int6),1,f)!=1)
          {
              strcpy(MessageString,"Error Reading Date and Time");
              IsOk = FALSE ; return(IsOk);
          }
      IsOk = CheckReadBlock(f,Invert,FHeader,MessageString);
      if(!IsOk) return FALSE ;
      /* TODO : pass date and time to output */
  }


/* Reading the mesh info : number of elements and nodes,
 * number of points of an element.  */

  RefVal = 4*sizeof(int);
  IsOk = ReadFHead(f,Invert,RefVal,&FHeader,TRUE,MessageString,&EndOfFile);
  if(!IsOk) return(IsOk);
  if(EndOfFile)
  {
      strcpy(MessageString,"Unexpected End Of File");
      return FALSE ;
  }
  if( fread(&Int4,sizeof(Int4),1,f)!=1)
  {
      strcpy(MessageString,"Error Reading Mesh Info section 2");
      IsOk = FALSE ;
      return(IsOk);
  }
  IsOk = CheckReadBlock(f,Invert,FHeader,MessageString);
  if(!IsOk) return FALSE ;
  if(Invert) for(i=0;i<4;i++) swapbytes32(Int4[i]);
  
  *nelem = Int4[0]; /* Number of elements */
  *npoin = Int4[1]; /* Number of Nodes */
  *ndp   = Int4[2]; /* Number of points per element */
 
  
  /*
  sprintf(str,"%d",*nelem);
  strcpy(*MessageString,"Number of elements : ");
  strcat(*MessageString, str);
  TecUtilDialogMessageBox(*MessageString,MessageBox_Information);

  sprintf(str,"%d",*npoin);
  strcpy(*MessageString,"Number of nodes : ");
  strcat(*MessageString, str);
  TecUtilDialogMessageBox(*MessageString,MessageBox_Information);

  sprintf(str,"%d",*ndp);
  strcpy(*MessageString,"Number of nodes per element : ");
  strcat(*MessageString, str);
  TecUtilDialogMessageBox(*MessageString,MessageBox_Information);

  sprintf(str,"%d",*nplan);
  strcpy(*MessageString,"Number of mesh levels : ");
  strcat(*MessageString, str);
  TecUtilDialogMessageBox(*MessageString,MessageBox_Information);
 
  */

  IsOk = TRUE;
  return(IsOk);
}
/************************************************************************/

/************************************************************************/
Boolean_t ReadMesh(
	FILE *f        ,      
	Boolean_t Invert,
	int nelem      ,
	int npoin      ,
	int ndp        ,
	int nplan      ,
	int *PtrIkleTec,
	long int *VarPosX,
	long int *VarPosY,
	char *MessageString)
/************************************************************************ 
 * Function : Read the mesh. This are the connectivity table and the X
 * and Y coordinates.
 * The connectivity table has to be re-written to match the tecplot
 * storage.
 ************************************************************************/
{
  Boolean_t IsOk = TRUE ;
  int *PtrIkle          ;
  int *PtrIkleTemp      ;
  int *PtrIkleTempTec   ;
  int  i,j              ;
  int RefVal, FHeader;
  Boolean_t EndOfFile;


/************************************************************************/

/*----------------------------------------------------------------------
 * READING IKLE
  ----------------------------------------------------------------------*/
/* In this section we read the mesh information. In the serafin file,
 * this are the connectivity table, the IPOBO table (ignored) and the
 * coordinates.
 * Each of this tables is written in one block by the fortran
 * subroutine.
 * */
  

  RefVal = ndp*nelem*sizeof(int);
  IsOk = ReadFHead(f,Invert,RefVal,&FHeader,TRUE,MessageString,&EndOfFile);
  if(!IsOk) return(IsOk);
  if(EndOfFile)
  {
      strcpy(MessageString,"Unexpected End Of File");
      return FALSE ;
  }

  PtrIkle = (int*)calloc(ndp*nelem,sizeof(int));

  if(fread(PtrIkle,nelem*ndp*sizeof(int),1,f)!=1)
  {
      strcpy(MessageString,"Error reading Connectivity.");
  }

  if(Invert) { for(i=0;i<nelem*ndp;i++) { swapbytes32(PtrIkle[i]); } }

  IsOk = CheckReadBlock(f,Invert,FHeader,MessageString);
  if(!IsOk) return FALSE ;

  if ( ndp == 6 ) /* prism, to transform into bricks */
  {
      if ( nplan < 2 )
      {
	  strcpy(MessageString,"3D serafin with less than 1 mesh level. stop.");
	  IsOk = FALSE ;
	  return(IsOk);
      }


      /* Tecplot knows only brick data. it is possible to repeat some
       * nodes in the 8 point connectivity to obtain 6 point data
       * (prisms)
       * ....*/

      PtrIkleTemp    = PtrIkle;
      PtrIkleTempTec = PtrIkleTec;

      for (j=0; j<nelem; j++) 
      {
          *PtrIkleTempTec  = *PtrIkleTemp ; /* N1 = N1 */
           PtrIkleTempTec++               ;

          *PtrIkleTempTec  = *PtrIkleTemp ; /* N2 = N1  */
           PtrIkleTempTec++               ;
           PtrIkleTemp++               ;

          *PtrIkleTempTec  = *PtrIkleTemp ; /* N3 = N2  */
           PtrIkleTempTec++               ;
           PtrIkleTemp++               ;

          *PtrIkleTempTec  = *PtrIkleTemp ; /* N4 = N3  */
           PtrIkleTempTec++               ;
           PtrIkleTemp++               ;
           

          *PtrIkleTempTec  = *PtrIkleTemp ; /* N5 = N4 */
           PtrIkleTempTec++               ;

          *PtrIkleTempTec  = *PtrIkleTemp ; /* N6 = N4 */
           PtrIkleTempTec++               ;
           PtrIkleTemp++               ;

          *PtrIkleTempTec  = *PtrIkleTemp ; /* N7 = N5 */
           PtrIkleTempTec++               ;
           PtrIkleTemp++               ;

          *PtrIkleTempTec  = *PtrIkleTemp ; /* N8 = N6 */
           PtrIkleTempTec++               ;
           PtrIkleTemp++               ;
      }
  }
  else /* triangles or quads */
  {
      PtrIkleTemp    = PtrIkle;
      PtrIkleTempTec = PtrIkleTec;
      for (j=0; j<nelem; j++) 
      {
          for ( i=0; i<ndp; i++)
          {
              *PtrIkleTempTec  = *PtrIkleTemp   ;
   	       PtrIkleTempTec++                 ;
   	       PtrIkleTemp++                 ;
          } /* end for ndp */
      } /* end for nelem */
  } /* end if not prisms */


  free(PtrIkle);
/*---------------------------------------------------------------------
 * READING IPOBO
 *-------------------------------------------------------------------*/
  /* skip the ipobo table. size : npoin (+2integers for fortran write)
   * */
  i = fseek(f,(npoin+2)*sizeof(int),1);
  if(i < 0 ) 
  {
      strcpy(MessageString,"Error skipping IPOBO ..");
      return FALSE ;
  }
/*---------------------------------------------------------------------
 * READING COORDINATES
 *-------------------------------------------------------------------*/
  /* X Coordinates */
  RefVal = npoin*sizeof(float);
  IsOk = ReadFHead(f,Invert,RefVal,&FHeader,TRUE,MessageString,&EndOfFile);
  if(!IsOk) return(IsOk);
  if(EndOfFile)
  {
      strcpy(MessageString,"Unexpected End Of File");
      return FALSE ;
  }

  *VarPosX = ftell(f); 
   fseek(f,RefVal,1);

  IsOk = CheckReadBlock(f,Invert,FHeader,MessageString);
  if(!IsOk) return FALSE ;


  /* Y Coordinates */
  IsOk = ReadFHead(f,Invert,RefVal,&FHeader,TRUE,MessageString,&EndOfFile);
  if(!IsOk) return(IsOk);
  if(EndOfFile)
  {
      strcpy(MessageString,"Unexpected End Of File");
      return FALSE ;
  }

  *VarPosY = ftell(f); 
   fseek(f,RefVal,1);
  
  IsOk = CheckReadBlock(f,Invert,FHeader,MessageString);
  if(!IsOk) return FALSE ;
 
  IsOk = TRUE ;

  return(IsOk);
}
/************************************************************************/



/************************************************************************/
int DetectFormat ( FILE  *f, Boolean_t Invert,
	Boolean_t *EndOfFile, int nelem, int npoin)
/************************************************************************ 
 * Function : The present file may containt either VOLFIN or SERAFIN data.
 * Volfin data are written on elements, wile serafin data are on
 * nodes.
 * Here we identify the file format by reading the header bytes of the
 * first data block. We knpw that each data entry is 4 bytes long, so
 * if the block is of the size 4*npoin, it is serafin, if it is
 * 4*nelem, it is volfin. Otherwise, unknown data format.
 ************************************************************************/
{
  int Format ;
  int BlockSize ;
  int ndata ;
  char str12[12];

/************************************************************************/

  /* Skip 4 + 4 + 4 bytes for time */

  if(fread(str12,12,1,f)!=1)
  {
      *EndOfFile = TRUE;
      return 0;
  }

  fread(&BlockSize,sizeof(int),1,f);
  if(Invert) swapbytes32(BlockSize);

  /* One data point is 4 bytes long. */
  ndata = BlockSize / 4 ;
  if(ndata == npoin )
  {
      Format = 2 ; /* Serafin */
  }
  else if (ndata == nelem)
  {
      Format = 1 ; /* Volfin */
  }
  else
  {
      Format = -1;
  }

  fseek(f,-16,1); /* Rewind the 16 bytes read */

  return(Format);
}
/************************************************************************/
/************************************************************************/
Boolean_t ReadDataSet(
	FILE  *f          ,
	Boolean_t Invert,
	int    NumVars    ,
	int    NDataPoints,
	float *PtrValues  ,
	float *time       ,
	char  *MessageString,
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
      strcpy(MessageString,"Error reading time");
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
          strcpy(MessageString,"Unexpected End Of File");
          return FALSE ;
      }
      /* Read datapoints of one variable ... */
      if(fread(PtrTemp,NDataPoints*sizeof(float),1,f)!=1)
      {
          strcpy(MessageString,"Unexpected End Of File");
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
