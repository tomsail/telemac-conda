/*********************************************************************
 * AddOn for loading files of the telemac Serafin or Volfin data
 * format into tecplot.
 * written by R Nebauer EDF R&D LNHE, may 2007
 * regina.nebauer@edf.fr
 **********************************************************************/

#include "TECADDON.h"
#include "ADDGLBL.h"
#include "TASSERT.h"
#include "ENGINE.h"
#include "UTIL.h"
#include "ADKUTIL.h"


/************************************************************************/
Boolean_t GetMeshInfo(
	FILE *f,    /* File to read */
	Boolean_t Invert,
	int *nelem,  /* number of mesh elements */
	int *npoin,  /* number of mesh nodes */
	int *ndp,    /* number of points per element */
	int *nplan,  /* number of prism layers */
	char MessageString[100] /* error message */
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

  Boolean_t CheckReadBlock();
  Boolean_t ReadFHead();
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
      if(!EndOfFile)
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
static Boolean_t ReadMesh(
	FILE *f        ,      
	Boolean_t Invert,
	int nelem      ,
	int npoin      ,
	int ndp        ,
	int nplan      ,
	int *PtrIkleTec,
	long int *VarPosX,
	long int *VarPosY,
	char MessageString[100])
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
  int  i,j,n            ;
  int RefVal, FHeader;
  Boolean_t EndOfFile;

  Boolean_t CheckReadBlock();
  Boolean_t ReadFHead();

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
	  strcpy(MessageString,"3D serafin with less than 2 mesh level. stop.");
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
static int DetectFormat ( FILE  *f, Boolean_t Invert,
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

  TRACE("entered detect format \n");
  *EndOfFile = FALSE;
  /* Skip 4 + 4 + 4 bytes for time */

  if(fread(str12,12,1,f)!=1)
  {
      TRACE("unable to read the time step when detecting the data format.\n");
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
static Boolean_t GetVarNames (
	FILE *f,
	Boolean_t Invert,
	int NumVar,
	StringList_pa *VarNames,
	char MessageString[100])
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

  Boolean_t CheckReadBlock();
  Boolean_t ReadFHead();

/************************************************************************/

  TRACE("entered getvarnames dans LoadFileSera.c\n");

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
Boolean_t GetVarNamesSera(FILE *f,
	                  Boolean_t Invert,
                          int *NbVars,
	                  StringList_pa *VarNames)
/************************************************************************/
{

  Boolean_t IsOk = TRUE  ;
  int   i ;
  int   NbV1 ;
  int   NbV2 ;
  char MessageString[100];

  Boolean_t EndOfFile;

  int FHeader ; /* Head bytes in fortran files */
  int RefVal ;

  Boolean_t CheckReadBlock();
  Boolean_t ReadFHead();

/************************************************************************/

//----------------------------------------------------------------------------
/* Read the number of variables in the data set 
 * These are two 4 byte integers, so head and trail should be 2*4=8 :
 */

  RefVal = 2 * sizeof(int);

  IsOk = ReadFHead(f,Invert,RefVal,&FHeader,TRUE,MessageString,&EndOfFile);
  if(!IsOk)
  {
      TRACE(MessageString);
      TecUtilDialogErrMsg(MessageString);
      return(IsOk);
  }
  if(EndOfFile) 
  {
      strcpy(MessageString,"Unexpected End Of File ");
      TRACE(MessageString);
      TecUtilDialogErrMsg(MessageString);
      IsOk = FALSE;
      return(IsOk);
  }
  // read the number of variables
  if ( fread(&NbV1,sizeof(int),1,f) != 1)
  {
      strcpy(MessageString,"Could not read the data file");
      TRACE(MessageString);
      TecUtilDialogErrMsg(MessageString);
      IsOk = FALSE;
      return(IsOk);
  }
  if(Invert) swapbytes32(NbV1) ;
  // read the number of variables 2 :
  if ( fread(&NbV2,sizeof(int),1,f) != 1)
  {
      strcpy(MessageString,"Could not read the data file");
      TRACE(MessageString);
      TecUtilDialogErrMsg(MessageString);
      IsOk = FALSE;
      return(IsOk);
  }
  if(Invert) swapbytes32(NbV2) ;

  sprintf(MessageString,"NbV1 : %d NbV2 : %d \n",NbV1,NbV2);
  TRACE(MessageString);

  TRACE("Got he number of variables to read.\n");
  // check the trailer bytes
  IsOk = CheckReadBlock(f,Invert,FHeader,MessageString);
  if(!IsOk)
  { strcpy(MessageString,"Error reading file header (variable names)\n");
    TecUtilDialogErrMsg(MessageString);
    return(IsOk);
  }

/* The total number of variables in the file */

  *NbVars = NbV1 + NbV2 ;

  /* Read the variable names */
  TRACE("Try to get the variable names \n");

  IsOk = GetVarNames(f,Invert,*NbVars,VarNames,MessageString) ;
  if (!IsOk)
  {
    TecUtilDialogErrMsg(MessageString);
    return(IsOk);
  }
  *NbVars = *NbVars + 2 ; // should add 2 variables for X and Y coordinates
  i = TecUtilStringListGetCount(*VarNames);
  sprintf(MessageString,"NumVars %d count %d\n",*NbVars,i);
  TRACE(MessageString);
  if(i != *NbVars)
  {
      TecUtilDialogErrMsg("Bad number of variables (GetVarNamesSera();)");
      IsOk = FALSE;
  }

 return(IsOk);
}


/************************************************************************/
Boolean_t LoadDataSera(
/************************************************************************/
          FILE *f           ,      /* IN - the file to read  */
	  char *FileName,          // The name of the file
	  Boolean_t Invert,        // if inverse byte order or not
	  int NbVarTot,            // total number of variables in tecplot
	                           // data set
	  int NumVars,             // number of variables to read in file
	  Boolean_t AddDelwaq,     // if delwaq file to read
          FILE *fDelwaq           ,// IN - the delwaq file to read 
	  char *FileNameDelwaq,    // the name of the delwaq file
	  Boolean_t InvertDelwaq,  // if inverse byte order for delwaq
	  int NumVarsDelwaq,       // number of variables in delwaq file
          EntIndex_t *VarCorr,     // correlation between vars to read and
	                           // variables in the tecplot data set
	  EntIndex_t StartNewZones,// no of starting zone
	  char DataSetName[],      // name of the telemac data set to read
	  Boolean_t GeomOnly      )// if file is mesh only
/************************************************************************/
{
/* Local Variables */

  Boolean_t IsOk = TRUE  ; // return value

  int  i , j, n      ;

//  char title[81]     ; // data set title

  // Mesh informations
  int ndp           ; // number of points per element
  int nelem         ; // total number of elements
  int npoin         ; // total number of nodes
  int npoin_pl      ; // nodes per layer
  int nplan         ; // number of layers for sigma mesh

  char MessageString[100];

  int *PtrIkle       ; /* Connectivity Table */
  int NDataPoints    ; /* Number of data points */
  float *VTab, *PtrF;

  int TimeStep ;
  int FileFormat ;
  Boolean_t FirstDataSet ;
  Boolean_t EndOfFile ;
  Boolean_t EndDelwaq ;
  Boolean_t ArgOk ;
  Boolean_t IsNativeByteOrder ;
  Boolean_t IsNativeByteOrderDelwaq ;
  int ShareConnect ;
  int Stride=1 ;
  ZoneType_e ZTyp ;
  ArgList_pa Arguments ;
  int izone ;

  int FHeader ; /* Head bytes in fortran files */
  int RefVal ;

  long int VarPosX,VarPosY,VarPos;

  int ndp_tec ; /* Number of nodes for element in tecplot */

  float time;
  double time_double;
  int kmax;
  Strand_t strand;
  ValueLocation_e *ValueLoc, Loc;
  FieldDataType_e *VarPrecision;
  FieldData_pa FieldDataRef ;


  NodeMap_pa NodeMap;

  EntIndex_t CurZone;

  Boolean_t CheckReadBlock();
  Boolean_t ReadFHead();

  Boolean_t MeshOnly;

/************************************************************************/

 if(GeomOnly == TRUE ) 
 {
     TRACE("loading mesh file\n");
 }
 else
 {
     TRACE("loading transient data file\n");
 }
  
 TRACE("Name of the data set :");
 TRACE(DataSetName);
 TRACE("\n");
  /* Read the mesh info */
  IsOk = GetMeshInfo(f,Invert,&nelem,&npoin,&ndp,&nplan,MessageString) ;
  if (!IsOk)
  {
    TecUtilDialogErrMsg(MessageString);
    return(IsOk);
  }

  // number of points per layer (used in case of sigma mesh)
  if (nplan > 0 ) npoin_pl = npoin / nplan ; 
  else npoin_pl = npoin;

  // Number of points per element and connectivity table :
  // Prisms are not known by tecplot. We should use the
  // BRICK type and change a bit the connectivity ...
  //
  if( ndp == 6 ) ndp_tec = 8; else ndp_tec = ndp ;
  // The tecplot FE type : 
  switch (ndp_tec )
  {
      case 3 : ZTyp = ZoneType_FETriangle;
	       break;
      case 4 : ZTyp = ZoneType_FEQuad;
               break ;
      case 8 : ZTyp = ZoneType_FEBrick ;
               break ;
      default : strcpy(MessageString,"Unknown FE Format");
      TecUtilDialogErrMsg(MessageString);
      		IsOk = FALSE;
                break ;
	      
  }
  // allocate the connetivity table.
  PtrIkle = (int*) calloc(nelem*ndp_tec,sizeof(int));

  /* Read the connectivity and the coordinates in X and Y.
   * PtrIkle contains the connectivity table in Tecplot format
   * (conversion telemac to tecplot done while reading ...)
   */
  IsOk = ReadMesh(f,Invert,nelem,npoin,ndp,nplan,PtrIkle,
	          &VarPosX,&VarPosY,MessageString) ;
  
  if (!IsOk)
  {
    TRACE(MessageString);
    TecUtilDialogErrMsg(MessageString);
    return(IsOk);
  }

  MeshOnly = FALSE;
  if(feof(f))
  {
      MeshOnly = TRUE;
      TRACE("Only Mesh in file.\n");
  }


  /* Detect the file format : serafin or volfin.  */

  if(!MeshOnly)
  FileFormat = DetectFormat(f,Invert,&EndOfFile,nelem,npoin);

  if(EndOfFile)
  {
      strcpy(MessageString,"Unexpected end of file\n");
      TRACE(MessageString);
      TecUtilDialogErrMsg(MessageString);
      IsOk = FALSE ;
  }
  
  switch ( FileFormat )
  {
      case 1 : NDataPoints = nelem ;
	       TRACE("Dataformat volfin\n");
	       break;

      case 2 : NDataPoints = npoin ;
	       TRACE("Dataformat serafin\n");
	       break;

      default : IsOk = FALSE ;
                strcpy(MessageString,"Unknown File Format");
		TRACE(MessageString);
                TecUtilDialogErrMsg(MessageString);
                IsOk = FALSE;
    break;
  }
  
/* Allocate the memory for the data set to read */

  i = NumVars;
  if (i==0) i=1;

  
  // the number to use for the KMAX parameter :
  kmax = 1;
  /* Valuelocation SV_VALUELOCATION ValueLocation_e*
   * default value : Null, all data are nodal.
   * otherwise array containing the lcation of the variables :
   * ValueLocation_CellCentered or ValueLocation_Nodal
   */
  ValueLoc = (ValueLocation_e*)calloc(NbVarTot,sizeof(int));

  if(NDataPoints == npoin ) {Loc = ValueLocation_Nodal;}
  else if(NDataPoints == nelem) { Loc = ValueLocation_CellCentered;}
  else
  {
      TRACE("erreur pour nombre de points de donnees\n");
      strcpy(MessageString,"Unconsistent number of data points");
      TecUtilDialogErrMsg(MessageString);
      IsOk = FALSE ;
  }
  // default value location for passive zone data
  for(i=0;i<NbVarTot;i++) { ValueLoc[i] = ValueLocation_Nodal ;}
  // value location for X and Y
  ValueLoc[VarCorr[0]] = ValueLocation_Nodal ;
  ValueLoc[VarCorr[1]] = ValueLocation_Nodal ;
  // value location for the non-passive zone data
  for(i=2;i<NumVars;i++) { ValueLoc[VarCorr[i]] = Loc; }
  // Delwaq data are treated as point data.
  Loc = ValueLocation_Nodal ;
  for(i=NumVars;i<NumVarsDelwaq;i++) { ValueLoc[VarCorr[i]] = Loc; }
  
  /* Variable precision : delwaq data as well as telemac data
   * are in simple precision (32 bits)
   * note the same precision for all passive data as well.
   */
  VarPrecision = (FieldDataType_e*)
                    calloc(NbVarTot,sizeof(FieldDataType_e));
  for(i=0;i<NbVarTot;i++)VarPrecision[i]=FieldDataType_Float;

  /* Allocation of the value table for delwaq data */
  VTab = (float*)calloc(NumVarsDelwaq*npoin,sizeof(float));

  /* Allocate the argument list for the zone description. */
  Arguments = TecUtilArgListAlloc();
  if(Arguments == NULL)
  {
      strcpy(MessageString,"Failed allocation of Arglist");
      TecUtilDialogErrMsg(MessageString);
      IsOk = FALSE;
   }


  /* The time strand : if there is already a data set present,
   * then get the max strand id. The strand id of the zones to
   * be added will be this max strand id +1. If this is the
   * first data set in the frame, strand ID will be 1.
   * Strand is zero for steady data, so mesh only will have a 
   * strand of zero
   */

  if (MeshOnly == TRUE || GeomOnly == TRUE ) 
  {
      TRACE("assign strand 0 \n");
      strand = 0;
  }
  else
  {
     if(StartNewZones > 0 )
     {
          strand = TecUtilDataSetGetMaxStrandID();
          strand += 1;
     }
     else
         strand = 1;
  }

  /* Read the data sets and write them to the temp tecfile. */
  TimeStep     = 0;
  izone        = StartNewZones+1;
  FirstDataSet = TRUE ;
  EndOfFile    = FALSE ;
  EndDelwaq    = FALSE ;
  RefVal       = NDataPoints * sizeof(float);

  if(Invert == TRUE)
      IsNativeByteOrder = FALSE;
  else
      IsNativeByteOrder = TRUE;

  if(InvertDelwaq == TRUE)
      IsNativeByteOrderDelwaq = FALSE;
  else
      IsNativeByteOrderDelwaq = TRUE;

  if(IsOk)
  {
  while (!EndOfFile)
  {

      /* Read header for the time ... */
      RefVal = sizeof(float);
      IsOk = ReadFHead(f,Invert,RefVal,&FHeader,TRUE,&MessageString,&EndOfFile);
      if(!IsOk) return(IsOk);
      if(EndOfFile)
      {
	  TRACE("End of file reached.\n");
          break;
      }

      /* Read the time ... */
      if(fread(&time,sizeof(float),1,f)!=1)
      {
          strcpy(MessageString,"Error reading time");
          TecUtilDialogErrMsg(MessageString);
          IsOk = FALSE;
          break;
      }
      if(Invert){ swapbytes32(time);  }


      time_double = (double)time;
  
      /* Read trailer for the time ... */
      IsOk = CheckReadBlock(f,Invert,FHeader,&MessageString);
      if(!IsOk)
      {
          strcpy(MessageString,"Error reading trail bytes time");
          TecUtilDialogErrMsg(MessageString);
          IsOk = FALSE;
          break;
      }

      if(!EndOfFile ) 
      {
        /* The zone title SV_TITLE char* 
         * use the solution time as zone title */
      
        //sprintf(title,"Telemac t= %f ",time);

	TecUtilArgListClear(Arguments);
        ArgOk = TecUtilArgListAppendString(Arguments,SV_NAME,DataSetName);
        ArgOk = TecUtilArgListAppendInt(Arguments,SV_ZONETYPE,ZTyp);
        ArgOk = TecUtilArgListAppendInt(Arguments,SV_DEFERVARCREATION, TRUE);
        ArgOk = TecUtilArgListAppendArray(Arguments,SV_VARDATATYPE,
                                                VarPrecision);


        /* Number of nodes SV_IMAX LgIndex_t  */
        ArgOk = TecUtilArgListAppendInt(Arguments,SV_IMAX,npoin);

        /* Number of elements SV_JMAX LgIndex_t */
        ArgOk = TecUtilArgListAppendInt(Arguments,SV_JMAX,nelem);

        ArgOk = TecUtilArgListAppendInt(Arguments,SV_KMAX,kmax);

        /* Solution Time SV_SOLUTIONTIME double */
        if (strand > 0 ) 
	{
	    TRACE("solution time and strand are written\n");
            ArgOk = TecUtilArgListAppendDouble(Arguments,SV_SOLUTIONTIME,
		                           time_double);
        /* StrandId  SV_STRANDID LgIndex_t
         * 0 for static 
         * > 1 for transient ( +1 for each timestep )
         * -1 for automatic based on solution time */
        
        ArgOk = TecUtilArgListAppendInt(Arguments,SV_STRANDID,strand);
	}

        ArgOk = TecUtilArgListAppendArray(Arguments,SV_VALUELOCATION,ValueLoc);

        /* Share connectivity from zone : SV_CONNECTSHAREZONE LgIndex_t
         * 0 if connectivity not shared, otherwise no of the zone
         * defining the connectivity to share */
	if(FirstDataSet){ShareConnect = 0;}
	else {ShareConnect = StartNewZones+1;} 
        ArgOk = TecUtilArgListAppendInt(Arguments,SV_CONNECTSHAREZONE,
		                        ShareConnect);


        /* Create the zone with the argument list */

        IsOk = TecUtilDataSetAddZoneX(Arguments);
        if ( !IsOk )
        {
            strcpy(MessageString,"Error creating zone");
            TecUtilDialogErrMsg(MessageString);
            IsOk = FALSE ;
            break;
        }
        TecUtilDataSetGetInfo(NULL,&CurZone,NULL);
//      sprintf(MessageString,"zone no %d\n",izone);
//      TRACE(MessageString);
     
        ///// END OF CREATING TECPLOT ZONE
      }


      if(FirstDataSet)
      {
	  //TRACE("write connectivity\n");
	  // get a writable handle to the node map and write
	  // it.
	  if(ndp_tec == 3 ) 
	       TecUtilFrameSetPlotType(PlotType_Cartesian2D);
  	  else TecUtilFrameSetPlotType(PlotType_Cartesian3D);
          TecUtilZoneGetInfo(izone,
                 NULL,NULL,NULL,NULL,NULL,NULL,
                 &NodeMap,
                 NULL,NULL,NULL,NULL,NULL,NULL);
	  TecUtilDataNodeArraySetByRef(NodeMap,
	                               1,nelem*ndp_tec,(NodeMap_t*)PtrIkle);
//	  free(PtrIkle);

	  // The X and Y coordinates does not change in the
	  // file. Load the data here (first data set).
          TecUtilDataValueAutoLOD(izone,1,
		  DataValueStructure_ClassicPlus,FileName,
		  VarPosX,Stride,IsNativeByteOrder);
          TecUtilDataValueAutoLOD(izone,2,
		  DataValueStructure_ClassicPlus,FileName,
		  VarPosY,Stride,IsNativeByteOrder);
      }
      else
      {
	  // If this is not the first time step,
	  //share connectivity and X,Y coordinates
	  TecUtilDataValueShare(StartNewZones+1,CurZone,VarCorr[0]);
	  TecUtilDataValueShare(StartNewZones+1,CurZone,VarCorr[1]);
      }


      if (!MeshOnly)
      {
      for(i=2;i<NumVars;i++)
      {
          /* Read header for datapoints of one variable ... */
	  RefVal = NDataPoints*4;
          IsOk = ReadFHead(f,Invert,RefVal,&FHeader,TRUE,&MessageString,&EndOfFile);
          if(!IsOk) break;
          if(EndOfFile)
          {
              strcpy(MessageString,"Unexpected End Of File");
	      TRACE(MessageString);
              TecUtilDialogErrMsg(MessageString);
	      break;
          }
          /* Read datapoints of one variable ... */
	  VarPos = ftell(f);
	  // CALL TECUTILDATAVALUEAUTOLOD
          TecUtilDataValueAutoLOD(izone,VarCorr[i],
		  DataValueStructure_ClassicPlus,FileName,
		  VarPos,Stride,IsNativeByteOrder);
	  fseek(f,RefVal,1);
          /* Read trailer for datapoints of one variable ... */
          IsOk = CheckReadBlock(f,Invert,FHeader,MessageString);
	  if(!IsOk)
          {
              strcpy(MessageString,"Error reading trail bytes\n");
	      TRACE(MessageString);
              TecUtilDialogErrMsg(MessageString);
	      break;
          }
	  //TRACE("check passed. \n");
      }
      }

      if ( AddDelwaq == TRUE && EndDelwaq == FALSE ) 
      {
	// The seconds as an integer 
	if(fread(&i,4,1,fDelwaq) != 1)
	{
            TRACE("End of delwaq file !!!\n");
	    EndDelwaq = TRUE;
	}
        //sprintf(MessageString,"Delwaq integer read : %d\n",i);
	//TRACE(MessageString);
	//*********************
        for (n=0;n<nplan;n++)
        {
           for(i=0;i<npoin_pl;i++)
	   {
	    for(j=0;j<NumVarsDelwaq;j++)
	    {
		 // read delwaq data values.
		 // Put all points for a variable together (=>see
		 // tecplot writing)
                 PtrF = VTab + j*npoin + i + (nplan-n-1)*npoin_pl ;
		 fread(PtrF,4,1,fDelwaq);
		 if (InvertDelwaq) swapbytes32(*PtrF)
	    }
	            
	  }
	}

	  // point to the beginning of the data table (the npts values
	  // of the first variable)
	  PtrF = VTab;
	  //TRACE("Try to add the data values : \n");
	  // loop over the nvar_delw added variables
	  TecUtilDataSetGetInfo(NULL,&CurZone,NULL);

	  for(j=NumVars;j<NumVars+NumVarsDelwaq;j++)
	  {
	    // Get a writable ref for the variable.
	    // Tecplot starts count at 1.
            FieldDataRef = TecUtilDataValueGetWritableRef(CurZone,VarCorr[j]);
	    // set the array as value for the variable. 
            //sprintf(str4,"Variable no : %d \n",j+1);
            //TRACE(str4);
	    // datavalues for tecplot start at 1 :
            TecUtilDataValueArraySetByRef(FieldDataRef, // data ref
		                          1,            // point offset
		                          npoin,         // no of points
					  PtrF );     // ptr to array
	    // point to the values of the next variable
	    PtrF += npoin;
	  }

      }
      izone++;
      FirstDataSet = FALSE;
      TimeStep++;
      
  }
} // if(IsOk)

  if(AddDelwaq)free(VTab);
  free(ValueLoc);
  free(VarPrecision);
  TecUtilArgListDealloc(&Arguments);
  
  return (IsOk);
}
/************************************************************************/


