/*********************************************************************
 * AddOn for loading files of the telemac Serfin or Volfin data
 * format
 * into tecplot.
 * written by R Nebauer EDF R&D LNHE, may 2007
 * regina.nebauer@edf.fr
 * DoConversion.c - converts a serafin or volfin datafile into a
 * temporary tecplot binary file, loaded into tecplot.
 **********************************************************************/


#include "TECADDON.h"
#include "ADDGLBL.h"
#include "TASSERT.h"
#include "ENGINE.h"
#include "UTIL.h"
#include "SERAFIN2.h"

/************************************************************************/
Boolean_t DoConversion(
/************************************************************************/
          char *DataFName   ,  /* IN - the temp file to write */
          char *TempFName   ,  /* IN - the temp file to write */
          char **MessageString) /* OUT - error message                  */
/************************************************************************/
{
/* Local Variables */

  Boolean_t IsOk  ;
  char text[50];
  int   i             ;
  char  title[80]     ;
  int   ndp           ;
  int   nelem         ;
  int   npoin         ;
  int   nplan         ;
  char  VarNames[3000];
  char  TecVarNames[3000];
  char  VarUnits[1000];

  int   Debug = 1;      /* Debug for tecplot writing */
  int   IsDouble = 1 ; // data are in double precision
  int   TecOk         ; /* Return value for TecUtilXxx calls */

  int   *PtrIkle    ; /* Connectivity Table */
  double *PtrValue  ; /* Values in the data set */
  int   ndat        ; /* Number of data points */
  char *PtrStr      ; // Pointer for character strings

  int   TimeStep;
  float Time;
  int FileFormat;
  float *PtrFloat ;
  Boolean_t EndOfFile;
  Boolean_t last;
  Boolean_t IsBlock;
  ArgList_pa Arguments ;

  int ndp_tec ; /* Number of nodes for element in tecplot */

  void m_read_serafin2_serafin2_open_file_(); 
  void m_read_serafin2_serafin_get_fileinfo_int_(); 
  void m_read_serafin2_serafin_get_fileinfo_int_tab(); 
  void m_read_serafin2_serafin_get_fileinfo_str_tab_(); 
  void m_read_serafin2_serafin_get_fileinfo_str_(); 
  void m_read_serafin2_serafin_get_zoneinfo_int(); 

  int l,n;
  int mode;

  int IdFile, IdZone;
  int IdFilePara,IdZonePara;


  char ZoneTitle[80];


  int NbVar;    // number of domain variables
  int NbVarAux; // number of aux variables
  int DataType;
  int IDisc;
  int ShareConnect;

  int ret; // return value

  int *VarLocations;
  int *PassiveVarList;
  int *VarShareList;

  int nelem_p, npoin_p;

  double *PtrDataP1;
  double *PtrDataP0;
  int *Ikle;

  double time;

  int nvar;


  ZoneType_e ZTyp;

/************************************************************************/
  TRACE("call serafin2 open file \n");
  TRACE(DataFName);
  TRACE("\n");

  l = strlen(DataFName);
  mode = 1;
  /* Open the data file. This will read the first zone of the 
   * data set.
   */
  m_read_serafin2_serafin2_open_file_(&l,DataFName,&mode,&IdFile,&IdZone,&last);
  if ( IdFile >= 0 ) 
  {
      TRACE("OK OPEN FILE");
  }

  /* Get the file parameters :
   * the number of variables and the number of aux variables
   * the data set type (finite elements or points)
   */
  IdFilePara = (int)ID_NVAR;
  m_read_serafin2_serafin_get_fileinfo_int_(&IdFile,&IdFilePara,&NbVar,&ret);
  IdFilePara = (int)ID_NVAR_AUX;
  m_read_serafin2_serafin_get_fileinfo_int_(&IdFile,&IdFilePara,&NbVarAux,&ret);
  IdFilePara = (int)ID_DATA_TYPE;
  m_read_serafin2_serafin_get_fileinfo_int_(&IdFile,&IdFilePara,&DataType,&ret);
  m_read_serafin2_serafin_get_fileinfo_str_(&IdFile,&IdFilePara,&title,&ret);
  /* Allocate a string for the variable names. Each variable
   * name is a 30 character length string. 
   */
  IdFilePara = (int)ID_VARNAMES;
  l = 30;
  n = NbVar+NbVarAux; 
  m_read_serafin2_serafin_get_fileinfo_str_tab_(&IdFile,&IdFilePara,&l,&n,&VarNames,&ret);
  for(i=0;i<NbVar+NbVarAux-1;i++)
  {
     VarNames[(i+1)*30-1]=',';
  }
  for(i=0;i<NbVar*30;i++ ) 
  {
      TecVarNames[i] = VarNames[i];
  }
  TecVarNames[NbVar*30-1] = 0;

  /* Allocate the tables ofr the variable properties :
   * Variable location, variable sharing and passive variable
   * list : 
   */

  VarShareList = (int*)calloc(NbVar,sizeof(int));
  PassiveVarList = (int*)calloc(NbVar,sizeof(int));
  VarLocations = (int*)calloc(NbVar,sizeof(int));

  /* The variable location list is a serafin2 file property. 
   * Read it now.
   */
  n = NbVar;
  IdFilePara = ID_VARLOCATIONS;
  m_read_serafin2_serafin_get_fileinfo_int_tab_(&IdFile,&IdFilePara,&n,VarLocations,&ret);
  TRACE("Variable names and locations : \n");
  for (i=0;i<NbVar;i++)
  {
    strcpy(text, "");
    for(n=0;n<29;n++)
    {
	text[n]=TecVarNames[i*30+n];
    }
    text[30]=0;
    if(VarLocations[i] == 0)
    {
    strcat(text,"P0 \n");
    }
    else
    {
    strcat(text,"P1 \n");
    }
    TRACE(text);
   
  }

  /* Create the tecplot temp file : 
   * We should provide the variable name list;
   *
   */
  TecOk = TecUtilTecIni(title, TecVarNames, TempFName,
	                            ".", &Debug, &IsDouble);
  if ( TecOk != 0 ) 
  {
      strcpy(*MessageString,"Error opening tecplot temp file");
      IsOk = FALSE ;
      return(IsOk) ;

  }

  /* Read and write the zone data. The first zone
   * was already read by the open_file procedure.
   * Write it down and try to read the next zone.
   */

  EndOfFile = FALSE;
  last = FALSE;
  nelem_p = 0;
  npoin_p = 0;

  while (!EndOfFile) 
  {
    if ( last ) EndOfFile = TRUE;
    /* Get the informations about the serafin2 data zone : 
     */
    
    IdZonePara = ID_DISC;
    m_read_serafin2_serafin_get_zoneinfo_int_(&IdFile,&IdZonePara,&IDisc,&ret);
   
    IdZonePara = ID_NDP;
    m_read_serafin2_serafin_get_zoneinfo_int_(&IdFile,&IdZonePara,&ndp,&ret);
  
    IdZonePara = ID_NELEM;
    m_read_serafin2_serafin_get_zoneinfo_int_(&IdFile,&IdZonePara,&nelem,&ret);
 
    IdZonePara = ID_NPOIN;
    m_read_serafin2_serafin_get_zoneinfo_int_(&IdFile,&IdZonePara,&npoin,&ret);

    IdZonePara = ID_SHARE_CONNECT;
    m_read_serafin2_serafin_get_zoneinfo_int_(&IdFile,&IdZonePara,&ShareConnect,&ret);

    IdZonePara = ID_TITLE;
    m_read_serafin2_serafin_get_zoneinfo_str_(&IdFile,&IdZonePara,&title,&ret);
    title[80]=0;

    sprintf(text,"npoin :  %d \n ",npoin);
    TRACE(text);
    sprintf(text,"nelem :  %d \n ",nelem);
    TRACE(text);
    sprintf(text,"ndp   :  %d \n ",ndp  );
    TRACE(text);
    /* Write the tecplot zoneheader : 
     * First create the argument list and fill in the values
     * read in the serafin2 file. 
     */

    Arguments = TecUtilArgListAlloc();
    // the title of the zone :

    TecOk = TecUtilArgListAppendString(Arguments,SV_TITLE,title);
    // the finite element type, if FE zone 

    switch (IDisc )
    {
      case POINTS   : ZTyp = ZoneType_Ordered;
		      TRACE("Points\n");
               break;
      case FE_TRI   : ZTyp = ZoneType_FETriangle;
		      TRACE("Triangles\n");
               break;
      case FE_TETRA : ZTyp = ZoneType_FETetra;
		      TRACE("Tetras\n");
               break;
      default : sprintf(*MessageString,"Unknown data type\n");
		     return(FALSE);
		     break;
    }

    TecOk = TecUtilArgListAppendInt(Arguments,SV_ZONETYPE,ZTyp);

    TecOk = TecUtilArgListAppendInt(Arguments,SV_IMAX,npoin);

    TecOk = TecUtilArgListAppendInt(Arguments,SV_JMAX,nelem);
    n = 1;

    TecOk = TecUtilArgListAppendInt(Arguments,SV_KMAX,n);

    IdZonePara = ID_SOLUTIONTIME;
    m_read_serafin2_serafin_get_zoneinfo_real_(&IdFile,&IdZonePara,&time,&ret);

    TecOk = TecUtilArgListAppendDouble(Arguments,SV_SOLUTIONTIME,time);

    TecOk = TecUtilArgListAppendArray(Arguments,SV_VALUELOCATION,VarLocations);

    TecOk = TecUtilArgListAppendInt(Arguments,SV_CONNECTSHAREZONE,ShareConnect);



    IdZonePara = ID_VAR_SHARE_LIST;
    n = NbVar;
    m_read_serafin2_serafin_get_zoneinfo_int_tab_(&IdFile,&IdZonePara,&n,VarShareList,&ret);

    IdZonePara = ID_PASSIVE_VAR_LIST;
    n = NbVar;

    m_read_serafin2_serafin_get_zoneinfo_int_tab_(&IdFile,&IdZonePara,&n,PassiveVarList,&ret);

    TecOk = TecUtilArgListAppendArray(Arguments,SV_VARSHAREZONELIST,VarShareList);

    TecOk = TecUtilArgListAppendArray(Arguments,SV_PASSIVEVARLIST,PassiveVarList);


    IsBlock = TRUE;
    TecOk = TecUtilArgListAppendInt(Arguments,SV_ISBLOCK,IsBlock);


    TecOk = TecUtilArgListAppendInt(Arguments,SV_STRANDID,-1);


    TecOk = TecUtilTecZneX(Arguments);


    if( nelem_p != nelem || npoin_p != npoin )
    {
	if (IdZone > 1 )
	{
	    free(PtrDataP0);
	    free(PtrDataP1);
	}
	TRACE("allocate data arrays\n");
	PtrDataP0 = (double*)calloc(nelem,sizeof(double));
	PtrDataP1 = (double*)calloc(npoin,sizeof(double));

        if ( DataType == DATA_FE ) 
        {
	    if (IdZone > 1) free(Ikle);
	    /* We should read/write a connectivity. */
	    TRACE("allocate ikle\n");
	    Ikle = (int*)calloc(nelem*ndp,sizeof(int));
	}
        nelem_p = nelem;
	npoin_p = npoin;
    }


    sprintf(text,"nelem : %d npoin %d \n ",nelem,npoin);
    TRACE(text);
    TRACE("Read and write data arrays : \n");
    for(i=0;i<NbVar;i++)
    {
	if ( VarLocations[i] == DISC_P0 ) 
	{
	    PtrValue = PtrDataP0;
	    ndat = nelem;
	}
	else if ( VarLocations[i] == DISC_P1 ) 
	{
	    PtrValue = PtrDataP1;
	    ndat = npoin;
	}
	else
	{
	    TRACE("ERROR! unknown data discretization type.\n");
	    ndat = 0;
	    PtrValue = NULL;
	}
	nvar = i+1;    
        sprintf(text,"nvar : %d ndat %d \n ",nvar,ndat);
        TRACE(text);
        m_read_serafin2_serafin2_get_data_array_(&IdFile,&nvar,&ndat,PtrValue,&ret);
        TecOk = TecUtilTecDat((LgIndex_t*)&ndat,(void*)PtrValue,&IsDouble);
	if(TecOk != 0 ) 
	{
	    sprintf(*MessageString,"Error writing data \n");
	    return(FALSE);
	}
    }
    sprintf(text,"ShareConnect = %d\n",ShareConnect);
    TRACE(text);
    if(ShareConnect == 0 && DataType != DATA_POINT ) 
    {
	sprintf(text,"npoin :  %d \n ",npoin);
	TRACE(text);
	sprintf(text,"nelem :  %d \n ",nelem);
	TRACE(text);
	sprintf(text,"ndp   :  %d \n ",ndp  );
	TRACE(text);
	mode = NO_INTERLACE;
	m_read_serafin2_serafin2_get_ikle_(&IdFile,&mode,&nelem,&ndp,Ikle,&ret);
	PtrIkle = Ikle;
	for(i=0;i<5;i++)
	{
	    sprintf(text,"nodes : %d %d %d %d \n",PtrIkle[0],PtrIkle[1],PtrIkle[2],PtrIkle[3]);
	    TRACE(text);
	    PtrIkle += 4;
	}
	TRACE("before tecnod\n");
	TecOk = TecUtilTecNod((LgIndex_t*)Ikle);
	sprintf(text,"after tecnod : %d\n",TecOk);
	TRACE(text);
	if(TecOk != 0 ) 
	{
	    sprintf(*MessageString,"Error writing connectivity \n");
	    return(FALSE);
	}
    }


    if (!EndOfFile) m_read_serafin2_serafin2_get_nextzone_(&IdFile,&IdZone,&last);

  }
   
    free(PtrDataP0);
    free(PtrDataP1);

    if ( DataType == DATA_FE ) free(Ikle);

  TecOk = TecUtilTecEnd();


  IsOk = TRUE ;
  return(IsOk);
}
/************************************************************************/


