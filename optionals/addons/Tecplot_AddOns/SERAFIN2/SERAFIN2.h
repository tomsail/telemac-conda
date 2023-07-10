#define ID_NVAR            60
#define ID_NVAR_AUX        61
#define ID_VARLOCATIONS    62
#define ID_DATA_TYPE       63
#define ID_FILE_DATE       64
#define ID_FILE_TIME       65
#define ID_VARNAMES        66
#define ID_VARUNITS        67


#define RECOVER            500
#define READALL            501


//The data types available (part of the file header)
#define DATA_FE     1
#define DATA_MHFE   2
#define DATA_POINT  3

//The discretization of variables to write (part of the file header)
#define DISC_P0  0
#define DISC_P1  1
#define DISC_P2  2

//The different discretisations for finite element data :
//TODO : define this kind of variables in the BIEF !!!!

#define POINTS    0
#define FE_TETRA  31
#define FE_TRI    11


#define FULL_INTERLACE   200
#define NO_INTERLACE     201
#define ID_NELEM          1  //ID_NELEM + P0
#define ID_NPOIN          2  //ID_NELEM + P1
#define ID_NFAC           3  //ID_NELEM + P2
#define ID_NPLAN          4  //Number of prism layers
#define ID_NPTFR          5  //Number of border nodes
#define ID_NPTIR          6  //?? //
#define ID_DISC           7  //FE_TETRA, FE_TRI ...
#define ID_NDP            8  //No of nodes per element



#define ID_SHARE_CONNECT  20   // Zone to share connect
                                                 

  //Other zone parameters :
#define ID_VAR_SHARE_LIST    51
#define ID_PASSIVE_VAR_LIST  52
#define ID_AUX_DATA          53
#define ID_TITLE             54
#define ID_SOLUTIONTIME      55
