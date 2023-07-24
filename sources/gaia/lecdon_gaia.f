!                   **********************
                    SUBROUTINE LECDON_GAIA
!                   **********************
!
     &(MOTCAR,FILE_DESC,PATH,NCAR,CODE,CAS_FILE,DICO_FILE)
!
!***********************************************************************
! GAIA
!***********************************************************************
!
!>@brief Reads the steering file of GAIA through a call to damocles.
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!>@param[in,out] MOTCAR     Values of key-words of type character
!>@param[in,out] FILE_DESC  Stores strings 'submit' of dictionary
!>@param[in]     PATH       Full path to code dictionary
!>@param[in]     NCAR       Number of letters in string path
!>@param[in]     CODE       Name of the code calling lecdon
!>@param[in]     CAS_FILE   Name of steering file (api)
!>@param[in]     DICO_FILE  Name of dictionary file (api)
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE DECLARATIONS_SPECIAL
      USE DECLARATIONS_TELEMAC
      USE DECLARATIONS_GAIA
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!!-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)               :: NCAR
      CHARACTER(LEN=24), INTENT(IN)     :: CODE
      CHARACTER(LEN=PATH_LEN), INTENT(IN)    :: PATH
      CHARACTER(LEN=PATH_LEN), INTENT(INOUT) :: MOTCAR(MAXKEYWORD)
      CHARACTER(LEN=PATH_LEN), INTENT(INOUT) :: FILE_DESC(4,MAXKEYWORD)
!     API
      CHARACTER(LEN=PATH_LEN), INTENT(IN)    :: CAS_FILE
      CHARACTER(LEN=PATH_LEN), INTENT(IN)    :: DICO_FILE
!
!!-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER            :: I,K,ERR
      INTEGER            :: MOTINT(MAXKEYWORD)
      INTEGER            :: TROUVE(4,MAXKEYWORD)
      INTEGER            :: ADRESS(4,MAXKEYWORD)
      INTEGER            :: DIMENS(4,MAXKEYWORD)
      DOUBLE PRECISION   :: SUMAVAI
      DOUBLE PRECISION   :: MOTREA(MAXKEYWORD)
      LOGICAL            :: DOC,EFFPEN
      LOGICAL            :: MOTLOG(MAXKEYWORD)
      CHARACTER(LEN=PATH_LEN) :: NOM_CAS
      CHARACTER(LEN=PATH_LEN) :: NOM_DIC
      CHARACTER(LEN=72)  :: MOTCLE(4,MAXKEYWORD,2)

      INTEGER :: ID_DICO, ID_CAS
!
!-----------------------------------------------------------------------
!
      CHARACTER(LEN=2) CHAR2
      CHARACTER(LEN=PATH_LEN) TEMPVAR
!
!-----------------------------------------------------------------------
!
! INITIALISES THE VARIABLES FOR DAMOCLES CALL
!
      DO K = 1, MAXKEYWORD
!       A FILENAME NOT GIVEN BY DAMOCLES WILL BE RECOGNIZED AS A WHITE SPACE
!       (IT MAY BE THAT NOT ALL COMPILERS WILL INITIALISE LIKE THAT)
        MOTCAR(K)(1:1)=' '
!
        DIMENS(1,K) = 0
        DIMENS(2,K) = 0
        DIMENS(3,K) = 0
        DIMENS(4,K) = 0
      ENDDO
!
!     WRITES OUT INFO
      DOC = .FALSE.
!
!-----------------------------------------------------------------------
!     OPENS DICTIONNARY AND STEERING FILES
!-----------------------------------------------------------------------
!
      IF(NCAR.GT.0) THEN
!
        NOM_DIC=PATH(1:NCAR)//'GAIDICO'
        NOM_CAS=PATH(1:NCAR)//'GAICAS'
!
      ELSE
!
        NOM_DIC='GAIDICO'
        NOM_CAS='GAICAS'
!
      ENDIF
      IF((CAS_FILE(1:1).NE.' ').AND.(DICO_FILE(1:1).NE.' ')) THEN
        WRITE(LU,*) 'FIXED DICO AND STEERING FILE PRESENT'
        NOM_DIC=DICO_FILE
        NOM_CAS=CAS_FILE
        WRITE(LU,*) 'NOM_DIC',NOM_DIC
        WRITE(LU,*) 'NOM_CAS',NOM_CAS
      ENDIF
!
      CALL GET_FREE_ID(ID_DICO)
      OPEN(ID_DICO,FILE=NOM_DIC,FORM='FORMATTED',ACTION='READ')
      CALL GET_FREE_ID(ID_CAS)
      OPEN(ID_CAS,FILE=NOM_CAS,FORM='FORMATTED',ACTION='READ')
!
!-----------------------------------------------------------------------
!     CALLS DAMOCLES
!-----------------------------------------------------------------------
!
      CALL DAMOCLE( ADRESS, DIMENS  ,MAXKEYWORD, DOC    , LNG , LU  ,
     &              MOTINT, MOTREA ,MOTLOG , MOTCAR ,
     &              MOTCLE, TROUVE ,ID_DICO, ID_CAS,.FALSE. ,FILE_DESC)
!
!-----------------------------------------------------------------------
!     CLOSES DICTIONNARY AND STEERING FILES
!-----------------------------------------------------------------------
!
      CLOSE(ID_DICO)
      CLOSE(ID_CAS)
!
!     DECODES 'SUBMIT' CHAINS
!
      CALL READ_SUBMIT(GAI_FILES,MAXLU_GAI,FILE_DESC,MAXKEYWORD)
!
!-----------------------------------------------------------------------
!
!     RETRIEVES FILES NUMBERS IN GAIA FORTRAN PARAMETERS
!     AT THIS LEVEL LOGICAL UNITS ARE EQUAL TO THE FILE NUMBER
!
      DO I=1,MAXLU_GAI
        IF(GAI_FILES(I)%TELNAME.EQ.'GAIGEO') THEN
          GAIGEO=I
        ELSEIF(GAI_FILES(I)%TELNAME.EQ.'GAICLI') THEN
          GAICLI=I
        ELSEIF(GAI_FILES(I)%TELNAME.EQ.'GAIPRE') THEN
          GAIPRE=I
        ELSEIF(GAI_FILES(I)%TELNAME.EQ.'GAIRES') THEN
          GAIRES=I
        ELSEIF(GAI_FILES(I)%TELNAME.EQ.'GAIREF') THEN
          GAIREF=I
        ELSEIF(GAI_FILES(I)%TELNAME.EQ.'GAICOU') THEN
          GAICOU=I
        ELSEIF(GAI_FILES(I)%TELNAME.EQ.'GAIFON') THEN
          GAIFON=I
        ELSEIF(GAI_FILES(I)%TELNAME.EQ.'GAISEC') THEN
          GAISEC=I
        ELSEIF(GAI_FILES(I)%TELNAME.EQ.'GAISEO') THEN
          GAISEO=I
        ELSEIF(GAI_FILES(I)%TELNAME.EQ.'GAILIQ') THEN
          GAILIQ=I
        ELSEIF(GAI_FILES(I)%TELNAME.EQ.'GAIFLX') THEN
          GAIFLX=I
!       === NESTOR FILES
        ELSEIF(GAI_FILES(I)%TELNAME.EQ.'SINACT') THEN
          SINACT=I
        ELSEIF(GAI_FILES(I)%TELNAME.EQ.'SINPOL') THEN
          SINPOL=I
        ELSEIF(GAI_FILES(I)%TELNAME.EQ.'SINREF') THEN
          SINREF=I
        ELSEIF(GAI_FILES(I)%TELNAME.EQ.'SINRST') THEN
          SINRST=I
!       ===
        ELSEIF(GAI_FILES(I)%TELNAME.EQ.'VSPRES') THEN
          VSPRES=I
!
        ENDIF
      ENDDO
!
!-----------------------------------------------------------------------
!
!     ALLOCATING ARRAYS DEPENDING ON MAXFRO
!
!-----------------------------------------------------------------------
!
!     MAXIMUM NUMBER OF BOUNDARIES
      MAXFRO   = MOTINT( ADRESS(1,58) )
!
      ALLOCATE(SOLDIS(MAXFRO))
      ALLOCATE(OKCGL(MAXFRO))
      ALLOCATE(OKQGL(MAXFRO))
!
      DO K=1,MAXFRO
        OKCGL(K)=.TRUE.
        OKQGL(K)=.TRUE.
      ENDDO
!
!-----------------------------------------------------------------------
!
!     ASSIGNS THE STEERING FILE VALUES TO THE PARAMETER FORTRAN NAME
!
!-----------------------------------------------------------------------
!
!     OPTION OF MATRIX ASSEMBLY IS HARD-CODED
!
      OPTASS = 1
      PRODUC = 1
!
!     DISCRETISATIONS
!
      IELMT     = 11 ! SEDIMENTOLOGICAL VARIABLES
      IELMH_GAI = 11 ! VARIABLES ASSOCIATED WITH WATER DEPTH
      IELMU_GAI = 11 ! VARIABLES ASSOCIATED WITH VELOCITIES
!
!     FOR NOW PRINTOUTS START AT ZERO
!
      PTINIG = 0
      PTINIL = 0
!
!     NON-EQUILIBIRUM BEDLOAD
!
!     ICM           = MOTINT( ADRESS(1,  1) )
      ICF           = MOTINT( ADRESS(1,  2) )
      BED_MODEL     = MOTINT( ADRESS(1,  60) )
!     N1            = MOTINT( ADRESS(1,  5) )

      OPTBAN        = MOTINT( ADRESS(1, 11) )
      LVMAC         = MOTINT( ADRESS(1, 12) )
      NSOUS         = MOTINT( ADRESS(1, 14) )
!
      MARDAT(1)     = MOTINT( ADRESS(1, 15) )
      MARDAT(2)     = MOTINT( ADRESS(1, 15) + 1 )
      MARDAT(3)     = MOTINT( ADRESS(1, 15) + 2 )
      MARTIM(1)     = MOTINT( ADRESS(1, 16) )
      MARTIM(2)     = MOTINT( ADRESS(1, 16) + 1 )
      MARTIM(3)     = MOTINT( ADRESS(1, 16) + 2 )
!
      NPRIV         = MOTINT( ADRESS(1, 23) )
      NADVAR    = MOTINT( ADRESS(1,30) )
!     NUMBER OF DIRECTIONS FOR DIFFERENTIATED VARIABLES
      AD_NUMOFDIR  = MOTINT( ADRESS(1,59) )
!
!     NCSIZE        = MOTINT( ADRESS(1, 24) )
!     NUMBER OF PROCESSORS (ALREADY KNOWN FROM THE HYDRODYNAMICS
!     MODULE: MUST BE THE SAME, BUT WHEN USING COUPLED MODELS
!     IT CAN BE DIFFERENT BY MISTAKE)
      IF(NCSIZE.NE.MOTINT(ADRESS(1,24))) THEN
        WRITE(LU,*) 'DIFFERENT NUMBER OF PARALLEL PROCESSORS:'
        WRITE(LU,*) 'DECLARED BEFORE (CASE OF COUPLING ?):',NCSIZE
        WRITE(LU,*) 'GAIA :',MOTINT(ADRESS(1,24))
        WRITE(LU,*) 'VALUE ',NCSIZE,' IS KEPT'
      ENDIF

      PRODUC        = MOTINT( ADRESS(1, 33) )
      OPTASS        = MOTINT( ADRESS(1, 34) )
      SLOPEFF       = MOTINT( ADRESS(1, 39) )
      DEVIA         = MOTINT( ADRESS(1, 40) )
      NUMSTRAT      = MOTINT( ADRESS(1,251) )
      NSICLA        = DIMENS(4,59)
!
!     INITIALISATION OF ARRAYS WITH DEFAULT VALUES
!
      DO I=1,NSICLM
        XMVS0(I) = 2650.D0
        DCLA(I)  = 0.01D0
        AC(I)    = -9.D0
        XWC0(I)  = -9.D0
        TOCD_MUD0(I) = 1000.D0
        PARTHENIADES0(I) = 1.D-3
        HIDI(I)  = 1.D0
        AVA0(I)  = 0.D0
      ENDDO
!     IF ONLY ONE SEDIMENT AVA0(1) = 1.D0
      AVA0(1) = 1.D0
!
      IF(NSICLA.GT.1) THEN
!       WHEN THERE IS A SEDIMENT MIXTURE THE
!       GRANULOMETRY DISTRIBUTION ALONG THE VERTICAL
!       IN THE BED EVOLVES AND THIS IS REPRESENTED
!       THROUGH THE USE OF AN ACTIVE LAYER
!       NOMBLAY IS THUS THE NUMBER OF PHYSICAL LAYERS
!       PLUS ONE, TO ACCOUNT FOR THE ACTIVE LAYER
        NOMBLAY = NUMSTRAT+1
      ELSE
        NOMBLAY = NUMSTRAT
      ENDIF
!
      DO I=1,NOMBLAY
        XKV0(I)  = 0.4D0
      ENDDO
!
!     FILL DEFAULT LAYER THICKNESSES
      DO I=1,NUMSTRAT
        SED_THICK(I)=100.D0/DBLE(NUMSTRAT)
      ENDDO
!
!
      HIDFAC        = MOTINT( ADRESS(1, 52) )
      ICQ           = MOTINT( ADRESS(1, 41) )
!     CONTROL SECTIONS
      NCP=DIMENS(1,42)
      ALLOCATE(CTRLSC(NCP),STAT=ERR)
      IF(ERR.NE.0) THEN
        WRITE(LU,2039) ERR
2039    FORMAT(1X,'LECDON_GAIA:',/,1X,
     &            'ERROR DURING ALLOCATION OF CTRLSC: ',/,1X,
     &            'ERROR CODE: ',1I6)
      ENDIF
      IF(NCP.GE.1) THEN
        DO K=1,NCP
          CTRLSC(K) = MOTINT( ADRESS(1,42) + K-1 )
        ENDDO
      ENDIF
!     COORDINATES OF THE ORIGIN
      I_ORIG = MOTINT( ADRESS(1,43)   )
      J_ORIG = MOTINT( ADRESS(1,43)+1 )
      DEBUG  = MOTINT( ADRESS(1,44)   )
!
      ICR  =   MOTINT(ADRESS(1,46)   )
!
      IKS  =   MOTINT(ADRESS(1,47)   )
!
!     MAXIMUM NUMBER OF ITERATIONS FOR ADVECTION SCHEMES
!     TO COMPUTE THE BEDLOAD, FINITE VOLUME SCHEME CENTERED UPWIND, OR
!     POSITIVE DEPTH
      MAXADV = MOTINT(ADRESS(1,54))
!
!     VERTICAL ADVECTION SCHEME
!
      SETDEP = MOTINT( ADRESS(1,62))
!
      TYPE_HOULE   = MOTINT( ADRESS(1,63) )
!
!     CVSM VARIABLES
      VSMTYPE = MOTINT( ADRESS(1,64))
      PRO_MAX_MAX = MOTINT( ADRESS(1,65))
      CVSMPPERIOD = MOTINT( ADRESS(1,66))
      ALT_MODEL = MOTINT( ADRESS(1,67))
! As long as there is no coupling period from telemac...
      PERCOU = 1

! ############### !
! REAL KEYWORDS   !
! ############### !
!
      IF(TROUVE(2,3).GE.1.AND.DIMENS(2,3).EQ.NSICLA) THEN
        DO K=1,NSICLA
          XMVS0(K) = MOTREA(ADRESS(2,3)+K-1)
        ENDDO
      ELSEIF(TROUVE(2,3).GE.1.AND.
     &       DIMENS(2,3).LT.NSICLA.AND.DIMENS(2,3).GT.0) THEN
!       READING WHAT HAS BEEN GIVEN
        DO K=1,DIMENS(2,3)
          XMVS0(K) = MOTREA(ADRESS(2,3)+K-1)
        ENDDO
!       COMPLETING WITH THE LAST GIVEN
        DO K=DIMENS(2,3)+1,NSICLA
          XMVS0(K) = MOTREA(ADRESS(2,3)+DIMENS(2,3)-1)
        ENDDO
      ENDIF
!
      IF(TROUVE(2,4).GE.1.AND.DIMENS(2,4).EQ.NSICLA) THEN
        DO K=1,NSICLA
          DCLA(K) = MOTREA(ADRESS(2,4)+K-1)
        ENDDO
      ELSEIF(TROUVE(2,4).GE.1.AND.
     &       DIMENS(2,4).LT.NSICLA.AND.DIMENS(2,4).GT.0) THEN
!       READING WHAT HAS BEEN GIVEN
        DO K=1,DIMENS(2,4)
          DCLA(K) = MOTREA(ADRESS(2,4)+K-1)
        ENDDO
!       COMPLETING WITH THE LAST GIVEN
        DO K=DIMENS(2,4)+1,NSICLA
          DCLA(K) = MOTREA(ADRESS(2,4)+DIMENS(2,4)-1)
        ENDDO
      ENDIF
!
!     SHIELDS NUMBERS
      IF(TROUVE(2,6).GE.1.AND.DIMENS(2,6).EQ.NSICLA) THEN
        DO K=1,NSICLA
          AC(K) = MOTREA(ADRESS(2,6)+K-1)
        ENDDO
      ELSEIF(TROUVE(2,6).GE.1.AND.
     &       DIMENS(2,6).LT.NSICLA.AND.DIMENS(2,6).GT.0) THEN
!       READING WHAT HAS BEEN GIVEN
        DO K=1,DIMENS(2,6)
          AC(K) = MOTREA(ADRESS(2,6)+K-1)
        ENDDO
!       COMPLETING WITH THE LAST GIVEN
        DO K=DIMENS(2,6)+1,NSICLA
          AC(K) = MOTREA(ADRESS(2,6)+DIMENS(2,6)-1)
        ENDDO
      ENDIF
      ZERO        = MOTREA( ADRESS(2,  9) )
      VCE         = MOTREA( ADRESS(2, 10) )
      HMIN        = MOTREA( ADRESS(2, 11) )
      BETA        = MOTREA( ADRESS(2, 16) )
!     SETTLING VELOCITIES (SAME TREATMENT AS SHIELDS NUMBERS)
!
      IF(TROUVE(2,22).GE.1.AND.DIMENS(2,22).EQ.NSICLA) THEN
        DO K=1,NSICLA
          XWC0(K) = MOTREA(ADRESS(2,22)+K-1)
        ENDDO
      ELSEIF(TROUVE(2,22).GE.1.AND.
     &       DIMENS(2,22).LT.NSICLA.AND.DIMENS(2,22).GT.0) THEN
!       READING WHAT HAS BEEN GIVEN
        DO K=1,DIMENS(2,22)
          XWC0(K) = MOTREA(ADRESS(2,22)+K-1)
        ENDDO
!       COMPLETING WITH THE LAST GIVEN
        DO K=DIMENS(2,22)+1,NSICLA
          XWC0(K) = MOTREA(ADRESS(2,22)+DIMENS(2,22)-1)
        ENDDO
      ENDIF
!
      KSPRATIO    = MOTREA( ADRESS(2, 24) )
      PHISED      = MOTREA( ADRESS(2, 25) )
      BETA2       = MOTREA( ADRESS(2, 26) )
      BIJK        = MOTREA( ADRESS(2, 27) )
      D90         = MOTREA( ADRESS(2, 28) )
!
!     COHESIVE SEDIMENT
!     +++++++++++++++++
!
      NCOUCH_TASS = MOTINT( ADRESS(1,45)   )
!
!     DEFAULT VALUES, PREVIOUSLY IN THE DICTIONARY
!     EVEN IF THERE ARE NOT 10 LAYERS
      CONC_MUD0(1)  =  50.D0
      CONC_MUD0(2)  = 100.D0
      CONC_MUD0(3)  = 150.D0
      CONC_MUD0(4)  = 200.D0
      CONC_MUD0(5)  = 250.D0
      CONC_MUD0(6)  = 300.D0
      CONC_MUD0(7)  = 350.D0
      CONC_MUD0(8)  = 400.D0
      CONC_MUD0(9)  = 450.D0
      CONC_MUD0(10) = 500.D0
!
      IF(DIMENS(2,32).GT.0) THEN
        DO K=1,DIMENS(2,32)
          CONC_MUD0(K)=MOTREA( ADRESS(2,32) + K-1 )
        ENDDO
      ENDIF

!
!
      IF(DIMENS(2,34).GT.0) THEN
        DO K=1,DIMENS(2,34)
          TOCE_MUD0(K)=MOTREA( ADRESS(2,34) + K-1 )
        ENDDO
      ENDIF
!
      IF(TROUVE(2,36).GE.1.AND.DIMENS(2,36).EQ.NSICLA) THEN
        DO K=1,NSICLA
          TOCD_MUD0(K) = MOTREA(ADRESS(2,36)+K-1)
        ENDDO
      ELSEIF(TROUVE(2,36).GE.1.AND.
     &       DIMENS(2,36).LT.NSICLA.AND.DIMENS(2,36).GT.0) THEN
!       READING WHAT HAS BEEN GIVEN
        DO K=1,DIMENS(2,36)
          TOCD_MUD0(K) = MOTREA(ADRESS(2,36)+K-1)
        ENDDO
!       COMPLETING WITH THE LAST GIVEN
        DO K=DIMENS(2,36)+1,NSICLA
          TOCD_MUD0(K) = MOTREA(ADRESS(2,36)+DIMENS(2,36)-1)
        ENDDO
      ENDIF
!
!
!     PARTHENIADES WITH CONVERSION TO M/S
!
!can be also NOMBLAY
      IF(TROUVE(2,37).GE.1.AND.DIMENS(2,37).EQ.NOMBLAY) THEN
        DO K=1,NOMBLAY
          PARTHENIADES0(K) = MOTREA(ADRESS(2,37)+K-1)
        ENDDO
      ELSEIF(TROUVE(2,37).GE.1.AND.
     &       DIMENS(2,37).LT.NOMBLAY.AND.DIMENS(2,37).GT.0) THEN
!       READING WHAT HAS BEEN GIVEN
        DO K=1,DIMENS(2,37)
          PARTHENIADES0(K) = MOTREA(ADRESS(2,37)+K-1)
        ENDDO
!       COMPLETING WITH THE LAST GIVEN
        DO K=DIMENS(2,37)+1,NOMBLAY
          PARTHENIADES0(K) = MOTREA(ADRESS(2,37)+DIMENS(2,37)-1)
        ENDDO
      ENDIF
!
!     DEFAULT VALUES, PREVIOUSLY IN THE DICTIONARY
!     EVEN IF THERE ARE NOT 10LAYERS
      TRANS_MASS0(1)  = 5.D-5
      TRANS_MASS0(2)  = 4.5D-5
      TRANS_MASS0(3)  = 4.D-5
      TRANS_MASS0(4)  = 3.5D-5
      TRANS_MASS0(5)  = 3.D-5
      TRANS_MASS0(6)  = 2.5D-5
      TRANS_MASS0(7)  = 2.D-5
      TRANS_MASS0(8)  = 1.5D-5
      TRANS_MASS0(9)  = 1.D-5
      TRANS_MASS0(10) = 0.D0
!
      IF(DIMENS(2,33).GT.0) THEN
        DO K=1,DIMENS(2,33)
          TRANS_MASS0(K)=MOTREA( ADRESS(2,33) + K-1 )
        ENDDO
      ENDIF
!
!     BED LAYER THICKNESS
      IF(TROUVE(2,42).GE.1.AND.DIMENS(2,42).EQ.NUMSTRAT) THEN
        DO K=1,NUMSTRAT
          SED_THICK(K) = MOTREA(ADRESS(2,42)+K-1)
        ENDDO
      ELSEIF(TROUVE(2,42).GE.1.AND.
     &       DIMENS(2,42).LT.NUMSTRAT.AND.DIMENS(2,42).GT.0) THEN
!       READING WHAT HAS BEEN GIVEN
        DO K=1,DIMENS(2,42)
          SED_THICK(K) = MOTREA(ADRESS(2,42)+K-1)
        ENDDO
!       COMPLETING WITH THE LAST GIVEN
        DO K=DIMENS(2,42)+1,NUMSTRAT
          SED_THICK(K) = MOTREA(ADRESS(2,42)+DIMENS(2,42)-1)
        ENDDO
        WRITE(LU,*)'WARNING:'
        WRITE(LU,*)'BED LAYER THICKNESS IS NOT SPECIFIED'
        WRITE(LU,*)'FOR ALL LAYERS'
        WRITE(LU,*)'LAST VALUE WILL BE KEPT FOR THE'
        WRITE(LU,*)'LAST LAYERS'
      ENDIF
!
!     PRESCRIBED SOLID DISCHARGES
!
      NSOLDIS=DIMENS(2,51)
      IF(NSOLDIS.GT.0) THEN
        DO K=1,NSOLDIS
          SOLDIS(K)=MOTREA(ADRESS(2,51)+K-1)
        ENDDO
      ENDIF
!
!     MINIMUM DEPTH FOR BEDLOAD WHEN USING FE
!
      HMIN_BEDLOAD=MOTREA(ADRESS(2,52))
!
!     CENTERING FOR FV
!
      DVF=MOTREA(ADRESS(2,53))
!
!     HIDING EXPOSURE MULTI GRAIN MODEL
!
      IF(TROUVE(2,7).GE.1.AND.DIMENS(2,7).EQ.NSICLA) THEN
        DO K=1,NSICLA
          HIDI(K) = MOTREA(ADRESS(2,7)+K-1)
        ENDDO
      ELSEIF(TROUVE(2,7).GE.1.AND.
     &       DIMENS(2,7).LT.NSICLA.AND.DIMENS(2,7).GT.0) THEN
!       READING WHAT HAS BEEN GIVEN
        DO K=1,DIMENS(2,7)
          HIDI(K) = MOTREA(ADRESS(2,7)+K-1)
        ENDDO
!       COMPLETING WITH THE LAST GIVEN
        DO K=DIMENS(2,7)+1,NSICLA
          HIDI(K) = MOTREA(ADRESS(2,7)+DIMENS(2,7)-1)
        ENDDO
      ENDIF
!
      IF(TROUVE(2,258).GE.1.AND.DIMENS(2,258).EQ.NSICLA) THEN
        DO K=1,NSICLA
          AVA0(K) = MOTREA(ADRESS(2,258)+K-1)
        ENDDO
      ELSEIF(TROUVE(2,258).GE.1.AND.
     &       DIMENS(2,258).LT.NSICLA.AND.DIMENS(2,258).GT.0) THEN
!       READING WHAT HAS BEEN GIVEN
        DO K=1,DIMENS(2,258)
          AVA0(K) = MOTREA(ADRESS(2,258)+K-1)
        ENDDO
!       COMPLETING WITH THE LAST GIVEN
        DO K=DIMENS(2,258)+1,NSICLA
          AVA0(K) = MOTREA(ADRESS(2,258)+DIMENS(2,258)-1)
        ENDDO
      ENDIF
!
      ELAY0       = MOTREA( ADRESS(2,259) )
!
! UM: MPM-Factor
      MPM         = MOTREA( ADRESS(2,260) )
! UM: ALPHA-Factor
      ALPHA       = MOTREA( ADRESS(2,261) )
! UM: MOFAC-Factor
      MOFAC       = MOTREA( ADRESS(2,262) )
      MOFAC_BED   = MOTREA( ADRESS(2,263) )
!
      CGEL= MOTREA( ADRESS(2,29) )
      CINI= MOTREA( ADRESS(2,35) )
      TURBA     = MOTREA(ADRESS(2,40))
      TURBB     = MOTREA(ADRESS(2,41))
! ################## !
      ! LOGICAL KEYWORDS !
      ! ################## !
! INDEX 99 IS ALREADY USED FOR KEYWORD 'LIST OF FILES'
! INDEX 54 IS ALREADY USED FOR KEYWORD 'DESCRIPTION OF LIBRARIES'
! INDEX 57 IS ALREADY USED FOR KEYWORD 'DEFAULT EXECUTABLE'
      ! SPHERICAL EQUATIONS HARD-CODED
      ! ----------------------------------
      SPHERI       = .FALSE.
!
      BILMA        = MOTLOG( ADRESS(3,  1) )
      IF(BILMA) THEN
        ALLOCATE(SUMBEDLOAD_B_FLUX(MAXFRO))
      ENDIF
      BANDEC       = MOTLOG( ADRESS(3,  3) )
      VALID        = MOTLOG( ADRESS(3,  4) )
      CONV_WAVES   = MOTLOG( ADRESS(3,  5) )
      SUSP_SAND    = MOTLOG( ADRESS(3,  7) )
      CHARR        = MOTLOG( ADRESS(3,  8) )
      HOULE        = MOTLOG( ADRESS(3, 10) )
      CONST_ALAYER = MOTLOG( ADRESS(3, 11) )
      IF(CONST_ALAYER.AND.ALT_MODEL.NE.0) THEN
        WRITE(LU,*)'IF CONSTANT ACTIVE LAYER THICKNESS=YES'
        WRITE(LU,*)'ACTIVE LAYER THICKNESS FORMULA MUST=0'
        STOP
      ENDIF
!     USED TO CHECK GAI_FILES(GAIPRE)%NAME
      DEBU         = MOTLOG( ADRESS(3, 14) )
      IMP_INFLOW_C = MOTLOG( ADRESS(3, 15) )
      SECCURRENT   = MOTLOG( ADRESS(3, 16) )
      HAVESECFILE  = MOTLOG( ADRESS(3, 59) )
      IF(CODE(1:9).EQ.'TELEMAC3D') SECCURRENT = .FALSE.
      VF           = MOTLOG( ADRESS(3,  2) )
      CORR_CONV    = MOTLOG( ADRESS(3, 18) )
      SLIDE    = MOTLOG( ADRESS(3, 20) )
      EFFPEN   = MOTLOG( ADRESS(3, 22) )
      IF(.NOT.EFFPEN) THEN
        SLOPEFF=0
        DEVIA=0
      ENDIF
!
!     COUPLING WITH NESTOR
      NESTOR=MOTLOG(ADRESS(3,25))
!     V6P1
      KSCALC   =MOTLOG(ADRESS(3,26))
!
!     Settling lag: determines choice between Rouse and Miles concentration profile
!     SET_LAG = TRUE : Miles
!             = FALSE: Rouse
!
      SET_LAG  = MOTLOG(ADRESS(3,27) )
!     Checking the mesh
      CHECK_MESH = MOTLOG(ADRESS(3,29) )
!     NEW IMPLEMENTATION FOR CROSS-SECTION
      DOFLUX = MOTLOG(ADRESS(3,61) )
!##> RK @ BAW
      IF ( CODE(1:7) .NE. 'TELEMAC' ) THEN
!##> JR @ ADJOINTWARE     SYMBOLIC LINEAR SOLVER FOR AD
!       SYMBOLIC LINEAR SOLVER FOR AD
        AD_SYMBLINSOLV  = MOTLOG( ADRESS(3,30) )
!       RESET TANGENTS ON ENTRY IN LINEAR SOLVER CG FOR AD
        AD_LINSOLV_RESETDERIV  = MOTLOG( ADRESS(3,31) )
!       ALLOW ADDITONAL INTERATIONS IN ITERATIVE LINEAR SOLVERS
!         SHOULD NOT BE USED IN PARALLEL MODE
        AD_LINSOLV_DERIVATIVE_CONVERGENCE  = MOTLOG( ADRESS(3,32) )
!##< JR @ ADJOINTWARE
      ENDIF
!##< RK @ BAW
      HINDER=  MOTLOG( ADRESS(3,9))
      FLOC =  MOTLOG( ADRESS(3,34))
!
! ################################### !
! CHARACTER STRING KEYWORDS           !
! ################################### !
!
      TITCA            = MOTCAR( ADRESS(4, 1) )(1:72)
      SORTIS           = MOTCAR( ADRESS(4, 2) )(1:72)
      VARIM            = MOTCAR( ADRESS(4, 3) )(1:72)
      GAI_FILES(GAIGEO)%NAME=MOTCAR( ADRESS(4,6) )
      GAI_FILES(GAICLI)%NAME=MOTCAR( ADRESS(4,9) )
      GAI_FILES(GAIPRE)%NAME=MOTCAR( ADRESS(4,11) )
      GAI_FILES(GAIRES)%NAME=MOTCAR( ADRESS(4,12) )
      GAI_FILES(GAIFON)%NAME=MOTCAR( ADRESS(4,16) )
      GAI_FILES(GAIRES)%FMT = MOTCAR( ADRESS(4,31) )(1:8)
      CALL MAJUS(GAI_FILES(GAIRES)%FMT)
!     RESULT FILE FORMAT FOR PREVIOUS SEDIMENTOLOGICAL
!     COMPUTATION...
      GAI_FILES(GAIPRE)%FMT = MOTCAR( ADRESS(4,34) )(1:8)
      CALL MAJUS(GAI_FILES(GAIPRE)%FMT)
!     REFERENCE FILE FORMAT
      GAI_FILES(GAIREF)%FMT = MOTCAR( ADRESS(4,33) )(1:8)
      CALL MAJUS(GAI_FILES(22)%FMT)
!     HYDRODYNAMIC FILE FORMAT
!     WAVE FILE FORMAT (COUPLING WITH TOMAWAC)
      GAI_FILES(GAICOU)%FMT = MOTCAR( ADRESS(4,35) )(1:8)
      CALL MAJUS(GAI_FILES(GAICOU)%FMT)
      GAI_FILES(GAIREF)%NAME=MOTCAR( ADRESS(4,22) )
!     NESTOR STEERING FILE
      GAI_FILES(SINACT)%NAME = MOTCAR( ADRESS(4,27) )  !  Nestor
      GAI_FILES(SINPOL)%NAME = MOTCAR( ADRESS(4,40) )  !  Nestor
      GAI_FILES(SINREF)%NAME = MOTCAR( ADRESS(4,41) )  !  Nestor
      GAI_FILES(SINRST)%NAME = MOTCAR( ADRESS(4,64) )  !  Nestor
!     ******           = MOTCAR( ADRESS(4,28) )
!     WAVE FILE
      GAI_FILES(GAICOU)%NAME=MOTCAR( ADRESS(4,30) )
!     SECTIONS
      GAI_FILES(GAISEC)%NAME=MOTCAR( ADRESS(4,36) )
      GAI_FILES(GAISEO)%NAME=MOTCAR( ADRESS(4,37) )
!     FILE FOR LIQUID BOUNDARIES
      GAI_FILES(GAILIQ)%NAME=MOTCAR( ADRESS(4,38) )
!     GEOMETRY FILE FORMAT
      GAI_FILES(GAIGEO)%FMT = MOTCAR( ADRESS(4,39) )(1:8)
      CALL MAJUS(GAI_FILES(GAIGEO)%FMT)
!     CVSM FILES
!     CVSM, But it's not Beautiful
      TEMPVAR  =   MOTCAR(ADRESS(4,60)   ) !cvsm
      CALL LECDON_SPLIT_OUTPUTPOINTS(TEMPVAR,CVSMOUTPUT,CVSM_OUT_FULL) !cvsm
!     C-VSM RESULTS FILE
      GAI_FILES(VSPRES)%NAME=MOTCAR( ADRESS(4,53) ) !cvsm
      GAI_FILES(VSPRES)%FMT =MOTCAR( ADRESS(4,55) )(1:8) !cvsm
      CALL MAJUS(GAI_FILES(VSPRES)%FMT) !cvsm
!     NAMES OF PRIVATE VARIABLES
      N_NAMES_PRIV = MIN(4,DIMENS(4,42))
      IF(N_NAMES_PRIV.GT.0) THEN
        DO K=1,N_NAMES_PRIV
          NAMES_PRIVE(K) = MOTCAR(ADRESS(4,42)+K-1)(1:32)
        ENDDO
      ENDIF
!
!     TYPE OF SEDIMENT AND NSICLA
      NSAND = 0
      NMUD = 0
      NSUSP_TEL=0
      IF(NSICLA.GT.0) THEN
      ALLOCATE(TYPE_SED(NSICLA))
! for the moment the following variables are overdimensioned
      ALLOCATE(NUM_IMUD_ICLA(NSICLA))
      ALLOCATE(NUM_ICLA_IMUD(NSICLA))
      ALLOCATE(NUM_ISAND_ICLA(NSICLA))
      ALLOCATE(NUM_ICLA_ISAND(NSICLA))
      ALLOCATE(NUM_ISUSP_ICLA(NSICLA))
        DO K=1,NSICLA
          NUM_ICLA_ISAND(K)=0
          NUM_ISAND_ICLA(K)=0
          NUM_ICLA_IMUD(K)=0
          NUM_IMUD_ICLA(K)=0
          NUM_ISUSP_ICLA(K)=0
          TYPE_SED(K) = MOTCAR(ADRESS(4,59)+K-1)(1:3)
          IF(TYPE_SED(K).EQ.'CO') THEN
            SEDCO(K)=.TRUE.
            NMUD=NMUD+1
            NSUSP_TEL=NSUSP_TEL+1
            NUM_ISUSP_ICLA(NSUSP_TEL)=K
            NUM_IMUD_ICLA(NMUD)=K
            NUM_ICLA_IMUD(K)=NMUD
!           Warning: If cohesif sediment forcing suspension
            SUSP = .TRUE.
!
          ELSEIF(TYPE_SED(K).EQ.'NCO') THEN
            SEDCO(K)=.FALSE.
            NSAND=NSAND+1
            NUM_ISAND_ICLA(NSAND)=K
            NUM_ICLA_ISAND(K)=NSAND
            IF(SUSP_SAND.EQV..TRUE.) THEN
              NSUSP_TEL=NSUSP_TEL+1
              NUM_ISUSP_ICLA(NSUSP_TEL)=K
            ENDIF
            IF(SUSP_SAND.EQV..TRUE.) SUSP = .TRUE.
          ELSE
            WRITE(LU,*)'CHECK TYPE OF SEDIMENT'
            WRITE(LU,*)'POSSIBLE CHOICES ARE: CO AND NCO'
            CALL PLANTE(1)
            STOP
          ENDIF
        ENDDO
      ENDIF
!
!     PRESCRIBED SOLID DISCHARGE DISTRIBUTION
!
      ALLOCATE(RATIO_DEBIMP(NSAND))
      NPROP = DIMENS(2,8)
      IF (NPROP.GT.0) THEN
        DO I = 1, NPROP
          RATIO_DEBIMP(I) = MOTREA(ADRESS(2,8)+I-1)
        ENDDO
      ENDIF
!
      IF(NSAND.GT.0) THEN
        IF (.NOT.(CHARR.OR.SUSP)) THEN
          WRITE(LU,*) "YOU HAVE NON COHESIVE SEDIMENT BUT NO "//
     &                "BED LOAD OR SUSPENSION"
          WRITE(LU,*) "ADD BED LOAD FOR ALL SANDS = YES "
          WRITE(LU,*) "OR SUSPENSION FOR ALL SANDS = YES "
          WRITE(LU,*) "IN YOUR STEERING FILE"
          CALL PLANTE(1)
        ENDIF
      ENDIF
!
      IF(NMUD.EQ.0) THEN
        DO I=1,NOMBLAY
          CONC_MUD0(I)=0.D0
        ENDDO
      ENDIF
!
      IF(NSAND.EQ.0.AND.ICR.NE.0)THEN
        WRITE(LU,*)'WITH NO SAND:'
        WRITE(LU,*)'SKIN FRICTION CORRECTION MUST BE EQUAL TO 0'
        CALL PLANTE(1)
        STOP
      ENDIF
!
      IF(NUMSTRAT.GE.2.AND.NSAND.GT.0) THEN
        IF(DIMENS(2,5).NE.NOMBLAY.AND.DIMENS(2,5).GE.2) THEN ! IF ZERO VALUE of POROSITY : DEFAULT, IF ONE VALUE : FILLS ALL LAYERS
          WRITE(LU,*)'WARNING:'
          WRITE(LU,*)'NOT ENOUGH VALUES OF POROSITY GIVEN'
          WRITE(LU,*)'VALUES WILL BE COMPLETED WITH THE LAST GIVEN'
        ENDIF
      ENDIF
!
!     FILLING THE POROSITY OF STRATUM
      IF(TROUVE(2,5).GE.1.AND.DIMENS(2,5).EQ.NOMBLAY) THEN ! NOMBLAY
        DO K=1,NOMBLAY
          XKV0(K) = MOTREA(ADRESS(2,5)+K-1)
        ENDDO
      ELSEIF(TROUVE(2,5).GE.1.AND.
     &       DIMENS(2,5).LT.NOMBLAY.AND.DIMENS(2,5).GT.0) THEN
!       READING WHAT HAS BEEN GIVEN
        DO K=1,DIMENS(2,5)
          XKV0(K) = MOTREA(ADRESS(2,5)+K-1)
        ENDDO
!       COMPLETING WITH THE LAST GIVEN
        DO K=DIMENS(2,5)+1,NOMBLAY
          XKV0(K) = MOTREA(ADRESS(2,5)+DIMENS(2,5)-1)
        ENDDO
      ENDIF
!
!
      N_NAMES_ADVAR = DIMENS(4,70)
      NADVAR = MAX( NADVAR,N_NAMES_ADVAR ) ! WARNING HERE ?
      IF(NADVAR.GT.0) THEN
        DO I=1,NADVAR
          WRITE(CHAR2,'(I2)') I
          NAMES_ADVAR(I) =  'DERIVATIVE '//ADJUSTL(CHAR2)//'   '
     &                   // '??              '
        ENDDO
      ENDIF
      IF(N_NAMES_ADVAR.GT.0) THEN
        DO K=1,N_NAMES_ADVAR
          NAMES_ADVAR(K) = MOTCAR(ADRESS(4,70)+K-1)(1:32)
        ENDDO
      ENDIF
!
!     FLUXLINEFILE
      GAI_FILES(GAIFLX)%NAME=MOTCAR( ADRESS(4,69) )
!     C-VSM RESULTS FILE
      GAI_FILES(VSPRES)%NAME=MOTCAR( ADRESS(4,53) )
      GAI_FILES(VSPRES)%FMT =MOTCAR( ADRESS(4,55) )(1:8)
      CALL MAJUS(GAI_FILES(VSPRES)%FMT)
!
      WRITE(LU,102)
102   FORMAT(1X,/,19X, '********************************************',/,
     &            19X, '*               LECDON:                    *',/,
     &            19X, '*        AFTER CALLING DAMOCLES            *',/,
     &            19X, '*        CHECKING OF DATA  READ            *',/,
     &            19X, '*         IN THE STEERING FILE             *',/,
     &            19X, '********************************************',/)
!
!-----------------------------------------------------------------------
!
! LOGICALS FOR OUTPUT VARIABLES
!-----------------------------------------------------------------------
!
      NOMBLAY=MAX(NOMBLAY,NCOUCH_TASS)
      NCOUCH_TASS=NOMBLAY
!
      CALL NOMVAR_GAIA
!
      CALL SORTIE(SORTIS , MNEMO , MAXVAR , SORLEO )
      CALL SORTIE(VARIM  , MNEMO , MAXVAR , SORIMP )
!
      DO I = 1, 4
        IF ((NPRIV.LT.I).AND.(SORLEO(I+NVAR_PRIV).OR.
     &       SORIMP(I+NVAR_PRIV))) THEN
          NPRIV=MAX(NPRIV,I)
        ENDIF
      ENDDO
!
!-----------------------------------------------------------------------
!     DISABLE OUTPUT OF BEDLOAD SEDIMENT FLOWRATES QS_C, QSCX, QSCY
!     IN CASE THERE IS NO BEDLOAD
!     THE SEDIMENT FLOWRATES SPECIFIC TO SUSPENSION ARE NOT IN THE
!     OUTPUT VARIABLES SO THERE IS NO NEED TO DISABLE THEM
!
      IF(.NOT.CHARR) THEN
        SORLEO(NVAR_QS_C+1)=.FALSE.
        SORLEO(NVAR_QSXC+1)=.FALSE.
        SORLEO(NVAR_QSYC+1)=.FALSE.
        SORIMP(NVAR_QS_C+1)=.FALSE.
        SORIMP(NVAR_QSXC+1)=.FALSE.
        SORIMP(NVAR_QSYC+1)=.FALSE.
      ENDIF
!
!-----------------------------------------------------------------------
!     FORCE THE VARIABLES NECESSARY FOR RESTARTING TO BE IN THE OUTPUT
!     WITH MUD THE MUD CONCENTRATION, TOCE_MUD, PARTHENIADES AND
!     TRANS_MASS (IF BED_MODEL.EQ.2) ARE REQUIRED
!     COEFFICIENT IN THE LAYERS
      IF(NMUD.GT.0) THEN
        DO I=1,NOMBLAY
          SORLEO(NVAR_LAYCONC+I)=.TRUE.
          SORLEO(NVAR_TOCEMUD+I)=.TRUE.
          SORLEO(NVAR_PARTHE+I)=.TRUE.
          IF(BED_MODEL.EQ.2) THEN
            SORLEO(NVAR_MTRANS+I)=.TRUE.
          ENDIF
        ENDDO
      ENDIF
!
!-----------------------------------------------------------------------
!
!     INITIALISES MSK (MASKING VARIABLE)
!     FOR NOW MASKING IS ONLY DONE FOR ONE 'OPTION FOR THE TREATMENT
!     OF TIDAL FLATS'. SHOULD BE OFFERED AS AN OPTION FOR THE USER TO
!     CREATE ISLANDS IN THE FUTURE
      MSK = .FALSE.
      IF (.NOT.BANDEC) OPTBAN=0
      IF (OPTBAN.EQ.2) MSK = .TRUE.
!
!-----------------------------------------------------------------------
!
!     CHECKS WHETHER THERE IS A REFERENCE FILE
!
      IF (VALID.AND.GAI_FILES(GAIREF)%NAME.EQ.' ') THEN
          VALID=.FALSE.
        WRITE(LU,71)
        WRITE(LU,*)
71      FORMAT(/,1X,'VALIDATION IS NOT POSSIBLE :  ',/
     &          ,1X,'NO REFERENCE FILE  !                 ')
        CALL PLANTE(1)
        STOP
      ENDIF
!
!MGDL
!     CHECKS THE NUMBER OF NSICLA
!      IF(NSICLA.GT.10) THEN
      IF(NSICLA.GT.NSICLM) THEN
        WRITE(LU,81) NSICLM
        WRITE(LU,*)
81      FORMAT(/,1X,'THE MAXIMUM NUMBER OF SEDIMENT CLASSES IS', I2)
        CALL PLANTE(1)
        STOP
      ENDIF
!     CHECKS THE SUM OF INITIAL AVAI
      SUMAVAI = 0.D0
      DO I=1,NSICLA
      SUMAVAI = SUMAVAI + AVA0(I)
      ENDDO
      IF(ABS(SUMAVAI-1).GE.1.D-8) THEN
        WRITE(LU,91)
91      FORMAT(/,1X,'SUM OF SEDIMENT FRACTIONS IS NOT 1  ')
        WRITE(LU,*)
        WRITE(LU,*) 'IT IS EQUAL TO', SUMAVAI
        CALL PLANTE(1)
        STOP
      ENDIF
!
!     CHECKS THAT THE BEDLOAD TRANSPORT FORMULATION AND THE HIDING
!     FACTOR FORMULATION CAN GO TOGETHER
!
      IF ((HIDFAC.EQ.3.AND.ICF.NE.6).OR.
     &    (HIDFAC.EQ.1.AND.ICF.NE.1).OR.
     &    (HIDFAC.EQ.2.AND.ICF.NE.1)) THEN
        WRITE(LU,111)
        WRITE(LU,*)
111     FORMAT(/,1X,'CHOICE OF TRANSPORT FORMULA AND HIDING FACTOR FORMU
     &LATION NOT ALLOWED ')
        CALL PLANTE(1)
        STOP
      ENDIF
!
!     MORPHOLOGICAL FACTOR
      IF( ((ABS(MOFAC - 1.D0).GT.1E-8) .OR.
     &    (ABS(MOFAC_BED - 1.D0).GT.1E-8))
     &  .AND.BED_MODEL.NE.1)THEN
        WRITE(LU,*)'MORPHOLOGICAL FACTOR IS NOT RECOMMANDED
     &              WITH BED MODEL AND CONSOLIDATION
     &              ->DISTORSION OF TIME'
        CALL PLANTE(1)
        STOP
      ENDIF
!
!     COMPUTATION CONTINUED
!
      IF(DEBU) THEN
        IF(GAI_FILES(GAIPRE)%NAME(1:1).EQ.' ') THEN
          WRITE(LU,313)
313       FORMAT(/,1X,'COMPUTATION CONTINUED:',/,
     &             1X,'PREVIOUS SEDIMENTOLOGICAL FILE MISSING')
          CALL PLANTE(1)
          STOP
        ENDIF
      ELSE
        IF(GAI_FILES(GAIPRE)%NAME(1:1).NE.' ') THEN
          GAI_FILES(GAIPRE)%NAME(1:1)=' '
          WRITE(LU,213)
213       FORMAT(/,1X,'NO COMPUTATION CONTINUED:',/,
     &             1X,'PREVIOUS SEDIMENTOLOGICAL FILE IGNORED')
        ENDIF
      ENDIF
!
! METHODS NOT CODED UP FOR SUSPENSION
! -------------------------------------------
!
      IF(.NOT.HOULE) GAI_FILES(GAICOU)%NAME(1:1)=' '
      IF(HOULE.AND.CHARR) THEN
        IF(ICF.NE.4.AND.ICF.NE.5.AND.ICF.NE.8.AND.
     &     ICF.NE.9.AND.ICF.NE.0) THEN
          WRITE(LU,1304) ICF
1304      FORMAT(' TRANSPORT FORMULA',1I3,1X,
     &       'DOES NOT TAKE WAVES INTO ACCOUNT,',/,1X,
     &       'TRY 4, 5, 8 OR 9')
          CALL PLANTE(1)
          STOP
        ENDIF
      ENDIF
!
! BEDLOAD AND SUSPENDED TRANSPORT COUPLING
! ---------------------------------
!
      IF((ICF==30.OR.ICF==3.OR.ICF==9).AND.SUSP.AND.CHARR) THEN
        WRITE(LU,1302) ICF
        CALL PLANTE(1)
        STOP
      ENDIF
1302  FORMAT('FOR THE FORMULA',1I3,/,1X,
     &       'THE SUSPENSION TERM IS CALCULATED TWICE,'
     &      ,' WITH TOTAL LOAD FORMULA AND SUSPENSION ')
!
!
      IF(ICQ.EQ.2.AND.(.NOT.CHARR)) THEN
        WRITE(LU,1402) ICQ
1402  FORMAT('FOR THE BIJKER REFERENCE CONCENTRATION',1I3,/,1X,
     &       'BEDLOAD MUST BE COMPUTED, CHOOSE:',/,1X,
     &       'BEDLOAD = YES')
        CALL PLANTE(1)
        STOP
      ENDIF
!
!     CHECKS CONSISTENCY OF BEDLOAD LAWS
!
!     SOULSBY SLOPE EFFECT : REQUIRES A THRESHOLD FORMULA
!
      IF(SLOPEFF.EQ.2) THEN
        IF(ICF.NE.1.AND.ICF.NE.5.AND.ICF.NE.6.AND.ICF.NE.7.AND.
     &       ICF.NE.9.AND.ICF.NE.10.AND.ICF.NE.0) THEN
          WRITE(LU,1404) ICF
 1404     FORMAT('BED-LOAD TRANSPORT FORMULA, HERE ICF=',1I3,/,1X,
     &       'MUST HAVE A THRESHOLD',/,1X,
     &       'IF FORMULA FOR SLOPE EFFECT=2 (SOULSBY)',/,1X,
     &       '(TRY: 1,5,6,7,9,10,0)')
          CALL PLANTE(1)
        ENDIF
      ENDIF

!
! V6P0 : COHERENCE IF CONSOLIDATION MODEL IS USED
! CSF_VASE STEM FROM THE FIRST LAYER OF THE MULTI-LAYER MODEL
! +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
! Mots cles supprimes vitesse critique d'erosion et concentration du lit
! si premiere couche est  vide cela n'est pas correct
      IF(NOMBLAY.GT.NLAYMAX) THEN
        WRITE (LU,*) 'NUMBER OF BED LOAD MODEL LAYERS LARGER THAN '
        WRITE (LU,*) 'THE MAXIMUM PROGRAMMED VALUE OF ', NLAYMAX
        CALL PLANTE(1)
        STOP
      ENDIF
!      IF(NOMBLAY.LT.2) THEN
!        WRITE (LU,*) 'BEWARE: NUMBER OF BED LOAD MODEL LAYERS'
!        WRITE (LU,*) '======= LOWER THAN THE DEFAULT VALUE OF 2'
!      ENDIF
!
!     ERROR MESSAGE FOR BED MODEL CHOISE
!
      IF(BED_MODEL.EQ.3) THEN
        WRITE(LU,*)'ONLY BED_MODEL = 1 OR 2 ARE AVAILABLE'
        WRITE(LU,*)'BED_MODEL 3: STILL TO DO'
        CALL PLANTE(1)
        STOP
      ENDIF
!
!----------------------------------------------------------------
!
      RETURN
      END
