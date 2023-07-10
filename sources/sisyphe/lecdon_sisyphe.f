!                   *************************
                    SUBROUTINE LECDON_SISYPHE
!                   *************************
!
     &(MOTCAR,FILE_DESC,PATH,NCAR,CODE,CAS_FILE,DICO_FILE)
!
!***********************************************************************
! SISYPHE   V7P2
!***********************************************************************
!
!brief    READS THE STEERING FILE BY CALL TO DAMOCLES.
!
!history  C.VILLARET + JMH (EDF-LNHE)
!+        02/05/2012
!+        V6P2
!+  File for liquid boundaries added
!+
!
!history  JWI
!+        31/05/2012
!+        V6P2
!+  added one increment to include wave orbital velocities
!+  (SORLEO(I+28+(NOMBLAY+4)*NSICLA+NOMBLAY).OR.
!
!history  PAT (LNHE)
!+        18/06/2012
!+        V6P2
!+   updated version with HRW's development
!
!history  Pablo Tassi PAT (EDF-LNHE)
!+        12/02/2013
!+        V6P3
!+ Preparing for the use of a higher NSICLM value
!+ (by Rebekka Kopmann)
!
!history  Pablo Tassi PAT (EDF-LNHE)
!+        12/02/2013
!+        V6P3
!+ Settling lag: determines choice between Rouse and Miles concentration profile
!+ (by Michiel Knaapen HRW)
!
!history  J-M HERVOUET (EDF LAB, LNHE)
!+        18/05/2015
!+        V7P1
!+  Adding CHECK_MESH for the keyword 'CHECKING THE MESH'
!
!history  R. KOPMANN (BAW)
!+        13/07/2016
!+        V7P2
!+        Integrating liquid boundary file for QS
!
!history  S.E. BOURBAN (HRW)
!+        20/06/2016
!+        V7P2
!+   Now taking into account names of differentiators given by user.
!
!history  R.KOPMANN (BAW)
!+        07/12/2017
!+        V7P2
!+   VSPRES results file for CVSM handled by CAS-file.
!
!history  B.GLANDER (BAW)
!+        06/12/2018
!+        V7P2
!+  NEW VARIABLE: ZRL  REFERENCE LEVEL FOR NESTOR, CHANGE OF SORLEO, SORIMP
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| FILE_DESC      |<--| STORES STRINGS 'SUBMIT' OF DICTIONARY
!| MOTCAR         |<--| VALUES OF KEY-WORDS OF TYPE CHARACTER
!| NCAR           |-->| NUMBER OF LETTERS IN STRING PATH
!| PATH           |-->| FULL PATH TO CODE DICTIONARY
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE DECLARATIONS_SPECIAL
      USE DECLARATIONS_TELEMAC
      USE DECLARATIONS_SISYPHE
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)               :: NCAR
      CHARACTER(LEN=24), INTENT(IN)     :: CODE
      CHARACTER(LEN=PATH_LEN), INTENT(IN)    :: PATH
      CHARACTER(LEN=PATH_LEN), INTENT(INOUT)      :: MOTCAR(MAXKEYWORD)
      CHARACTER(LEN=PATH_LEN), INTENT(INOUT) :: FILE_DESC(4,MAXKEYWORD)
!     API
      CHARACTER(LEN=PATH_LEN), INTENT(IN)    :: CAS_FILE
      CHARACTER(LEN=PATH_LEN), INTENT(IN)    :: DICO_FILE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
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

      CHARACTER(LEN=PATH_LEN) TEMPVAR
      INTEGER :: ID_DICO, ID_CAS
!
!-----------------------------------------------------------------------
!
      CHARACTER(LEN=2) CHAR2
!
!-----------------------------------------------------------------------
!
! INITIALISES THE VARIABLES FOR DAMOCLES CALL :
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
        NOM_DIC=PATH(1:NCAR)//'SISDICO'
        NOM_CAS=PATH(1:NCAR)//'SISCAS'
!
      ELSE
!
        NOM_DIC='SISDICO'
        NOM_CAS='SISCAS'
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
      CALL READ_SUBMIT(SIS_FILES,MAXLU_SIS,FILE_DESC,MAXKEYWORD)
!
!-----------------------------------------------------------------------
!
!     RETRIEVES FILES NUMBERS IN TELEMAC-2D FORTRAN PARAMETERS
!     AT THIS LEVEL LOGICAL UNITS ARE EQUAL TO THE FILE NUMBER
!
      DO I=1,MAXLU_SIS
        IF(SIS_FILES(I)%TELNAME.EQ.'SISHYD') THEN
          SISHYD=I
        ELSEIF(SIS_FILES(I)%TELNAME.EQ.'SISGEO') THEN
          SISGEO=I
        ELSEIF(SIS_FILES(I)%TELNAME.EQ.'SISCLI') THEN
          SISCLI=I
        ELSEIF(SIS_FILES(I)%TELNAME.EQ.'SISPRE') THEN
          SISPRE=I
        ELSEIF(SIS_FILES(I)%TELNAME.EQ.'SISRES') THEN
          SISRES=I
        ELSEIF(SIS_FILES(I)%TELNAME.EQ.'SISREF') THEN
          SISREF=I
        ELSEIF(SIS_FILES(I)%TELNAME.EQ.'SISCOU') THEN
          SISCOU=I
        ELSEIF(SIS_FILES(I)%TELNAME.EQ.'SISFON') THEN
          SISFON=I
        ELSEIF(SIS_FILES(I)%TELNAME.EQ.'SISSEC') THEN
          SISSEC=I
        ELSEIF(SIS_FILES(I)%TELNAME.EQ.'SISSEO') THEN
          SISSEO=I
        ELSEIF(SIS_FILES(I)%TELNAME.EQ.'SISLIQ') THEN
          SISLIQ=I
        ELSEIF(SIS_FILES(I)%TELNAME.EQ.'SISFLX') THEN
          SISFLX=I
!       === NESTOR FILES
        ELSEIF(SIS_FILES(I)%TELNAME.EQ.'SINACT') THEN
          SINACT=I
        ELSEIF(SIS_FILES(I)%TELNAME.EQ.'SINPOL') THEN
          SINPOL=I
        ELSEIF(SIS_FILES(I)%TELNAME.EQ.'SINREF') THEN
          SINREF=I
        ELSEIF(SIS_FILES(I)%TELNAME.EQ.'SINRST') THEN
          SINRST=I
!       ===
        ELSEIF(SIS_FILES(I)%TELNAME.EQ.'VSPRES') THEN
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
      ALLOCATE(CBOR_CLASSE(NSICLM*MAXFRO))
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
      IELMH_SIS = 11 ! VARIABLES ASSOCIATED WITH WATER DEPTH
      IELMU_SIS = 11 ! VARIABLES ASSOCIATED WITH VELOCITIES
!
!     FOR NOW PRINTOUTS START AT ZERO
!
      PTINIG = 0
      PTINIL = 0
!
!     NON-EQUILIBIRUM BEDLOAD
!
      LOADMETH = 0
!
!     ICM           = MOTINT( ADRESS(1,  1) )
      ICF           = MOTINT( ADRESS(1,  2) )
      NPAS          = MOTINT( ADRESS(1,  3) )
      NMAREE        = MOTINT( ADRESS(1,  4) )
!     N1            = MOTINT( ADRESS(1,  5) )

      LEOPR         = MOTINT( ADRESS(1,  6) )
      LISPR         = MOTINT( ADRESS(1,  7) )
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
      SLVSED%SLV    = MOTINT( ADRESS(1, 17) )
      SLVSED%KRYLOV = MOTINT( ADRESS(1, 18) )
      SLVSED%PRECON = MOTINT( ADRESS(1, 19) )
      SLVSED%NITMAX = MOTINT( ADRESS(1, 20) )
      CHOIX         = MOTINT( ADRESS(1, 21) )
      DIRFLU        = MOTINT( ADRESS(1, 22) )
      NPRIV         = MOTINT( ADRESS(1, 23) )
      NADVAR    = MOTINT( ADRESS(1,30) )
!     NUMBER OF DIRECTIONS FOR DIFFERENTIATED VARIABLES
      AD_NUMOFDIR  = MOTINT( ADRESS(1,59) )
!
!     NCSIZE        = MOTINT( ADRESS(1, 24) )
!     NUMBER OF PROCESSORS (ALREADY GIVEN IN INIT_FILES2;
!     MUST BE THE SAME, BUT WHEN USING COUPLED MODELS IT CAN
!     WRONGLY BE DIFFERENT)
      IF(NCSIZE.NE.MOTINT(ADRESS(1,24))) THEN
        WRITE(LU,*) 'DIFFERENT NUMBER OF PARALLEL PROCESSORS:'
        WRITE(LU,*) 'DECLARED BEFORE (CASE OF COUPLING ?):',NCSIZE
        WRITE(LU,*) 'SISYPHE :',MOTINT(ADRESS(1,24))
        WRITE(LU,*) 'VALUE ',NCSIZE,' IS KEPT'
      ENDIF
      RESOL         = MOTINT( ADRESS(1, 25) )
      SLVTRA%SLV    = MOTINT( ADRESS(1, 26) )
      SLVTRA%KRYLOV = MOTINT( ADRESS(1, 27) )
      SLVTRA%PRECON = MOTINT( ADRESS(1, 28) )
      SLVTRA%NITMAX = MOTINT( ADRESS(1, 29) )
      OPTDIF        = MOTINT( ADRESS(1, 31) )
      OPTSUP        = MOTINT( ADRESS(1, 32) )
      PRODUC        = MOTINT( ADRESS(1, 33) )
      OPTASS        = MOTINT( ADRESS(1, 34) )
      OPDTRA        = MOTINT( ADRESS(1, 35) )
      KFROT         = MOTINT( ADRESS(1, 37) )
      NCONDIS       = MOTINT( ADRESS(1, 38) )
      SLOPEFF       = MOTINT( ADRESS(1, 39) )
      DEVIA         = MOTINT( ADRESS(1, 40) )
      NOMBLAY       = MOTINT( ADRESS(1,251) )
      NSICLA        = MOTINT( ADRESS(1,252) )
      HIDFAC        = MOTINT( ADRESS(1,253) )
      ICQ           = MOTINT( ADRESS(1, 41) )
!     CONTROL SECTIONS
      NCP=DIMENS(1,42)
      ALLOCATE(CTRLSC(NCP),STAT=ERR)
      CALL CHECK_ALLOCATE(ERR, 'CTRLSC')
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
!     CVSM MODEL
      PRO_MAX_MAX =   MAX(NSICLA*4+4,MOTINT(ADRESS(1,49) ))
      CVSMPPERIOD =   MOTINT(ADRESS(1,50)   )
      ALT_MODEL   =   MOTINT(ADRESS(1,52)   )
      VSMTYPE     =   MOTINT(ADRESS(1,53)   )
!
!     MAXIMUM NUMBER OF ITERATIONS FOR ADVECTION SCHEMES
!
      MAXADV = MOTINT(ADRESS(1,54))
!
!     SCHEME OPTION FOR ADVECTION
!
      OPTADV=1
      IF(RESOL.EQ.1) THEN
!       CHARACTERISTICS
!       OPTADV=OPTCHA (IN TELEMAC-2D, NOT IN SISYPHE YET)
      ELSEIF(RESOL.EQ.2) THEN
!       SUPG
        OPTADV=OPTSUP
      ELSEIF(RESOL.EQ.5) THEN
!       PSI SCHEME
!       OPTADV_VI=OPTPSI (IN TELEMAC-2D, NOT IN SISYPHE YET)
      ENDIF
!     SCHEME OPTION FOR ADVECTION HAS PRIORITY WHEN PRESENT
      IF(TROUVE(1,55).EQ.2) THEN
        OPTADV = MOTINT(ADRESS(1,55))
      ENDIF
      NCO_DIST = MOTINT( ADRESS(1,56) )
      NSP_DIST = MOTINT( ADRESS(1,57) )
!
! ############### !
! REAL KEYWORDS   !
! ############### !
!
      ! NON-EQUILIBRIRUM BEDLOAD
      ! ------------------------
      LS0         = 1.D0
!
      RC          = MOTREA( ADRESS(2,  1) )
      XMVE        = MOTREA( ADRESS(2,  2) )
      XMVS        = MOTREA( ADRESS(2,  3) )
      DO K=1,NSICLA
        FDM(K)   = MOTREA( ADRESS(2,  4) + K-1 )
      ENDDO
!     IF OLD NAME OF KEYWORD 28 HAS BEEN FOUND
      IF(TROUVE(2,28).EQ.2) THEN
        DO K=1,NSICLA
          FDM(K) = MOTREA( ADRESS(2,28) + K-1 )
        ENDDO
      ENDIF
      XKV         = MOTREA( ADRESS(2,  5) )
!     SHIELDS NUMBERS
      DO K=1,DIMENS(2,6)
        AC(K)   = MOTREA( ADRESS(2, 6) + K-1 )
      ENDDO
!     IF ALL SHIELDS NUMBERS ARE NOT GIVEN, THE LATEST
!     ONE IS TAKEN FOR THE FOLLOWING
!     FOR EXAMPLE IF ONLY ONE IS GIVEN, ALL WILL HAVE
!     THE SAME VALUE
      IF(DIMENS(2,6).LT.NSICLA) THEN
        DO K=DIMENS(2,6)+1,NSICLA
          AC(K) = MOTREA( ADRESS(2, 6)+MAX(DIMENS(2,6),1)-1 )
        ENDDO
      ENDIF
      SFON        = MOTREA( ADRESS(2,  7) )
      GRAV        = MOTREA( ADRESS(2,  8) )
      ZERO        = MOTREA( ADRESS(2,  9) )
      SLVSED%ZERO = ZERO
      VCE         = MOTREA( ADRESS(2, 10) )
      HMIN        = MOTREA( ADRESS(2, 11) )
      DELT        = MOTREA( ADRESS(2, 12) )
      TPREC       = MOTREA( ADRESS(2, 13) )
      PMAREE      = MOTREA( ADRESS(2, 14) )
      TETA        = MOTREA( ADRESS(2, 15) )
      BETA        = MOTREA( ADRESS(2, 16) )
      SLVSED%EPS  = MOTREA( ADRESS(2, 17) )
      TETA_SUSP   = MOTREA( ADRESS(2, 18) )
      XKX         = MOTREA( ADRESS(2, 19) )
      XKY         = MOTREA( ADRESS(2, 20) )
      SLVTRA%EPS  = MOTREA( ADRESS(2, 21) )
!     SETTLING VELOCITIES (SAME TREATMENT THAN SHIELDS NUMBERS)
      DO K=1,DIMENS(2,22)
        XWC(K)   = MOTREA( ADRESS(2, 22) + K-1 )
      ENDDO
      IF(DIMENS(2,22).LT.NSICLA) THEN
        DO K=DIMENS(2,22)+1,NSICLA
          XWC(K) = MOTREA( ADRESS(2, 22)+DIMENS(2,22)-1 )
        ENDDO
      ENDIF
      CRIT_CFD    = MOTREA( ADRESS(2, 23) )
      KSPRATIO    = MOTREA( ADRESS(2, 24) )
      PHISED      = MOTREA( ADRESS(2, 25) )
      BETA2       = MOTREA( ADRESS(2, 26) )
      BIJK        = MOTREA( ADRESS(2, 27) )
!
!     INITIAL CONCENTRATIONS
!     ++++++++++++++++++++++
!
      DO K=1,NSICLM
!       DEFAULT VALUE
        CS0(K) = 0.D0
      ENDDO
      DO K=1,NSICLA
        CS0(K)=MOTREA( ADRESS(2,30) + K-1 )
      ENDDO
      DO K=1,10*MAXFRO
        CBOR_CLASSE(K)=0.D0
      ENDDO
      IF(DIMENS(2,31).GT.0) THEN
        DO K=1,DIMENS(2,31)
          CBOR_CLASSE(K)=MOTREA( ADRESS(2,31) + K-1 )
        ENDDO
      ENDIF
!
!     COHESIVE SEDIMENT
!     +++++++++++++++++
!
      NCOUCH_TASS = MOTINT( ADRESS(1,45)   )
!
!     DEFAULT VALUES, PREVIOUSLY IN THE DICTIONARY
!     EVEN IF THERE ARE NOT 10 LAYERS
      CONC_VASE(1)  =  50.D0
      CONC_VASE(2)  = 100.D0
      CONC_VASE(3)  = 150.D0
      CONC_VASE(4)  = 200.D0
      CONC_VASE(5)  = 250.D0
      CONC_VASE(6)  = 300.D0
      CONC_VASE(7)  = 350.D0
      CONC_VASE(8)  = 400.D0
      CONC_VASE(9)  = 450.D0
      CONC_VASE(10) = 500.D0
!
      IF(DIMENS(2,32).GT.0) THEN
        DO K=1,DIMENS(2,32)
          CONC_VASE(K)=MOTREA( ADRESS(2,32) + K-1 )
        ENDDO
      ENDIF
!
!     OBSOLETE KEY WORD : CSF_VASE = MOTREA( ADRESS(2, 29) )
!
!      CSF_VASE = CONC_VASE(1)/XMVS
!
      IF(DIMENS(2,34).GT.0) THEN
        DO K=1,DIMENS(2,34)
          TOCE_VASE(K)=MOTREA( ADRESS(2,34) + K-1 )
        ENDDO
      ENDIF
!
!     KRONE AND PARTHENIADES EROSION AND DEPOSITION LAW
!
!     OBSOLETE KEY WORD : VITCE= MOTREA( ADRESS(2,35))
!
      VITCE = SQRT(TOCE_VASE(1)/XMVE)
      VITCD= MOTREA( ADRESS(2,36))
!     PARTHENIADES WITH CONVERSION TO M/S
      PARTHENIADES = MOTREA( ADRESS(2,37))/XMVS
!
!     CONSOLIDATION MODEL
!
      TASS = MOTLOG(ADRESS(3,23))
      ITASS  =   MOTINT(ADRESS(1,48)   )
!
!     MULTILAYER MODEL (WALTHER, 2008)
!     ITASS = 1
!
!     DEFAULT VALUES, PREVIOUSLY IN THE DICTIONARY
!     EVEN IF THERE ARE NOT 10LAYERS
      TRANS_MASS(1)  = 5.D-5
      TRANS_MASS(2)  = 4.5D-5
      TRANS_MASS(3)  = 4.D-5
      TRANS_MASS(4)  = 3.5D-5
      TRANS_MASS(5)  = 3.D-5
      TRANS_MASS(6)  = 2.5D-5
      TRANS_MASS(7)  = 2.D-5
      TRANS_MASS(8)  = 1.5D-5
      TRANS_MASS(9)  = 1.D-5
      TRANS_MASS(10) = 0.D0
!
      IF(DIMENS(2,33).GT.0) THEN
        DO K=1,DIMENS(2,33)
          TRANS_MASS(K)=MOTREA( ADRESS(2,33) + K-1 )
        ENDDO
      ENDIF
!
! V6P1
! THIEBOT MULTI LAYER MODEL
! ITASS=2
!
      CONC_GEL=MOTREA( ADRESS(2,38))
      COEF_N= MOTREA( ADRESS(2,39))
      CONC_MAX=MOTREA( ADRESS(2,50))
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
!     MINIMUM DEPTH FOR BEDLOAD
!
      HMIN_BEDLOAD=MOTREA(ADRESS(2,52))
!
!     HIDING EXPOSURE MULTI GRAIN MODEL
!
      DO K=1,NSICLA
        HIDI(K)  = MOTREA( ADRESS(2,253) + K-1 )
        IF (TROUVE(2,255).EQ.1) THEN
          FD90(K)= FDM(K)
        ELSE
          FD90(K)= MOTREA( ADRESS(2,255) + K-1 )
        ENDIF
        AVA0(K)  = MOTREA( ADRESS(2,258) + K-1 )
      ENDDO
      ELAY0       = MOTREA( ADRESS(2,259) )
!
! UM: MPM-Factor
      MPM         = MOTREA( ADRESS(2,260) )
! UM: ALPHA-Factor
      ALPHA       = MOTREA( ADRESS(2,261) )
! UM: MOFAC-Factor
      MOFAC       = MOTREA( ADRESS(2,262) )

      ! ################## !
      ! LOGICAL KEYWORDS !
      ! ################## !
! INDEX 99 IS ALREADY USED FOR KEYWORD 'LIST OF FILES'
! INDEX 54 IS ALREADY USED FOR KEYWORD 'DESCRIPTION OF LIBRARIES'
! INDEX 57 IS ALREADY USED FOR KEYWORD 'DEFAULT EXECUTABLE'
      ! SPHERICAL EQUATIONS HARD-CODED
      ! ----------------------------------
      SPHERI       = .FALSE.


      ! COMPUTATION OF FALL VELOCITIES
      ! ------------------------------------------
      CALWC = .FALSE.
      ! IF TROUVE: VELOCITIES ARE USER-DEFINED
      ! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      IF (TROUVE(2, 22).EQ.2) CALWC = .TRUE.
! CV
      ! SHIELDS PARAMETER
      ! ------------------------------------------
      CALAC = .FALSE.
      ! IF TROUVE
      ! ~~~~~~~~~~~~~
      IF (TROUVE(2, 6).EQ.2) CALAC = .TRUE.


      BILMA        = MOTLOG( ADRESS(3,  1) )
      PERMA        = MOTLOG( ADRESS(3,  2) )
      BANDEC       = MOTLOG( ADRESS(3,  3) )
      VALID        = MOTLOG( ADRESS(3,  4) )
!     DTVAR        = MOTLOG( ADRESS(3,  5) )
      LUMPI        = MOTLOG( ADRESS(3,  6) )
      SUSP         = MOTLOG( ADRESS(3,  7) )
      CHARR        = MOTLOG( ADRESS(3,  8) )
      HOULE        = MOTLOG( ADRESS(3, 10) )
      CONST_ALAYER = MOTLOG( ADRESS(3, 11) )
      LCONDIS      = MOTLOG( ADRESS(3, 12) )
      LGRAFED      = MOTLOG( ADRESS(3, 13) )
!     USED TO CHECK SIS_FILES(SISPRE)%NAME
      DEBU         = MOTLOG( ADRESS(3, 14) )
      IMP_INFLOW_C = MOTLOG( ADRESS(3, 15) )
      SECCURRENT   = MOTLOG( ADRESS(3, 16) )
      HAVESECFILE  = MOTLOG( ADRESS(3, 59) )
      IF(CODE(1:9).EQ.'TELEMAC3D') SECCURRENT = .FALSE.
      UNIT         = MOTLOG( ADRESS(3, 17) )
      VF           = MOTLOG( ADRESS(3,253) )
      CORR_CONV    = MOTLOG( ADRESS(3, 18) )
      DO K=1,NSICLA
        SEDCO(K)   = .FALSE.
      ENDDO
      IF(DIMENS(3,19).GT.0) THEN
        DO K=1,DIMENS(3,19)
          SEDCO(K) = MOTLOG( ADRESS(3,19) + K-1 )
        ENDDO
      ENDIF
      SLIDE    = MOTLOG( ADRESS(3, 20) )
      DIFT     = MOTLOG( ADRESS(3, 21) )
      EFFPEN   = MOTLOG( ADRESS(3, 22) )
      IF(.NOT.EFFPEN) THEN
        SLOPEFF=0
        DEVIA=0
      ENDIF
!
      MIXTE=MOTLOG(ADRESS(3,24))
!     COUPLING WITH NESTOR
      NESTOR=MOTLOG(ADRESS(3,25))
!     V6P1
      KSPRED   =MOTLOG(ADRESS(3,26))
!
!     Settling lag: determines choice between Rouse and Miles concentration profile
!     SET_LAG = TRUE : Miles
!             = FALSE: Rouse
!
      SET_LAG  = MOTLOG(ADRESS(3,27) )
!     STATIONARY MODE: calculate sediment transport without updating the bed.
      STAT_MODE  = MOTLOG(ADRESS(3,28) )
!     Checking the mesh
      CHECK_MESH = MOTLOG(ADRESS(3,29) )
!     NEW IMPLEMENTATION FOR CROSS-SECTION
      DOFLUX = MOTLOG(ADRESS(3,61) )
      IF ( CODE(1:7) .NE. 'TELEMAC' ) THEN
!       SYMBOLIC LINEAR SOLVER FOR AD
        AD_SYMBLINSOLV  = MOTLOG( ADRESS(3,30) )
!       RESET TANGENTS ON ENTRY IN LINEAR SOLVER CG FOR AD
        AD_LINSOLV_RESETDERIV  = MOTLOG( ADRESS(3,31) )
!       ALLOW ADDITONAL INTERATIONS IN ITERATIVE LINEAR SOLVERS
!         SHOULD NOT BE USED IN PARALLEL MODE
        AD_LINSOLV_DERIVATIVE_CONVERGENCE  = MOTLOG( ADRESS(3,32) )
      ENDIF
      PARTEL_CONCAT = MOTLOG(ADRESS(3,62))
      IF(NCSIZE.LE.1) PARTEL_CONCAT=.FALSE.
!
! ################################### !
! CHARACTER STRING KEYWORDS           !
! ################################### !
!
      TITCA            = MOTCAR( ADRESS(4, 1) )(1:72)
      SORTIS           = MOTCAR( ADRESS(4, 2) )(1:72)
      VARIM            = MOTCAR( ADRESS(4, 3) )(1:72)
      SIS_FILES(SISGEO)%NAME=MOTCAR( ADRESS(4,6) )
      IF(SIS_FILES(SISGEO)%NAME(1:1).EQ.' ') THEN
        WRITE(LU,*) 'THE FOLLOWING KEYWORD IS MANDATORY:'
        WRITE(LU,*) 'GEOMETRY FILE (FICHIER DE GEOMETRIE)'
        CALL PLANTE(1)
        STOP
      ENDIF
      SIS_FILES(SISCLI)%NAME=MOTCAR( ADRESS(4,9) )
      IF(SIS_FILES(SISCLI)%NAME(1:1).EQ.' ') THEN
        WRITE(LU,*) 'THE FOLLOWING KEYWORD IS MANDATORY:'
        WRITE(LU,*) 'BOUNDARY CONDITIONS FILE '//
     &              '(FICHIER DES CONDITIONS AUX LIMITES)'
        CALL PLANTE(1)
        STOP
      ENDIF
      SIS_FILES(SISHYD)%NAME=MOTCAR( ADRESS(4,29) )
      SIS_FILES(SISPRE)%NAME=MOTCAR( ADRESS(4,11) )
      SIS_FILES(SISRES)%NAME=MOTCAR( ADRESS(4,12) )
      SIS_FILES(SISFON)%NAME=MOTCAR( ADRESS(4,16) )
      SIS_FILES(SISRES)%FMT = MOTCAR( ADRESS(4,31) )(1:8)
      IF(SIS_FILES(SISRES)%NAME(1:1).EQ.' ') THEN
        WRITE(LU,*) 'THE FOLLOWING KEYWORD IS MANDATORY:'
        WRITE(LU,*) 'RESULTS FILE (FICHIER DES RESULTATS)'
        CALL PLANTE(1)
        STOP
      ENDIF
      CALL MAJUS(SIS_FILES(SISRES)%FMT)
!     RESULT FILE FORMAT FOR PREVIOUS SEDIMENTOLOGICAL
!     COMPUTATION...
      SIS_FILES(SISPRE)%FMT = MOTCAR( ADRESS(4,34) )(1:8)
      CALL MAJUS(SIS_FILES(SISPRE)%FMT)
!     REFERENCE FILE FORMAT
      SIS_FILES(SISREF)%FMT = MOTCAR( ADRESS(4,33) )(1:8)
      CALL MAJUS(SIS_FILES(22)%FMT)
!     HYDRODYNAMIC FILE FORMAT
      SIS_FILES(SISHYD)%FMT = MOTCAR( ADRESS(4,32) )(1:8)
      CALL MAJUS(SIS_FILES(SISHYD)%FMT)
!     WAVE FILE FORMAT (COUPLING WITH TOMAWAC)
      SIS_FILES(SISCOU)%FMT = MOTCAR( ADRESS(4,35) )(1:8)
      CALL MAJUS(SIS_FILES(SISCOU)%FMT)
      BINGEOSIS        = MOTCAR( ADRESS(4,18) )(1:3)
      BINHYDSIS        = MOTCAR( ADRESS(4,19) )(1:3)
      BINPRESIS        = MOTCAR( ADRESS(4,20) )(1:3)
      BINRESSIS        = MOTCAR( ADRESS(4,21) )(1:3)
      SIS_FILES(SISREF)%NAME=MOTCAR( ADRESS(4,22) )
      BINREFSIS        = MOTCAR( ADRESS(4,23) )(1:3)
!     NESTOR STEERING FILE
      SIS_FILES(SINACT)%NAME = MOTCAR( ADRESS(4,27) )  !  Nestor
      SIS_FILES(SINPOL)%NAME = MOTCAR( ADRESS(4,40) )  !  Nestor
      SIS_FILES(SINREF)%NAME = MOTCAR( ADRESS(4,41) )  !  Nestor
      SIS_FILES(SINRST)%NAME = MOTCAR( ADRESS(4,64) )  !  Nestor
!     ******           = MOTCAR( ADRESS(4,28) )
!     WAVE FILE
      SIS_FILES(SISCOU)%NAME=MOTCAR( ADRESS(4,30) )
!     SECTIONS
      SIS_FILES(SISSEC)%NAME=MOTCAR( ADRESS(4,36) )
      SIS_FILES(SISSEO)%NAME=MOTCAR( ADRESS(4,37) )
!     FILE FOR LIQUID BOUNDARIES
      SIS_FILES(SISLIQ)%NAME=MOTCAR( ADRESS(4,38) )
!     GEOMETRY FILE FORMAT
      SIS_FILES(SISGEO)%FMT = MOTCAR( ADRESS(4,39) )(1:8)
      CALL MAJUS(SIS_FILES(SISGEO)%FMT)
!     NAMES OF PRIVATE VARIABLES
      N_NAMES_PRIV = MIN(4,DIMENS(4,42))
      IF(N_NAMES_PRIV.GT.0) THEN
        DO K=1,N_NAMES_PRIV
          NAMES_PRIVE(K) = MOTCAR(ADRESS(4,42)+K-1)(1:32)
        ENDDO
      ENDIF
!
!     ADDITIONAL DIFFERENTIATED VARIABLES
!     NAMES OF THE DIFFERENTIATED VARIABLES
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
!     CVSM, But it's not Beautiful
      TEMPVAR  =   MOTCAR(ADRESS(4,51)   )
      CALL LECDON_SPLIT_OUTPUTPOINTS(TEMPVAR,CVSMOUTPUT,CVSM_OUT_FULL)
!     FLUXLINEFILE
      SIS_FILES(SISFLX)%NAME=MOTCAR( ADRESS(4,69) )
!     C-VSM RESULTS FILE
      SIS_FILES(VSPRES)%NAME=MOTCAR( ADRESS(4,53) )
      SIS_FILES(VSPRES)%FMT =MOTCAR( ADRESS(4,55) )(1:8)
      CALL MAJUS(SIS_FILES(VSPRES)%FMT)
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
!     ADDITIONAL DIFFERENTIATED VARIABLES
      CALL NOMVAR_SISYPHE(TEXTE,TEXTPR,MNEMO,NSICLA,UNIT,MAXVAR,
     &  NPRIV,NOMBLAY,N_NAMES_PRIV,NAMES_PRIVE,NADVAR,NAMES_ADVAR)
!
      CALL SORTIE(SORTIS , MNEMO , MAXVAR , SORLEO )
      CALL SORTIE(VARIM  , MNEMO , MAXVAR , SORIMP )
!
      DO I = 1, 4
        IF ((NPRIV.LT.I).AND.
            ! JWI 31/05/2012 - added 1 to include wave orbital velocities
     &      (SORLEO(I+29+(NOMBLAY+4)*NSICLA+NOMBLAY).OR.
     &       SORIMP(I+29+(NOMBLAY+4)*NSICLA+NOMBLAY))) THEN
          NPRIV=MAX(NPRIV,I)
        ENDIF
      ENDDO
!
!-----------------------------------------------------------------------
!
!     CANCELS OUTPUT OF VARIABLES WHICH ARE NOT BUILT IN THIS CASE
!
      IF(.NOT.SUSP) THEN
!V 7/09/2006 MIGHT WANT TO OUTPUT THE SUSPENDED COMPONENT IN BIJKER
!        SORIMP(24+4*NSICLA)=.FALSE.
!        SORIMP(25+4*NSICLA)=.FALSE.
!        SORIMP(26+4*NSICLA)=.FALSE.
      ENDIF
!
! JWI 31/05/2012 - added 1 to include wave orbital velocities
      IF(.NOT.CHARR) THEN
        SORLEO(24+(NOMBLAY+2)*NSICLA)=.FALSE.
        SORLEO(25+(NOMBLAY+2)*NSICLA)=.FALSE.
        SORLEO(26+(NOMBLAY+2)*NSICLA)=.FALSE.
        SORIMP(24+(NOMBLAY+2)*NSICLA)=.FALSE.
        SORIMP(25+(NOMBLAY+2)*NSICLA)=.FALSE.
        SORIMP(26+(NOMBLAY+2)*NSICLA)=.FALSE.
      ENDIF
! JWI END
!
!-----------------------------------------------------------------------
!
! CHECKS TETA VALUE
!
      IF( TETA.LT.0.D0.OR.TETA.GT.1.D0) THEN
          WRITE(LU,51)
51      FORMAT(/,1X,'BAD VALUE FOR TETA !                   ',/
     &          ,1X,'TETA MUST BE WITHIN 0 AND 1            ')
        CALL PLANTE(1)
        STOP
      ENDIF
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
      IF (VALID.AND.SIS_FILES(SISREF)%NAME.EQ.' ') THEN
          VALID=.FALSE.
        WRITE(LU,71)
        WRITE(LU,*)
71      FORMAT(/,1X,'VALIDATION IS NOT POSSIBLE :  ',/
     &          ,1X,'NO REFERENCE FILE  !                 ')
        CALL PLANTE(1)
        STOP
      ENDIF
!
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
        WRITE(LU,*)
91      FORMAT(/,1X,'SUM OF SEDIMENT FRACTIONS IS NOT 1  ')
        CALL PLANTE(1)
        STOP
      ENDIF
!
!     WARNING FOR THE CHOICE OF RIGID BED METHOD
!
      IF(CHOIX.GT.0.AND.CHOIX.LT.4.AND.VF) THEN
        WRITE(LU,201)
        WRITE(LU,*)
201     FORMAT(/,1X,'FINITE VOLUMES CHOSEN: ',/
     &          ,1X,'METHOD 4 FOR RIGID BED WILL BE USED ')
        CHOIX=4
      ENDIF
      IF (CHOIX.EQ.4.AND..NOT.VF) THEN
        WRITE(LU,301)
        WRITE(LU,*)
301     FORMAT(/,1X,'FINITE ELEMENTS CHOSEN: ',/
     &          ,1X,'METHOD 4 FOR RIGID BED CAN NOT BE USED, METHOD 3 US
     &ED INSTEAD')
        CHOIX=3
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
!     WITHOUT AND WITH COUPLING, SOME CORRECTIONS
!
      IF(CODE(1:7).EQ.'TELEMAC'.AND.
     &   SIS_FILES(SISHYD)%NAME(1:1).NE.' ') THEN
        SIS_FILES(SISHYD)%NAME(1:1)=' '
        WRITE(LU,113)
113     FORMAT(/,1X,'COUPLING: HYDRODYNAMIC FILE IGNORED')
      ENDIF
!
!     COMPUTATION CONTINUED
!
      IF(DEBU) THEN
        IF(SIS_FILES(SISPRE)%NAME(1:1).EQ.' ') THEN
          WRITE(LU,313)
313       FORMAT(/,1X,'COMPUTATION CONTINUED:',/,
     &             1X,'PREVIOUS SEDIMENTOLOGICAL FILE MISSING')
          CALL PLANTE(1)
          STOP
        ENDIF
      ELSE
        IF(SIS_FILES(SISPRE)%NAME(1:1).NE.' ') THEN
          SIS_FILES(SISPRE)%NAME(1:1)=' '
          WRITE(LU,213)
213       FORMAT(/,1X,'NO COMPUTATION CONTINUED:',/,
     &             1X,'PREVIOUS SEDIMENTOLOGICAL FILE IGNORED')
        ENDIF
      ENDIF
!
! METHODS NOT CODED UP FOR SUSPENSION
! -------------------------------------------
!
      IF(SUSP) THEN
        IF(RESOL.NE.ADV_CAR   .AND.RESOL.NE.ADV_SUP   .AND.
     &     RESOL.NE.ADV_LPO   .AND.RESOL.NE.ADV_NSC   .AND.
     &     RESOL.NE.ADV_PSI   .AND.RESOL.NE.ADV_LPO_TF.AND.
     &     RESOL.NE.ADV_NSC_TF.AND.RESOL.NE.ADV_PSI_TF      ) THEN
          WRITE(LU,303) RESOL
303       FORMAT(1X,'RESOLVING METHOD NOT IMPLEMENTED : ',1I6)
          CALL PLANTE(1)
          STOP
        ENDIF
      ENDIF
!
      IF(.NOT.HOULE) SIS_FILES(SISCOU)%NAME(1:1)=' '
      IF(HOULE) THEN
        IF(ICF.NE.4.AND.ICF.NE.5.AND.ICF.NE.8.AND.ICF.NE.9) THEN
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
! REFERENCE CONCENTRATION
!
! MODIFICATION CV 31/12      IF(ICQ.EQ.2.AND.(PERCOU.NE.1.OR..NOT.CHARR)) THEN
!
      IF(ICQ.EQ.2.AND.(PERCOU.GT.1.OR..NOT.CHARR)) THEN
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
        IF(ICF.NE.1) THEN
          WRITE(LU,1404) ICF
1404      FORMAT('BED-LOAD TRANSPORT FORMULA, HERE ICF=',1I3,/,1X,
     &           'MUST HAVE A THRESHOLD',/,1X,
     &           'IF FORMULA FOR SLOPE EFFECT=2 (SOULSBY)')
        ENDIF
      ENDIF
!
! V6P0 : COHERENCE IF CONSOLIDATION MODEL IS USED
! VITCE AND CSF_VASE STEM FROM THE FIRST LAYER OF THE MULTI-LAYER MODEL
! +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
! Mots cles supprimes vitesse critique d'erosion et concentration du lit
! CV : si premiere couche est  vide cela n'est pas correct

!
      IF(MIXTE) THEN
!
!       FILLS VOIDS WITH MUD:
! CV: verifier que la concentration en cohesif est non nulle
!
        CSF_SABLE= 1.D0
!V: verrouiller les options
        NSICLA=2
        SEDCO(1)=.FALSE.
        SEDCO(2)=.TRUE.
        CHARR=.FALSE.
        SUSP=.TRUE.
!V
      ELSE
        CSF_SABLE= (1.D0-XKV)
      ENDIF
!
      IF((.NOT.MIXTE).AND.SEDCO(1)) THEN
        CHARR=.FALSE.
        !SUSP=.TRUE. (In general, but not necessary)
      ENDIF
!
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
!----------------------------------------------------------------
!
!  V6P1: FOR THE BED FRICTION PREDICTOR USE LAW OF FRICTION 5 (NIKURADSE)
!
      IF(KSPRED) KFROT=5
!
!----------------------------------------------------------------
!
      RETURN
      END
