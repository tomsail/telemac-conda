!                   ************************
                    SUBROUTINE LECDON_KHIONE
!                   ************************
!
     & (FILE_DESC,PATH,NCAR)
!
!***********************************************************************
! KHIONE   V7P2                                              02/11/2016
!***********************************************************************
!
!brief    READS THE STEERING FILE THROUGH A DAMOCLES CALL.
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| FILE_DESC      |-->| STORES THE FILES 'SUBMIT' ATTRIBUTES
!|                |   | IN DICTIONARIES. IT IS FILLED BY DAMOCLES.
!| NCAR           |-->| LENGTH OF PATH
!| PATH           |-->| NAME OF CURRENT DIRECTORY
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE DECLARATIONS_SPECIAL
      USE DECLARATIONS_KHIONE
      USE METEO_TELEMAC
      USE FREEZEUP_KHIONE, ONLY : BUOYANCY_VELOCITY
      USE INTERFACE_KHIONE, EX_LECDON_KHIONE => LECDON_KHIONE
!
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      CHARACTER(LEN=PATH_LEN), INTENT(INOUT) :: FILE_DESC(4,MAXKEYWORD)
      INTEGER, INTENT(IN)                    :: NCAR
      CHARACTER(LEN=PATH_LEN), INTENT(IN)    :: PATH
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      CHARACTER(LEN=8) ::   MNEMO(MAXVAR)
      CHARACTER(LEN=8) ::   MNEMO3(MAXVAR)
      INTEGER          ::   K,I
      DOUBLE PRECISION ::   ANG
      CHARACTER(LEN=2) CHAR2
!
      CHARACTER(LEN=PATH_LEN) :: NOM_CAS
      CHARACTER(LEN=PATH_LEN) :: NOM_DIC
!
!-----------------------------------------------------------------------
!
! ARRAYS USED IN THE DAMOCLES CALL
!
      INTEGER            ADRESS(4,MAXKEYWORD)
      INTEGER            DIMENS(4,MAXKEYWORD)
      DOUBLE PRECISION   MOTREA(MAXKEYWORD)
      INTEGER            MOTINT(MAXKEYWORD)
      LOGICAL            MOTLOG(MAXKEYWORD)
      CHARACTER(LEN=PATH_LEN) MOTCAR(MAXKEYWORD)
      CHARACTER(LEN=72)  MOTCLE(4,MAXKEYWORD,2)
      INTEGER            TROUVE(4,MAXKEYWORD)
      LOGICAL            DOC
      INTEGER :: ID_DICO, ID_CAS
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      IF (LNG.EQ.1) WRITE(LU,1)
      IF (LNG.EQ.2) WRITE(LU,2)
1     FORMAT(1X,/,19X, '********************************************',/,
     &            19X, '*     SOUS-PROGRAMME LECDON_KHIONE         *',/,
     &            19X, '*           APPEL DE DAMOCLES              *',/,
     &            19X, '*     VERIFICATION DES DONNEES LUES        *',/,
     &            19X, '*           SUR LE FICHIER CAS             *',/,
     &            19X, '********************************************',/)
2     FORMAT(1X,/,19X, '********************************************',/,
     &            19X, '*        SUBROUTINE LECDON_KHIONE          *',/,
     &            19X, '*           CALL OF DAMOCLES               *',/,
     &            19X, '*        VERIFICATION OF READ DATA         *',/,
     &            19X, '*            ON STEERING FILE              *',/,
     &            19X, '********************************************',/)
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
!     DAMOCLE CALL - PARSING DICO AND CAS FILES
!
!     __________________________________________________________________
!     INITIALISES THE VARIABLES FOR DAMOCLES CALL
      DO K=1,MAXKEYWORD
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
!     __________________________________________________________________
!     WRITES OUT INFO
      DOC = .FALSE.
!
!     __________________________________________________________________
!     OPENS DICTIONNARY AND STEERING FILES
!
      IF(NCAR.GT.0) THEN
!
        NOM_DIC=PATH(1:NCAR)//'ICEDICO'
        NOM_CAS=PATH(1:NCAR)//'ICECAS'
!
      ELSE
!
        NOM_DIC='ICEDICO'
        NOM_CAS='ICECAS'
!
      ENDIF
!
      CALL GET_FREE_ID(ID_DICO)
      OPEN(ID_DICO,FILE=NOM_DIC,FORM='FORMATTED',ACTION='READ')
      CALL GET_FREE_ID(ID_CAS)
      OPEN(ID_CAS,FILE=NOM_CAS,FORM='FORMATTED',ACTION='READ')
!
!     __________________________________________________________________
!     CALL DAMOCLE
      CALL DAMOCLE
     &( ADRESS, DIMENS, MAXKEYWORD, DOC, LNG, LU, MOTINT,
     &  MOTREA, MOTLOG, MOTCAR, MOTCLE , TROUVE, ID_DICO, ID_CAS,
     &  .FALSE.,FILE_DESC)
!
!     __________________________________________________________________
!     CLOSES DICTIONNARY AND STEERING FILES
      CLOSE(ID_DICO)
      CLOSE(ID_CAS)
!
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
!     RETRIEVES FILE NUMBERS FROM KHIONE FORTRAN PARAMETERS
!
!     DECODES 'SUBMIT' CHAINS
      CALL READ_SUBMIT(ICE_FILES,MAXLU_ICE,FILE_DESC,MAXKEYWORD)
!
!     AT THIS LEVEL LOGICAL UNITS ARE EQUAL TO THE FILE NUMBER
      DO I=1,MAXLU_ICE
        IF    (ICE_FILES(I)%TELNAME.EQ.'ICECLI') THEN
          ICECLI=I
        ELSEIF(ICE_FILES(I)%TELNAME.EQ.'ICEGEO') THEN
          ICEGEO=I
        ELSEIF(ICE_FILES(I)%TELNAME.EQ.'ICEREF') THEN
          ICEREF=I
        ELSEIF(ICE_FILES(I)%TELNAME.EQ.'ICERES') THEN
          ICERES=I
        ELSEIF(ICE_FILES(I)%TELNAME.EQ.'ICECOV') THEN
          ICECOV=I
        ELSEIF(ICE_FILES(I)%TELNAME.EQ.'ICEBLK') THEN
          ICEBLK=I
        ELSEIF(ICE_FILES(I)%TELNAME.EQ.'CLGRFO') THEN
          CLGRFO=I
        ELSEIF(ICE_FILES(I)%TELNAME.EQ.'ICER3D') THEN
          ICER3D=I
        ENDIF
      ENDDO
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
!     ALGORITHMIC DIFFERENTIATION
!
      NADVAR    = MOTINT( ADRESS(1,13) )
!
!     NAMES OF DIFFERENTIATED VARIABLES
      N_NAMES_ADVAR = DIMENS(4,13)
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
          NAMES_ADVAR(K) = MOTCAR(ADRESS(4,13)+K-1)(1:32)
        ENDDO
      ENDIF
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
!     LISTING AND GRAPHICAL OUTPUTS
!     __________________________________________________________________
!     TITLE OF THE STUDY
      TITICECAS = MOTCAR( ADRESS(4, 3) )(1:72)
!     __________________________________________________________________
!     GRAPHIC PRINTOUT PERIOD
      LEOPRD    = MOTINT( ADRESS(1,  1) )
!     __________________________________________________________________
!     LISTING PRINTOUT PERIOD
      LISPRD    = MOTINT( ADRESS(1,  2) )
!     __________________________________________________________________
!     FILES IN THE STEERING FILE
!
      ICE_FILES(ICECLI)%NAME=MOTCAR( ADRESS(4,4 ) )
      ICE_FILES(ICEGEO)%NAME=MOTCAR( ADRESS(4,5 ) )
      ICE_FILES(ICEGEO)%FMT=MOTCAR( ADRESS(4,35) )(1:8)
      ICE_FILES(ICEREF)%NAME=MOTCAR( ADRESS(4,6) )
      ICE_FILES(ICEREF)%FMT=MOTCAR( ADRESS(4,36) )(1:8)
      ICE_FILES(ICERES)%NAME=MOTCAR( ADRESS(4,7 ) )
      ICE_FILES(ICERES)%FMT=MOTCAR( ADRESS(4,37) )(1:8)
      ICE_FILES(ICER3D)%NAME=MOTCAR( ADRESS(4,9 ) )
      ICE_FILES(ICER3D)%FMT=MOTCAR( ADRESS(4,17) )(1:8)
      ! PREVIOUS ICE COVER FILE
      ICE_FILES(ICECOV)%NAME=MOTCAR( ADRESS(4,8) )
      ICE_FILES(ICECOV)%FMT=MOTCAR( ADRESS(4,14) )(1:8)
      CALL MAJUS(ICE_FILES(ICECOV)%FMT)
      ! PREVIOUS ICE BLOCKS FILE
      ICE_FILES(ICEBLK)%NAME=MOTCAR( ADRESS(4,15) )
      ICE_FILES(ICEBLK)%FMT=MOTCAR( ADRESS(4,16) )(1:8)
      CALL MAJUS(ICE_FILES(ICECOV)%FMT)
      ! CLOGGING RESULTS FILE
      ICE_FILES(CLGRFO)%NAME=MOTCAR( ADRESS(4,25 ) )
!     __________________________________________________________________
!     UPDATE OF MNEMO AND NOMVARS
      DO I=1,MAXVAR
        MNEMO(I) = '        '
        MNEMO3(I) = '        '
      ENDDO
      CALL NOMVAR_KHIONE( TEXTE,TEXTPR,MNEMO,NADVAR,NAMES_ADVAR )
      CALL NOMVAR3D_KHIONE( TEXTE3,TEXTPR3,MNEMO3 )
!     __________________________________________________________________
!     GRAPHICAL OUTPUT VARIABLES
      VARDES = MOTCAR( ADRESS(4, 10) )(1:72)
      CALL MAJUS( VARDES )
      CALL SORTIE( VARDES , MNEMO , MAXVAR , SORLEO )
      VARD3D = MOTCAR( ADRESS(4, 18) )(1:72)
      CALL MAJUS( VARD3D )
      CALL SORTIE( VARD3D , MNEMO3 , MAXVAR , SORLEO3 )
      VARIMP = MOTCAR( ADRESS(4, 11) )(1:72)
      CALL MAJUS( VARIMP )
      CALL SORTIE( VARIMP , MNEMO , MAXVAR , SORIMP )
      CALL SORTIE( VARIMP , MNEMO3 , MAXVAR , SORIMP3 )
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
!     PHYSICAL PRPERTIES AND CONSTANTS RELATED KEYWORDS
!     __________________________________________________________________
!     WATER KINEMATIC VISCOSITY
      XNU = MOTREA( ADRESS(2,8) )
!     __________________________________________________________________
!     DENSITIES
      RO0       = MOTREA( ADRESS(2,  1) ) !WATER
      RHO_AIR   = MOTREA( ADRESS(2, 11) )
      RHO_ICE   = MOTREA( ADRESS(2, 12) )
!     __________________________________________________________________
!     SPECIFIC AND LATENT HEATS
      CP_EAU    = MOTREA( ADRESS(2, 5) )
      CP_ICE    = MOTREA( ADRESS(2, 16) )
      LH_ICE    = MOTREA( ADRESS(2, 17) )
!     __________________________________________________________________
!     ICE ALBEDO
      ALBE = MOTREA( ADRESS(2,34) )
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
!     ATMOSPHERIC PROPERTY RELATED KEYWORDS (IN METEO_TELEMAC)
!
      ATMOEXCH  = MOTINT( ADRESS(1,17) )
!     __________________________________________________________________
!     VERTICAL DATUM
      WINDZ     = MOTREA( ADRESS(2,22) )
      MODELZ    = MOTREA( ADRESS(2,23) )
!     __________________________________________________________________
!     SUN RISE AND SUN SET ANGLES
      ALPHSD    = MOTREA( ADRESS(2,25) )     !? sun exit angle
      ALPHRD    = MOTREA( ADRESS(2,26) )     !? sun emission angle
!     __________________________________________________________________
!     SOLAR CONSTANT
      SIO       = MOTREA( ADRESS(2,33) )     !? solar constant
!     __________________________________________________________________
!     HEAT EXCHANGE CALIBRATION COEFFICIENTS
!     (ATMOSPHERE-WATER EXCHANGE MODEL=3)
      LIN_WATAIR = MOTREA( ADRESS(2, 18) )   ! replaces HWA
      CST_WATAIR = MOTREA( ADRESS(2, 20) )   ! replaces ALPW
      LIN_ICEAIR = MOTREA( ADRESS(2, 19) )   ! replaces HIA
      CST_ICEAIR = MOTREA( ADRESS(2, 21) )   ! replaces ALP
!     __________________________________________________________________
!     HEAT EXCHANGE CALIBRATION COEFFICIENTS
      COEF_PHIB = MOTREA( ADRESS(2, 56) )
      COEF_PHIE = MOTREA( ADRESS(2, 57) )
      COEF_PHIH = MOTREA( ADRESS(2, 58) )
      COEF_PHIP = MOTREA( ADRESS(2, 59) )
!     __________________________________________________________________
!     METEOROLOGY, IN CASE ABSENT FROM METEO FILES
!     CST_TAIR  = MOTREA( ADRESS(2, 41) )
      CST_TDEW  = MOTREA( ADRESS(2, 42) )
!     CST_CLDC  = MOTREA( ADRESS(2, 43) )
      CST_VISBI = MOTREA( ADRESS(2, 45) )
!     CST_RAINFALL = MOTREA( ADRESS(2, 46) )
!     IF( DIMENS(2,47).EQ.2 ) THEN
!       CST_WINDS = MOTREA( ADRESS(2,47) + 0 )
!       CST_WINDD = MOTREA( ADRESS(2,47) + 1 )
!       DTRS4 = ATAN(1.D0) / 45.D0
!       CST_WINDX = - CST_WINDS * SIN( CST_WINDD*DTRS4 )
!       CST_WINDY = - CST_WINDS * COS( CST_WINDD*DTRS4 )
!     ENDIF
!     IF( DIMENS(2,48).EQ.2 ) THEN
!       CST_WINDX = MOTREA( ADRESS(2,48) + 0 )
!       CST_WINDY = MOTREA( ADRESS(2,48) + 1 )
!     ENDIF
!     CST_PATMOS  = MOTREA( ADRESS(2, 49) )
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
!     WATER PROPERTY RELATED KEYWORDS
!
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
!     FREEZEUP RELATED KEYWORDS
!
      DE    = MOTREA( ADRESS(2,28) ) ! DIAMETER THICKNESS RATIO
      NUSS  = MOTREA( ADRESS(2,24) ) ! CONSTANT NUSSELT NUMBER
      NUSSI = MOTINT( ADRESS(1, 9) ) ! MODEL FOR NUSSLET NUMBER COMPUTATION
      IBUOY = MOTINT( ADRESS(1,10) ) ! MODEL FOR BUOYANCY VELOCITY
      ISNUC = MOTINT( ADRESS(1,12) ) ! MODEL FOR SECONDARY NUCLEATION
      SNNMAX= MOTREA( ADRESS(2,13) ) ! NMAX PARAM FOR SECONDARY NUCLEATION
      IFLOC = MOTINT( ADRESS(1,15) ) ! MODEL FOR FLOCULATION AND BREAKUP
      AFLOC = MOTREA( ADRESS(2,14) ) ! AFLOC PARAM FOR FLOCULATION
      INRJB = MOTINT( ADRESS(1,18) ) ! ENERGY BALANCE VERSION
      ISEED = MOTINT( ADRESS(1,19) ) ! SEEDING MODEL
      MINNK = MOTREA( ADRESS(2,51) ) ! MINIMUM NUMBER OF FRAZIL PARTICLES PER UNIT VOLUME
      PRE_MIN = MOTREA( ADRESS(2,3)) ! MINIMAL THICKNESS TO CONSIDER ICE COVER FROM PRECIPITATION
      ITURB = MOTINT( ADRESS(1,4) )  ! TURBULENCE MODEL FOR K AND EPSILON APPROXIMATION
!     TURBULENCE PARAMETERS USED WHEN ITURB=1
      DO K=1,4
        KTURB(K) = MOTREA( ADRESS(2,53) + K-1 )
      ENDDO
!
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
!     ICE COVER PROPERTY RELATED KEYWORDS
!
      IFROT = MOTINT(ADRESS(1,8))       ! ICE COVER FRICTION LAW
      IFICE = MOTINT(ADRESS(1,16))      ! LAW FOR FRICTION COEF
      IFRIC = MOTINT(ADRESS(1,24))      ! FRICTION MODEL
      FICE = MOTREA(ADRESS(2,4))        ! ICE COVER FRICTION COEFFICIENT
      FICE_MAX = MOTREA(ADRESS(2,52))   ! ICE COVER MAX FRICTION COEFFICIENT
      THIE = MOTREA( ADRESS(2,75) )     ! EQUIVALENT SURFACE ICE THICKNESS
      IPRES = MOTINT(ADRESS(1,26))      ! ICE COVER PRESSURE GRADIENT MODEL
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
!     STATIC BORDER ICE RELATED KEYWORDS
!
!
      BCH = MOTREA( ADRESS(2,98) )       ! (15) CHANNEL WIDTH FOR THE COMPUTATION OF SURFACE WATER TEMPERATURE
      TC = MOTREA( ADRESS(2,71) )        ! (-1.1) CRITICAL WATER SURFACE TEMPERATURE FOR BORDER ICE FORMATION
      VCRBOR = MOTREA(ADRESS(2,50))      ! (0.07) THRESHOLD VELOCITY FOR STATIC BORDER ICE
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
!     DYNAMIC BORDER ICE RELATED KEYWORDS
!
      VCRBOM = MOTREA(ADRESS(2,73))      ! (0.4) THRESHOLD VELOCITY ABOVE WHICH DYNAMIC BORDER ICE WILL NOT FORM
!
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
!     ICE PROPERTY RELATED KEYWORDS
!
!*******************************
!     REAL KEYWORDS            *
!*******************************
!
      ALSM = MOTREA( ADRESS(2,30) )     ! standard longitude, in degrees
      ALLM = MOTREA( ADRESS(2,31) )     ! local longitude, in degrees
      ETADIR = MOTREA( ADRESS(2,32) )   ! - for west longitude, + for east longitude
      AF   = MOTREA( ADRESS(2,29) )      ! (1.) DEPOSITION COEFFICIENT OF FRAZIL ON THE BAR
      SURF_EF  = MOTREA( ADRESS(2,39) )  ! POROSITY OF SURFACE ICE
      TC_WT = MOTREA( ADRESS(2,44) )     ! => XKWP: (0.56594) WATER-ICE THERMAL CONDUCTIVITY
      EXTINC = MOTREA( ADRESS(2,49) )    ! Light extension coefficient (for 3D)
!
      CLOG_EF    = MOTREA( ADRESS(2,37) )! POROSITY OF FRAZIL ICE
      CLOG_THETA = MOTREA( ADRESS(2,36) )
      IF( DIMENS(2,35).EQ.4 ) THEN
        CLOG_TDIST = MOTREA( ADRESS(2,35) + 0 )
        CLOG_TDIAM = MOTREA( ADRESS(2,35) + 1 )
        CLOG_VDIST = MOTREA( ADRESS(2,35) + 2 )
        CLOG_VDIAM = MOTREA( ADRESS(2,35) + 3 )
      ENDIF
!
      NFRCLOG = DIMENS(1,14)
      IF(MODULO(DIMENS(1,3),2).EQ.1) THEN
        IF ( MOTINT( ADRESS(1,3) ).EQ.0 ) THEN
          NSECLOG = 0
        ELSE
          WRITE(LU,*) ' '
          WRITE(LU,*) 'LECDON_KHIONE'
          WRITE(LU,*) 'CLOOGING SECTION WORKS BY PAIR'
          WRITE(LU,*) 'PLEASE GIVE A MULTIPLE OF 2 NUMBER OF NODES'
          CALL PLANTE(1)
          STOP
        ENDIF
      ELSE
        NSECLOG = DIMENS(1,3)/2
      ENDIF
      NTOTCLOG = NFRCLOG + NSECLOG
      IF( MOTINT( ADRESS(1,14) ).EQ.0 ) NFRCLOG = 0
      IF( NTOTCLOG.GT.0 ) THEN
!
        ALLOCATE( LINES%CELLS(NTOTCLOG) )
        LINES%NVAL = NTOTCLOG
        IF(NFRCLOG.GT.0) ALLOCATE(NUMCLOG(NFRCLOG))
        IF(NSECLOG.GT.0) ALLOCATE(SECLOG(DIMENS(1,3)))
        IF(NSECLOG.GT.0) ALLOCATE(UN1(NSECLOG))
        IF(NSECLOG.GT.0) ALLOCATE(UN2(NSECLOG))
        ALLOCATE(CLOG_TLGTH(NTOTCLOG))
        ALLOCATE(CLOG_TWDTH(NTOTCLOG))
        ALLOCATE(CLOG_VLGTH(NTOTCLOG))
        ALLOCATE(CLOG_VWDTH(NTOTCLOG))
        ALLOCATE(CLOG_VOLUM(NTOTCLOG))
!
!       ACUMULATION ANGLE
        ANG = 4.D0*ATAN(1.D0) * CLOG_THETA / 180.0
        DO K = 1,NFRCLOG
          NUMCLOG(K) = MOTINT( ADRESS(1,14) + K-1 )
        ENDDO
        DO K = 1,NSECLOG*2
          SECLOG(K) = MOTINT( ADRESS(1,3) + K-1 )
        ENDDO
        DO K = 1,NTOTCLOG
          LINES%CELLS(K)%NVAL = 0
          CLOG_TLGTH(K) = 0.D0
          CLOG_TWDTH(K) = 2.0*CLOG_TDIAM*COS(ANG)*SIN(ANG)
          CLOG_VLGTH(K) = 0.D0
          CLOG_VWDTH(K) = 2.0*CLOG_VDIAM*COS(ANG)*SIN(ANG)
          CLOG_VOLUM(K) = 0.D0
        ENDDO
!
      ENDIF
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
!     ICE PROCESS
!
      CWI1 = MOTREA( ADRESS(2,62) )       ! CONSTANT FOR HEAT TRANSFER BETWEEN TURBULENT WATER AND ICE
      CIW1 = MOTREA( ADRESS(2,63) )       ! CONSTANT FOR HEAT TRANSFER FOR SUPERCOOLED TURBULENT FLOW
      ATA = MOTREA( ADRESS(2,64) )        ! NUSSELT NUMBER FOR HEAT TRANSFER BETWEEN LAMINAR WATER AND ICE
      TC_BI = MOTREA( ADRESS(2,65) )      ! THERMAL CONDUCTIVITY OF BLACK ICE (W/M/OC) => XKI
!
      TC_S = MOTREA( ADRESS(2,67) )       ! THERMAL CONDUCTIVITY OF SNOW(W/M/OC) => XKS
      SGMA = MOTREA( ADRESS(2,69) )       ! BOLTZMANN CONSTANT (WM-2K-4)
      CST_TMELT = MOTREA( ADRESS(2,70) )  ! FREEZING POINT OF WATER
!
      ANFEM0 = MOTREA( ADRESS(2,74) )     ! (1.) MAXIMUM CONCENTRATION FOR BORDER ICE FORMATION
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
!     INIT OF FRAZIL CLASS SPECIFIC PARAMETERS
!
      ALLOCATE(RK_FRZL(NC_FRA))
      ALLOCATE(EK_FRZL(NC_FRA))
      ALLOCATE(VK_FRZL(NC_FRA))
      ALLOCATE(NUSST(NC_FRA))
      ALLOCATE(VBK(NC_FRA))
      ALLOCATE(DEPK(NC_FRA))
      ALLOCATE(EROK(NC_FRA))
      ALLOCATE(SEEDR(NC_FRA))
      ALLOCATE(MINCK(NC_FRA))
!
      IF(NC_FRA.GT.0) THEN
        DO K=1,NC_FRA
!
!         PARTICLE GEOM PARAMETERS
          IF(DIMENS(2, 27).GE.K) THEN
            RK_FRZL(K) = MOTREA( ADRESS(2, 27) + K-1 )
          ELSE
            IF(K.EQ.1) THEN
              RK_FRZL(K) = 1.0E-4
            ELSE
              RK_FRZL(K) = RK_FRZL(1) + (K-1)*1.0E-4
            ENDIF
            WRITE(LU,*) 'WARNING: THE VALUE OF FRAZIL CRISTAL RADIUS ',K
            WRITE(LU,*) ' IS NOT GIVEN, RADIUS IS SET TO ', RK_FRZL(K)
          ENDIF
          PI = 4.D0*ATAN(1.D0)
          EK_FRZL(K) = 2.D0*RK_FRZL(K)/DE
          VK_FRZL(K) = 2.D0*PI*RK_FRZL(K)**3/DE
!
!         TURBULENT NUSSELT NUMBER INIT
          NUSST(K) = NUSS
!
!         FRAZIL PARTICLE BUOYANCY VELOCITY
          VBK(K) = BUOYANCY_VELOCITY(RK_FRZL(K), EK_FRZL(K))
!
!         FRAZIL SEEDING RATE
          IF(DIMENS(2, 60).GE.K) THEN
            SEEDR(K) = MOTREA( ADRESS(2, 40) + K-1 )
          ELSE
            SEEDR(K) = 0.D0 !WARNING: REPLACES DICO DEFAULT VALUE
          ENDIF
!
!         MINIMUM FRAZIL VOLUME FRACTION 
          MINCK(K) = MINNK*VK_FRZL(K)/NC_FRA
!
!         PROBABILITY OF DEPOSITION OF FRAZIL PARTICLES REACHING THE
!           SURFACE LAYER - OPEN WATER
          IF(DIMENS(2, 60).GE.K) THEN
            DEPK(K) = MOTREA( ADRESS(2, 60) + K-1 )
          ELSE
            DEPK(K) = 1.D0 !WARNING: REPLACES DICO DEFAULT VALUE
          ENDIF
!
!         COEFFICIENT QUANTIFYING THE RATE OF REENTRAINMENT OF SURFACE
!           ICE PER UNIT AREA
          IF(DIMENS(2, 61).GE.K) THEN
            EROK(K) = MOTREA( ADRESS(2, 61) + K-1 )
          ELSE
            EROK(K) = 1.D-4 !WARNING: REPLACES DICO DEFAULT VALUE
          ENDIF
!
        ENDDO
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END SUBROUTINE
