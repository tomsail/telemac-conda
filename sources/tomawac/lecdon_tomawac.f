!                   *************************
                    SUBROUTINE LECDON_TOMAWAC
!                   *************************
!
     &(FILE_DESC,PATH,NCAR,CAS_FILE,DICO_FILE,PART)
!
!***********************************************************************
! TOMAWAC   V7P1
!***********************************************************************
!
!brief    READS THE STEERING FILE THROUGH A DAMOCLES CALL.
!
!history  G.MATTAROLO (EDF)
!+        16/05/2011
!+        V6P1
!+   Declaration of new keywords defined by
!+       E. GAGNAIRE-RENOU for solving new source terms models.
!
!history  G.MATTAROLO (EDF - LNHE)
!+        20/06/2011
!+        V6P1
!+   Translation of French names of the variables in argument
!
!history  G.MATTAROLO (EDF - LNHE)
!+        25/06/2012
!+        V6P2
!+   Declaration of new keywords for representing diffraction
!
!history  J-M HERVOUET (EDF R&D LNHE)
!+        01/02/2013
!+        V6P3
!+   New keywords added. Call to tomawac_constants added.
!
!history  J-M HERVOUET (EDF R&D LNHE)
!+        09/05/2014
!+        V7P0
!+   Retrieving MODASS for new parallel assembly with integers.
!
!history  J-M HERVOUET (EDF R&D LNHE)
!+        12/09/2014
!+        V7P0
!+   Retrieving VEGETATION for VEGETATION TAKEN INTO ACCOUNT
!
!history  J-M HERVOUET (EDF LAB, LNHE)
!+        18/05/2015
!+        V7P1
!+  Adding CHECK_MESH for the keyword 'CHECKING THE MESH'
!
!history Y AUDOUIN (LNHE)
!+       25/05/2015
!+       V7P0
!+       Modification to comply with the hermes module
!
!history  J-M HERVOUET (EDF LAB, LNHE)
!+        08/03/2016
!+        V7P2
!+  Retrieving NPLEO in a different way.
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| FILE_DESC      |-->| STORES THE FILES 'SUBMIT' ATTRIBUTES
!|                |   | IN DICTIONARIES. IT IS FILLED BY DAMOCLES.
!| NCAR           |-->| LENGTH OF PATH
!| PATH           |-->| NAME OF CURRENT DIRECTORY
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_TELEMAC
      USE DECLARATIONS_TOMAWAC
      USE DECLARATIONS_SPECIAL
!
      USE INTERFACE_TOMAWAC, EX_LECDON_TOMAWAC => LECDON_TOMAWAC
      IMPLICIT NONE
!
!
!-----------------------------------------------------------------------
!
      CHARACTER(LEN=PATH_LEN), INTENT(INOUT) :: FILE_DESC(4,MAXKEYWORD)
      INTEGER, INTENT(IN)               :: NCAR, PART
      CHARACTER(LEN=PATH_LEN), INTENT(IN)    :: PATH
      CHARACTER(LEN=PATH_LEN), INTENT(IN)    :: DICO_FILE
      CHARACTER(LEN=PATH_LEN), INTENT(IN)    :: CAS_FILE
!
!-----------------------------------------------------------------------
!
! ARRAYS USED IN THE DAMOCLES CALL
!
      CHARACTER(LEN=8)      MNEMO(MAXVAR)
      INTEGER          K
      INTEGER            ADRESS(4,MAXKEYWORD),DIMEN(4,MAXKEYWORD)
      DOUBLE PRECISION   MOTREA(MAXKEYWORD)
      INTEGER            MOTINT(MAXKEYWORD)
      LOGICAL            MOTLOG(MAXKEYWORD)
      CHARACTER(LEN=PATH_LEN) MOTCAR(MAXKEYWORD)
      CHARACTER(LEN=72)  MOTCLE(4,MAXKEYWORD,2)
      INTEGER            TROUVE(4,MAXKEYWORD)
      LOGICAL            DOC
      CHARACTER(LEN=PATH_LEN) :: NOM_CAS
      CHARACTER(LEN=PATH_LEN) :: NOM_DIC
! ARGUMENTS
      INTEGER :: I
      INTEGER :: ID_DICO,ID_CAS
!
! END OF DECLARATIONS FOR DAMOCLES CALL
!
!
!***********************************************************************
!
      WRITE(LU,2)
2     FORMAT(1X,/,19X, '********************************************',/,
     &            19X, '*        SUBROUTINE LECDON_TOMAWAC         *',/,
     &            19X, '*           CALL OF DAMOCLES               *',/,
     &            19X, '*        VERIFICATION OF READ DATA         *',/,
     &            19X, '*            ON STEERING FILE              *',/,
     &            19X, '********************************************',/)
!
!-----------------------------------------------------------------------
!
! INITIALISES THE VARIABLES FOR DAMOCLES CALL :
!
      DO K=1,MAXKEYWORD
!       A FILENAME NOT GIVEN BY DAMOCLES WILL BE RECOGNIZED AS A WHITE SPACE
!       (IT MAY BE THAT NOT ALL COMPILERS WILL INITIALISE LIKE THAT)
        MOTCAR(K)(1:1)=' '
!
        DIMEN(1,K) = 0
        DIMEN(2,K) = 0
        DIMEN(3,K) = 0
        DIMEN(4,K) = 0
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
        NOM_DIC=PATH(1:NCAR)//'WACDICO'
        NOM_CAS=PATH(1:NCAR)//'WACCAS'
!
      ELSE
!
        NOM_DIC='WACDICO'
        NOM_CAS='WACCAS'
!
      ENDIF
      IF((CAS_FILE(1:1).NE.' ').AND.(DICO_FILE(1:1).NE.' ')) THEN
        NOM_DIC=DICO_FILE
        NOM_CAS=CAS_FILE
      ENDIF
!
      CALL GET_FREE_ID(ID_DICO)
      OPEN(ID_DICO,FILE=NOM_DIC,FORM='FORMATTED',ACTION='READ')
      CALL GET_FREE_ID(ID_CAS)
      OPEN(ID_CAS,FILE=NOM_CAS,FORM='FORMATTED',ACTION='READ')
!
      CALL DAMOCLE
     &( ADRESS, DIMEN , MAXKEYWORD  , DOC    , LNG   , LU    , MOTINT,
     &  MOTREA, MOTLOG, MOTCAR, MOTCLE , TROUVE, ID_DICO, ID_CAS,
     &  .FALSE.,FILE_DESC)

      CLOSE(ID_DICO)
      CLOSE(ID_CAS)
!
!     DECODES 'SUBMIT' CHAINS
!
      CALL READ_SUBMIT(WAC_FILES,MAXLU_WAC,FILE_DESC,MAXKEYWORD)
!
!-----------------------------------------------------------------------
!
!     RETRIEVES FILE NUMBERS FROM TOMAWAC FORTRAN PARAMETERS
!
      DO I=1,MAXLU_WAC
        IF(WAC_FILES(I)%TELNAME.EQ.'WACGEO') THEN
          WACGEO=I
          LUGEO=>WAC_FILES(I)%LU
          FMTGEO=> WAC_FILES(I)%FMT
        ELSEIF(WAC_FILES(I)%TELNAME.EQ.'WACCAS') THEN
          WACCAS=I
        ELSEIF(WAC_FILES(I)%TELNAME.EQ.'WACCLI') THEN
          WACCLI=I
        ELSEIF(WAC_FILES(I)%TELNAME.EQ.'WACFON') THEN
          WACFON=I
          LUFON=>WAC_FILES(I)%LU
          NAMFON=> WAC_FILES(I)%NAME
        ELSEIF(WAC_FILES(I)%TELNAME.EQ.'WACRES') THEN
          WACRES=I
          LURES=>WAC_FILES(I)%LU
          NAMRES=> WAC_FILES(I)%NAME
          FMTRES=> WAC_FILES(I)%FMT
        ELSEIF(WAC_FILES(I)%TELNAME.EQ.'WACREF') THEN
          WACREF=I
          LUREF=>WAC_FILES(I)%LU
          FMTREF=> WAC_FILES(I)%FMT
        ELSEIF(WAC_FILES(I)%TELNAME.EQ.'WACSPE') THEN
          WACSPE=I
          LUSPE=>WAC_FILES(I)%LU
          NAMSPE=> WAC_FILES(I)%NAME
        ELSEIF(WAC_FILES(I)%TELNAME.EQ.'WACLEO') THEN
          WACLEO=I
          LULEO=>WAC_FILES(I)%LU
          NAMLEO=> WAC_FILES(I)%NAME
          FMTLEO=> WAC_FILES(I)%FMT
        ELSEIF(WAC_FILES(I)%TELNAME.EQ.'WACPRE') THEN
          WACPRE=I
          LUPRE=>WAC_FILES(I)%LU
          FMTPRE=> WAC_FILES(I)%FMT
        ELSEIF(WAC_FILES(I)%TELNAME.EQ.'WACRBI') THEN
          WACRBI=I
          LURBI=>WAC_FILES(I)%LU
          FMTRBI=> WAC_FILES(I)%FMT
        ELSEIF(WAC_FILES(I)%TELNAME.EQ.'WACCOB') THEN
          WACCOB=I
          LUCOB=>WAC_FILES(I)%LU
          NAMCOB=> WAC_FILES(I)%NAME
          FMTCOB=> WAC_FILES(I)%FMT
        ELSEIF(WAC_FILES(I)%TELNAME.EQ.'WACCOF') THEN
          WACCOF=I
          LUCOF=>WAC_FILES(I)%LU
          NAMCOF=> WAC_FILES(I)%NAME
          FMTCOF=> WAC_FILES(I)%FMT
        ELSEIF(WAC_FILES(I)%TELNAME.EQ.'WACBI1') THEN
          WACBI1=I
          LUBI1=>WAC_FILES(I)%LU
          FMTBI1=> WAC_FILES(I)%FMT
        ELSEIF(WAC_FILES(I)%TELNAME.EQ.'WACFO1') THEN
          WACFO1=I
          LUFO1=>WAC_FILES(I)%LU
        ELSEIF(WAC_FILES(I)%TELNAME.EQ.'WACVEB') THEN
          WACVEB=I
          LUVEB=>WAC_FILES(I)%LU
          NAMVEB=> WAC_FILES(I)%NAME
          FMTVEB=> WAC_FILES(I)%FMT
        ELSEIF(WAC_FILES(I)%TELNAME.EQ.'WACVEF') THEN
          WACVEF=I
          LUVEF=>WAC_FILES(I)%LU
          NAMVEF=> WAC_FILES(I)%NAME
          FMTVEF=> WAC_FILES(I)%FMT
        ELSEIF(WAC_FILES(I)%TELNAME.EQ.'WACMAB') THEN
          WACMAB=I
          LUMAB=>WAC_FILES(I)%LU
          NAMMAB=> WAC_FILES(I)%NAME
          FMTMAB=> WAC_FILES(I)%FMT
        ELSEIF(WAC_FILES(I)%TELNAME.EQ.'WACMAF') THEN
          WACMAF=I
          LUMAF=>WAC_FILES(I)%LU
          NAMMAF=> WAC_FILES(I)%NAME
          FMTMAF=> WAC_FILES(I)%FMT
        ELSEIF(WAC_FILES(I)%TELNAME.EQ.'LEOWXY') THEN
          LEOWXY=I
          NAMWXY=> WAC_FILES(I)%NAME
          LUWXY=>WAC_FILES(I)%LU
        ELSEIF(WAC_FILES(I)%TELNAME.EQ.'LEOIXY') THEN
          LEOIXY=I
          NAMIXY=> WAC_FILES(I)%NAME
          LUIXY=>WAC_FILES(I)%LU
        ELSEIF(WAC_FILES(I)%TELNAME.EQ.'WACZON') THEN
          WACZON=I
        ELSEIF(WAC_FILES(I)%TELNAME.EQ.'IMPSPE') THEN
          IMPSPE=I
        ENDIF
      ENDDO
!
!-----------------------------------------------------------------------
!
!     ASSIGNS THE STEERING FILE VALUES TO THE PARAMETER FORTRAN NAME
!
!-----------------------------------------------------------------------
!
!     INTEGER KEYWORDS
!
      GRAPRD = MOTINT( ADRESS(1,  1) )
      LISPRD = MOTINT( ADRESS(1,  2) )
      NIT    = MOTINT( ADRESS(1,  3) )
      NDIRE  = MOTINT( ADRESS(1,  4) )
      NF     = MOTINT( ADRESS(1,  5) )
      GRADEB = MOTINT( ADRESS(1,  6) )
      LISFON = MOTINT( ADRESS(1,  7) )
      SVENT  = MOTINT( ADRESS(1,  8) )
      SMOUT  = MOTINT( ADRESS(1,  9) )
      SFROT  = MOTINT( ADRESS(1, 10) )
      STRIF  = MOTINT( ADRESS(1, 11) )
      INDIC  = MOTINT( ADRESS(1, 12) )
      INDIV  = MOTINT( ADRESS(1, 13) )
      NSITS  = MOTINT( ADRESS(1, 14) )
      INISPE = MOTINT( ADRESS(1, 15) )
!     DISSIPATION BY STRONG CURRENT
      SDSCU  = MOTINT( ADRESS(1, 16) )
      NPTT   = MOTINT( ADRESS(1, 17) )
      LVMAC  = MOTINT( ADRESS(1, 18) )
      SBREK  = MOTINT( ADRESS(1, 19) )
      IQBBJ  = MOTINT( ADRESS(1, 20) )
      IHMBJ  = MOTINT( ADRESS(1, 21) )
      IFRBJ  = MOTINT( ADRESS(1, 22) )
      IWHTG  = MOTINT( ADRESS(1, 23) )
      IFRTG  = MOTINT( ADRESS(1, 24) )
      IDISRO = MOTINT( ADRESS(1, 25) )
      IEXPRO = MOTINT( ADRESS(1, 26) )
      IFRRO  = MOTINT( ADRESS(1, 27) )
      IFRIH  = MOTINT( ADRESS(1, 28) )
      NDTBRK = MOTINT( ADRESS(1, 29) )
      LIMIT  = MOTINT( ADRESS(1, 30) )
      LVENT  = MOTINT( ADRESS(1, 31) )
      STRIA  = MOTINT( ADRESS(1, 32) )
      LIMSPE = MOTINT( ADRESS(1, 33) )
      LAM    = MOTINT( ADRESS(1, 34) )
      INDIM  = MOTINT( ADRESS(1, 35) )
      IDHMA  = MOTINT( ADRESS(1, 36) )
      FRABI  = MOTINT( ADRESS(1, 37) )
      NPRIV  = MOTINT( ADRESS(1, 38) )
      FRABL  = MOTINT( ADRESS(1, 39) )
!     COORDINATES OF THE ORIGIN IN (X, Y)
      I_ORIG = MOTINT( ADRESS(1, 40) )
      J_ORIG = MOTINT( ADRESS(1, 40)+1 )
!     DEBUG KEYWORD
      DEBUG  = MOTINT( ADRESS(1, 41) )
! GQM PARAMETERS
      IQ_OM1 = MOTINT( ADRESS(1, 42) )
      NQ_TE1 = MOTINT( ADRESS(1, 43) )
      NQ_OM2 = MOTINT( ADRESS(1, 44) )
!
! Diffraction
      DIFFRA = MOTINT( ADRESS(1, 45) )
      NPTDIF = MOTINT( ADRESS(1, 46) )
! END diffraction
!
      DIAGHF = MOTINT( ADRESS(1, 47) )
!
!     OPTION FOR SECOND DERIVATIVES
!
      OPTDER = MOTINT( ADRESS(1, 48) )
!
!     49 IS PARALLEL PROCESSORS
!
!     PARALLEL ASSEMBLY MODE
!
      MODASS = MOTINT( ADRESS(1, 50) )
!
!     FOR BAJ MODELISATION
!     SEE NEXT BAJ FOR CONSEQUENCES ON PARAMETERS
      CBAJ   = MOTINT( ADRESS(1, 51) )
!
! REAL KEYWORDS
!
      DT     = MOTREA( ADRESS(2,  1) )
      F1     = MOTREA( ADRESS(2,  2) )
      RAISF  = MOTREA( ADRESS(2,  3) )
      IF(DIMEN(2,4).NE.DIMEN(2,5)) THEN
        WRITE(LU,*) 'ABSCISSAE AND ORDINATES OF SPECTRUM PRINTOUT'
        WRITE(LU,*) 'POINTS MUST BE GIVEN IN EQUAL NUMBERS'
        WRITE(LU,*) 'THERE ARE HERE',DIMEN(2,4),' ABCISSAE AND '
        WRITE(LU,*) DIMEN(2,5),' ORDINATES'
        CALL PLANTE(1)
        STOP
      ENDIF
      IF(TROUVE(2,4).EQ.2) THEN
        NPLEO = DIMEN(2,4)
      ELSE
!       IF KEYWORD NOT FOUND
        NPLEO = 0
      ENDIF
      ALLOCATE(NOLEO(NPLEO))
      ALLOCATE(XLEO(NPLEO))
      ALLOCATE(YLEO(NPLEO))
      DO K=1,DIMEN(2,4)
        XLEO(K)= MOTREA( ADRESS(2,  4) + K-1)
      ENDDO
      DO K=1,DIMEN(2,5)
        YLEO(K)= MOTREA( ADRESS(2,  5) + K-1)
      ENDDO
      DDC    = MOTREA( ADRESS(2,  6) )
      CFROT1 = MOTREA( ADRESS(2,  7) )
      CMOUT1 = MOTREA( ADRESS(2,  8) )
      CMOUT2 = MOTREA( ADRESS(2,  9) )
      ROAIR  = MOTREA( ADRESS(2, 10) )
      ROEAU  = MOTREA( ADRESS(2, 11) )
      BETAM  = MOTREA( ADRESS(2, 12) )
      XKAPPA = MOTREA( ADRESS(2, 13) )
      ALPHA  = MOTREA( ADRESS(2, 14) )
      DECAL  = MOTREA( ADRESS(2, 15) )
      ZVENT  = MOTREA( ADRESS(2, 16) )
      CDRAG  = MOTREA( ADRESS(2, 17) )
      HM0I   = MOTREA( ADRESS(2, 18) )
      FPIC   = MOTREA( ADRESS(2, 19) )
      GAMMA  = MOTREA( ADRESS(2, 20) )
      SIGMAA = MOTREA( ADRESS(2, 21) )
      SIGMAB = MOTREA( ADRESS(2, 22) )
      ALPHIL = MOTREA( ADRESS(2, 23) )
      FETCH  = MOTREA( ADRESS(2, 24) )
      FREMAX = MOTREA( ADRESS(2, 25) )
      TETA1  = MOTREA( ADRESS(2, 26) )*DEGRAD
      SPRED1 = MOTREA( ADRESS(2, 27) )
      TETA2  = MOTREA( ADRESS(2, 28) )*DEGRAD
      SPRED2 = MOTREA( ADRESS(2, 29) )
      XLAMDA = MOTREA( ADRESS(2, 30) )
      TAILF  = MOTREA( ADRESS(2, 31) )
      E2FMIN = MOTREA( ADRESS(2, 32) )
      ALFABJ = MOTREA( ADRESS(2, 33) )
      GAMBJ1 = MOTREA( ADRESS(2, 34) )
      GAMBJ2 = MOTREA( ADRESS(2, 35) )
      BORETG = MOTREA( ADRESS(2, 36) )
      GAMATG = MOTREA( ADRESS(2, 37) )
      ALFARO = MOTREA( ADRESS(2, 38) )
      GAMARO = MOTREA( ADRESS(2, 39) )
      GAM2RO = MOTREA( ADRESS(2, 40) )
      BETAIH = MOTREA( ADRESS(2, 41) )
      EM2SIH = MOTREA( ADRESS(2, 42) )
      COEFHS = MOTREA( ADRESS(2, 43) )
      XDTBRK = MOTREA( ADRESS(2, 44) )
      XLAMD  = MOTREA( ADRESS(2, 45) )
      ZREPOS = MOTREA( ADRESS(2, 46) )
      ALFLTA = MOTREA( ADRESS(2, 47) )
      RFMLTA = MOTREA( ADRESS(2, 48) )
      KSPB   = MOTREA( ADRESS(2, 49) )
      BDISPB = MOTREA( ADRESS(2, 50) )*DEGRAD
      BDSSPB = MOTREA( ADRESS(2, 51) )*DEGRAD
      HM0L   = MOTREA( ADRESS(2, 52) )
      FPICL  = MOTREA( ADRESS(2, 53) )
      SIGMAL = MOTREA( ADRESS(2, 54) )
      SIGMBL = MOTREA( ADRESS(2, 55) )
      APHILL = MOTREA( ADRESS(2, 56) )
      FETCHL = MOTREA( ADRESS(2, 57) )
      FPMAXL = MOTREA( ADRESS(2, 58) )
      TETA1L = MOTREA( ADRESS(2, 59) )*DEGRAD
      SPRE1L = MOTREA( ADRESS(2, 60) )
      TETA2L = MOTREA( ADRESS(2, 61) )*DEGRAD
      SPRE2L = MOTREA( ADRESS(2, 62) )
      XLAMDL = MOTREA( ADRESS(2, 63) )
      GAMMAL = MOTREA( ADRESS(2, 64) )
      PROMIN = MOTREA( ADRESS(2, 65) )
      VX_CTE = MOTREA( ADRESS(2, 66) )
      VY_CTE = MOTREA( ADRESS(2, 67) )
      CIMPLI = MOTREA( ADRESS(2, 68) )
      COEFWD = MOTREA( ADRESS(2, 69) )
      COEFWE = MOTREA( ADRESS(2, 70) )
      COEFWF = MOTREA( ADRESS(2, 71) )
      COEFWH = MOTREA( ADRESS(2, 72) )
      CMOUT3 = MOTREA( ADRESS(2, 73) )
      CMOUT4 = MOTREA( ADRESS(2, 74) )
      CMOUT5 = MOTREA( ADRESS(2, 75) )
      CMOUT6 = MOTREA( ADRESS(2, 76) )
      SEUIL  = MOTREA( ADRESS(2, 77) )
      SEUIL1 = MOTREA( ADRESS(2, 78) )
      SEUIL2 = MOTREA( ADRESS(2, 79) )
      F2DIFM = MOTREA( ADRESS(2, 80) )
!     TIME UNITS IN FILES
      UNITCOB= MOTREA( ADRESS(2, 81) )
      UNITMAB= MOTREA( ADRESS(2, 82) )
      UNITVEB= MOTREA( ADRESS(2, 83) )
      UNITSPE= MOTREA( ADRESS(2, 88) )
!     TIME SHIFTS IN FILES
      PHASCOB= MOTREA( ADRESS(2, 84) )
      PHASMAB= MOTREA( ADRESS(2, 85) )
      PHASVEB= MOTREA( ADRESS(2, 86) )
      PHASSPE= MOTREA( ADRESS(2, 89) )
!     DISSIPATION COEFFICIENT FOR STRONG CURRENT
      CDSCUR = MOTREA( ADRESS(2, 87) )
      SMAX   = MOTREA( ADRESS(2, 90) )
      COEREF = MOTREA( ADRESS(2, 91) )
!     VEGETATION
      COEVEG = MOTREA( ADRESS(2, 92) )
      NUMPLAM2 = MOTREA( ADRESS(2, 93) )
      STEMDIAM = MOTREA( ADRESS(2, 94) )
      BDC    = MOTREA( ADRESS(2, 95) )
      VEGH   = MOTREA( ADRESS(2, 96) )
!
! LOGICAL KEYWORDS
!
      TSOU   = MOTLOG( ADRESS(3,  1) )
      SPHE   = MOTLOG( ADRESS(3,  2) )
      PROINF = MOTLOG( ADRESS(3,  5) )
      COUSTA = MOTLOG( ADRESS(3,  6) )
      VENT   = MOTLOG( ADRESS(3,  7) )
      DONTEL = MOTLOG( ADRESS(3,  8) )
      PROP   = MOTLOG( ADRESS(3,  9) )
      VENSTA = MOTLOG( ADRESS(3, 10) )
      VALID  = MOTLOG( ADRESS(3, 11) )
      MAREE  = MOTLOG( ADRESS(3, 12) )
      TRIGO  = MOTLOG( ADRESS(3, 13) )
      SPEULI = MOTLOG( ADRESS(3, 14) )
      FLTDIF = MOTLOG( ADRESS(3, 15) )
      RAZTIM = MOTLOG( ADRESS(3, 16) )
      VEGETATION = MOTLOG( ADRESS(3, 17) )
      CHECK_MESH = MOTLOG( ADRESS(3, 18) )
      SOURCE_ON_BND = MOTLOG( ADRESS(3, 19) )
      AVANT = MOTLOG( ADRESS(3, 20) )
      ECRET = MOTLOG( ADRESS(3, 21) )
      POROUS =  MOTLOG( ADRESS(3, 22) )
      PARTEL_CONCAT = MOTLOG( ADRESS(3,23) )
      IF(NCSIZE.LE.1) PARTEL_CONCAT=.FALSE.
      REFLECTION = MOTLOG( ADRESS(3, 24) )
!
! STRING KEYWORDS
!
      TITCAS = MOTCAR( ADRESS(4, 1) ) (1:72)
      SORT2D = MOTCAR( ADRESS(4, 2) ) (1:250)
!
! FILES IN THE STEERING FILE
!
      WAC_FILES(WACGEO)%NAME=MOTCAR( ADRESS(4,3) )
      IF(WAC_FILES(WACGEO)%NAME(1:1).EQ.' ') THEN
        WRITE(LU,*) 'THE FOLLOWING KEYWORD IS MANDATORY:'
        WRITE(LU,*) 'GEOMETRY FILE (FICHIER DE GEOMETRIE)'
        CALL PLANTE(1)
        STOP
      ENDIF
!     NOMFOR = MOTCAR( ADRESS(4, 4) )
!     NOMCAS = MOTCAR( ADRESS(4, 5) )
      WAC_FILES(WACCLI)%NAME=MOTCAR( ADRESS(4,6) )
      IF(WAC_FILES(WACCLI)%NAME(1:1).EQ.' ') THEN
        WRITE(LU,*) 'THE FOLLOWING KEYWORD IS MANDATORY:'
        WRITE(LU,*) 'BOUNDARY CONDITIONS FILE '//
     &              '(FICHIER DES CONDITIONS AUX LIMITES)'
        CALL PLANTE(1)
        STOP
      ENDIF
      WAC_FILES(WACFON)%NAME=MOTCAR( ADRESS(4,7) )
      WAC_FILES(WACRES)%NAME=MOTCAR( ADRESS(4,8) )
      WAC_FILES(WACLEO)%NAME=MOTCAR( ADRESS(4,9) )
      WAC_FILES(WACPRE)%NAME=MOTCAR( ADRESS(4,10) )
      WAC_FILES(WACRBI)%NAME=MOTCAR( ADRESS(4,11) )
      WAC_FILES(WACCOB)%NAME=MOTCAR( ADRESS(4,12) )
      WAC_FILES(WACCOF)%NAME=MOTCAR( ADRESS(4,13) )
      WAC_FILES(WACBI1)%NAME=MOTCAR( ADRESS(4,14) )
      WAC_FILES(WACFO1)%NAME=MOTCAR( ADRESS(4,15) )
      WAC_FILES(LEOWXY)%NAME=MOTCAR( ADRESS(4,23) )
      WAC_FILES(LEOIXY)%NAME=MOTCAR( ADRESS(4,24) )
      WAC_FILES(IMPSPE)%NAME=MOTCAR( ADRESS(4,25) )
      WAC_FILES(WACZON)%NAME=MOTCAR( ADRESS(4,27) )
      IF( WAC_FILES(WACRBI)%NAME.NE.' ') THEN
! ONE WANTS TO HAVE A GLOBAL RESULT
        GLOB=.TRUE.
        IF (INCLUS(COUPLING,'TOMAWAC')) THEN
          IF(ABS(NIT*DT-CPL_WAC_DATA%NIT_TEL*CPL_WAC_DATA%DT_TEL)
     &          .GE.1D-6)THEN
            WRITE(LU,*) NIT,DT,CPL_WAC_DATA%NIT_TEL,CPL_WAC_DATA%DT_TEL
            WRITE(LU,*)'WHEN ONE WANTS A GLOBAL RESULT WITH COUPLING'
            WRITE(LU,*)'DURATION OF TELEMAC AND TOMAWAC MUST',
     &                 'BE THE SAME'
            CALL PLANTE(1)
          ENDIF
          IF(MOD(NIT,CPL_WAC_DATA%PERCOU_WAC).NE.0)THEN
            WRITE(LU,*)'WHEN ONE WANTS A GLOBAL RESULT WITH COUPLING'
            WRITE(LU,*)'NUMBER OF TIME STEP OF TOMAWAC MUST',
     &                 'BE A MULTIPLE OF COUPLING PERIOD'
            CALL PLANTE(1)
          ENDIF
        ENDIF
      ELSE
        GLOB=.FALSE.
      ENDIF
      IF( WAC_FILES(WACPRE)%NAME.NE.' ') THEN
! ONE WANTS TO HAVE A GLOBAL RESULT
        SUIT=.TRUE.
      ELSE
        SUIT=.FALSE.
      ENDIF
      WAC_FILES(WACVEB)%NAME=MOTCAR( ADRESS(4,31) )
      WAC_FILES(WACVEF)%NAME=MOTCAR( ADRESS(4,32) )
      IF(PART.NE.-1.AND.(NAMVEB.NE.' '.OR.NAMVEF.NE.' ')) THEN
        WRITE(LU,*) 'WHEN YOU COUPLE WITH TELEMAC, ', 
     &       'YOU CAN NOT HAVE A WIND FILE'
        WRITE(LU,*) 'DELETE THE WIND FILE IN YOUR STEERING FILE'
        CALL PLANTE(1)
      ENDIF
      WAC_FILES(WACREF)%NAME=MOTCAR( ADRESS(4,34) )
      WAC_FILES(WACMAB)%NAME=MOTCAR( ADRESS(4,35) )
      WAC_FILES(WACMAF)%NAME=MOTCAR( ADRESS(4,36) )
      EQUA   = 'TOMAWAC-COWADIS'
!BD_INCKA FILE FORMATS
!     GEOMETRY FILE
      WAC_FILES(WACGEO)%FMT = MOTCAR( ADRESS(4,39) )(1:8)
      CALL MAJUS(WAC_FILES(WACGEO)%FMT)
!     RESULTS FILE FORMAT
      WAC_FILES(WACRES)%FMT = MOTCAR( ADRESS(4,17) )(1:8)
      CALL MAJUS(WAC_FILES(WACRES)%FMT)
!     INITIAL RESULTS FILE FORMAT (< PREVIOUS COMPUTATION)
!     SEDIMENT...
      WAC_FILES(WACPRE)%FMT = MOTCAR( ADRESS(4,41) )(1:8)
      CALL MAJUS(WAC_FILES(WACPRE)%FMT)
!     REFERENCE FILE FORMAT
      WAC_FILES(WACREF)%FMT = MOTCAR( ADRESS(4,42) )(1:8)
      CALL MAJUS(WAC_FILES(WACREF)%FMT)
!     BINARY FILE 1 FORMAT
      WAC_FILES(WACBI1)%FMT = MOTCAR( ADRESS(4,43) )(1:8)
      CALL MAJUS(WAC_FILES(WACBI1)%FMT)
!     SPECTRAL FILE FORMAT
      WAC_FILES(WACLEO)%FMT = MOTCAR( ADRESS(4,44) )(1:8)
      CALL MAJUS(WAC_FILES(WACLEO)%FMT)
!     GLOBAL RESULT FILE FORMAT
      WAC_FILES(WACRBI)%FMT = MOTCAR( ADRESS(4,51) )(1:8)
      CALL MAJUS(WAC_FILES(WACRBI)%FMT)
!     BINARY CURRENTS FILE FORMAT
      WAC_FILES(WACCOB)%FMT = MOTCAR( ADRESS(4,40) )(1:8)
      CALL MAJUS(WAC_FILES(WACCOB)%FMT)
!     BINARY WINDS FILE FORMAT
      WAC_FILES(WACVEB)%FMT = MOTCAR( ADRESS(4,46) )(1:8)
      CALL MAJUS(WAC_FILES(WACVEB)%FMT)
!     BINARY TIDAL WATER FILE FORMAT
      WAC_FILES(WACMAB)%FMT = MOTCAR( ADRESS(4,47) )(1:8)
      CALL MAJUS(WAC_FILES(WACMAB)%FMT)
!     IMPOSED SPECTRA FILE FORMAT
      WAC_FILES(IMPSPE)%FMT = MOTCAR( ADRESS(4,26) )(1:8)
      CALL MAJUS(WAC_FILES(IMPSPE)%FMT)

!
!     NAMES OF VARIABLES
!
      NAMEU =MOTCAR( ADRESS(4,45)   )(1:32)
      NAMEV =MOTCAR( ADRESS(4,45)+1 )(1:32)
      NAMEWX=MOTCAR( ADRESS(4,45)+2 )(1:32)
      NAMEWY=MOTCAR( ADRESS(4,45)+3 )(1:32)
      NAMEH =MOTCAR( ADRESS(4,45)+4 )(1:32)
!
      WAC_FILES(WACSPE)%NAME=MOTCAR( ADRESS(4,50) )
!
!     CORRECTS OR COMPUTES OTHER PARAMETERS FROM THOSE THAT
!     HAVE JUST BEEN READ
!
      IF(COUSTA.OR.MAREE) THEN
        COURAN=.TRUE.
      ELSE
        COURAN=.FALSE.
      ENDIF
      IF(.NOT.VENT.AND.SVENT.NE.0) THEN
        WRITE(LU,*)
     &     'INCOMPATIBILITY OF KEY-WORDS CONCERNING WIND => NO
     &      WIND'
        SVENT=0
      ENDIF
      IF(TRIGO) THEN
        TETA1  = PISUR2-TETA1
        TETA2  = PISUR2-TETA2
        TETA1L = PISUR2-TETA1L
        TETA2L = PISUR2-TETA2L
        BDISPB = PISUR2-BDISPB
        BDSSPB = PISUR2-BDSSPB
      ENDIF
      IF(CIMPLI.LT.0.OR.CIMPLI.GT.1) THEN
        WRITE(LU,*) 'INCOMPATIBILITY OF IMPLICITATION COEFFICIENT'
        WRITE(LU,*) 'VALUE READ = ',CIMPLI
        WRITE(LU,*) 'WE TAKE THE DEFAULT VALUE CIMPLI=0.5'
        CIMPLI=0.5D0
      ENDIF
!     NEW SOURCE TERMS
      IF(.NOT.PROINF.AND.STRIF.EQ.3) THEN
        WRITE(LU,*) 'INCOMPATIBILITY OF DEPTH AND'
        WRITE(LU,*) 'NON-LINEAR TRANSFER TERM'
        CALL PLANTE(1)
        STOP
      ENDIF
      IF(CBAJ.EQ.1) THEN
        LIMIT=3
!     IT used to include non linear transfert =1
!     Now user has to set a non linear Law. 
!       STRIF=1
        SVENT=1
        SMOUT=1
        CMOUT1=2.1D0
        CMOUT2=0.4D0
        ALPHA=0.0095D0
      ENDIF
!
      IF ((NPLEO.EQ.0.AND.NAMWXY(1:1).EQ.' ' ).AND.
     &     (NAMLEO(1:1).NE.' '.OR.NAMSPE(1:1).NE.' ')) THEN

        WRITE(LU,*) ''
        WRITE(LU,*) '********************************************'
        WRITE(LU,*) ' A PUNCTUAL NAME FILE HAS BEEN DEFINED      '
        WRITE(LU,*) ' BUT NO COORDINATE HAS BEEN DEFINED         '
        WRITE(LU,*) '********************************************'
        CALL PLANTE(1)
      ENDIF
      IF ((NPLEO.GT.0.OR.NAMWXY(1:1).NE.' ' ) .AND.
     &     NAMLEO(1:1).EQ.' '.AND.NAMSPE(1:1).EQ.' ') THEN
        WRITE(LU,*) ''
        WRITE(LU,*) '********************************************'
        WRITE(LU,*) ' SOME COORDINATES HAVE  BEEN DEFINED        '
        WRITE(LU,*) ' BUT NO  PUNCTUAL NAME FILE HAS BEEN DEFINED'
        WRITE(LU,*) '********************************************'
        CALL PLANTE(1)
      ENDIF

!
!
!-----------------------------------------------------------------------
!  NAME OF THE VARIABLES FOR THE RESULTS AND GEOMETRY FILES:
!-----------------------------------------------------------------------
!
! LOGICAL ARRAY FOR OUTPUT
!
      CALL NOMVAR_TOMAWAC(TEXTE,MNEMO,MAXVAR)
!
      CALL SORTIE(SORT2D , MNEMO , MAXVAR , SORLEO )
!
!.....IF NO WIND, THERE SHOULD BE NO INFORMATION WRITTEN ABOUT WINDS
      IF (.NOT.VENT) THEN
        SORLEO( 9) = .FALSE.
        SORLEO(10) = .FALSE.
        SORLEO(25) = .FALSE.
        SORLEO(26) = .FALSE.
        SVENT = 0
      ENDIF
!.....IF NO CURRENT, THERE SHOULD BE NO INFORMATION WRITTEN ABOUT COURANT
      IF (.NOT.COURAN.AND..NOT.INCLUS(COUPLING,'TOMAWAC')) THEN
        SORLEO(7)=.FALSE.
        SORLEO(8)=.FALSE.
        IF (SDSCU.NE.0) THEN
! AND NO DISSIPATION COEFFICIENT FOR STRONG CURRENT
          WRITE(LU,*) '*****************************************'
          WRITE(LU,*) ' NO DISSIPATION FOR STRONG CURRENT'
          WRITE(LU,*) '*****************************************'
          SDSCU = 0
        ENDIF
      ENDIF
!
!.....IF INFINITE DEPTH, THE RADIATION STRESSES ARE NOT COMPUTED
      IF (PROINF) THEN
        IF (SORLEO(11) .OR. SORLEO(12) .OR. SORLEO(13) .OR.
     &      SORLEO(14) .OR. SORLEO(15) ) THEN
          WRITE(LU,*) '*****************************************'
          WRITE(LU,*) '   RADIATION STRESSES ARE NOT COMPUTED  '
          WRITE(LU,*) '       OVER INFINITE WATER DEPTHS       '
          WRITE(LU,*) '******************************************'
          DO K=11,15
            SORLEO(K) = .FALSE.
          ENDDO
        ENDIF
!       LA HAUTEUR D'EAU ET LA PROFONDEUR NE DOIVENT PAS ETRE CALCULEE
!       WATER DEPTH AND DEPTH MUST NOT BE COMPUTED
        SORLEO(5) = .FALSE.
        SORLEO(6) = .FALSE.
!     DEPTH BREAKING = 0
        SBREK = 0
        SORLEO(35) = .FALSE.
      ENDIF
      IF (SMOUT.EQ.0) SORLEO(36) = .FALSE.
!     
      DO K=1,MAXVAR
        SORIMP(K)=.FALSE.
      ENDDO
!
!-----------------------------------------------------------------------
!
      RETURN
      END
