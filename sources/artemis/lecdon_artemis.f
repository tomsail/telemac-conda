!                   *************************
                    SUBROUTINE LECDON_ARTEMIS
!                   *************************
!
     &(FILE_DESC,PATH,NCAR,
     & CAS_FILE,DICO_FILE)
!
!***********************************************************************
! ARTEMIS   V8P2
!***********************************************************************
!
!brief    READS THE STEERING FILE THROUGH A DAMOCLES CALL.
!
!history  J-M HERVOUET (LNH)
!+        17/08/1994
!+
!+   LINKED TO BIEF 5.0
!
!history  D. AELBRECHT (LNH)
!+        20/04/1999
!+        V6P0
!+
!
!history  C/PEYRARD (EDF)
!+        18/03/2014
!+        V6P1 - V7P0
!+   NEW KEY WORDS
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
!history  N.DURAND (HRW)
!+        November 2016
!+        V7P2
!+   Addition of new keywords in the steering file re: animation of the
!+   free surface. ANIMFS and ART_FILES(ARTAMP)%NAME read from file
!
!history  N.DURAND (HRW)
!+        August 2017
!+        V7P3
!+   1. Consolidation of nesting methods, meaning that CHAINTWC is now a
!+   choice of integers, and no longer a logical
!+   2. NDTWC (now NDIR) and NFTWC (now NF) are no longer read in the
!+   steering file but directly extracted from the spectrum file
!+   3. Addition of various checks before simulation can start
!+   4. Call to ARTEMIS_CONSTANTS added.
!+   5. Addition of new keywords in the steering file for nesting option 2:
!+   X_SFREF, Y_SFREF, ART_FILES(WACRES)%NAME
!
!history  N.DURAND (HRW)
!+        December 2018
!+        V8P0
!+   Addition of new keywords in the steering file for nesting option 2:
!+   ART_FILES(WACLQD)%NAME
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| FILE_DESC      |<--| STORES STRINGS 'SUBMIT' OF DICTIONARY
!| NCAR           |-->| NUMBER OF LETTERS IN STRING PATH
!| PATH           |-->| FULL PATH TO CODE DICTIONARY
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE DECLARATIONS_SPECIAL
      USE DECLARATIONS_TELEMAC
      USE DECLARATIONS_ARTEMIS
      USE INTERFACE_ARTEMIS, EX_LECDON_ARTEMIS => LECDON_ARTEMIS
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      CHARACTER(LEN=PATH_LEN), INTENT(INOUT) :: FILE_DESC(4,MAXKEYWORD)
      INTEGER, INTENT(IN)               :: NCAR
      CHARACTER(LEN=PATH_LEN), INTENT(IN)    :: PATH
!     API
      CHARACTER(LEN=PATH_LEN), INTENT(IN)    :: DICO_FILE
      CHARACTER(LEN=PATH_LEN), INTENT(IN)    :: CAS_FILE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      LOGICAL LSTOP
      INTEGER I,KK
!
      CHARACTER(LEN=8) MNEMO(MAXVAR)
!
!-----------------------------------------------------------------------
!
! ARRAYS USED IN THE DAMOCLES CALL
!
      INTEGER ADRESS(4,MAXKEYWORD),DIMENS(4,MAXKEYWORD)
      DOUBLE PRECISION MOTREA(MAXKEYWORD)
      INTEGER          MOTINT(MAXKEYWORD)
      LOGICAL          MOTLOG(MAXKEYWORD)
      CHARACTER(LEN=PATH_LEN) MOTCAR(MAXKEYWORD)
      CHARACTER(LEN=72)  MOTCLE(4,MAXKEYWORD,2)
      INTEGER          TROUVE(4,MAXKEYWORD)
      LOGICAL DOC
      CHARACTER(LEN=PATH_LEN) :: NOM_CAS
      CHARACTER(LEN=PATH_LEN) :: NOM_DIC
      INTEGER :: ID_DICO, ID_CAS
! EXTRA DECLARATIONS FOR OUTPUT OF AMPLITUDE AND PHASE FILE
      CHARACTER(LEN=2)                  :: CLT      ! PERIOD COMPONENT NUMBER
!
! END OF DECLARATIONS FOR DAMOCLES CALL :
!
!-----------------------------------------------------------------------
!
! INITIALISES THE VARIABLES FOR DAMOCLES CALL :
!
      DO KK=1,MAXKEYWORD
!
!       A FILENAME NOT GIVEN BY DAMOCLES WILL BE RECOGNIZED AS A WHITE SPACE
!       (IT MAY BE THAT NOT ALL COMPILERS WILL INITIALISE LIKE THAT)
!
        MOTCAR(KK)(1:1)=' '
!
        DIMENS(1,KK) = 0
        DIMENS(2,KK) = 0
        DIMENS(3,KK) = 0
        DIMENS(4,KK) = 0
!
      ENDDO
!
!     WRITES OUT INFO
!
      DOC = .FALSE.
!
!-----------------------------------------------------------------------
!     OPENS DICTIONNARY AND STEERING FILES
!-----------------------------------------------------------------------
!
      IF(NCAR.GT.0) THEN
!
        NOM_DIC=PATH(1:NCAR)//'ARTDICO'
        NOM_CAS=PATH(1:NCAR)//'ARTCAS'
!
      ELSE
!
        NOM_DIC='ARTDICO'
        NOM_CAS='ARTCAS'
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
      CALL DAMOCLE( ADRESS , DIMENS , MAXKEYWORD, DOC, LNG    , LU ,
     &              MOTINT , MOTREA , MOTLOG , MOTCAR  , MOTCLE ,
     &              TROUVE , ID_DICO, ID_CAS , .FALSE. , FILE_DESC)
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
      CALL READ_SUBMIT(ART_FILES,MAXLU_ART,FILE_DESC,MAXKEYWORD)
!
!-----------------------------------------------------------------------
!
      DO I=1,MAXLU_ART
        IF(ART_FILES(I)%TELNAME.EQ.'ARTGEO') THEN
          ARTGEO=I
        ELSEIF(ART_FILES(I)%TELNAME.EQ.'ARTCAS') THEN
          ARTCAS=I
        ELSEIF(ART_FILES(I)%TELNAME.EQ.'ARTCLI') THEN
          ARTCLI=I
        ELSEIF(ART_FILES(I)%TELNAME.EQ.'ARTFON') THEN
          ARTFON=I
        ELSEIF(ART_FILES(I)%TELNAME.EQ.'ARTRES') THEN
          ARTRES=I
        ELSEIF(ART_FILES(I)%TELNAME.EQ.'ARTREF') THEN
          ARTREF=I
        ELSEIF(ART_FILES(I)%TELNAME.EQ.'ARTBI1') THEN
          ARTBI1=I
        ELSEIF(ART_FILES(I)%TELNAME.EQ.'ARTBI2') THEN
          ARTBI2=I
        ELSEIF(ART_FILES(I)%TELNAME.EQ.'ARTFO1') THEN
          ARTFO1=I
        ELSEIF(ART_FILES(I)%TELNAME.EQ.'ARTFO2') THEN
          ARTFO2=I
        ELSEIF(ART_FILES(I)%TELNAME.EQ.'ARTRBI') THEN
          ARTRBI=I
        ELSEIF(ART_FILES(I)%TELNAME.EQ.'ARTRFO') THEN
          ARTRFO=I
        ELSEIF(ART_FILES(I)%TELNAME.EQ.'ARTAMP') THEN
          ARTAMP=I
        ELSEIF(ART_FILES(I)%TELNAME.EQ.'WACSPE') THEN
          WACSPE=I
        ELSEIF(ART_FILES(I)%TELNAME.EQ.'WACRES') THEN
          WACRES=I
        ELSEIF(ART_FILES(I)%TELNAME.EQ.'WACLQD') THEN
          WACLQD=I
        ENDIF
      ENDDO
!
!-----------------------------------------------------------------------
!
!     SETTING CONSTANTS (PI AND RELATED)
!
      CALL ARTEMIS_CONSTANTS
!
!-----------------------------------------------------------------------
!
! ASSIGNS THE STEERING FILE VALUES TO THE PARAMETER FORTRAN NAME :
!
!-----------------------------------------------------------------------
!
! INTEGER KEYWORDS :
!
      LEOPRD    = MOTINT( ADRESS(1, 1) )
      LISPRD    = MOTINT( ADRESS(1, 2) )
      SLVART%NITMAX    = MOTINT( ADRESS(1,3) )
      SLVART%PRECON    = MOTINT( ADRESS(1,4) )
!     DISESP    = MOTINT( ADRESS(1, 5) )
!     STDGEO    = MOTINT( ADRESS(1, 6) )
!     STDRES    = MOTINT( ADRESS(1, 7) )
      SLVART%SLV = MOTINT( ADRESS(1,8) )
      LISFON    = MOTINT( ADRESS(1, 9) )
      NPALE     = MOTINT( ADRESS(1,10) )
      NDALE     = MOTINT( ADRESS(1,11) )
      IBREAK    = MOTINT( ADRESS(1,12) )
      NITDIS    = MOTINT( ADRESS(1,13) )
      REGIDO    = MOTINT( ADRESS(1,14) )
      FORMFR    = MOTINT( ADRESS(1,15) )
      SLVART%KRYLOV = MOTINT( ADRESS(1,16) )
      LVMAC     = MOTINT( ADRESS(1,17) )
!     KFROT     = MOTINT( ADRESS(1,18) )
      OPTASS    = MOTINT( ADRESS(1,19) )
      PRODUC    = MOTINT( ADRESS(1,20) )
      MARDAT(1) = MOTINT( ADRESS(1,21) )
      MARDAT(2) = MOTINT( ADRESS(1,21) + 1 )
      MARDAT(3) = MOTINT( ADRESS(1,21) + 2 )
      MARTIM(1) = MOTINT( ADRESS(1,22) )
      MARTIM(2) = MOTINT( ADRESS(1,22) + 1 )
      MARTIM(3) = MOTINT( ADRESS(1,22) + 2 )
      NPRIV     = MOTINT( ADRESS(1,23) )
!     PLACEHOLDER FOR NCSIZE:
!     NCSIZE    = MOTINT( ADRESS(1,24) )
!     ORIGIN COORDINATES
      I_ORIG    = MOTINT( ADRESS(1,25)   )
      J_ORIG    = MOTINT( ADRESS(1,25)+1 )
!     RAPIDLY VARYING TOPOGRAPHY
      IPENTCO   = MOTINT( ADRESS(1,26) )
!     MAX ITERATION ON TETAP
      NITTP   = MOTINT( ADRESS(1,27) )
!     DEBUGGER KEYWORD
      DEBUG   = MOTINT( ADRESS(1,28) )
!     2 PLACEHOLDERS FOR NITFS AND NADVAR:
!     NITFS   = MOTINT( ADRESS(1,29) )
!     NADVAR  = MOTINT( ADRESS(1,30) )
      CHAINTWC= MOTINT( ADRESS(1,31) )

! FOR THE MOMENT WTITES TO FILE FROM THE START OF SIMULATION
      PTINIG    = 0
      PTINIL    = 0
!
! REAL KEYWORDS :
!
      PER       = MOTREA( ADRESS(2, 1) )
      TETAH     = MOTREA( ADRESS(2, 2) )
      GRAV      = MOTREA( ADRESS(2, 3) )
      SLVART%ZERO = MOTREA( ADRESS(2, 4) )
      SLVART%EPS = MOTREA( ADRESS(2, 5) )
!     HMIN      = MOTREA( ADRESS(2, 6) )
      COTINI    = MOTREA( ADRESS(2, 7) )
      HAUTIN    = MOTREA( ADRESS(2, 8) )
      PERDEB    = MOTREA( ADRESS(2, 9) )
      PERFIN    = MOTREA( ADRESS(2,10) )
      PERPAS    = MOTREA( ADRESS(2,11) )
      PERPIC    = MOTREA( ADRESS(2,12) )
      GAMMA     = MOTREA( ADRESS(2,13) )
      TETMIN    = MOTREA( ADRESS(2,14) )
      TETMAX    = MOTREA( ADRESS(2,15) )
      EXPOS     = MOTREA( ADRESS(2,16) )
!     RELAX     = MOTREA( ADRESS(2,17) )
      EPSDIS    = MOTREA( ADRESS(2,18) )
      RELDIS    = MOTREA( ADRESS(2,19) )
      ALFABJ    = MOTREA( ADRESS(2,20) )
      GAMMAS    = MOTREA( ADRESS(2,21) )
      KDALLY    = MOTREA( ADRESS(2,22) )
      GDALLY    = MOTREA( ADRESS(2,23) )
      VISCO     = MOTREA( ADRESS(2,24) )
      DIAM90    = MOTREA( ADRESS(2,25) )
      DIAM50    = MOTREA( ADRESS(2,26) )
      MVSED     = MOTREA( ADRESS(2,27) )
      MVEAU     = MOTREA( ADRESS(2,28) )
      FWCOEF    = MOTREA( ADRESS(2,29) )
      RICOEF    = MOTREA( ADRESS(2,30) )
      FFON      = MOTREA( ADRESS(2,31) )
      PMIN      = MOTREA( ADRESS(2,32) )
      PMAX      = MOTREA( ADRESS(2,33) )
      DEPREF    = MOTREA( ADRESS(2,34) )
      X_PHREF   = MOTREA( ADRESS(2,35) )
      Y_PHREF   = MOTREA( ADRESS(2,35)+1 )
      EPSDIR    = MOTREA( ADRESS(2,36) )
      EPSTP     = MOTREA( ADRESS(2,37) )
      RELTP     = MOTREA( ADRESS(2,38) )
      TPSTWC    = MOTREA( ADRESS(2,39) )
!     PLACEHOLDERS FOR TINIFS AND DTFS:
!     TINIFS    = MOTREA( ADRESS(2,40) )
!     DTFS      = MOTREA( ADRESS(2,41) )
!     FOR THE F SPECTRUM REFERENCE POINT
      X_SFREF   = MOTREA( ADRESS(2,42) )
      Y_SFREF   = MOTREA( ADRESS(2,42)+1 )
!
! LOGICAL KEYWORDS :
!
      LISTIN    = MOTLOG( ADRESS(3, 1) )
      INFOGR    = MOTLOG( ADRESS(3, 2) )
      BALAYE    = MOTLOG( ADRESS(3, 3) )
      ALEMON    = MOTLOG( ADRESS(3, 4) )
      ALEMUL    = MOTLOG( ADRESS(3, 5) )
      DEFERL    = MOTLOG( ADRESS(3, 6) )
      FROTTE    = MOTLOG( ADRESS(3, 7) )
      ENTFW     = MOTLOG( ADRESS(3, 8) )
      ENTREG    = MOTLOG( ADRESS(3, 9) )
      ENTRUG    = MOTLOG( ADRESS(3, 10) )
      LISHOU    = MOTLOG( ADRESS(3, 11) )
      VALID     = MOTLOG( ADRESS(3, 12) )
      COURANT   = MOTLOG( ADRESS(3, 13) )
      LANGAUTO  = MOTLOG( ADRESS(3, 14) )
      LPHASEAUTO= MOTLOG( ADRESS(3, 15) )
!     SPHERICAL EQUATIONS, HARD-CODED
      SPHERI    = .FALSE.
      CHECK_MESH= MOTLOG( ADRESS(3, 16) )
      ANIMFS    = MOTLOG( ADRESS(3, 17) )
      PARTEL_CONCAT = MOTLOG(ADRESS(3,18))
      IF(NCSIZE.LE.1) PARTEL_CONCAT=.FALSE.!
! STRING KEYWORDS : SOME ARE USED BY THE LAUNCHING
!                   PROCEDURE
!
      TITCAS    = MOTCAR( ADRESS(4, 1) )(1:72)
      VARDES    = MOTCAR( ADRESS(4, 2) )(1:72)
      CALL MAJUS(VARDES)
      VARIMP    = MOTCAR( ADRESS(4, 3) )(1:72)
      CALL MAJUS(VARIMP)
!     FROM 4 TO 5: READ AND USED BY PRECOS
      ART_FILES(ARTGEO)%NAME=MOTCAR( ADRESS(4,6) )
      IF(ART_FILES(ARTGEO)%NAME(1:1).EQ.' ') THEN
        WRITE(LU,*) 'THE FOLLOWING KEYWORD IS MANDATORY:'
        WRITE(LU,*) 'GEOMETRY FILE (FICHIER DE GEOMETRIE)'
        CALL PLANTE(1)
        STOP
      ENDIF
!     PLACEHOLDERS:
!     NOMFOR    = MOTCAR( ADRESS(4, 7) )
!     NOMCAS    = MOTCAR( ADRESS(4, 8) )
!     ART_FILES(ARTCAS)%NAME=MOTCAR( ADRESS(4,8) )
      ART_FILES(ARTCLI)%NAME=MOTCAR( ADRESS(4,9) )
      IF(ART_FILES(ARTCLI)%NAME(1:1).EQ.' ') THEN
        WRITE(LU,*) 'THE FOLLOWING KEYWORD IS MANDATORY:'
        WRITE(LU,*) 'BOUNDARY CONDITIONS FILE '//
     &              '(FICHIER DES CONDITIONS AUX LIMITES)'
        CALL PLANTE(1)
        STOP
      ENDIF
!      WRITE(*,*) 'IN LECDON ',ART_FILES(ARTGEO)%NAME
      ART_FILES(ARTRES)%NAME=MOTCAR( ADRESS(4,10) )
      IF(ART_FILES(ARTRES)%NAME(1:1).EQ.' ') THEN
        WRITE(LU,*) 'THE FOLLOWING KEYWORD IS MANDATORY:'
        WRITE(LU,*) 'RESULTS FILE (FICHIER DES RESULTATS)'
        CALL PLANTE(1)
        STOP
      ENDIF
!     FROM 11 TO 14 : READ AND USED BY PRECOS
      ART_FILES(ARTFON)%NAME=MOTCAR( ADRESS(4,15) )
      ART_FILES(ARTBI1)%NAME=MOTCAR( ADRESS(4,16) )
      ART_FILES(ARTBI2)%NAME=MOTCAR( ADRESS(4,17) )
      ART_FILES(ARTFO1)%NAME=MOTCAR( ADRESS(4,18) )
      ART_FILES(ARTFO2)%NAME=MOTCAR( ADRESS(4,19) )
      ART_FILES(ARTRBI)%NAME=MOTCAR( ADRESS(4,20) )
      ART_FILES(ARTRFO)%NAME=MOTCAR( ADRESS(4,21) )
!     FROM 22 TO 23 : READ AND USED BY PRECOS OR NOT USED
      CDTINI    = MOTCAR( ADRESS(4,24) )(1:72)
      CALL MAJUS(CDTINI)
!     BINGEO    = MOTCAR( ADRESS(4,25) )(1:3)
!     CALL MAJUS(BINGEO)
!     BINRES    = MOTCAR( ADRESS(4,26) )(1:3)
!     CALL MAJUS(BINRES)
!     ACCOUNTNUMBER    = MOTCAR( ADRESS(4, 27) )
      ART_FILES(ARTREF)%NAME=MOTCAR( ADRESS(4,28) )
!     GEOMETRY FILE FORMAT
      ART_FILES(ARTGEO)%FMT = MOTCAR( ADRESS(4,29) )(1:8)
      CALL MAJUS(ART_FILES(ARTGEO)%FMT)
!     RESULTS FILE FORMAT
      ART_FILES(ARTRES)%FMT = MOTCAR( ADRESS(4,30) )(1:8)
      CALL MAJUS(ART_FILES(ARTRES)%FMT)
!     REFERENCE FILE FORMAT
      ART_FILES(ARTREF)%FMT = MOTCAR( ADRESS(4,31) )(1:8)
      CALL MAJUS(ART_FILES(ARTREF)%FMT)
!     BINARY FILE 1 FORMAT
      ART_FILES(ARTBI1)%FMT = MOTCAR( ADRESS(4,32) )(1:8)
      CALL MAJUS(ART_FILES(ARTBI1)%FMT)
!     BINARY FILE 2 FORMAT
      ART_FILES(ARTBI2)%FMT = MOTCAR( ADRESS(4,33) )(1:8)
      CALL MAJUS(ART_FILES(ARTBI2)%FMT)
      IF(ANIMFS) THEN
!     PHASE AND AMPLITUDE FILENAME
        ART_FILES(ARTAMP)%NAME=MOTCAR( ADRESS(4,34) )
!     PHASE AND AMPLITUDE FILE FORMAT
        ART_FILES(ARTAMP)%FMT = MOTCAR( ADRESS(4,35) )(1:8)
        CALL MAJUS(ART_FILES(ARTAMP)%FMT)
      ENDIF
      IF(CHAINTWC.GE.1) THEN
!     TOMAWAC OUTER SPECTRAL FILENAME
        ART_FILES(WACSPE)%NAME=MOTCAR( ADRESS(4,36) )
!     TOMAWAC OUTER SPECTRAL FILE FORMAT
        ART_FILES(WACSPE)%FMT = MOTCAR( ADRESS(4,37) )(1:8)
        CALL MAJUS(ART_FILES(WACSPE)%FMT)
      ENDIF
!     PLACEHOLDERS:
!     NOMFST        = MOTCAR( ADRESS(4, 38) )
!     FREE SURFACE FILE FORMAT = MOTCAR( ADRESS(4, 39) )
!     NAME\_ADVAR   = MOTCAR( ADRESS(4, 40) )
      IF(CHAINTWC.EQ.2) THEN
!     TOMAWAC OUTER RESULT FILENAME
        ART_FILES(WACRES)%NAME=MOTCAR( ADRESS(4,41) )
!     TOMAWAC OUTER RESULT FILE FORMAT
        ART_FILES(WACRES)%FMT = MOTCAR( ADRESS(4,42) )(1:8)
        CALL MAJUS(ART_FILES(WACRES)%FMT)
!     LIQUID BOUNDARY FILENAME, DERIVED FROM TOMAWAC OUTER RESULT FILE
        ART_FILES(WACLQD)%NAME=MOTCAR( ADRESS(4,43) )
!     LIQUID BOUNDARY FILE FORMAT
        ART_FILES(WACLQD)%FMT = MOTCAR( ADRESS(4,44) )(1:8)
        CALL MAJUS(ART_FILES(WACLQD)%FMT)
      ENDIF
!     PLACEHOLDERS:
!     DICTIONARY    = MOTCAR( ADRESS(4, 4) )
!     PARTITIONING TOOL = MOTCAR( ADRESS(4, 5) )
!
      IF(LISTIN) THEN
        WRITE(LU,102)
      ENDIF
102   FORMAT(1X,/,19X, '********************************************',/,
     &            19X, '*               LECDON:                    *',/,
     &            19X, '*        AFTER CALLING DAMOCLES            *',/,
     &            19X, '*        CHECKING OF DATA  READ            *',/,
     &            19X, '*         IN THE STEERING FILE             *',/,
     &            19X, '********************************************',/)
!
!-----------------------------------------------------------------------
!  NAME OF THE VARIABLES FOR THE RESULTS AND GEOMETRY FILES :
!-----------------------------------------------------------------------
!
      MNEMO = '        '
      CALL NOMVAR_ARTEMIS(TEXTE,TEXTPR,MNEMO)
!
! LOGICAL ARRAY FOR OUTPUT
!
      SORLEO = .FALSE.
      SORIMP = .FALSE.
      CALL SORTIE(VARDES , MNEMO , 100 , SORLEO )
      CALL SORTIE(VARIMP , MNEMO , 100 , SORIMP )
!
!-----------------------------------------------------------------------
!
! IN THE CASE OF A PERIOD SWEEPING, THE FIRST PERIOD TO BE COMPUTED
! IS PERDEB
!
      IF (BALAYE) PER = PERDEB
!
!-----------------------------------------------------------------------
!
! IF NOTHING IS TO BE WRITTEN TO THE LISTING FILE, THERE SHOULD BE
! NO INFORMATION WRITTEN ABOUT THE SOLVEUR
!
      IF (.NOT.LISTIN) INFOGR = .FALSE.
!
!-----------------------------------------------------------------------
!
! FOR RANDOM SEA COMPUTATIONS, INHIBITS THE GRAPHICAL AND LISTING
! OUTPUT OF THE PHASES, SPEEDS, FREE SURFACE ELEVATION, AND
! POTENTIAL BECAUSE THEY DO NOT MEAN ANYTHING.
!
! BUT WRITES OUT AN AVERAGE WAVE NUMBER, AN AVERAGE PHASE CELERITY
! AND AN AVERAGE GROUP VELOCITY, COMPUTED FROM THE MEAN WAVE
! PERIOD T01. ALSO WRITES OUT AN AVERAGE INCIDENCE
!
! MOREOVER, CHECKS THAT THE NUMBER OF DISCRETISED PERIODS AND
! DIRECTIONS IS AT LEAST 1
!
      IF(ALEMON .OR. ALEMUL) THEN
!
!       2 : PHASE
!
        SORLEO( 2) = .FALSE.
        SORIMP( 2) = .FALSE.
!
!       3 AND 4 : U0 AND V0
!
        SORLEO( 3) = .FALSE.
        SORIMP( 3) = .FALSE.
        SORLEO( 4) = .FALSE.
        SORIMP( 4) = .FALSE.
!
!       5 : FREE SURFACE
!
        SORLEO( 5) = .FALSE.
        SORIMP( 5) = .FALSE.
!
!       11 AND 12 : REAL AND IMAGINARY PARTS OF THE POTENTIAL
!
        SORLEO(11) = .FALSE.
        SORIMP(11) = .FALSE.
        SORLEO(12) = .FALSE.
        SORIMP(12) = .FALSE.
!
        IF (NPALE.LE.0) NPALE = 1
        IF (NDALE.LE.0) NDALE = 1
!
      ELSE
!
!       17, 18 AND 19 : T01, T02 AND TM
!
        SORLEO(17) = .FALSE.
        SORIMP(17) = .FALSE.
        SORLEO(18) = .FALSE.
        SORIMP(18) = .FALSE.
        SORLEO(19) = .FALSE.
        SORIMP(19) = .FALSE.
      ENDIF
!
! NO PERIOD SWEEPING FOR RANDOM SEAS
!
      IF (ALEMON .OR. ALEMUL) BALAYE = .FALSE.
!
! NDALE=1 FOR MONO-DIRECTIONAL SEAS
!
      IF (ALEMON .AND. .NOT.ALEMUL) NDALE = 1
!
! NPALE=1 FOR REGULAR SEAS
!
      IF (.NOT.ALEMON .AND. .NOT.ALEMUL .AND. .NOT.BALAYE) NPALE = 1
!
! IF AMPLITUDE AND PHASE FILE IS REQUIRED, NAME OF VARIABLES IN TEXTANIM
! AND LOGICAL ARRAY FOR OUTPUT IN SORNIM
!
      IF (ANIMFS) THEN
        DO I=1,NDALE
          WRITE(CLT,205) I
205       FORMAT(I2.2)
          TEXTANIM (2*I-1) = 'WAVE HEIGHT D'//CLT//'              (M)'
          TEXTANIM (2*I)   = 'WAVE PHASE D'//CLT//'             (RAD)'
!
          SORNIM (2*I-1) = .TRUE.
          SORNIM (2*I) = .TRUE.
        ENDDO
      ENDIF
!
!-----------------------------------------------------------------------
!
!  DOES NOT WRITE OUT 'PRIVE' VARIABLES IF THEY'VE NOT BEEN ALLOCATED
!
      LSTOP = .FALSE.
      DO I=1,4
        IF ((SORLEO(12+I).OR.SORIMP(12+I)).AND.(NPRIV.LT.I)) THEN
          WRITE(LU,17) I,NPRIV
          LSTOP = .TRUE.
        ENDIF
      ENDDO
      IF (LSTOP) THEN
        CALL PLANTE(1)
        STOP
      ENDIF
 17   FORMAT(1X,'PRIVATE ARRAY ',1I1,' CANNOT BE USED '
     &      ,1X,'BECAUSE IT WAS NOT ALLOCATED.',/
     &      ,1X,'CHECK ''NPRIV''  (AT THE TIME BEING ',1I1,' ).',/)
!
!-----------------------------------------------------------------------
!
!  WRITES OUT THE TITLE
!
      IF(LISTIN) THEN
        WRITE(LU,3001) TITCAS
3001    FORMAT(/1X,'NAME OF THE STUDY :',1X,A72,/)
      ENDIF
!
!-----------------------------------------------------------------------
!
! NO TIDAL FLATS ==> MSK = .FALSE.
!
      MSK = .FALSE.
!
!-----------------------------------------------------------------------
!
! COMPULSORY CHOICE OF KEYWORD FOR THE DIRECT SOLVEUR
!
      IF( (SLVART%SLV.EQ.8).AND.(OPTASS.NE.3) ) THEN
        WRITE(LU,3003)
!
3003    FORMAT(1X,'WITH DIRECT SYSTEM SOLVER, EDGE-BASED STORAGE',/,1X,
     &             'IS MANDATORY',///)
        CALL PLANTE(1)
        STOP
      ENDIF
!
! USE OF DIRECT SOLVEUR IS NOT POSSIBLE WITH PARALLELISM
!
      IF(NCSIZE.GT.1) THEN
        IF(SLVART%SLV.EQ.8) THEN
          WRITE(LU,3005)
3005      FORMAT(1X,'WITH PARALLELISM,',/,1X,
     &            'NO DIRECT SYSTEM SOLVER 8 USE 9',///)
          CALL PLANTE(1)
          STOP
        ENDIF
      ELSE
        IF(SLVART%SLV.EQ.9) THEN
          WRITE(LU,3006)
3006      FORMAT(1X,'WITH NO PARALLELISM,',/,1X,
     &            'NO DIRECT SYSTEM SOLVER 9 USE 8',///)
          CALL PLANTE(1)
          STOP
        ENDIF
      ENDIF
!
!-----------------------------------------------------------------------
!
!  TESTS PMIN%MAX and TETMIN%MAX
!
!     CHECKS IF TETMIN<TETMAX
      IF(TETMAX.LE.TETMIN) THEN
        WRITE(LU,*) 'LECDON_ARTEMIS:'
        WRITE(LU,*) 'MAXIMUM ANGLE OF PROPAGATION < MINIMUM ANGLE'
        WRITE(LU,*) '=> CHANGE MAXIMUM OR MINIMUM ANGLE OF PROPAGATION'
        WRITE(LU,*) '   IN THE USER PARAMETER FILE.                   '
        CALL PLANTE(1)
        STOP
      ENDIF
!     CHECKS IF PMIN<PMAX
      IF(PMAX.LE.PMIN) THEN
        WRITE(LU,*) 'LECDON_ARTEMIS:'
        WRITE(LU,*) 'MAXIMUM SPECTRAL PERIOD < MINIMUM SPECTRAL PERIOD'
        WRITE(LU,*) '=> CHANGE MAXIMUM OR MINIMUM SPECTRAL PERIOD     '
        WRITE(LU,*) '   IN THE USER PARAMETER FILE.                   '
        CALL PLANTE(1)
        STOP
      ENDIF
!
!-----------------------------------------------------------------------
!
!  TESTS SUPPLY OF REQUIRED INPUT FOR NESTING
!
      IF(CHAINTWC.GE.1) THEN
        IF (ART_FILES(WACSPE)%NAME .EQ. '') THEN
          WRITE(LU,3007)
3007      FORMAT(/1X,'PLEASE SUPPLY TOMAWAC OUTER SPECTRAL FILE',/)
          STOP
        ENDIF
      ENDIF
      IF(CHAINTWC.EQ.2) THEN
        IF (ART_FILES(WACRES)%NAME .EQ. '') THEN
          WRITE(LU,3009)
3009      FORMAT(/1X,'PLEASE SUPPLY TOMAWAC OUTER RESULT FILE',/)
          STOP
        ENDIF
        IF (ART_FILES(WACLQD)%NAME .EQ. '') THEN
          WRITE(LU,3011)
3011      FORMAT(/1X,'PLEASE SUPPLY TOMAWAC LIQUID BOUNDARY FILE',/)
          STOP
        ENDIF
        IF (X_SFREF .LT. -99999.) THEN
          WRITE(LU,3013)
3013      FORMAT(/1X,'PLEASE SUPPLY X FOR THE REFERENCE F SPECTRUM',/)
          STOP
        ENDIF
        IF (Y_SFREF .LT. -99999.) THEN
          WRITE(LU,3015)
3015      FORMAT(/1X,'PLEASE SUPPLY Y FOR THE REFERENCE F SPECTRUM',/)
          STOP
        ENDIF
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
