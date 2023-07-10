!                   ******************
                    SUBROUTINE ARTEMIS
!                   ******************
!
!
!***********************************************************************
! ARTEMIS   V7P4                                     Nov 2017
!***********************************************************************
!
!brief    SOLVES THE MODIFIED BERKHOFF EQUATION.
!
!history  D. AELBRECHT (LNH)
!+        21/04/1999
!+        V5P1
!+   First version.
!
!history  C. DENIS (SINETICS)
!+        21/06/2010
!+        V6P0
!+   PARALLEL VERSION
!
!history  N.DURAND (HRW), S.E.BOURBAN (HRW)
!+        13/07/2010
!+        V6P0
!+   Translation of French comments within the FORTRAN sources into
!+   English comments
!
!history  N.DURAND (HRW), S.E.BOURBAN (HRW)
!+        21/08/2010
!+        V6P0
!+   Creation of DOXYGEN tags for automated documentation and
!+   cross-referencing of the FORTRAN sources
!
!history Y AUDOUIN (LNHE)
!+       25/05/2015
!+       V7P0
!+       Modification to comply with the hermes module
!
!history  N.DURAND (HRW)
!+        November 2016
!+        V7P2
!+   cleaned up declaration / initialisation for NVARCL and ISTO (not used)
!+   modified call to LECLIM to harvest HB, TETAP, ALFAP and RP values
!+   from the cli file
!
!history  N.DURAND (HRW)
!+        November 2016
!+        V7P2
!+   writes out the amplitude and phase file 'IF (ANIMFS)'
!
!history  J,RIEHME (ADJOINTWARE)
!+        November 2016
!+        V7P2
!+   Replaced EXTERNAL statements to parallel functions / subroutines
!+   by the INTERFACE_PARALLEL
!
!history  N.DURAND (HRW)
!+        August 2017
!+        V7P3
!+   1. CHAINTWC is now a choice of integers, and no longer a logical
!+   2. Data in the TOMAWAC spectrum file are now read using the HERMES
!+   functionalities: get_tomspec_values.f rather than lecwac1.f
!+   3. PI and RADDEG now defined in DECLARATIONS_ARTEMIS
!+   4. Data in the TOMAWAC res file (to inform HB) are read in
!+   read_bin_frliq.f
!
!history  N.DURAND (HRW)
!+        November 2017
!+        V7P4
!+   1. Use of ART_READ_BIN_FRLIQ to inform HB for liquid boundaries in
!+   case of CHAINTWC=2
!+   2. Calls TWCALE to get frequency discretisation
!+   3. Calls TWCAL2 to get directional discretisation in case of
!+   CHAINTWC=2, and use of BDALE in place of DALE where appropriate
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_TELEMAC
      USE DECLARATIONS_ARTEMIS
      USE GRACESTOP
!
!-----------------------------------------------------------------------
! DECLARES TYPES AND DIMENSIONS
!-----------------------------------------------------------------------
!
      USE DECLARATIONS_SPECIAL
      USE INTERFACE_PARALLEL, ONLY : P_MAX
      IMPLICIT NONE
!
! INTEGERS
!
      INTEGER LT,NPERBA,ITERMUR, I , LF
      INTEGER RECORD
!
! VARIABLE FOR SUBROUTINE DISMOY
!
      INTEGER LISHHO
!
! VARIABLES FOR CALLS TO TELEMAC-2D SUBROUTINES
!
      DOUBLE PRECISION LAMBD0
!
! USED FOR DUMMY ARGUMENTS
!
      DOUBLE PRECISION ECRHMU,MODHMU,PONDER
!
!-----------------------------------------------------------------------
!
!  VARIABLES TO READ IF COMPUTATION IS CONTINUED :
!  0 : DISCARD    1 : READ  (SEE SUBROUTINE NOMVAR)
!
      INTEGER :: ALIRE(MAXVAR) = (/ 1,(0,I=2,MAXVAR) /)
!
      INTEGER :: IPTFR,ITMP
!
!=======================================================================
!
! : 1          READS, PREPARES AND CONTROLS THE DATA
!
!=======================================================================
!
!  TYPES OF DISCRETISATION:
!
!  TRIANGLES : P1
      IELM  = 11
!  SEGMENTS  : P1 FOR THE BOUNDARY
      IELMB = 1
!
!
!  MAXIMUM SIZE (CASE OF AN ADAPTIVE GRID)
!  THESE PARAMETERS ARE USED IN BIEF CALLS
!
!     NODES
      NPMAX = NPOIN
!     ELEMENTS
      NELMAX = NELEM
!
      IF(BALAYE) THEN
        NPERBA = INT((PERFIN-PERDEB)/PERPAS) + 1
      ENDIF
!
!=======================================================================
!
      SPHERI = .FALSE.
!
!-----------------------------------------------------------------------
!
! READS THE BOUNDARY CONDITIONS AND INDICES FOR THE BOUNDARY NODES.
!
! CCP : WARNING :
!       V6P2 LECLIM_ARTEMIS IS NOT USED ANYMORE.
!       IN LECLIM we use KENT KENTU 0 0 0 0 values for KENT,KENTU, etc...
!       This way LECLIM ONLY READ the boundary conditions file and
!       DO NOT CHANGE the LIHBOR values when LIHBOR=KINC, OR GENERATE A
!       MESSAGE when LIHBOR=KADH
!       AND DOES NOT RESET (TETAPS,ALFAPS) if LIUBOR=KENT or KENTU
!
      IF(DEBUG.GT.0) WRITE(LU,*) '> CALLING LECLIM'
      CALL LECLIM (LIHBOR%I   , LIUBOR%I , ITB1%I  , ITB1%I,
     &             HBS%R      , TETAPS%R , ALFAPS%R, TB1%R ,
     &             RP%R       , TB1%R    , TB1%R   ,
     &             MESH%NPTFR , 'ART'    ,.FALSE.  ,
     &             ART_FILES(ARTGEO)%FMT,ART_FILES(ARTGEO)%LU,
     &             KENT       , KENTU    , 0 ,  0 , 0 , 0  ,
     &             NUMLIQ%I   ,MESH,BOUNDARY_COLOUR%I)
      DO I=1,NPTFR
        HB%R(I)=HBS%R(I)
        TETAP%R(I)=TETAPS%R(I)
        ALFAP%R(I)=ALFAPS%R(I)
      ENDDO
      IF(DEBUG.GT.0) WRITE(LU,*) '< LECLIM CALLED'
!
!-----------------------------------------------------------------------
!
! COMPLEMENTS THE DATA STRUCTURE FOR BIEF
!
      IF(DEBUG.GT.0) WRITE(LU,*) '> CALLING INBIEF'
      CALL INBIEF(LIHBOR%I,KLOG,IT1,IT2,IT3,LVMAC,IELM,
     &         LAMBD0,SPHERI,MESH,T1,T2,OPTASS,PRODUC,EQUA)
      IF(DEBUG.GT.0) WRITE(LU,*) '< INBIEF CALLED'
!-----------------------------------------------------------------------
!  LOOKS FOR BOTTOM AND BOTTOM FRICTION IN THE GEOMETRY FILE :
!-----------------------------------------------------------------------
!
      IF(DEBUG.GT.0) WRITE(LU,*) '> CALLING FONSTR'
      CALL FONSTR(T1,ZF,T2,FW,ART_FILES(ARTGEO)%LU,
     &            ART_FILES(ARTGEO)%FMT,ART_FILES(ARTFON)%LU,
     &            ART_FILES(ARTFON)%NAME,MESH,FFON,LISTIN,
     &            0,NAMES_PRIVE,PRIVE)
      IF(DEBUG.GT.0) WRITE(LU,*) '< FONSTR CALLED'
!
!-----------------------------------------------------------------------
!
! PREPARES THE RESULTS FILE (OPTIONAL)
!
!     STANDARD SELAFIN FORMAT
!
      IF(DEBUG.GT.0) WRITE(LU,*) '> PREPARES THE RESULTS FILE'
        ! CREATES DATA FILE USING A GIVEN FILE FORMAT : FORMAT_RES.
        ! THE DATA ARE CREATED IN THE FILE: NRES, AND ARE
        ! CHARACTERISED BY A TITLE AND NAME OF OUTPUT VARIABLES
        ! CONTAINED IN THE FILE.
        CALL WRITE_HEADER(ART_FILES(ARTRES)%FMT, ! RESULTS FILE FORMAT
     &                    ART_FILES(ARTRES)%LU,  ! LU FOR RESULTS FILE
     &                    TITCAS,     ! TITLE
     &                    MAXVAR,     ! MAX NUMBER OF OUTPUT VARIABLES
     &                    TEXTE,      ! NAMES OF OUTPUT VARIABLES
     &                    SORLEO)     ! PRINT TO FILE OR NOT
        ! WRITES THE MESH IN THE OUTPUT FILE :
        ! IN PARALLEL, REQUIRES NCSIZE AND NPTIR.
        ! THE REST OF THE INFORMATION IS IN MESH.
        ! ALSO WRITES : START DATE/TIME AND COORDINATES OF THE
        ! ORIGIN.
        CALL WRITE_MESH(ART_FILES(ARTRES)%FMT, ! RESULTS FILE FORMAT
     &                  ART_FILES(ARTRES)%LU,  ! LU FOR RESULTS FILE
     &                  MESH,
     &                  1,             ! NUMBER OF PLANES /NA/
     &                  MARDAT,        ! START DATE
     &                  MARTIM,        ! START TIME
     &                  T1,T2,         ! WORKING ARRAYS
     &                  NCSIZE.GT.1,NPTIR,
     &                  NGEO=ART_FILES(ARTGEO)%LU,
     &                  GEOFORMAT=ART_FILES(ARTGEO)%FMT)
!
!-----------------------------------------------------------------------
!
!  IF ANIMATION IS REQUIRED, PREPARES THE OUTPUT FILES
!
        IF (ANIMFS) THEN
          CALL WRITE_HEADER(ART_FILES(ARTAMP)%FMT,
     &                      ART_FILES(ARTAMP)%LU,
     &                      TITCAS,
     &                      MAXVAR,
     &                      TEXTANIM,
     &                      SORNIM)
          CALL WRITE_MESH(ART_FILES(ARTAMP)%FMT,
     &                    ART_FILES(ARTAMP)%LU,
     &                    MESH,
     &                    1,
     &                    MARDAT,
     &                    MARTIM,
     &                    T1,T2,
     &                    NCSIZE.GT.1,NPTIR)
        ENDIF
!
        IF(DEBUG.GT.0) WRITE(LU,*) '< RESULTS FILE PREPARED'
!
!-----------------------------------------------------------------------
!
!     INITIALISES PRIVE
!
      IF(NPRIV.GT.0) CALL OS('X=C     ',X=PRIVE, C=0.D0)
!
!=======================================================================
!
      IF(DEBUG.GT.0) WRITE(LU,*) '> PREPARES LIQUID BOUNDARIES'
      IF(NCSIZE.GT.1) THEN
        NFRLIQ=0
        DO I=1,NPTFR
          NFRLIQ=MAX(NFRLIQ,NUMLIQ%I(I))
        ENDDO
        NFRLIQ=P_MAX(NFRLIQ)
        WRITE(LU,*) ' '
        WRITE(LU,*) 'NUMBER OF LIQUID BOUNDARIES:',NFRLIQ
      ELSE
        CALL FRONT2(NFRLIQ,
     &        LIHBOR%I,LIUBOR%I,
     &        MESH%X%R,MESH%Y%R,MESH%NBOR%I,MESH%KP1BOR%I,
     &        IT1%I,NPOIN,NPTFR,KLOG,LISTIN,NUMLIQ%I,MAXFRO)
      ENDIF
      IF(NFRLIQ.GT.MAXFRO) THEN
        WRITE(LU,*) 'FRONT2: SIZE OF ARRAYS EXCEEDED'
        WRITE(LU,*) '        INCREASE THE KEYWORD'
        WRITE(LU,*) '        MAXIMUM NUMBER OF BOUNDARIES'
        WRITE(LU,*) '        IN THE CALLING PROGRAM'
        WRITE(LU,*) '        THE CURRENT VALUE IS ',MAXFRO
        WRITE(LU,*) '        THE VALUE SHOULD BE ',NFRLIQ
        CALL PLANTE(1)
        STOP
      ENDIF
      IF(DEBUG.GT.0) WRITE(LU,*) '< LIQUID BOUNDARIES PREPARED'
! LOCATES THE BOUNDARIES
!
!=======================================================================
!
! CORRECTS THE VALUES OF THE BOTTOM (OPTIONAL)
!
! STANDARD SUBROUTINE DOES NOT DO ANYTHING
!
      IF(DEBUG.GT.0) WRITE(LU,*) '> CALLING ART_CORFON'
      CALL ART_CORFON
      IF(DEBUG.GT.0) WRITE(LU,*) '< ART_CORFON CALLED'
!
!=======================================================================
!
!  PREPARES NESTING WITHIN TOMAWAC
!
      IF(DEBUG.GT.0) WRITE(LU,*) '> PREPARES NESTING WITHIN TOMAWAC'
      IF (CHAINTWC.GE.1 .AND. (.NOT.ALEMUL)) THEN
        WRITE(LU,*) 'MULTIDIRECTIONAL RANDOM WAVE OPTION NEEDS ',
     &              'TO BE USED IF NESTING ARTEMIS WITHIN TOMAWAC'
        CALL PLANTE(1)
        STOP
      ENDIF
!     READ TOMAWAC SPECTRUM IF NECESSARY
      IF (CHAINTWC.GE.1) THEN
        IF(DEBUG.GT.0) WRITE(LU,*) '> CALLING GET_TOMSPEC_VALUES'
        CALL GET_TOMSPEC_VALUES(CHAINTWC,S_TOM)
        IF(DEBUG.GT.0) WRITE(LU,*) '< GET_TOMSPEC_VALUES CALLED'
      ENDIF
!     READ TOMAWAC INTERPOLATED HM0 IF NECESSARY
!     NOTING THAT BINARY LIQUID FILE ONLY HOLDS VALUES FOR LIQUID NODES
      IF (CHAINTWC.EQ.2) THEN
        IF(DEBUG.GT.0) WRITE(LU,*) '> CALLING ART_READ_BIN_FRLIQ'
        IF (NPTFR.GT.0) THEN
        CALL ART_READ_BIN_FRLIQ('WAVE HEIGHT HM0 ')
        ITMP = 0
        DO IPTFR=1,NPTFR
          IF(LIHBOR%I(IPTFR) .NE. 2) THEN
            ITMP = ITMP+1
            HB%R(IPTFR) = HBS%R(ITMP)
          ENDIF
        ENDDO
        ENDIF !(NPTFR.GT.0) THEN
        CALL OS( 'X=Y     ' , X=HBS , Y=HB )
        IF(DEBUG.GT.0) WRITE(LU,*) '< ART_READ_BIN_FRLIQ CALLED'
      ENDIF
      IF(DEBUG.GT.0) WRITE(LU,*) '< NESTING WITHIN TOMAWAC PREPARED'
!
!=======================================================================
!
! INITIALISES THE WAVE HEIGHT FOR RANDOM SEAS AT 0.
!
      IF (ALEMON .OR. ALEMUL) THEN
        CALL OS('X=C     ', X=HALE, C=0.D0)
      ENDIF
!
!     DETERMINES THE DIFFERENT PERIODS FOR A RANDOM SEA UNI-DIRECTIONAL COMPUTATION
!     JONSWAP OR PM SPECTRUM DEPENDING ON GAMMA VALUE   ===
!
      IF (ALEMON) THEN
        CALL PERALE(PALE%R,GAMMA,PERPIC,NPALE,PMIN,PMAX)
        PER = PALE%R(1)
      ENDIF
!
!     DETERMINES THE DIFFERENT DIRECTIONS AND PERIODS FOR A RANDOM SEA
!     MULTI-DIRECTIONAL COMPUTATION
!     =====
      IF (ALEMUL) THEN
        IF(DEBUG.GT.0) WRITE(LU,*) '> PREPARES FOR MULTIDIRECTIONAL SEA'
!
!       IF SPECTRUM IS COMPUTED BY ARTEMIS
        IF (CHAINTWC.EQ.0) THEN
          CALL DIRALE(DALE%R,EXPOS,TETAH,TETMIN,TETMAX,NDALE)
          CALL PERALE(PALE%R,GAMMA,PERPIC,NPALE,PMIN,PMAX)
          PER = PALE%R(1)
!
!       IF SPECTRUM IS TAKEN FROM TOMAWAC, NESTING OPTION 1
        ELSEIF (CHAINTWC.EQ.1) THEN
          CALL TWCALE(1)
!              (DALE%R,PDALE%R,PMAX,PMIN,TETMAX,TETMIN,NPALE,NDALE)
          PER=PDALE%R(1)
          DO I=1,NPALE
            PALE%R(I)=PDALE%R(I)
          ENDDO
!
!       IF SPECTRUM IS TAKEN FROM TOMAWAC, NESTING OPTION 2
        ELSEIF (CHAINTWC.EQ.2) THEN
          CALL TWCALE(N_SFREF)
          PER=PDALE%R(1)
          DO I=1,NPALE
            PALE%R(I)=PDALE%R(I)
          ENDDO
!
!       DALEs ARE NOT USED IN THIS CASE AND TWCAL2 IS CALLED TO GET SPATIALLY VARYING VALUES
!       ONLY CALLED IN THE (SUB)MESH WHERE IT MATTERS : NPTFR>0
!
          CALL TWCAL2
!
        ENDIF  !CHAINTWC
!
        IF(DEBUG.GT.0) WRITE(LU,*) '< MULTIDIRECTIONAL SEA PREPARED'
      ENDIF  !(ALEMUL)
!
!=======================================================================
!
! START OF COMPUTATION
      IF(DEBUG.GT.0) WRITE(LU,*) '###> START OF COMPUTATION'
!
! LT REFERS TO THE CURRENT CALCULATION
!  (STARTS FROM 0 SO THAT THE FIRST COMPUTATION ALWAYS BE RECORDED)
!  (ENDS AT NDALE x NPALE -1 SO THAT ALL DIRECTION AND FREQUENCIES ARE SOLVED)
      LT  = 0
! FOR A RANDOM SEA COMPUTATION, LPER AND LDIR REFER TO THE COMPUTED
! PERIOD AND DIRECTION. LT COUNT THE NUMBER OF BERKHOFF RESOLUTION
      LPER= 1
      LDIR= 1
!
! LF =0 INDICATES IF THIS IS THE FIRST CALCULATION OF RANDOM SEA
! (MU=0 IMPOSED IN BERKHO.F)
      LF = 0
!

300   CONTINUE
! INITIALISES THE WAVE HEIGHT FOR RANDOM SEAS AT 0.
!
      IF (ALEMON .OR. ALEMUL) THEN
        CALL OS('X=C     ', X=HALE, C=0.D0)
        CALL OS('X=C     ', X=UEB , C=0.D0)
        IF (LF.EQ.0) THEN
          ITERMUR=0
        ENDIF
      ENDIF
!
! INITIALISES QB, T01, T02 AND TM : SET TO 0 AT THE START OF COMPUTATION
!
      CALL OS('X=C     ', X=QB , C=0.D0 )
      CALL OS('X=C     ', X=T01 , C=0.D0 )
      CALL OS('X=C     ', X=T02 , C=0.D0 )
      CALL OS('X=C     ', X=TM , C=0.D0 )
!
!
! INITIALISES RADIATION STRESSES AND
! FORCINGS
!
      CALL OS('X=C     ', X=FX , C=0.D0 )
      CALL OS('X=C     ', X=FY , C=0.D0 )
      CALL OS('X=C     ', X=SXX , C=0.D0 )
      CALL OS('X=C     ', X=SXY , C=0.D0 )
      CALL OS('X=C     ', X=SYY , C=0.D0 )
      CALL OS('X=C     ', X=MCOS , C=0.D0 )
      CALL OS('X=C     ', X=MSIN , C=0.D0 )
!
!-----------------------------------------------------------------------
!
! START OF DIRECTION LOOP
!
!     PRINT NEW VALUE OF THE DIRECTION
!
!     IN MULTIDIRECTIONAL RANDOM SEA, THE DIRECTIONS OF PROPAGATION
!     (AT THE BOUNDARY) HAVE BEEN CALCULATED IN DALE.
!
200   IF (ALEMUL) THEN
        IF (CHAINTWC.LT.2) THEN
          CALL OS('X=C     ', X=TETAB, C=DALE%R(LDIR) )
        ELSE
          CALL OS('X=Y     ', X=TETAB, Y=BDALE%ADR(LDIR)%P )
        ENDIF
        IF(DEBUG.GT.0) WRITE(LU,*) '> CALLING ENTART'
        IF (CHAINTWC.LT.2) THEN
          CALL ENTART(2,DALE%R(LDIR),LDIR,NDALE,ALEMON,ALEMUL,BALAYE)
        ELSEIF(NPTFR.GT.0) THEN
          CALL ENTART(2,DALE%R(LDIR),LDIR,NDALE,ALEMON,ALEMUL,BALAYE)
        ENDIF
        IF(DEBUG.GT.0) WRITE(LU,*) '< ENTART CALLED'
      ELSE
!
! TETAB = TETAH IN THE CASE OF UNIDIRECTIONAL WAVES,
! SUCH THAT TETAB CAN BE USED CONSISTENTLY
!
        CALL OS('X=C     ', X=TETAB ,C=TETAH )
      ENDIF
!
!-----------------------------------------------------------------------
!
! START OF PERIOD LOOP
!
100   CONTINUE
!
!     PRINT NEW VALUE OF THE PERIOD
      IF (BALAYE) THEN
        IF(DEBUG.GT.0) WRITE(LU,*) '> CALLING ENTART'
        CALL ENTART(1,PER,LPER,NPERBA,ALEMON,ALEMUL,BALAYE)
        IF(DEBUG.GT.0) WRITE(LU,*) '< ENTART CALLED'
      ELSE
        IF(DEBUG.GT.0) WRITE(LU,*) '> CALLING ENTART'
        CALL ENTART(1,PER,LPER,NPALE,ALEMON,ALEMUL,BALAYE)
        IF(DEBUG.GT.0) WRITE(LU,*) '< ENTART CALLED'
      ENDIF
!
!
!=======================================================================
!
! : 2          INITIALISES
!
!=======================================================================
!
! INITIALISES PHYSICAL PARAMETERS
!
!
      IF(DEBUG.GT.0) WRITE(LU,*) '> CALLING CONDIH'
      CALL CONDIH
      IF(DEBUG.GT.0) WRITE(LU,*) '< CONDIH CALLED'
!
!=======================================================================
!
! : 3          BOUNDARY CONDITIONS
!
!=======================================================================
!
! MASKING FOR THE BOUNDARY CONDITIONS
!
! CALLS THE USER SUBROUTINE
!
      IF(DEBUG.GT.0) WRITE(LU,*) '> CALLING BORH'
      CALL BORH
      IF(DEBUG.GT.0) WRITE(LU,*) '< BORH CALLED'
!     REASSIGNS TETAP TO THE IMPOSED USER VALUE
      IF( LANGAUTO ) THEN
        DO I=1,NPTFR
          TETAP%R(I)=TETAPS%R(I)
        ENDDO
      ENDIF
!
! ===================================================================================
!
! : 3.1        BOUNDARY CONDITIONS FOR RANDOM SPECTRUM
!              ---------------------------------------
! CALCULATES THE BOUNDARY CONDITIONS ON THE POTENTIAL FROM USER INPUT.
! RANDOM INCIDENT WAVE for freq i : HBi = Hs/sqrt(Ndale*Npale)
! This way Hs**2 = (HB1**2+HB2**2+...+HBN**2)
! Thus, HB is a significant wave height such as :
! HB = sqrt(2) * Hi where Hi=Ai/2 where Ai**2 = 2 Sp(f,teta) df dteta)
! N.B :
! If sign. wave height has to be varied depending on f,teta,
! USE HB(I) = 16D0*(Sp(f,teta)*df*dteta) , or PONDER = 16D0*(Sp(f,teta)*df*dteta)/Hs
! ==================================================================================
      PONDER=1D0/DBLE(NPALE*NDALE)
      IF (ALEMON.OR.ALEMUL) THEN
        IF (CHAINTWC.EQ.1) THEN
!         IF SPECTRUM FROM TOMAWAC NESTING OPTION 1,
!         HS TAKEN FROM SPECTRUM INTEGRATION: HSCAL
          DO I=1,NPTFR
            HB%R(I)=HSCAL*SQRT(PONDER)
          ENDDO
        ELSE
!         IF SPECTRUM FROM ARTEMIS, OR FROM TOMAWAC NESTING OPTION 2,
!         HS ALREADY STORED IN HB%R
          DO I=1,NPTFR
            HB%R(I)=HB%R(I)*SQRT(PONDER)
          ENDDO
        ENDIF
      ENDIF
!
!      IF (LT .EQ. 0) THEN
      IF(DEBUG.GT.0) WRITE(LU,*) '> CALLING MASQUE_ARTEMIS'
      CALL MASQUE_ARTEMIS
      IF(DEBUG.GT.0) WRITE(LU,*) '< MASQUE_ARTEMIS CALLED'
!
      IF(DEBUG.GT.0) WRITE(LU,*) '> CALLING PHBOR'
      CALL PHBOR
      IF(DEBUG.GT.0) WRITE(LU,*) '< PHBOR CALLED'
!      END IF
!
!=======================================================================
!
! : 4          SOLVES THE BERKHOFF EQUATION
!
!=======================================================================
!
      IF(DEBUG.GT.0) WRITE(LU,*) '> SOLVING THE BERKHOFF EQUATION'
      CALL BERKHO (LF)
      IF(DEBUG.GT.0) WRITE(LU,*) '< BERKHOFF EQUATION SOLVED'
!
!
!=======================================================================
!
! : 5.1        COMPUTES SPEED, FREE SURFACE ELEVATION,
!              WAVE HEIGHT AND PHASE
!
!=======================================================================
!
      IF(DEBUG.GT.0) WRITE(LU,*) '> CALLING CALRES'
      CALL CALRES
      IF(DEBUG.GT.0) WRITE(LU,*) '< CALRES CALLED'
!
      IF (ALEMON .OR. ALEMUL) THEN
!
!       CUMULATIVELY COMPUTES THE M1, M2, AND MT1 MOMENTUMS
!       STORED UNTIL THE LAST COMPUTATION IN T01, T02, AND TM
        IF(DEBUG.GT.0) WRITE(LU,*) '> CALLING CALCMN'
        CALL CALCMN
        IF(DEBUG.GT.0) WRITE(LU,*) '< CALCMN CALLED'
!
      ENDIF
!
!
!=======================================================================
!
! : 5.2        COMPUTES RADIATION STRESSES AND
!              DRIVING FORCES FOR REGULAR WAVES.
!
!=======================================================================
!
      IF (.NOT.ALEMON .AND. .NOT.ALEMUL) THEN
!
        IF (LISHOU) THEN
          IF(DEBUG.GT.0) WRITE(LU,*) '> CALLING DISMOY'
          CALL DISMOY
     &    (NPOIN,NELEM,MESH%X%R,MESH%Y%R,MESH%IKLE%I,K%R,LISHHO)
          IF(DEBUG.GT.0) WRITE(LU,*) '< DISMOY CALLED'
        ELSE
          LISHHO = 0
        ENDIF
!
        IF(DEBUG.GT.0) WRITE(LU,*) '> CALLING RADIA1'
        CALL RADIA1 (LISHHO)
        IF(DEBUG.GT.0) WRITE(LU,*) '< RADIA1 CALLED'
!
      ELSE
        LISHHO = 0
      ENDIF
!=======================================================================
!
! : 6   CALLS A USER SUBROUTINE FOR PRINT OUTS, ANALYTICAL SOLUTIONS...
!       (STANDARD SUBROUTINE DOES NOT DO ANYTHING)
!
!=======================================================================
!
      IF(DEBUG.GT.0) WRITE(LU,*) '> PRINTING USER VARIABLES'
      CALL UTIMP_ART
      IF(DEBUG.GT.0) WRITE(LU,*) '< USER VARIABLES PRINTED'
!
!=======================================================================
!
! : 7          PRINTS OUT THE RESULTS NOW IF REGULAR SEAS
!
!=======================================================================
!
!
      IF (.NOT.ALEMON .AND. .NOT.ALEMUL) THEN
!
! CONVERTS INCI INTO DEGREES
!
        CALL OS('X=CX    ', X=INCI , C=RADDEG )
!
! RUBENS FILE
!
! FOR REGULAR SEAS,
! TIME IS THE WAVE PERIOD: PER
!
        IF(DEBUG.GT.0) WRITE(LU,*) '> WRITING RESULT FILE'
        RECORD = 0
        IF(BALAYE) RECORD = LT
        CALL BIEF_DESIMP(ART_FILES(ARTRES)%FMT,VARSOR,
     &            NPOIN,ART_FILES(ARTRES)%LU,PER,RECORD,
     &            LISPRD,LEOPRD,
     &            SORLEO,SORIMP,MAXVAR,TEXTE,0,0)
        IF(DEBUG.GT.0) WRITE(LU,*) '< RESULT FILE WRITTEN'
!
!-----------------------------------------------------------------------
!
!     COMPARISON AGAINST A REFERENCE FILE
!
!     THE VALIDA SUBROUTINE FROM THE BIEF LIBRARY IS STANDARD.
!     IT CAN BE MODIFIED BY THE USER FOR THEIR PARTICULAR CASE.
!     BUT THE CALL TO THE SUBROUTINE MUST STAY IN THE TIME LOOP.
!
        IF(VALID) THEN
          IF(DEBUG.GT.0) WRITE(LU,*) '> VALIDATING RESULTS'
          CALL BIEF_VALIDA(TB,TEXTE,
     &                      ART_FILES(ARTREF)%LU,ART_FILES(ARTREF)%FMT,
     &                      VARSOR,TEXTE,
     &                      ART_FILES(ARTRES)%LU,ART_FILES(ARTRES)%FMT,
     &                      MAXVAR,NPOIN,LT,LT,ALIRE)
          IF(DEBUG.GT.0) WRITE(LU,*) '< RESULTS VALIDATED'
        ENDIF
!
      ENDIF
!
!=======================================================================
!
! : 7.1        STARTS WRITING OUTPUT IF ANIMATION IS REQUIRED
!
!=======================================================================
!
      IF (ANIMFS) THEN
        IF(DEBUG.GT.0) WRITE(LU,*) '> WRITING AMP AND PHASE FILE'
        CALL BIEF_ANIMP(ART_FILES(ARTAMP)%FMT,VARNIM,NPOIN,
     &  ART_FILES(ARTAMP)%LU,PER,LT,LDIR,TEXTANIM,NPALE)
        IF(DEBUG.GT.0) WRITE(LU,*) '< WRITING AMP AND PHASE FILE'
      ENDIF
!
!
!=======================================================================
!
! : 8          GOES TO NEXT COMPUTATION
!
!=======================================================================
!
!=======================================================================
!
! IF SWEEPS A RANGE OF PERIODS, DOES THE NEXT PERIOD
!
!=======================================================================
!
      IF (BALAYE) THEN
        LT   = LT  + 1
        LPER = LPER + 1
        IF (LPER.LE.NPERBA) THEN
          PER  = PER + PERPAS
          GOTO 100
        ENDIF
!        IF (PER.LE.PERFIN) GOTO 100
      ENDIF
!
!
!=======================================================================
!
! IF RANDOM SEAS
!
!=======================================================================
!
      IF (ALEMON .OR. ALEMUL) THEN
!
        LT  = LT  + 1
!
        IF (LT.LT.NPALE*NDALE) THEN
!
          WRITE(LU,221) ITERMUR+1
!
!         REACTUALISES THE ENERGY OF THE RANDOM SEA
          CALL OS('X=X+CYZ ',X=HALE,Y=HHO,Z=HHO,C=1.D0)
!
!         VELOCITY FOR BOTTOM FRICTION
          IF(DEBUG.GT.0) WRITE(LU,*) '> CALLING CALUEB2'
          CALL CALUEB2
          IF(DEBUG.GT.0) WRITE(LU,*) '< CALUEB2 CALLED'
!
!         GOES TO NEXT PERIOD
!
          LPER = LPER + 1
          IF (LPER.LE.NPALE) THEN
            PER = PALE%R(LPER)
            GOTO 100
          ENDIF
!
!         WHEN ALL PERIODS HAVE BEEN RUN, GOES TO NEXT DIRECTION
!         AND RESETS LPER TO 1
!         UPDATE OF PALE IF SPECTRUM FROM TOMAWAC
!
          LDIR = LDIR + 1
          IF (CHAINTWC.GE.1) THEN
            DO I=1,NPALE
              PALE%R(I)=PDALE%R((LDIR-1)*NPALE+I)
            ENDDO
          ENDIF
          LPER=1
          PER = PALE%R(LPER)
          IF (LDIR.LE.NDALE) GOTO 200
!
        ELSE
!
!         LAST COMPUTATION: DETERMINES THE MEAN PERIODS
!         (T01 AND T02), AND THE MEAN DIRECTION (INCI)
!
!
          IF(DEBUG.GT.0) WRITE(LU,*) '> CALLING CALCTM'
          CALL CALCTM
          IF(DEBUG.GT.0) WRITE(LU,*) '< CALCTM CALLED'
!
!         DETERMINES MEAN K, C AND CG
!
          IF(DEBUG.GT.0) WRITE(LU,*) '> CALLING CALRE2'
          CALL CALRE2
          IF(DEBUG.GT.0) WRITE(LU,*) '< CALRE2 CALLED'
!
!         TAKES INTO ACCOUNT THE LAST WAVE HEIGHT
!         FOR RANDOM SEAS
          CALL OS('X=X+CYZ ',X=HALE,Y=HHO,Z=HHO,C=1.D0)
          CALL OS('X=SQR(Y)', X=HALE, Y=HALE)
!
!         VELOCITY FOR BOTTOM FRICTION
          IF(DEBUG.GT.0) WRITE(LU,*) '> CALLING CALUEB2'
          CALL CALUEB2
          IF(DEBUG.GT.0) WRITE(LU,*) '< CALUEB2 CALLED'
          CALL OS('X=SQR(Y)', X=UEB, Y=UEB)
!
!
!=======================================================================
!         LOOP ON THE DISSIPATION COEFFICIENT
!                    FOR IRREGULAR WAVES
!
          IF (DEFERL .OR. FROTTE) THEN
            IF(DEBUG.GT.0) WRITE(LU,*) '> CALLING CALCMU'
            CALL CALCMU(ITERMUR)
            IF(DEBUG.GT.0) WRITE(LU,*) '< CALCMU CALLED'
!           WORK TABLE USED                      : T1,T4
!           WORK TABLE USED AND TO BE CONSERVED  : T3 => QB
            IF(DEBUG.GT.0) WRITE(LU,*) '> CALLING RELAXMU'
            CALL RELAXMU(ECRHMU,MODHMU,ITERMUR)
            IF(DEBUG.GT.0) WRITE(LU,*) '< RELAXMU CALLED'
!           ----------------------------------------------------
!           CHECKS CONVERGENCE ON THE DISSIPATION ITERATIVE LOOP
!           ----------------------------------------------------
            WRITE(LU,*) ' '
            WRITE(LU,*) '--------------------------------------------'
            IF (ECRHMU.GT.EPSDIS*MODHMU) THEN
              LDIR = 1
              LPER = 1
              PER  = PALE%R(LPER)
!             FOR USE OF CALCULATED MU IN BERKHO
              LF   = 1
              LT   = 0
              GOTO 300
            ENDIF
!
            WRITE(LU,701) ITERMUR
!
          ENDIF
 701      FORMAT(/,1X,'NUMBER OF SUB-ITERATIONS FOR DISSIPATION:',
     &       1X,I3)
 221      FORMAT(/,1X,'SUB-ITERATION NUMBER :',1X,I3,/)
!
      IF(DEBUG.GT.0) WRITE(LU,*) '###< END OF COMPUTATION'
!
!=======================================================================
!
!           COMPUTES RADIATION STRESSES
!           AND DRIVING FORCES FOR RANDOM SEAS
!
!=======================================================================
!
      IF(DEBUG.GT.0) WRITE(LU,*) '> CALLING RADIA2'
          CALL RADIA2 (LISHHO)
      IF(DEBUG.GT.0) WRITE(LU,*) '< RADIA2 CALLED'
!
!=======================================================================
!
!        CONVERTS INCI INTO DEGREES
!
!=======================================================================
!
          CALL OS('X=CX    ', X=INCI, C=RADDEG)
!
!=======================================================================
!
!           RUBENS FILE
!
!=======================================================================
! CCP ON IMPRIME OMEGAM ET OMEGAP pour BJ 78
!            DO I = 1,NPOIN
!              PRIVE%ADR(1)%P%R(I) = OMEGAM%R(I)
!              PRIVE%ADR(2)%P%R(I) = 2D0*3.1415D0/PERPIC
!              PRIVE%ADR(3)%P%R(I) = T01%R(I)
!              PRIVE%ADR(4)%P%R(I) = PERPIC
!            ENDDO
!
! FOR RANDOM SEAS,
! TIME IS THE PEAK WAVE PERIOD: PERPIC
      IF(DEBUG.GT.0) WRITE(LU,*) '> WRITES RESULTS'
          CALL BIEF_DESIMP(ART_FILES(ARTRES)%FMT,VARSOR,
     &            NPOIN,ART_FILES(ARTRES)%LU,PERPIC,0,
     &            LISPRD,LEOPRD,
     &            SORLEO,SORIMP,MAXVAR,TEXTE,0,0)
      IF(DEBUG.GT.0) WRITE(LU,*) '< RESULTS WRITTEN'
!
!=======================================================================
!
!              COMPARISON AGAINST A REFERENCE FILE
!
!=======================================================================
!
!
!     THE VALIDA SUBROUTINE FROM THE BIEF LIBRARY IS STANDARD.
!     IT CAN BE MODIFIED BY THE USER FOR THEIR PARTICULAR CASE.
!     BUT THE CALL TO THE SUBROUTINE MUST STAY IN THE TIME LOOP.
!
          IF(VALID) THEN
      IF(DEBUG.GT.0) WRITE(LU,*) '> CALLING BIEF_VALIDA'
            CALL BIEF_VALIDA(TB,TEXTE,
     &                       ART_FILES(ARTREF)%LU,ART_FILES(ARTREF)%FMT,
     &                       VARSOR,TEXTE,
     &                       ART_FILES(ARTRES)%LU,ART_FILES(ARTRES)%FMT,
     &                       MAXVAR,NPOIN,LT,LT,ALIRE)
      IF(DEBUG.GT.0) WRITE(LU,*) '< BIEF_VALIDA CALLED'
          ENDIF
!
        ENDIF
!
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
