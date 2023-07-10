!                   **********************
                    SUBROUTINE USER_LIMWAC
!                   **********************
     &(F     , FBOR  , NPTFR , NDIRE , NF    , NPOIN2,
     & KENT  , PRIVE , NPRIV , IMP_FILE)
!
!***********************************************************************
! TOMAWAC   V7P3                                   23/02/2017
!***********************************************************************
!
!brief    BOUNDARY CONDITIONS.
!
!warning  BY DEFAULT, THE BOUNDARY CONDITIONS SPECIFIED IN THE FILE
!+            DYNAM ARE DUPLICATED ON ALL THE DIRECTIONS AND FREQUENCIES
!
!history  F. MARCOS (LNH)
!+        01/02/95
!+        V1P0
!+
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
!history  G.MATTAROLO (EDF - LNHE)
!+        20/06/2011
!+        V6P1
!+   Translation of French names of the variables in argument
!
!history  E. GAGNAIRE-RENOU & J.-M. HERVOUET (EDF R&D, LNHE)
!+        12/03/2013
!+        V6P3
!+   A line IF(LIMSPE.EQ.0...) RETURN removed.
!
!history  A. JOLY (EDF R&D, LNHE)
!+        23/02/2017
!+        V7P3
!+   SPECTRA READ FROM AN EXTERNAL MESH CAN NOW BE IMPOSED ON THE
!+   OPEN BOUNDARIES.
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| APHILL         |-->| BOUNDARY PHILLIPS CONSTANT
!| AT             |-->| COMPUTATION TIME
!| BOUNDARY_COLOUR|-->| COLOUR OF BOUNDARY POINT (DEFAULT: ITS RANK)
!| DDC            |-->| DATE OF COMPUTATION BEGINNING
!| DEPTH          |-->| WATER DEPTH
!| F              |-->| VARIANCE DENSITY DIRECTIONAL SPECTRUM
!| FBOR           |<->| SPECTRAL VARIANCE DENSITY AT THE BOUNDARIES
!| FETCHL         |-->| BOUNDARY MEAN FETCH VALUE
!| FPICL          |-->| BOUNDARY PEAK FREQUENCY
!| FPMAXL         |-->| BOUNDARY MAXIMUM PEAK FREQUENCY
!| FRA            |<--| DIRECTIONAL SPREADING FUNCTION VALUES
!| FRABL          |-->| BOUNDARY ANGULAR DISTRIBUTION FUNCTION
!| GAMMAL         |-->| BOUNDARY PEAK FACTOR
!| HM0L           |-->| BOUNDARY SIGNIFICANT WAVE HEIGHT
!| IMP_FILE       |-->| MESH FILE WITH THE IMPOSED SPECTRA
!| KENT           |-->| B.C.: A SPECTRUM IS PRESCRIBED AT THE BOUNDARY
!| KSORT          |-->| B.C.: FREE BOUNDARY: NO ENERGY ENTERING THE DOMAIN
!| LIFBOR         |-->| TYPE OF BOUNDARY CONDITION ON F
!| LIMSPE         |-->| TYPE OF BOUNDARY DIRECTIONAL SPECTRUM
!| LT             |-->| NUMBER OF THE TIME STEP CURRENTLY SOLVED
!| NBOR           |-->| GLOBAL NUMBER OF BOUNDARY POINTS
!| NF             |-->| NUMBER OF FREQUENCIES
!| LUFO1           |-->| LOGICAL UNIT NUMBER OF THE USER FORMATTED FILE
!| NDIRE          |-->| NUMBER OF DIRECTIONS
!| NPOIN2         |-->| NUMBER OF POINTS IN 2D MESH
!| NPRIV          |-->| NUMBER OF PRIVATE ARRAYS
!| NPTFR          |-->| NUMBER OF BOUNDARY POINTS
!| PRIVE          |-->| USER WORK TABLE
!| SIGMAL         |-->| BOUNDARY SPECTRUM VALUE OF SIGMA-A
!| SIGMBL         |-->| BOUNDARY SPECTRUM VALUE OF SIGMA-B
!| SPEC           |<--| VARIANCE DENSITY FREQUENCY SPECTRUM
!| SPEULI         |-->| INDICATES IF B.C. SPECTRUM IS MODIFIED BY USER
!| SPRE1L         |-->| BOUNDARY DIRECTIONAL SPREAD 1
!| SPRE2L         |-->| BOUNDARY DIRECTIONAL SPREAD 2
!| TETA1L         |-->| BOUNDARY MAIN DIRECTION 1
!| TETA2L         |-->| BOUNDARY MAIN DIRECTION 2
!| UV, VV         |-->| WIND VELOCITIES AT THE MESH POINTS
!| VENSTA         |-->| INDICATES IF THE WIND IS STATIONARY
!| VENT           |-->| INDICATES IF WIND IS TAKEN INTO ACCOUNT
!| X              |-->| ABSCISSAE OF POINTS IN THE MESH
!| XLAMDL         |-->| BOUNDARY WEIGHTING FACTOR FOR ANGULAR
!|                |   | DISTRIBUTION FUNCTION
!| Y              |-->| ORDINATES OF POINTS IN THE MESH
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE INTERFACE_TOMAWAC, EX_USER_LIMWAC => USER_LIMWAC
      USE DECLARATIONS_TOMAWAC, ONLY : UV2D,VV2D,PROF,FB_CTE,NPB,
     &     LIMSPE, FPMAXL, FETCHL, SIGMAL, SIGMBL, GAMMAL, FPICL ,
     &     HM0L  , APHILL, TETA1L, SPRE1L, TETA2L, SPRE2L, XLAMDL,
     &     SPEULI, VENT  , VENSTA, DEPTH ,
     &     SPEC  , FRA   , FRABL , AT    , LT    , DDC   , UV    , VV,
     &     BOUNDARY_COLOUR,LIFBOR, NBOR, LUFO1

      USE DECLARATIONS_SPECIAL
      USE BND_SPECTRA
      USE BIEF_DEF, ONLY : BIEF_FILE
      IMPLICIT NONE
!
!
      INTEGER, INTENT(IN)            :: NPTFR,NDIRE,NF,NPOIN2,NPRIV
      INTEGER, INTENT(IN)            :: KENT
      DOUBLE PRECISION, INTENT(IN)   :: PRIVE(NPOIN2,NPRIV)
      TYPE(BIEF_FILE), INTENT(IN)    :: IMP_FILE
      DOUBLE PRECISION, INTENT(INOUT):: F(NPOIN2,NDIRE,NF)
      DOUBLE PRECISION, INTENT(INOUT):: FBOR(NPTFR,NDIRE,NF)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-
      INTEGER NPCL
      PARAMETER (NPCL=21)
      INTEGER IFF,IPLAN,IPTFR,NPCLI
!
      LOGICAL FLAG
      DOUBLE PRECISION AT1,AT2,COEF2,ATT,C,FCL1,FCL2,COEF
      DOUBLE PRECISION CL1(12,25,NPCL),CL2(12,25,NPCL)
      INTEGER NENR,NPB2(NPCL),IP,I
      INTEGER DEB0,FIN,IPDEB,IPFIN
      INTEGER MAXI,IMIN,MINI,IMAXI
!
      SAVE AT1,AT2,CL1,CL2,NENR,NPCLI,NPB2,MINI,MAXI,IMIN,IMAXI
!
!
!***********************************************************************
!
!     MODIFIES THE TYPE OF BOUNDARY CONDITION (OPTIONAL)
!
!     CAN BE CODED BY THE USER (SPEULI=.TRUE.)
!
!     LIFBOR(IPTFR)=KENT OR KSORT
!
      FLAG=.FALSE.
      IF (VENT .AND. (LIMSPE.EQ.1 .OR. LIMSPE.EQ.2 .OR. LIMSPE.EQ.3
     & .OR. LIMSPE.EQ.5)) FLAG=.TRUE.
!
!     THE FIRST TIME, ALLOCATES MEMORY FOR THE USEFUL ARRAYS
!     ---------------------------------------------------------------
!
      IF(LT.LT.1) THEN
        NPB=1
        IF(FLAG) THEN
          ALLOCATE(UV2D(1:NPTFR),VV2D(1:NPTFR))
          NPB=NPTFR
        ENDIF
        IF(LIMSPE.EQ.7 .OR. SPEULI) THEN
          IF (.NOT.ALLOCATED(PROF)) ALLOCATE(PROF(1:NPTFR))
          NPB=NPTFR
        ENDIF
        IF(NPB.EQ.1) THEN
          IF (.NOT.ALLOCATED(FB_CTE)) ALLOCATE(FB_CTE(1:NDIRE,1:NF))
        ENDIF
      ENDIF
      IF (.NOT.ALLOCATED(UV2D)) ALLOCATE(UV2D(NPTFR))
      IF (.NOT.ALLOCATED(VV2D)) ALLOCATE(VV2D(NPTFR))
      IF (.NOT.ALLOCATED(PROF)) ALLOCATE(PROF(NPTFR))
      IF (.NOT.ALLOCATED(FB_CTE)) ALLOCATE(FB_CTE(1:NDIRE,1:NF))
!
!     THE FIRST TIME (AND POSSIBLY SUBSEQUENTLY IF THE WIND IS NOT
!     STATIONARY AND IF THE BOUNDARY SPECTRUM DEPENDS ON IT),
!     COMPUTES THE BOUNDARY SPECTRUM
!
      IF(LT.LT.1 .OR. (.NOT.VENSTA.AND.FLAG) .OR. SPEULI .OR.
     &   (IMP_FILE%NAME(1:1).NE.' ')) THEN
        IF(FLAG) THEN
          DO IPTFR=1,NPTFR
            UV2D(IPTFR)=UV(NBOR(IPTFR))
            VV2D(IPTFR)=VV(NBOR(IPTFR))
          ENDDO
        ENDIF
        IF(LIMSPE.EQ.7 .OR. SPEULI) THEN
          DO IPTFR=1,NPTFR
            PROF(IPTFR)=DEPTH(NBOR(IPTFR))
          ENDDO
        ENDIF
!
!       WHEN NPB=1 FBOR ONLY FILLED FOR FIRST POINT
!
!       SPECTRUM ON BOUNDARIES
!
        IF(NPB.EQ.NPTFR) THEN
          CALL SPEINI
     &(   FBOR  ,SPEC  ,FRA   ,UV2D  ,VV2D  , FPMAXL,FETCHL,
     &    SIGMAL,SIGMBL,GAMMAL,FPICL, HM0L  ,APHILL,TETA1L,
     &    SPRE1L,TETA2L,SPRE2L,XLAMDL, NPB   ,NDIRE ,NF    ,
     &    LIMSPE,PROF  ,FRABL )
        ELSE
          CALL SPEINI
     &(   FB_CTE,SPEC  ,FRA   ,UV2D  ,VV2D  ,FPMAXL,FETCHL,
     &    SIGMAL,SIGMBL,GAMMAL,FPICL,HM0L  ,APHILL,TETA1L,
     &    SPRE1L,TETA2L,SPRE2L,XLAMDL,NPB   ,NDIRE ,NF    ,
     &    LIMSPE,PROF  ,FRABL )
        ENDIF
!
!       IF THERE IS A MESHED FILE WITH THE BOUNDARY SPECTRA
!       THEY NEED TO BE IMPOSED
!
        IF(IMP_FILE%NAME(1:1).NE.' ')THEN
          CALL IMPOSE_BND_SPECTRA(IMP_FILE,LT,AT,FBOR,NPTFR,NDIRE,NF)
        ENDIF
!
!     ===========================================================
!     TO BE MODIFIED BY USER - RESU CAN BE CHANGED
!     ===========================================================
!
        IF(SPEULI) THEN

          IF (LT.EQ.0) THEN
            REWIND LUFO1
            READ(LUFO1,1000) NPCLI
            IF (NPCLI.NE.0) THEN
              READ(LUFO1,2000) (NPB2(I),I=1,NPCLI)
              MINI=NPTFR
              MAXI=0
              DO I=1,NPCLI
                IF (NPB2(I).GE.MAXI) THEN
                  MAXI=NPB2(I)
                ENDIF
                IF(NPB2(I).LE.MINI) THEN
                  MINI=NPB2(I)
                  IMIN=I
                ENDIF
              ENDDO
              IMAXI=0
              DO I=1,NPTFR
                IF (LIFBOR(I).EQ.KENT.AND.
     &               BOUNDARY_COLOUR(I).GE.IMAXI) THEN
                  IMAXI=BOUNDARY_COLOUR(I)
                ENDIF
              ENDDO
              READ(LUFO1,3000) AT1
              ATT=AT1
              CALL TEMP(AT1,ATT,DDC)
              IF (AT1.GT.AT) THEN
                PRINT*,'ERREUR DEMARAGE LECTURE',AT1,AT
                CALL PLANTE(0)
              ENDIF
50            CONTINUE
              DO IP=1,NPCLI
                READ(LUFO1,4000) ((CL1(I,IFF,IP),I=1,NDIRE),IFF=1,NF)
              ENDDO
              READ(LUFO1,3000) AT2
              ATT=AT2
              CALL TEMP(AT2,ATT,DDC)
              IF (AT2.LT.AT) THEN
                AT1=AT2
                GOTO 50
              ENDIF
              DO IP=1,NPCLI
                READ(LUFO1,4000) ((CL2(I,IFF,IP),I=1,NDIRE),IFF=1,NF)
              ENDDO
              NENR=2
            ENDIF               !NPCLI.NE.0
          ELSE !LT.EQ.0
            IF (NPCLI.NE.0) THEN
              IF (AT.GT.AT2) THEN
                AT1=AT2
                CALL OV('X=Y     ', CL1 , CL2 , CL1 , C , 300*NPCLI)
                READ(LUFO1,3000) AT2
                NENR=NENR+1
                ATT=AT2
                CALL TEMP(AT2,ATT,DDC)
                IF (AT2.LT.AT) THEN
                  CALL PLANTE(0)
                ENDIF
                DO IP=1,NPCLI
                  READ(LUFO1,4000) ((CL2(I,IFF,IP),I=1,NDIRE),IFF=1,NF)
                ENDDO
              ENDIF
            ENDIF
          ENDIF
          COEF=(AT-AT1)/(AT2-AT1)
          DO IPTFR=1,NPTFR
            IF (LIFBOR(IPTFR).EQ.KENT) THEN
              DEB0=0
              FIN=MAXI+1
              DO IP=1,NPCLI
                IF (NPB2(IP).GE.DEB0.AND.
     &              NPB2(IP).LE.BOUNDARY_COLOUR(IPTFR))THEN
                  DEB0=NPB2(IP)
                  IPDEB=IP
                ENDIF
                IF (NPB2(IP).GE.BOUNDARY_COLOUR(IPTFR).AND.
     &              NPB2(IP).LE.FIN)THEN
                  FIN=NPB2(IP)
                  IPFIN=IP
                ENDIF
              ENDDO
              IF(FIN.EQ.MAXI+1) THEN
                IPFIN=IMIN
                IF (IMAXI.LT.MAXI) THEN
                WRITE(LU,*)'THAT SITUATION IS THEORICALLY NOT POSSIBLE'
                  STOP
                ENDIF
                FIN=IMAXI+1
              ENDIF
              IF(FIN.NE.DEB0) THEN
                COEF2=REAL(BOUNDARY_COLOUR(IPTFR)-DEB0)/REAL(FIN-DEB0)
              ELSE
                COEF2=0
              ENDIF
              DO IPLAN=1,NDIRE
                DO IFF=1,NF
                  FCL1=CL1(IPLAN,IFF,IPDEB)+COEF2*
     &                 (CL1(IPLAN,IFF,IPFIN)-CL1(IPLAN,IFF,IPDEB))
                  FCL2=CL2(IPLAN,IFF,IPDEB)+COEF2*
     &                 (CL2(IPLAN,IFF,IPFIN)-CL2(IPLAN,IFF,IPDEB))
                  FBOR(IPTFR,IPLAN,IFF)=FCL1+(FCL2-FCL1)*COEF
                  F(NBOR(IPTFR),IPLAN,IFF)=FBOR(IPTFR,IPLAN,IFF)
                ENDDO
              ENDDO
            ENDIF
          ENDDO
        ENDIF !SPEULI
1000  FORMAT(I5)
2000  FORMAT(21I5)
3000  FORMAT(F9.1)
4000  FORMAT(5G16.7)
!     ===========================================================
!     END OF USER MODIFICATIONS
!     ===========================================================
!
      ENDIF
!

!-----------------------------------------------------------------------
!
      RETURN
      END
