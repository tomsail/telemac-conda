!                   **************************
                    SUBROUTINE TIDAL_MODEL_T3D
!                   **************************
!
!
!***********************************************************************
! TELEMAC3D   V8P2
!***********************************************************************
!
!brief    FINDS TIDAL BOUNDARY CONDITIONS AT THE OPEN SEA BOUNDARIES
!+
!
!history  C-T PHAM (LNHE)
!+        27/09/2011
!+        V6P2
!+
!
!history  C-T PHAM (LNHE)
!+        02/10/2014
!+        V7P0
!+   BORD_TIDE_LEGOS changed into BORD_TIDE_MISC
!+   (e.g. LEGOS-NEA, FES20XX, Previmer)
!+   Default NODALCORR = 0 (not frozen, computed at each time step)
!+
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!|                |-->|
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_TELEMAC
      USE DECLARATIONS_TELEMAC3D
      USE INTERFACE_TELEMAC2D
      USE TPXO
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER K,NODALCORR,NP,IBORD,IFRLIQ
      DOUBLE PRECISION XSHIFT,YSHIFT,BETA0
      LOGICAL TIDALBCGEN,TM2S2N2EQUAL
!
!-----------------------------------------------------------------------
!
!     PARAMETERS FOR TIDAL BOUNDARY CONDITIONS
!
!     CTIDEV: COEFFICIENT TO CALIBRATE THE VELOCITIES
!             DEFAULT = SQRT(CTIDE)
!
!     ACCORDING TO DV, IF A CORRECTION COEFFICIENT CTIDE IS APPLIED
!     FOR WATER DEPTHS, ANOTHER ONE MUST BE APPLIED FOR VELOCITIES
!     = SQRT(CTIDE)
!
      IF(CTIDEV.EQ.999999.D0) CTIDEV = SQRT(CTIDE)
!
!     NODALCORR: OPTION FOR CALCULATION OF NODAL FACTOR CORRECTION F
!                IN SUBROUTINES BORD_TIDE AND BORD_TIDE_MISC
!                DEFAULT = 0 (NOT FROZEN, WARNING: CHANGED VALUE, 1 UNTIL V6P3)
!                0: NOT FROZEN, COMPUTED AT EACH TIME STEP
!                1: FROZEN WITH VALUE AT THE BEGINNING OF THE SIMULATION
!                2: FROZEN WITH VALUE AT THE MIDDLE OF THE YEAR IN MARDAT
!                   (SINGLE FORMER POSSIBILITY FOR TIDALTYPE = 7)
!     IN THE STEERING FILE, THE KEYWORDS 'ORIGINAL DATE OF TIME'
!     AND 'ORIGINAL HOUR OF TIME' HAVE TO BE SET
!     WARNING, FORMAT: YEAR, MONTH, DAY
!
      NODALCORR = 0
!
!     TIDALBCGEN: LOGICAL FOR GENERATION OF TIDAL BOUNDARY CONDITIONS OR NOT
!                 CURRENTLY WORKS ONLY FOR SCALAR COMPUTATIONS
!                 FOR JMJ DATA BASE ONLY AT THE MOMENT
!
      TIDALBCGEN = .FALSE.
!
!     TM2S2N2EQUAL: LOGICAL TO IMPOSE THE PERIODS OF S2 AND N2 WAVES
!                   TO BE EQUAL TO THE PERIOD OF M2 WAVE
!                   DEFAULT = .TRUE. (WARNING: CHANGED VALUE, .FALSE. IN V6P2)
!                   FOR SCHEMATIC TIDES MODELLING ONLY!
!                   FOR JMJ DATA BASE ONLY AT THE MOMENT
!
      TM2S2N2EQUAL = .TRUE.
!
!     OPTIONAL SHIFT OF COORDINATES
!     FOR JMJ DATA BASE ONLY AT THE MOMENT
!
      XSHIFT = 0.D0
      YSHIFT = 0.D0
!
!     BETA0: OPTIONAL ANGLE (IN DEGREES) BETWEEN LAMBERT AND MERCATOR-JMJ
!            REFERENCES (EAST OR X AXES, TRIGONOMETRIC)
!            DEFAULT = 0.D0 DEGREES
!            FOR JMJ DATA BASE ONLY AT THE MOMENT
!
      BETA0 = 0.D0
!
!     FILES:
!
!     T3DBDD: TIDE DATA BASE
!     T3DHAR: HARMONIC CONSTANTS FILE
!     T3DTID: TIDAL MODEL FILE
!     T3DL93: CONVERSION GRID FOR LAMBERT 93
!
!-----------------------------------------------------------------------
!
!     AUTOMATIC TIDAL BOUNDARY CONDITIONS
!
      IF(TIDALDB.EQ.1) THEN
        IF(TIDALBCGEN) THEN
          IF(T3D_FILES(T3DBDD)%NAME(1:1).EQ.' ') THEN
            WRITE(LU,*) 'TO GENERATE THE HARMONIC CONTANTS FILE'
            WRITE(LU,*) 'FOR JMJ DATA BASE, PLEASE GIVE'
            WRITE(LU,*) 'ASCII DATABASE FOR TIDE FILE.'
            CALL PLANTE(1)
            STOP
          ENDIF
          IF(T3D_FILES(T3DTID)%NAME(1:1).EQ.' ') THEN
            WRITE(LU,*) 'TO GENERATE THE HARMONIC CONTANTS FILE'
            WRITE(LU,*) 'FOR JMJ DATA BASE, PLEASE GIVE'
            WRITE(LU,*) 'THE TIDAL MODEL FILE'
            CALL PLANTE(1)
            STOP
          ENDIF
          IF(T3D_FILES(T3DHAR)%NAME(1:1).EQ.' ') THEN
            WRITE(LU,*) 'PLEASE GIVE THE HARMONIC CONTANTS FILE.'
            CALL PLANTE(1)
            STOP
          ENDIF
          IF(     GEOSYST.EQ.4.AND.NUMZONE.EQ.93
     &       .AND.T3D_FILES(T3DL93)%NAME(1:1).EQ.' ') THEN
            WRITE(LU,*) 'PLEASE GIVE THE LAMBERT 93 CONVERSION FILE'
            CALL PLANTE(1)
            STOP
          ENDIF
          CALL BORD_TIDAL_BC(MESH2D%NBOR%I,LIHBOR%I,LIUBOL%I,
     &                       NPTFR2,KENT,KENTU,
     &                       MESH2D,GEOSYST,NUMZONE,
     &                       T3D_FILES(T3DL93)%LU,LATIT,LONGIT,
     &                       TIDALTYPE,BOUNDARY_COLOUR,MAXFRO,
     &                       T3D_FILES(T3DBDD)%LU,
     &                       T3D_FILES(T3DTID)%FMT,T3D_FILES(T3DTID)%LU,
     &                       T3D_FILES(T3DHAR)%LU,XSHIFT,YSHIFT,BETA0,
     &                       I_ORIG,J_ORIG)
        ENDIF
!
        IF(T3D_FILES(T3DHAR)%NAME(1:1).EQ.' ') THEN
          WRITE(LU,*) 'PLEASE GIVE THE HARMONIC CONTANTS FILE.'
          CALL PLANTE(1)
          STOP
        ENDIF
        CALL BORD_TIDE(ZF%R,MESH2D%NBOR%I,LIHBOR%I,LIUBOL%I,
     &                 NPOIN2,NPTFR2,AT,DT,
     &                 NUMLIQ%I,KENT,KENTU,
     &                 TIDALTYPE,
     &                 CTIDE,MSL,CTIDEV,NODALCORR,T3D_FILES(T3DHAR)%LU,
     &                 BOUNDARY_COLOUR,
     &                 HBTIDE,UBTIDE,VBTIDE,NUMTIDE,ICALHWB,
     &                 MARDAT,MARTIM,TM2S2N2EQUAL)
      ELSEIF(TIDALDB.EQ.2) THEN
        CALL BORD_TIDE_TPXO(ZF%R,MESH2D%NBOR%I,LIHBOR%I,LIUBOL%I,
     &                      NPOIN2,NPTFR2,AT,
     &                      NUMLIQ%I,KENT,KENTU,
     &                      TIDALTYPE,
     &                      CTIDE,MSL,CTIDEV,
     &                      BOUNDARY_COLOUR,
     &                      HBTIDE,UBTIDE,VBTIDE,ICALHWG,
     &                      MARDAT,MARTIM,T3D_FILES,T3DBB1,T3DBB2,
     &                      X,Y,GEOSYST,NUMZONE,T3DL93,LATIT,LONGIT,
     &                      I_ORIG,J_ORIG,INTMICON,HMIN_VIT_BC)
      ELSEIF(TIDALDB.EQ.3) THEN
        IF(T3D_FILES(T3DHAR)%NAME(1:1).EQ.' ') THEN
          WRITE(LU,*) 'PLEASE GIVE THE HARMONIC CONTANTS FILE.'
          CALL PLANTE(1)
          STOP
        ENDIF
        CALL BORD_TIDE_MISC(ZF%R,MESH2D%NBOR%I,LIHBOR%I,LIUBOL%I,
     &                      NPOIN2,NPTFR2,AT,DT,
     &                      NUMLIQ%I,KENT,KENTU,
     &                      TIDALTYPE,
     &                      CTIDE,MSL,CTIDEV,NODALCORR,
     &                      T3D_FILES(T3DHAR)%LU,BOUNDARY_COLOUR,
     &                      HBTIDE,UBTIDE,VBTIDE,NUMTIDE,ICALHWB,
     &                      MARDAT,MARTIM)
      ELSEIF(TIDALDB.EQ.-1) THEN
        WRITE(LU,*) 'INCORRECT DEFAULT VALUE FOR TIDAL DATA BASE.'
        WRITE(LU,*) 'POSSIBLE CHOICES:'
        WRITE(LU,*) '  1: JMJ,'
        WRITE(LU,*) '  2: TPXO,'
        WRITE(LU,*) '  3: MISC (LEGOS-NEA, FES20XX, PREVIMER...).'
        CALL PLANTE(1)
        STOP
      ELSE
        WRITE(LU,*) 'TIDAL DATA BASE NOT TAKEN INTO ACCOUNT.'
        WRITE(LU,*) 'POSSIBLE CHOICES:'
        WRITE(LU,*) '  1: JMJ,'
        WRITE(LU,*) '  2: TPXO,'
        WRITE(LU,*) '  3: MISC (LEGOS-NEA, FES20XX, PREVIMER...).'
        CALL PLANTE(1)
        STOP
      ENDIF
!
!-----------------------------------------------------------------------
!
      DO K=1,NPTFR2
        IFRLIQ=NUMLIQ%I(K)
!       TEST ON NUMTIDE PROBABLY NO LONGER USEFUL
        IF(NUMTIDE%I(K).GT.0.AND.IFRLIQ.GT.0) THEN
          IF(BND_TIDE(IFRLIQ).GT.0) THEN
!           POSSIBLE SMOOTHING AT THE BEGINNING
!           IF(AT.LT.1800.D0) THEN
!             UBTIDE%R(K) = UBTIDE%R(K)*(AT/1800.D0)
!             VBTIDE%R(K) = VBTIDE%R(K)*(AT/1800.D0)
!           ENDIF
            IF(LIUBOL%I(K).EQ.KENTU) THEN
              DO NP=1,NPLAN
                IBORD=(NP-1)*NPTFR2+K
                UBORL%R(IBORD) = UBTIDE%R(K)
                VBORL%R(IBORD) = VBTIDE%R(K)
                WBORL%R(IBORD) = 0.D0
                U%R((NP-1)*NPOIN2+NBOR2%I(K)) = UBORL%R(IBORD)
                V%R((NP-1)*NPOIN2+NBOR2%I(K)) = VBORL%R(IBORD)
                W%R((NP-1)*NPOIN2+NBOR2%I(K)) = 0.D0
              ENDDO
            ENDIF
            IF(LIHBOR%I(K).EQ.KENT) THEN
              HBOR%R(K) = HBTIDE%R(K)
            ENDIF
          ENDIF
        ENDIF
      ENDDO
!
!-----------------------------------------------------------------------
!
      RETURN
      END
