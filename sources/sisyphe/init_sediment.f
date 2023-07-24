!                   ************************
                    SUBROUTINE INIT_SEDIMENT
!                   ************************
!
     &(NSICLA,ELAY,ZF,ZR,NPOIN,AVAIL,FRACSED_GF,AVA0,
     & LGRAFED,CALWC,XMVS,XMVE,GRAV,VCE,XWC,FDM,
     & CALAC,AC,SEDCO,ES,ES_SABLE, ES_VASE ,NOMBLAY,CONC_VASE,
     & MS_SABLE,MS_VASE,ACLADM,UNLADM,TOCE_SABLE,
     & CONC,DEBU,MIXTE)
!
!***********************************************************************
! SISYPHE   V7P2                                   27/06/2016
!***********************************************************************
!
!brief
!
!history  C. VILLARET (LNHE)
!+        30/12/2008
!+
!+
!
!history  JMH
!+        16/09/2009
!+        V6P0
!+   AVAIL(NPOIN,10,NSICLA)
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
!history  C.VILLARET (EDF-LNHE), P.TASSI (EDF-LNHE)
!+        19/07/2011
!+        V6P1
!+  Name of variables
!+
!
!history  P.TASSI (EDF-LNHE)
!+        30/05/2012
!+        V6P2
!+  Case DSTAR > 150 AC(I) = 0.045D0
!+
!
!history  P.TASSI (EDF-LNHE)
!+        06/07/2012
!+        V6P2
!+  Line MIXTE=.FALSE. added.
!
!history  R.KOPMANN (BAW)
!+        27/06/2016
!+        V7P2
!+  CONTINUOUS APPROSIMATION CRITICAL SHILEDS CURVE DSTAR > 72 AC(I) = 0.045D0
!
!history  U.MERKEL AND R.KOPMANN (BAW)
!+        07/12/2017
!+        V7P2
!+  CALL TO CVSP_INIT
!
!history  R.KOPMANN (BAW)
!+        15/02/2019
!+        V7P2
!+  CALCULATION INITIAL VOLUME FOR SINGLE CLASS MODE
!+  ZEROING VARIABLES FOR MASS BALANCE
!
!history  R.KOPMANN (BAW)
!+        17/04/2019
!+        V8P2
!+  INITIALISING ES(:,1)=ELAY(:) IN CASE OF NSICLA=1
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| AC             |<->| CRITICAL SHIELDS PARAMETER
!| ACLADM         |-->| MEAN DIAMETER OF SEDIMENT
!| AT0            |<->| TIME IN S
!| AVAIL          |<->| VOLUME PERCENT OF EACH CLASS
!| CALAC          |-->| IF YES, SHIELDS PARAMETER FOUND IN PARAMETER FILE
!| CALWC          |-->| IF YES, SETTLING VELOCITIES FOUND IN PARAMETER FILE
!| CONC_VASE      |<->| MUD CONCENTRATION FOR EACH LAYER
!| ELAY           |<->| THICKNESS OF SURFACE LAYER
!| ES             |<->| LAYER THICKNESSES AS DOUBLE PRECISION
!| ES_SABLE       |<->| LAYER THICKNESSES OF SAND AS DOUBLE PRECISION
!| ES_VASE        |<->| LAYER THICKNESSES OF MUD AS DOUBLE PRECISION
!| FDM            |-->| DIAMETER DM FOR EACH CLASS
!| FRACSED_GF     |-->|
!| GRAV           |-->| ACCELERATION OF GRAVITY
!| LGRAFED        |-->|
!| MS_SABLE       |<->| MASS OF SAND PER LAYER (KG/M2)
!| MS_VASE        |<->| MASS OF MUD PER LAYER (KG/M2)
!| ES_SABLE       |<->| THICKNESS OF SAND LAYER (M)
!| ES_VASE        |<->| THICKNESS OF MUD LAYER  (M)
!| MIXTE          |<->| SEDIMENT MIXTE  (SABLE /VASE)
!| NOMBLAY        |-->| NUMBER OF BED LAYERS
!| NPOIN          |-->| NUMBER OF POINTS
!| NSICLA         |-->| NUMBER OF SEDIMENT CLASSES
!| SEDCO          |-->| LOGICAL, SEDIMENT COHESIVE OR NOT
!| UNLADM         |-->| MEAN DIAMETER OF ACTIVE STRATUM LAYER
!| VCE            |-->| WATER VISCOSITY
!| XMVE           |-->| FLUID DENSITY
!| XMVS           |-->| WATER DENSITY
!| XWC            |-->| SETTLING VELOCITY
!| ZF             |-->| ELEVATION OF BOTTOM
!| ZR             |-->| NON ERODABLE BED
!| CONC           |<->| CONCENTRATION OF BED LAYER
!| DEBU           |-->| FLAG, RESTART ON SEDIMENTOLOGICAL FILE
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE INTERFACE_SISYPHE, EX_INIT_SEDIMENT => INIT_SEDIMENT
      USE DECLARATIONS_SISYPHE, ONLY : VSMTYPE
     & ,ALT_MODEL, PRO_MAX_MAX, CVSMPPERIOD,NIT,VOLTOT,VOLU2D,VOLINI
     & ,RMASCL,VCUMUCL,VOLNESTORCL

      USE INTERFACE_PARALLEL, ONLY : P_DSUM
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER,           INTENT(IN)     :: NSICLA,NPOIN,NOMBLAY
      TYPE(BIEF_OBJ),    INTENT(INOUT)  :: ELAY,ZF,ZR
      TYPE(BIEF_OBJ), INTENT(INOUT)     :: MS_SABLE, MS_VASE
      TYPE(BIEF_OBJ),    INTENT(INOUT)  :: ACLADM, UNLADM
      LOGICAL,           INTENT(IN)     :: LGRAFED,CALWC
      LOGICAL,           INTENT(IN)     :: CALAC
      DOUBLE PRECISION,  INTENT(IN)     :: XMVS,XMVE,GRAV,VCE
      DOUBLE PRECISION,  INTENT(INOUT)  :: AVA0(NSICLA)
      DOUBLE PRECISION,  INTENT(INOUT)  :: AVAIL(NPOIN,NOMBLAY,NSICLA)
      DOUBLE PRECISION,  INTENT(INOUT)  :: FRACSED_GF(NSICLA)
      DOUBLE PRECISION,  INTENT(INOUT)  :: FDM(NSICLA),XWC(NSICLA)
      DOUBLE PRECISION,  INTENT(INOUT)  :: AC(NSICLA),TOCE_SABLE
      LOGICAL,           INTENT(IN)     :: SEDCO(NSICLA), DEBU
      LOGICAL,           INTENT(IN)     :: MIXTE
      DOUBLE PRECISION, INTENT(IN)    :: CONC_VASE(NOMBLAY)
      DOUBLE PRECISION, INTENT(INOUT) :: ES(NPOIN,NOMBLAY)
      DOUBLE PRECISION, INTENT(INOUT) :: ES_SABLE(NPOIN,NOMBLAY)
      DOUBLE PRECISION, INTENT(INOUT) :: ES_VASE(NPOIN,NOMBLAY)
      DOUBLE PRECISION, INTENT(INOUT) :: CONC(NPOIN,NOMBLAY)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER            :: I,J
      DOUBLE PRECISION   :: DENS,DSTAR
!
!======================================================================!
!======================================================================!
!                               PROGRAM                                !
!======================================================================!
!======================================================================!
!
!  ------ BED COMPOSITION
!
      CALL OS('X=Y-Z   ',X=ELAY,Y=ZF,Z=ZR)
!
!     ONLY ONE CLASS
!
      IF(NSICLA.EQ.1) THEN
        DO I=1,NPOIN
          AVAIL(I,1,1) = 1.D0
          ACLADM%R(I) = FDM(1)
        ENDDO
!
        IF(NOMBLAY.EQ.1) THEN
          DO I=1,NPOIN
            ES(I,1) = ELAY%R(I)
          ENDDO
        ENDIF
!
!     COMPUTES THE INITIAL VOLUME OF SEDIMENTS
        VOLTOT(1)=0.D0
        DO J=1,NPOIN
            VOLTOT(1) = VOLTOT(1) + ELAY%R(J)*VOLU2D%R(J)
        ENDDO
        IF(NCSIZE.GT.1) THEN
          VOLTOT(1) = P_DSUM(VOLTOT(1))
        ENDIF
        VOLINI(1) = VOLTOT(1)
!     INITIALISING VOLUMES
        RMASCL = 0.D0
        VCUMUCL = 0.D0
        VOLNESTORCL = 0.D0
!
!       PURE MUD ONLY
        IF(SEDCO(1)) CALL INIT_MIXTE(XMVS,NPOIN,AVAIL,NSICLA,ES,
     &                               ES_SABLE, ES_VASE,
     &                               ELAY%R,NOMBLAY,CONC_VASE,
     &                                MS_SABLE%R,MS_VASE%R,ZF%R,
     &                               ZR%R,AVA0,CONC,DEBU,.FALSE.)
!
      ELSE
!
!     NON-COHESIVE, MULTI-CLASSES
!
        IF(.NOT.MIXTE) THEN
!
!
          CALL INIT_AVAI
!         CALL MEAN_GRAIN_SIZE
!         THIS PART CAN BE INTEGRATED INTO INIT_AVAI
          DO J=1,NPOIN
            ACLADM%R(J) = 0.D0
            UNLADM%R(J) = 0.D0
            DO I=1,NSICLA
              IF(AVAIL(J,1,I).GT.0.D0) THEN
                ACLADM%R(J) = ACLADM%R(J) + FDM(I)*AVAIL(J,1,I)
                UNLADM%R(J) = UNLADM%R(J) + FDM(I)*AVAIL(J,2,I)
              ENDIF
            ENDDO
            ACLADM%R(J)=MAX(ACLADM%R(J),0.D0)
            UNLADM%R(J)=MAX(UNLADM%R(J),0.D0)
          ENDDO
        ELSE
!
          CALL INIT_MIXTE(XMVS,NPOIN,AVAIL,NSICLA,ES,
     &               ES_SABLE, ES_VASE, ELAY%R,
     &               NOMBLAY,CONC_VASE,MS_SABLE%R,
     &               MS_VASE%R,ZF%R,ZR%R,AVA0,CONC,DEBU,MIXTE)
          DO I=1,NPOIN
            ACLADM%R(I) = FDM(1)
          ENDDO
        ENDIF
!
      ENDIF
!
      IF(LGRAFED) THEN
        DO I=1, NSICLA
          FRACSED_GF(I)=AVA0(I)
        ENDDO
      ENDIF
!
!     SETTLING VELOCITY
!
      IF(.NOT.CALWC) THEN
        DENS = (XMVS - XMVE) / XMVE
        DO I = 1, NSICLA
          CALL VITCHU_SISYPHE(XWC(I),DENS,FDM(I),GRAV,VCE)
        ENDDO
      ENDIF
!
!     SHIELDS PARAMETER
!
      IF(.NOT.CALAC) THEN
        DENS  = (XMVS - XMVE )/ XMVE
        DO I = 1, NSICLA
          DSTAR = FDM(I)*(GRAV*DENS/VCE**2)**(1.D0/3.D0)
          IF (DSTAR <= 4.D0) THEN
            AC(I) = 0.24D0/DSTAR
          ELSEIF (DSTAR <= 10.D0) THEN
            AC(I) = 0.14D0*DSTAR**(-0.64D0)
          ELSEIF (DSTAR <= 20.D0) THEN
            AC(I) = 0.04D0*DSTAR**(-0.1D0)
!           CORRECTION 27/06/2016
!          ELSEIF (DSTAR <= 150.D0) THEN
          ELSEIF (DSTAR <= 72.D0) THEN
            AC(I) = 0.013D0*DSTAR**0.29D0
          ELSE
!           CORRECTION 30/05/2012
!           AC(I) = 0.055D0
            AC(I) = 0.045D0
          ENDIF
        ENDDO
      ENDIF
!
!     FOR MIXED SEDIMENTS
!
      IF(MIXTE) TOCE_SABLE=AC(1)*FDM(1)*GRAV*(XMVS - XMVE)
!
!-----------------------------------------------------------------------
!
      IF(VSMTYPE.EQ.1) THEN
        WRITE(LU,*) ' '
        WRITE(LU,*) '--------------------------------------------------'
        WRITE(LU,*) 'C-VSM MODEL'
        WRITE(LU,*) 'CONTINUOUS VERTICAL GRAIN SORTING STRATIGRAPHY'
        WRITE(LU,*) ' '
        WRITE(LU,*) 'ACTIVE LAYER THICKNESS MODEL:', ALT_MODEL
!
        IF (PRO_MAX_MAX .GT. 250) THEN
          WRITE(LU,*) 'HIGH NUMBER OF SECTIONS IS EXPENSIVE',PRO_MAX_MAX
          WRITE(LU,*) 'BETTER < 250 and > 4 + 4 x NUMBER OF FRACTIONS'
        ENDIF
!
        IF ((CVSMPPERIOD / NIT) .GT. 5) THEN
          WRITE(LU,*) 'HIGH NUMBER OF FULL CVSM PRINTOUTS'
          WRITE(LU,*) 'ATTENTION: DISK SPACE AND SIMULATION TIME'
          WRITE(LU,*) 'ADAPT C-VSM FULL PRINTOUT PERIOD'
        ENDIF
!
        WRITE(LU,*) ' '
        CALL CVSP_INIT()
        WRITE(LU,*)'CVSM INITIALISED!'
        WRITE(LU,*) '--------------------------------------------------'
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
