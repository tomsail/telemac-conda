!                   ************************
                    SUBROUTINE SOULSBYFLOC3D
!                   ************************
!
     &(WCHU, FC, MESH3, NPOIN2, NPOIN3, NPLAN, HN, UETCAR)
!
!***********************************************************************
! TELEMAC3D   V7P0                                   29/11/2011
!***********************************************************************
!
!brief  COMPUTES THE FALL VELOCITY OF MUD FLOCS BASED ON SOULSBY ET AL
!       (2013) FORMULATION DERIVED FROM MANNING'S FLOC DATABASE.
!
!history  C. VILLARET & T. BENSON & D. KELLY (HR-WALLINGFORD)
!+        27/02/2014
!+        V7P0
!+   New developments in sediment merged on 25/02/2014.
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| FC             |-->| SUSPENDED SEDIMENT CONCENTRATION
!| HN             |-->| WATER DEPTH
!| MESH3          |-->| 3D MESH BIEF OBJECT
!| NPOIN2         |-->| NUMBER OF POINTS IN 2D MESH
!| NPOIN3         |-->| NUMBER OF POINTS IN 3D MESH (=NPOIN2*NPLAN)
!| NPLAN          |-->| NUMBER OF 3D HORIZONTAL PLANES
!| WCHU           |<--| SPATIALLY VARYING FALL VELOCITY
!| UETCAR         |-->| SHEAR VELOCITY (USTAR) SQUARED AT THE BED
!| WCHU           |<--| SPATIALLY VARYING FALL VELOCITY
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      TYPE(BIEF_MESH), INTENT(INOUT)  :: MESH3
      TYPE(BIEF_OBJ), INTENT(IN)      :: HN
      TYPE(BIEF_OBJ), INTENT(INOUT)   :: WCHU
      INTEGER, INTENT(IN)             :: NPOIN2,NPOIN3,NPLAN
      DOUBLE PRECISION, INTENT(IN)    :: UETCAR(NPOIN2),FC(NPOIN3)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
!     LOCAL
      INTEGER                         :: IPOIN,IP,IPLAN
      DOUBLE PRECISION                :: DENS, USTAR, EPS, DI4, DM4
      DOUBLE PRECISION                :: WS_MIC, WS_MAC, RR, DLC, BIGX
      DOUBLE PRECISION                :: EXP1, NU_WCUB, ETA
      DOUBLE PRECISION                :: Z

!     CONSTANTS:
      DOUBLE PRECISION, PARAMETER     :: KAPPA=0.4D0
      DOUBLE PRECISION, PARAMETER     :: RHO_FIX=1000.D0, NU_W=1.004E-6

!     CALIBRATED COEFFS:
!     MICRO:
      DOUBLE PRECISION, PARAMETER     :: USTARSMIC=0.025D0
      DOUBLE PRECISION, PARAMETER     :: BMIC=0.363D0, LITN=0.66D0

!     MACRO:
      DOUBLE PRECISION, PARAMETER     :: USTARSMAC=0.067D0
      DOUBLE PRECISION, PARAMETER     :: BMAC=0.860D0, LITK=0.0825D0
      DOUBLE PRECISION, PARAMETER     :: DENS2=1.15D0, BIGN=0.463D0
      DOUBLE PRECISION, PARAMETER     :: HMIN_SED=0.2D0
!
!-----------------------------------------------------------------------
!
!     COMPUTE FALL VELOCITY SO THAT IT IS A FUNCTION OF SUSPENDED CONC
!     (AT PREVIOUS TIME STEP)
!     USING THE FORMULA DERIVED BY RICHARD SOULSBY AT HRW.
!
!     RELATIVE WATER SEDIMENT DENSITY
!     (HARD CODED FOR NOW AFTER SOULSBY ET AL [2013])
      DENS    = 2.64D0
!
!     OPTIMISE VARIABLES WHERE POSSIBLE (OUTSIDE LOOP)
      NU_WCUB = NU_W**3      ! KINEMATIC VISCOCITY OF WATER CUBED
      DI4     = 1.D-20       ! DI^4
      DM4     = 1.D-16       !(D_MICRO)^4
      EXP1    = LITK*2.672D0
!
      DO IPOIN = 1,NPOIN2
!
!     TAKE USTAR DIRECTLY FROM TELEMAC
      USTAR  = SQRT(UETCAR(IPOIN))
!
        DO IPLAN=1,NPLAN
!
!         3D NODE INDEX
          IP = IPOIN+NPOIN2*(IPLAN-1)
!
!         LIMIT THE SETTLING VELOCITY IN VERY SHALLOW WATER
!         THIS WILL USUALLY BE UNNECESSARY SINCE HMIN IS USUALLY LARGER
          IF(HN%R(IPOIN).LT.0.01D0.OR.USTAR.LT.0.00001.OR.
     &      FC(IP).LT.1.E-6) THEN
!           if inside loop?
            WCHU%R(IP) = 0.0002D0
          ELSE
!
!           COMPUTE DIMENSIONLESS CONCENTRATION IN SAME WAY AS SOULSBY
!           (I.E USING WATER DENSITY)
!
!           NOTE ALSO: TRACERS ARE TREATED AS DIMENSIONLESS HERE.
!           SO WE NEED MASS CONCOF SEDIMENT PER UNIT MASS OF WATER.
!
            DLC = FC(IP)/RHO_FIX
!
!           NB: AS UNITS OF RHO_FIX ARE KG/M3 IT IS IMPORTANT THAT THE SED
!               CONCIS IN KG/M3
!
!           CALCULATE THE HEIGHT OF THE CELL CENTRE FOR TOP AND BOTTOM PLANES
            IF (IPLAN.EQ.1) THEN
!             25% OF DISTANCE FROM BED TO PLANE 2
              Z = (MESH3%Z%R(IP+NPOIN2)-MESH3%Z%R(IP))*0.25
            ELSEIF (IPLAN.EQ.NPLAN) THEN
!             25% OF DISTINCE FROM SURFACE TO PLANE NPLAN-1
              Z = (MESH3%Z%R(IP)-MESH3%Z%R(IP-NPOIN2))*0.75 +
     &        MESH3%Z%R(IP-NPOIN2) - MESH3%Z%R(IPOIN)
            ELSE
!             INTERNAL VERTICAL CELL CENTRES ARE AT THE PLANE ELEVATIONS
              Z = MESH3%Z%R(IP) - MESH3%Z%R(IPOIN)
            ENDIF
!
!           FIND ETA & PREVENT FAILURE IN SHALLOW WATERS (IN CASE HMIN<0.01)
            IF(HN%R(IPOIN).GE.HMIN_SED) THEN
              ETA = 1.D0-Z/HN%R(IPOIN)
            ELSE
              ETA = 0.5D0
            ENDIF
!
!           CALCULATE EPSILON
!           NOTE: IT WOULD BE POSSIBLE TO USE EPSILON FROM K-E MODEL INSTEAD HERE
!             (SEE COMMENTED CODE BELOW).
!             ALSO, SINCE WE ARE USING CELL CENTRE HEIGHTS THEN Z WILL
!             NEVER BE ZERO (ZERO DEPTH ACCOUNTED FOR ABOVE) HENCE
!             THERE IS NO POSSIBILITY OF DIVIDE BY ZERO.
            EPS    = USTAR**3*ETA/(KAPPA*Z)
!
!           MICROFLOCS:
            WS_MIC = BMIC*(DENS-1.D0)*((EPS*DI4/NU_WCUB)**0.39D0)   *
     &             9.81D0*SQRT(NU_W/EPS)              *
     &             EXP(-(USTARSMIC/(USTAR*SQRT(ETA)))**LITN)
!
!           MACROFLOCS:
            WS_MAC = BMAC*(DENS2-1.D0)*((EPS*DM4/NU_WCUB)**0.166D0) *
     &             9.81D0*(DLC**EXP1)*SQRT(NU_W/EPS)              *
     &             EXP(-(USTARSMAC/(USTAR*SQRT(ETA)))**BIGN)
!
!           SECOND SET OF EQUATIONS WHICH SUBSTITUTE EPSILON FROM K-E MODEL
!           (AS IN SOULSBY ET AL (2013). COMMENTED FOR NOW.
!           MICROFLOCS:
!              WS_MIC = 0.5372D0*((USTAR^3*ETA*DI4)/(NU_WCUB*Z))**0.39D0 *
!                       9.81D0                  * SQRT((NU_W*Z)/(USTAR**3*ETA)) *
!                       EXP(-(USTARSMIC/(USTAR*SQRT(ETA)))**0.266);
!
!           MACROFLOCS:
!              WS_MAC = 0.095D0*((USTAR**3*ETA*DM4)/(NU_WCUB*Z))**0.166D0 *
!           &           9.81D0*(DLC**0.22044D0) * SQRT((NU_W*Z)/(USTAR**3*ETA)) *
!           &           EXP(-(USTARSMAC/(USTAR*SQRT(ETA)))**0.463);
!
!           CALCULATE PROPORTIONAL SCALING SWITCH FROM DIMENSONLESS CONC
!           (THIS IS EQUIVALENT TO LOG10(FC) WITH FC IN MG/L)
            BIGX = LOG10(DLC)+6.D0
!
            IF(BIGX.LT.0.D0) THEN
              RR=0.1D0
            ELSEIF ((BIGX.GE.0.D0).AND.(BIGX.LT.4.07D0)) THEN
              RR=0.1D0+0.221D0*BIGX
            ELSE
              RR=1.D0
            ENDIF
!
!           GET WEIGHTED AVERAGE (BASED ON RR) TO PROVIDE THE EFFECTIVE
!           SETTLING VELOCITY:
            WCHU%R(IP) = MAX(WS_MAC*RR+(1.D0-RR)*WS_MIC,0.0002D0)
!
          ENDIF
!
        END DO
      END DO
!
!-----------------------------------------------------------------------
!
      RETURN
      END


