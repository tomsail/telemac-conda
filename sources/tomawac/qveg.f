!                   ***************
                    SUBROUTINE QVEG
!                   ***************
!
     &   (TSTOT, TSDER, F, VARIAN, FMOY, XKMOY, NF, NDIRE, NPOIN2)
!
!***********************************************************************
! TOMAWAC   V8P4
!***********************************************************************
!
!brief    Takes into account the friction due to vegetation
!
!history  VITO BACCHI (EDF - LNHE)
!+        12/09/2014
!+        V7P0
!+   First version, on birthday eve...
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| DEPTH          |-->| WATER DEPTH
!| F              |-->| DIRECTIONAL SPECTRUM
!| FMOY           |-->| MEAN SPECTRAL FRQUENCY FMOY (relative frequency)
!| NF             |-->| NUMBER OF FREQUENCIES
!| NDIRE          |-->| NUMBER OF DIRECTIONS
!| NPOIN2         |-->| NUMBER OF POINTS IN 2D MESH
!| TSDER          |<->| DERIVED PART OF THE SOURCE TERM CONTRIBUTION
!| TSTOT          |<->| TOTAL PART OF THE SOURCE TERM CONTRIBUTION
!| VARIAN         |-->| ENERGY SPECTRUM VARIANCE
!| XKMOY          |-->| AVERAGE WAVE NUMBER
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_TOMAWAC, ONLY : DEUPI, GRAVIT, PI, DEPTH, X, Y,
     &                                 WAC_FILES, WACZON, LT, COEVEG,
     &                                 NUMPLAM2, STEMDIAM, BDC, VEGH
!     COEVEG : VEGETATION DISSIPATION COEFFICIENT
!     NUMSQM : NUMBER OF PLANTS PER M2
!     STEMDIAM : STEM DIAMETER
!     BDC : BULK DRAG COEFFICIENT
!     VEGH : VEGETATION HEIGHT
!
      USE INTERFACE_TOMAWAC, EX_QVEG => QVEG
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)             :: NF,NDIRE,NPOIN2
      DOUBLE PRECISION, INTENT(IN)    :: XKMOY(NPOIN2),VARIAN(NPOIN2)
      DOUBLE PRECISION, INTENT(INOUT) :: TSTOT(NPOIN2,NDIRE,NF)
      DOUBLE PRECISION, INTENT(INOUT) :: TSDER(NPOIN2,NDIRE,NF)
      DOUBLE PRECISION, INTENT(IN)    :: F(NPOIN2,NDIRE,NF),FMOY(NPOIN2)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER  JP, JF, IP, POLY
      DOUBLE PRECISION C1, CVEG, ALFA, KH, AKH
      DOUBLE PRECISION AUX, NUM, DENUM, SHAKH, BETA
!     To define the areas
      INTEGER,SAVE :: NPOLY
      INTEGER, ALLOCATABLE,DIMENSION(:),SAVE :: NSOM
      DOUBLE PRECISION, ALLOCATABLE,SAVE :: XSOM(:,:),YSOM(:,:)
!
      IF (LT.EQ.1) THEN
        CALL READ_POLY(WAC_FILES(WACZON),NPOLY,NSOM,XSOM,YSOM)
      ENDIF
!
!-----------------------------------------------------------------------
!
      C1 = - SQRT(2.D0/PI)*GRAVIT**2
      CVEG = C1*BDC*STEMDIAM*NUMPLAM2/(DEUPI**3)
!     VEGETATION OVER A CONSTANT DEPTH
!     COMPUTES THE BETA COEFFICIENT : QVEG1 = BETA * F
      DO IP=1,NPOIN2
        ALFA=0.
        DO POLY=1,NPOLY
          IF (INPOLY( X(IP), Y(IP), XSOM(:,POLY), YSOM(:,POLY),
     &        NSOM(POLY))) THEN
            ALFA = VEGH/DEPTH(IP)
          ENDIF
        ENDDO
        KH = XKMOY(IP)*DEPTH(IP)
        AKH = ALFA*KH
        SHAKH = SINH(AKH)
        NUM = SHAKH*(SHAKH**2 + 3.D0)
        DENUM = 3.D0*XKMOY(IP)*COSH(KH)**3
        AUX = (XKMOY(IP)/FMOY(IP))**3
        BETA = COEVEG*CVEG*AUX*(NUM/DENUM)*SQRT(VARIAN(IP))
!
!     LOOP OVER THE DISCRETISED FREQUENCIES
!
!     TAKES THE SOURCE TERM INTO ACCOUNT
!
        DO JF=1,NF
          DO JP=1,NDIRE
            TSTOT(IP,JP,JF) = TSTOT(IP,JP,JF)+BETA*F(IP,JP,JF)
            TSDER(IP,JP,JF) = TSDER(IP,JP,JF)+BETA
          ENDDO
        ENDDO

      ENDDO

!
!-----------------------------------------------------------------------
!
      RETURN
      END
