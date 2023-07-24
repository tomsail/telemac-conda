!                   ************************
                    SUBROUTINE BEDLOAD_WILCOCK_CROWE
!                   ************************
!
     &(TOB, MU, ACLADM, DM, AVA, GRAV, XMVE, XMVS, SANFRA, QSC, AC, ACP,
     & SLOPEFF,COEFPN)
!
!***********************************************************************
! SISYPHE   V8P0                                   16/09/2018
!***********************************************************************
!
!brief    WILCOCK AND CROWE NON-UNIFORM TRANSPORT FORMULATION.
!
!history  F.CORDIER & P. TASSI (EDF-LNHE)
!+        16/09/2018
!+        V8P0
!+  Implementation of the Wilcock and Crowe formula
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| AC             |<->| CRITICAL SHIELDS PARAMETER
!| ACP            |<->| MODIFIED SHIELDS PARAMETER
!| COEB           |-->| POWER COEFF OF THE HIDING-EXPOSURE FUNCTION
!| COEFPN         |<->| CORRECTION OF TRANSORT FOR SLOPING BED EFFECT
!| DENS           |-->| RELATIVE DENSITY
!| DM             |-->| SEDIMENT GRAIN DIAMETER
!| GRAV           |-->| ACCELERATION OF GRAVITY
!| QSC            |<->| BED LOAD TRANSPORT
!| SANFRA         |-->| SAND FRACTION
!| SLOPEFF        |-->| LOGICAL, SLOPING BED EFFECT OR NOT
!| TETAP          |-->| ADIMENSIONAL SKIN FRICTION
!| TORATIO        |-->| RATIO BETWEEN BED SHEAR STRESS AND TORI
!| TORI           |-->| REFERENCE SHEAR STRESS OF THE I-TH SIZE SEDIMENT
!| TORM           |-->| REFERENCE SHEAR STRESS OF THE MEDIAN SEDIMENT
!| WI             |-->| DIMENSIONLESS TRANSPORT RATE OF WC-2003
!| WCC            |-->| COEFFICIENT OF CALIBRATION
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE INTERFACE_SISYPHE,
     &      EX_BEDLOAD_WILCOCK_CROWE => BEDLOAD_WILCOCK_CROWE
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER,          INTENT(IN)    :: SLOPEFF
      TYPE(BIEF_OBJ),   INTENT(INOUT) :: QSC, COEFPN
      DOUBLE PRECISION, INTENT(IN)    :: XMVE, XMVS, GRAV, DM, AC,
     &                                   AVA(QSC%DIM1)
      DOUBLE PRECISION, INTENT(IN)    :: SANFRA(QSC%DIM1)
      TYPE(BIEF_OBJ),   INTENT(INOUT) :: ACP ! WORK ARRAY T1
      TYPE(BIEF_OBJ),   INTENT(IN)    :: ACLADM
      TYPE(BIEF_OBJ),   INTENT(IN)    :: TOB, MU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER I
      DOUBLE PRECISION TORM, TORI, TORATIO, WI, COEFB, WCC
!
!======================================================================!
!                               PROGRAM                                !
!=======================================================================
!
      CALL CPSTVC(QSC,ACP)
      CALL OS('X=C     ', X=ACP, C=AC)
!
!     SLOPE EFFECT: SOULBY FORMULATION
!
      IF(SLOPEFF.EQ.2) THEN
        CALL OS('X=XY    ', X=ACP, Y=COEFPN)
      ENDIF
!
!     COEFFICIENT TO CALIBRATE THE FORMULA (by default = 1.0)
      WCC = 1.0D0
!
      DO I=1,QSC%DIM1
        TORM = (0.021D0 + 0.015D0*EXP(-20.D0*SANFRA(I)))*
     &       (XMVS/XMVE-1.D0)*XMVE*GRAV*ACLADM%R(I)
        COEFB = 0.67D0/(1.0D0+EXP(1.5D0-DM/ACLADM%R(I)))
        TORI = TORM*((DM/ACLADM%R(I))**COEFB)
        TORATIO = TOB%R(I)*MU%R(I)/TORI
        IF (TORATIO.LT.1.35D0) THEN
          WI = 2.D-3*(TORATIO**7.5D0)
        ELSE
          WI = 14.D0*((1.D0-0.894D0/SQRT(TORATIO))**4.5D0)
        ENDIF
        QSC%R(I)=WCC*WI*AVA(I)*((TOB%R(I)*MU%R(I)
     &  /XMVE)**1.5D0)/((XMVS/XMVE-1.D0)*GRAV)

!       IF VERY LOW TRANSPORT WE IMPOSE QB = 0 (TO AVOID NUMERICAL
!       ARTIFACTS)
        IF (QSC%R(I).LT.1.D-13) THEN
          QSC%R(I)=0.0D0
        ENDIF
      ENDDO
!
!=======================================================================
!
      RETURN
      END
!
