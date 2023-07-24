!                 *******************************
                  SUBROUTINE BEDLOAD_VANRIJN_GAIA
!                 *******************************
!
     &     (TETAP, NPOIN, DCLA, DENS, GRAV, DSTAR, AC, ACP, QSC, XMVS,
     &      SLOPEFF, COEFCR)
!
!***********************************************************************
! GAIA   V8P3                                   02/02/2022
!***********************************************************************
!
!>@brief Van Rijn bedload transport formulation.
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!>@param[in]     AC     Critical shields parameter
!>@param[in,out] ACP     Modified shields parameter
!>@param[in]     COEFCR  Correction of critical Shields for sloping bed effect         
!>@param[in]     DENS   Relative density of sediment
!>@param[in]     DCLA    Sediment grain diameter
!>@param[in]     DSTAR  Non-dimensional diameter
!>@param[in]     GRAV   Acceleration of gravity
!>@param[in]     NPOIN  Number of points
!>@param[in,out] QSC    Bedload transport rate
!>@param[in]     SLOPEFF Formula for slope effect
!>@param[in]     TETAP  Adimensional skin friction
!>@param[in]     XMVS   Sediment density
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE INTERFACE_GAIA,EX_BEDLOAD_VANRIJN => BEDLOAD_VANRIJN_GAIA
      USE BIEF
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
      TYPE(BIEF_OBJ),   INTENT(IN)  :: TETAP,COEFCR
      INTEGER,          INTENT(IN)  :: NPOIN, SLOPEFF
      DOUBLE PRECISION, INTENT(IN)  :: DCLA, DENS, GRAV, DSTAR, AC,XMVS
      TYPE(BIEF_OBJ),   INTENT(INOUT) :: ACP ! WORK ARRAY T1      
      TYPE(BIEF_OBJ),   INTENT(INOUT) :: QSC
!
      INTEGER          :: I
      DOUBLE PRECISION :: C2, T
!======================================================================!
!======================================================================!
!                               PROGRAM                                !
!======================================================================!
!======================================================================!
      CALL CPSTVC(QSC,ACP)
      CALL OS('X=C     ', X=ACP, C=AC)
!
!     SLOPE EFFECT: SOULBY FORMULATION
!
      IF(SLOPEFF.EQ.2) THEN
        CALL OS('X=XY    ', X=ACP, Y=COEFCR )
      ENDIF
      
      C2 = 0.053D0 * SQRT(DCLA**3*DENS*GRAV) * DSTAR**(-0.3D0)
      DO I = 1, NPOIN
!       ******************************
!       I - TRANSPORT STAGE PARAMETER
!       ******************************
        IF(TETAP%R(I) .LE. ACP%R(I)) THEN
          T = 0.D0
        ELSE
          T = (TETAP%R(I)-ACP%R(I))/MAX(ACP%R(I),1.D-06)
        ENDIF
!
!       *****************************
!       II - BEDLOAD TRANSPORT RATE
!       *****************************
        QSC%R(I) = C2 * T**2.1D0
      ENDDO
!======================================================================!
!     SOLID DISCHARGE IS TRANSFORMED IN [kg/(m*s)]
!
      CALL OS('X=CX    ', X=QSC, C=XMVS)
!======================================================================!
      RETURN
      END SUBROUTINE BEDLOAD_VANRIJN_GAIA
