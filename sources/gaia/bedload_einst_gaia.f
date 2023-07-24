!                 *****************************
                  SUBROUTINE BEDLOAD_EINST_GAIA
!                 *****************************
!
     &(TETAP, NPOIN, DENS, GRAV, DCLA, DSTAR, QSC,XMVS)
!
!***********************************************************************
! GAIA
!***********************************************************************
!
!>@brief Einstein-Brown bedload transport formulation.
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!>@param[in]     DENS  Relative density of sediment
!>@param[in]     DCLA   Sediment grain diameter
!>@param[in]     DSTAR Non-dimensional diameter
!>@param[in]     GRAV  Acceleration of gravity
!>@param[in]     NPOIN Number of points
!>@param[in,out] QSC   Bed load transport rate
!>@param[in]     TETAP Adimensional skin friction
!>@param[in]     XMVS  Sediment density
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE INTERFACE_GAIA, EX_BEDLOAD_EINST => BEDLOAD_EINST_GAIA
      USE BIEF
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!!-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      TYPE(BIEF_OBJ),   INTENT(IN)    :: TETAP
      INTEGER,          INTENT(IN)    :: NPOIN
      DOUBLE PRECISION, INTENT(IN)    :: DENS, GRAV, DCLA, DSTAR, XMVS
      TYPE(BIEF_OBJ),   INTENT(INOUT)   :: QSC
!
!!-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER          :: I
      DOUBLE PRECISION :: CEINST
!
!======================================================================!
!                               PROGRAM                                !
!======================================================================!
!
!     BEDLOAD TRANSPORT
!
      CEINST = 36.D0/(DSTAR**3)
      CEINST = SQRT(2.D0/3.D0+CEINST) -  SQRT(CEINST)
      CEINST = CEINST * SQRT(DENS*GRAV*(DCLA**3))
      DO I = 1, NPOIN
        IF(TETAP%R(I) < 2.5D-3) THEN
          QSC%R(I) = 0.D0
        ELSE IF (TETAP%R(I) < 0.2D0) THEN
          QSC%R(I) = 2.15D0* CEINST * EXP(-0.391D0/TETAP%R(I))
        ELSE
          QSC%R(I) = 40.D0 * CEINST * (TETAP%R(I)**3.D0)
        ENDIF
      ENDDO
!
!     SOLID DISCHARGE IS TRANSFORMED IN [kg/(m*s)]
!
      CALL OS('X=CX    ', X=QSC, C=XMVS)
!=======================================================================
!
      RETURN
      END
