!               *****************************
                SUBROUTINE BEDLOAD_ENGEL_GAIA
!               *****************************
!
     &(TOB,CF,DENS,GRAV,DCLA,XMVE,TETA,QSC,XMVS)
!
!***********************************************************************
! GAIA   V7P3                                   10/01/2018
!***********************************************************************
!
!>@brief Engelund-Hansen bedload transport formulation.
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!>@param[in]     CF   Quadratic friction coefficient
!>@param[in]     DENS Relative density of sediment
!>@param[in]     DCLA  Sediment grain diameter
!>@param[in]     GRAV Acceleration of gravity
!>@param[in,out] QSC  Bed load transport rate
!>@param[in,out] TETA Dimensionless bed shear stress
!>@param[in]     TOB  Bed shear stress (total friction)
!>@param[in]     XMVE Fluid density
!>@param[in]     XMVS Sediment density
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE INTERFACE_GAIA, EX_BEDLOAD_ENGEL => BEDLOAD_ENGEL_GAIA
      USE BIEF
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!!-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      TYPE(BIEF_OBJ),   INTENT(IN)    :: TOB, CF
      DOUBLE PRECISION, INTENT(IN)    :: DENS, GRAV, DCLA, XMVE, XMVS
      TYPE(BIEF_OBJ),   INTENT(INOUT) :: TETA ! WORK ARRAY T1
      TYPE(BIEF_OBJ),   INTENT(INOUT) :: QSC
!
!!-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER I
      DOUBLE PRECISION :: CENGEL, C1
!
!======================================================================!
!                               PROGRAM                                !
!======================================================================!
!
      C1 = 1.D0/(DENS*XMVE*GRAV*DCLA)
      CENGEL = 0.05D0*SQRT(DENS*GRAV*DCLA**3)
!     CALL OS('X=CY    ', X=TETA, Y=TOB , C=C1)
!     CALL OS('X=Y**C  ', X=TETA, Y=TETA, C=5.D0/2.D0)
!
!     CALL OS('X=+(Y,C)', X=QSC , Y=CF  , C=1.D-06)
!     CALL OS('X=1/Y   ', X=QSC , Y=QSC)
!     CALL OS('X=CXY   ', X=QSC , Y=TETA, C=CENGEL)
      DO I=1,QSC%DIM1
!       TOTAL NON DIMENSIONAL STRESS = SQRT(C1*TOB%R(I))**5
!       BEDLOAD TRANSPORT
        QSC%R(I)=CENGEL*SQRT(C1*TOB%R(I))**5/MAX(CF%R(I),1.D-6)
      ENDDO
!
!     SOLID DISCHARGE IS TRANSFORMED IN [kg/(m*s)]
!
      CALL OS('X=CX    ', X=QSC, C=XMVS)
!======================================================================
!
      RETURN
      END
