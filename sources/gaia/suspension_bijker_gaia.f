!                 *********************************
                  SUBROUTINE SUSPENSION_BIJKER_GAIA
!                 *********************************
!
     &(TAUP,NPOIN,CHARR,QSC,ZREF,ZERO,CSTAEQ,XMVE,RATIO_TOCE)
!
!***********************************************************************
! GAIA
!***********************************************************************
!
!>@brief Computes the reference concentration at z= 2*d50
!!       using ZYSERMAN and FREDSOE formulation (1994).
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!>@param[in]     TAUP       Skin froction
!>@param[in]     NPOIN      Number of points
!>@param[in]     CHARR      Bedload
!>@param[in]     QSC        Bed load transpport rate
!>@param[in]     ZREF       Reference elevation
!>@param[in]     ZERO       Real lower than that value are considered
!!                          equal to zero
!>@param[in,out] CSTAEQ     Equilibrium concentration
!>@param[in]     XMVE       Fluid density
!>@param[in]     RATIO_TOCE Ratio between critical shear stress of
!!                          pure sediment and mixed sediment
!!                          in the same layer
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE INTERFACE_GAIA,EX_SUSPENSION_BIJKER => SUSPENSION_BIJKER_GAIA
      USE BIEF
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!     GLOBAL VARIABLES
!     -------------------
      TYPE(BIEF_OBJ),   INTENT(IN)    :: TAUP,QSC
      TYPE (BIEF_OBJ),  INTENT(IN)    :: ZREF
      INTEGER,          INTENT(IN)    :: NPOIN
      LOGICAL,          INTENT(IN)    :: CHARR
      DOUBLE PRECISION, INTENT(IN)    :: ZERO,XMVE
!
      TYPE(BIEF_OBJ),   INTENT(INOUT) ::  CSTAEQ
      TYPE(BIEF_OBJ),   INTENT(IN)    ::  RATIO_TOCE
!
!     LOCAL VARIABLES
!      ------------------
      INTEGER                     :: I
      DOUBLE PRECISION            :: USTARP
!
!     MAXIMUM CONCENTRATION CORRESPONDING TO DENSE PACKING
!
      DOUBLE PRECISION, PARAMETER :: CMAX = 0.6D0
!
!======================================================================!
!======================================================================!
!                               PROGRAM                                !
!======================================================================!
!======================================================================!
!
      IF(.NOT.CHARR) THEN
        WRITE(LU,*) 'SUSPENSION_BIJKER_GAIA ERROR ON CHARR'
        CALL PLANTE(1)
        STOP
      ENDIF
!
      DO I=1,NPOIN
!  CSTAEQ IS FUNCTION OF QB, AND QB TAKE IN ACCOUNT THE RATIO OF MUD
!  RATIO_TOCE IS NOT USE HERE
        IF(TAUP%R(I).LE.ZERO) THEN
          CSTAEQ%R(I) = 0.D0
        ELSE
          USTARP=SQRT(TAUP%R(I)/XMVE)
          CSTAEQ%R(I) = QSC%R(I)/(6.34D0*USTARP*ZREF%R(I))
          CSTAEQ%R(I) = MIN(CSTAEQ%R(I),CMAX)
        ENDIF
!------------ CSTAEQ%R is in kg/m3 because QSC is already in kg/m3
!
      ENDDO
!
!======================================================================!
!======================================================================!
!
      RETURN
      END SUBROUTINE SUSPENSION_BIJKER_GAIA
