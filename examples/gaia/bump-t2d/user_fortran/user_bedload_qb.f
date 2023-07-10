!               **************************
                SUBROUTINE USER_BEDLOAD_QB
!               **************************
     & (HN, U2D, V2D, THETAC, HOULE, HW, TW, THETAW,
     &  TOB,TOBW,TOBCW_MEAN,TOBCW_MAX, DCLA, DENS, GRAV, DSTAR, AC,
     &  XMVE, XMVS, TETAP, MU, NPOIN, QSC, QSS, CSTAEQ)
!
!***********************************************************************
! GAIA
!***********************************************************************
!
!>@brief Allows the user to code their own bedload transport
!!       formulation, best suited to their application.
!
!>@warning User subroutine; sand transport formula must be coded by the user
!>@todo Missing description of arguments
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!>@param[in]     HN
!>@param[in]     U2D
!>@param[in]     V2D
!>@param[in]     THETAC
!>@param[in]     HOULE
!>@param[in]     HW
!>@param[in]     TW
!>@param[in]     THETAW
!>@param[in,out] TOB
!>@param[in,out] TOBW
!>@param[in,out] TOBCW_MEAN
!>@param[in,out] TOBCW_MAX
!>@param[in]     DM
!>@param[in]     DENS
!>@param[in]     GRAV
!>@param[in]     DSTAR
!>@param[in]     AC
!>@param[in]     XMVE
!>@param[in]     XMVS
!>@param[in]     TETAP
!>@param[in]     MU
!>@param[in]     NPOIN
!>@param[in,out] QSC
!>@param[in,out] QSS
!>@param[in,out] CSTAEQ
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE INTERFACE_GAIA, EX_USER_BEDLOAD_QB => USER_BEDLOAD_QB
      USE BIEF
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!!-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      TYPE(BIEF_OBJ),   INTENT(IN)    :: HN,U2D,V2D,THETAC
      TYPE(BIEF_OBJ),   INTENT(IN)    :: HW, TW, THETAW
      TYPE(BIEF_OBJ),   INTENT(IN)    :: TOB,TOBW,TOBCW_MEAN,TOBCW_MAX
      DOUBLE PRECISION, INTENT(IN)    :: DCLA, DENS, GRAV, DSTAR, AC
      DOUBLE PRECISION, INTENT(IN)    :: XMVE, XMVS
      TYPE(BIEF_OBJ),   INTENT(IN)    :: TETAP, MU
      TYPE(BIEF_OBJ),   INTENT(IN)    :: CSTAEQ
      INTEGER,          INTENT(IN)    :: NPOIN
      LOGICAL,          INTENT(IN)    :: HOULE
      TYPE(BIEF_OBJ),   INTENT(INOUT) :: QSC, QSS
!
!!-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER          :: I
      DOUBLE PRECISION :: UNORM
      DOUBLE PRECISION, PARAMETER :: ACOEFF = 0.00167D0 ! Sediment transport param (m^2s^-1)
!
!======================================================================!
!======================================================================!
!          IMPLEMENTATION OF THE GRASS BEDLOAD FORMULA                 !
!======================================================================!
!======================================================================!
!
!     GRASS (1981) TYPE
!     later u/sqrt(u**2 + v**2)
!     later v/sqrt(u**2 + v**2)
!     qsc_u = QSC *  u/sqrt(u**2 + v**2)
!     qsc_v = QSC *  v/sqrt(u**2 + v**2)
!
      DO I = 1, NPOIN
        UNORM = SQRT(U2D%R(I)**2+V2D%R(I)**2)
        QSC%R(I) = ACOEFF * (U2D%R(I)**2+V2D%R(I)**2) * UNORM *XMVS  ! Grass (1981) type bedload (total load)
!   QSC%R(I) += ACOEFF * V2D%R(I) * (U2D%R(I)**2+V2D%R(I)**2)
        QSS%R(I) = 0.D0                                            ! Zero suspended load
      ENDDO
!
!
!-----------------------------------------------------------------------
!
      RETURN
      END
