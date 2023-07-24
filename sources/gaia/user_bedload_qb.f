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
!>@param[in]     THETAW (DEG WRT OX AXIS)
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
!     DOUBLE PRECISION :: C1, C2, T
!
!======================================================================!
!======================================================================!
!                               PROGRAM                                !
!======================================================================!
!======================================================================!
!
!     EXAMPLE BY VAN RIJN
!
!     C1 = DENS * GRAV * DCLA
!     C2 = 0.053D0 * SQRT(DCLA**3*DENS*GRAV) * DSTAR**(-0.3D0)
!
      DO I = 1, NPOIN
!
!       TRANSPORT STAGE PARAMETER
!
!       IF(TETAP%R(I) .LE. AC) THEN
!         T = 0.D0
!       ELSE
!         T = (TETAP%R(I)-AC)/MAX(AC,1.D-06)
!       ENDIF
!
!       BEDLOAD TRANSPORT RATE
!
        QSC%R(I) = 0.D0 ! C2 * T**2.1D0
        QSS%R(I) = 0.D0
!
      ENDDO
!
!  FOLLOWING LINES NEED TO BE COMMENTED OUT
!
      WRITE(LU,53)

53    FORMAT(/,1X,'GAIA IS STOPPED : ',/
     &   ,1X,' SAND TRANSPORT MUST BE CALCULATED IN USER_BEDLOAD_QB')
      CALL PLANTE(1)
      STOP
!
!-----------------------------------------------------------------------
!
      RETURN
      END
