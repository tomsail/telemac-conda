!                 **********************************
                  SUBROUTINE BEDLOAD_HUNZ_MEYER_GAIA
!                 **********************************
!
     &  (TOB, MU, ACLADM, UNLADM, NPOIN, DENS, XMVE, GRAV, DCLA, AC,
     &    TETAP, AHUNZI, ACP, HIDING, QSC, XMVS, SLOPEFF, COEFCR)
!
!***********************************************************************
! GAIA  V8P3
!***********************************************************************
!
!>@brief Hunziker bedload formulation (1995)
!!       (adapted from Meyer-Peter formulation).
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!>@param[in]     AC     Critical shields parameter
!>@param[in]     ACLADM Mean diameter of active layer
!>@param[in,out] ACP    Modified shields parameter
!>@param[in,out] AHUNZI Coefficient of hunziker formula
!>@param[in]     DENS   Relative density of sediment
!>@param[in]     DCLA    Sediment grain diameter
!>@param[in]     GRAV   Acceleration of gravity
!>@param[in]     HIDING Hiding factor correction
!>@param[in,out] MU     Skin friction correction factor for bed roughness
!>@param[in]     NPOIN  Number of points
!>@param[in,out] QSC    Bed load transport rate
!>@param[in,out] TETAP  Adimensional skin friction
!>@param[in]     TOB    Bed shear stress (total friction)
!>@param[in]     UNLADM Mean diameter of under-layer
!>@param[in]     XMVE   Fluid density
!>@param[in]     XMVS   Sediment density
!>@param[in]     SLOPEFF Formula for slope effect
!>@param[in]     COEFCR  Correction of critical Shields for sloping bed effect
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE INTERFACE_GAIA,
     &          EX_BEDLOAD_HUNZ_MEYER => BEDLOAD_HUNZ_MEYER_GAIA
      USE BIEF
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
      ! 2/ GLOBAL VARIABLES
      ! -------------------
      TYPE(BIEF_OBJ),   INTENT(IN)    :: TOB, MU, ACLADM, UNLADM, TETAP
      TYPE(BIEF_OBJ),   INTENT(IN)    :: COEFCR
      INTEGER,          INTENT(IN)    :: NPOIN, SLOPEFF
      DOUBLE PRECISION, INTENT(IN)    :: DENS, XMVE, GRAV, DCLA, AC,XMVS
      TYPE(BIEF_OBJ),   INTENT(INOUT) :: AHUNZI, ACP ! WORK ARRAY T1, T2
      TYPE(BIEF_OBJ),   INTENT(INOUT) :: HIDING, QSC
!
      ! 3/ LOCAL VARIABLES
      ! ------------------
      INTEGER          :: I
      DOUBLE PRECISION :: C1
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


      C1 = 5.D0*SQRT(GRAV*DENS*DCLA**3)

      DO I =1,NPOIN

! ADIMENSIONAL SKIN STRESS
! BEWARE: AHUNZI CAN BECOME SO LARGE THAT THE HIDING FACTOR BECOMES
! INFINITE; HUNZIKER HIMSELF SUGGESTS THAT IT BE CAPPED TO 2.3 .
        AHUNZI%R(I) = MIN(0.011D0 * TETAP%R(I)**(-1.5D0)-0.3D0,2.3D0)

! CORRECTS THE ADIMENSIONAL CRITICAL STRESS ACCORDING HIDING
        ACP%R(I) = ACP%R(I) * (UNLADM%R(I)/ACLADM%R(I))**0.33D0
! HIDING
        HIDING%R(I) = (DCLA/ACLADM%R(I))**(-AHUNZI%R(I))
!
! TRANSPORT RATE    !
        QSC%R(I) = MAX(TETAP%R(I) - ACP%R(I), 0.D0)
        QSC%R(I) = C1 * (QSC%R(I) * HIDING %R(I))**1.5D0
      ENDDO

!======================================================================!
!     SOLID DISCHARGE IS TRANSFORMED IN [kg/(m*s)]
!
      CALL OS('X=CX    ', X=QSC, C=XMVS)
!======================================================================!
      RETURN
      END
