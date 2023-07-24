!                 *******************************
                  SUBROUTINE BEDLOAD_SOULSBY_GAIA
!                 *******************************
!
     &  (UNORM,HN, UW, NPOIN, DENS, GRAV, DCLA, DSTAR, QSC,
     &   QSS,XMVS, UCRP, COEFCR, SLOPEFF)
!
!***********************************************************************
! GAIA  V8P3
!***********************************************************************
!
!>@brief Soulsby & Van Rijn bedload transport formulation.
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!>@param[in]     DENS  Relative density of sediment
!>@param[in]     DCLA   Sediment grain diameter
!>@param[in]     DSTAR Non-dimensional diameter
!>@param[in]     GRAV  Acceleration of gravity
!>@param[in]     HN    Water depth
!>@param[in]     NPOIN Number of points
!>@param[in,out] QSC   Bed load transport rate
!>@param[in,out] QSS   Suspended load transport rate
!>@param[in]     XMVS  Sediment density
!>@param[in]     UNORM Norm of the mean flow velocity (m/s)
!>@param[in]     UW    Orbital wave velocity
!>@param[in]     SLOPEFF Formula for slope effect
!>@param[in,out] UCRP  Modified ucritical
!>@param[in]     COEFCR  Correction of critical Shields for sloping bed effect
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE INTERFACE_GAIA,EX_BEDLOAD_SOULSBY => BEDLOAD_SOULSBY_GAIA
      USE BIEF
      USE DECLARATIONS_SPECIAL
      USE DECLARATIONS_GAIA, ONLY : KSPRATIO,D90,NSAND
      IMPLICIT NONE
      ! 2/ GLOBAL VARIABLES
      ! -------------------
      TYPE(BIEF_OBJ),   INTENT(IN)  :: HN, UNORM, UW, COEFCR
      INTEGER,          INTENT(IN)  :: NPOIN, SLOPEFF
      DOUBLE PRECISION, INTENT(IN)  :: DENS, GRAV, DCLA, DSTAR,XMVS
      TYPE(BIEF_OBJ),   INTENT(INOUT) :: QSC, QSS
      TYPE(BIEF_OBJ),   INTENT(INOUT) :: UCRP ! WORK ARRAY T1
!
      ! 3/ LOCAL VARIABLES
      ! ------------------
      INTEGER                     :: I
      DOUBLE PRECISION            :: COEF, ASS, ASB, CD
      DOUBLE PRECISION            :: UCR, VTOT, TRA, URMS, FD90
      DOUBLE PRECISION, PARAMETER :: Z0=0.006D0
!
!======================================================================!
!======================================================================!
!                               PROGRAM                                !
!======================================================================!
!======================================================================!
!
      CALL CPSTVC(QSC,UCRP)
      CALL OS('X=C     ', X=UCRP, C=1.D0)
!
!     SLOPE EFFECT: SOULBY FORMULATION
!
      IF(SLOPEFF.EQ.2) THEN
        ! CHANGING UCRIT WHICH IS PROPORTIONAL TO SQRT(SHIELDSCRIT)
        DO I = 1,NPOIN
          UCRP%R(I) = SQRT(COEFCR%R(I))
        END DO
      ENDIF


! ************************* !
      ! I - SUSPENSION COEFFCIENT !
      ! ************************* !
      COEF = (DENS *GRAV*DCLA)**1.2D0
      ASS  = 0.012D0*DCLA*(DSTAR**(-0.6D0))/COEF
!---------------------------------------------------------
!     VALUE OF D90 OF SEDIMENT
!     WITH ONE SAND CLASS, IT IS USER SET
!     WITH MORE THAN ONE SAND CLASS, IT IS
!     CONSIDERED AS THE RATIO BETWEEN SKIN FRICTION AND MEAN
!     DIAMETER * D50
      IF (NSAND.EQ.1) THEN
        FD90 = D90
      ELSE
        FD90  = DCLA * KSPRATIO
      ENDIF
!
!
!!      WRITE(LU,201)
!!201   FORMAT(1X,'BEDLOAD_SOULSBY_GAIA:',/,1X,
!!     &            'DEFINED FD90 OF SEDIMENT')
!!      CALL PLANTE(1)
!!      STOP
!------------------------------------------------------------
!
      DO I = 1, NPOIN
!
        ! *************************** !
        ! III - BEDLOAD COEFFICIENT   !
        ! *************************** !
        ASB = 0.005D0*HN%R(I)*(DCLA/MAX(HN%R(I),DCLA))**1.2D0 / COEF
!
        ! ********************************** !
        ! IV - ROUGHNESS COEFFICIENT CD      !
        !      SOULSBY: Z0=0.006 --> KS=18CM !
        ! ********************************** !
        CD = (0.4D0 / (LOG(MAX(HN%R(I),Z0)/Z0)-1.D0))**2
!
        ! ************************************************ !
        ! V - CRTITICAL CURRENT SPEED UCR                  !
        ! ************************************************ !
        IF (DCLA < 0.0005D0) THEN
          UCR = 0.19D0*(DCLA**0.1D0)*LOG10(4.D0*MAX(HN%R(I),FD90)/FD90)
        ELSE
          UCR = 8.50D0*(DCLA**0.6D0)*LOG10(4.D0*MAX(HN%R(I),FD90)/FD90)
        ENDIF
        UCR = UCR*UCRP%R(I)
!
        ! ************************************************* !
        ! VI - SPEED INDUCED BY THE CURRENT AND WAVES       !
        ! ************************************************* !
        URMS = UW%R(I)/SQRT(2.D0)
        VTOT = SQRT(UNORM%R(I)**2+(0.018D0/CD)*URMS**2)
        ! *********************************************** !
        ! VII - SUSPENDED AND BEDLOAD TRANSPORT           !
        ! *********************************************** !
        IF (VTOT > UCR) THEN
          TRA     = UNORM%R(I)  * (VTOT - UCR )**2.4D0
          QSS%R(I)= ASS * TRA
          QSC%R(I)= ASB * TRA
        ELSE
          QSS%R(I) = 0.D0
          QSC%R(I) = 0.D0
        ENDIF
      ENDDO
!======================================================================!
!     SOLID DISCHARGE IS TRANSFORMED IN [kg/(m*s)]
!
      CALL OS('X=CX    ', X=QSC, C=XMVS)
      CALL OS('X=CX    ', X=QSS, C=XMVS)
!======================================================================!
      RETURN
      END SUBROUTINE BEDLOAD_SOULSBY_GAIA
