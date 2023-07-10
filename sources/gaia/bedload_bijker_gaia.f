!                 ******************************
                  SUBROUTINE BEDLOAD_BIJKER_GAIA
!                 ******************************
!
     &  (TOBW,TOB,MU,KSP,KSR,HN,NPOIN,DCLA,DENS,XMVE,GRAV,XWC,
     &   KARMAN,ZERO,T4,T7,T8,T9,QSC,QSS,BIJK,HOULE,XMVS)
!
!***********************************************************************
! GAIA
!***********************************************************************
!
!>@brief Bijker bedload transport formulation.
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!>@param[in]     BIJK   B value for the bijker formula
!>@param[in]     DENS   Relative density of sediment
!>@param[in]     DCLA    Sediment grain diameter
!>@param[in]     GRAV   Acceleration of gravity
!>@param[in]     HN     Water depth
!>@param[in]     HOULE  Logical, for wave effects
!>@param[in]     KARMAN Von karman constant
!>@param[in]     KSP    Bed skin roughness
!>@param[in]     KSR    Ripple bed roughness
!>@param[in]     MU     Skin friction correction factor for bed roughness
!>@param[in]     NPOIN  Number of points
!>@param[in,out] QSC    Bed load transport rate
!>@param[in,out] QSS    Suspended load transport rate
!>@param[in,out] T4     Work bief_obj structure
!>@param[in,out] T7     Work bief_obj structure
!>@param[in,out] T8     Work bief_obj structure
!>@param[in,out] T9     Work bief_obj structure
!>@param[in]     TOB    Bed shear stress (total friction)
!>@param[in]     TOBW   Wave induced shear stress
!>@param[in]     XMVE   Fluid density
!>@param[in]     XMVS   Sediment density
!>@param[in]     XWC    Settling velocity
!>@param[in]     ZERO   Zero
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE INTERFACE_GAIA,EX_BEDLOAD_BIJKER => BEDLOAD_BIJKER_GAIA
      USE BIEF
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!!-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      TYPE(BIEF_OBJ),   INTENT(IN)    :: TOBW, TOB, KSR,KSP, HN,MU
      INTEGER,          INTENT(IN)    :: NPOIN
      LOGICAL,          INTENT(IN)    :: HOULE
      DOUBLE PRECISION, INTENT(IN)    :: DCLA, DENS, XMVE, GRAV, XWC
      DOUBLE PRECISION, INTENT(IN)    :: XMVS
      DOUBLE PRECISION, INTENT(IN)    :: KARMAN, ZERO
      DOUBLE PRECISION, INTENT(IN)    :: BIJK
      TYPE(BIEF_OBJ),   INTENT(INOUT) :: T4
      TYPE(BIEF_OBJ),   INTENT(INOUT) :: T7, T8, T9
      TYPE(BIEF_OBJ),   INTENT(INOUT) :: QSC, QSS
!
!!-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER                      :: I
      DOUBLE PRECISION             :: C1, C2, UCF
!
!======================================================================!
!======================================================================!
!                               PROGRAM                                !
!======================================================================!
!======================================================================!
!
      ! ***************************************************** !
      ! I - STRESS UNDER THE COMBINED ACTION OF WAVES AND CURRENTS !
      ! ***************************************************** !
      IF(HOULE) THEN
        CALL OS('X=CY    ', X=T4, Y=TOBW, C= 0.5D0)
        CALL OS('X=X+Y   ', X=T4, Y=TOB)
      ELSE
        CALL OS('X=Y     ', X=T4, Y=TOB)
      ENDIF
      ! ******************************************************* !
      ! II - CORRECTION TO TAKE BED FORMS INTO ACCOUNT          !
      ! ******************************************************* !
!      CALL OS('X=Y/Z   ', X=MU, Y=CFP, Z=CF)
!      CALL OS('X=Y**C  ', X=MU, Y=MU , C=0.75D0)
      ! ***************************** !
      ! III - BEDLOAD TRANSPORT       !
      ! ***************************** !
      C1 = BIJK*DCLA
      C2 = DENS*DCLA*XMVE*GRAV
      DO I = 1, NPOIN
        IF (T4%R(I)*MU%R(I)> ZERO) THEN
          QSC%R(I) = C1*SQRT(TOB%R(I)/XMVE )
     &             * EXP(-0.27D0*(C2/(T4%R(I)*MU%R(I))))
        ELSE
          QSC%R(I) = 0.D0
        ENDIF
      ENDDO
      ! *********************************************************** !
      ! IV- ROUSE NUMBER AND LOWER BOUND OF EINSTEIN INTEGRAL       !
      ! *********************************************************** !
      DO I = 1, NPOIN
        IF (T4%R(I) > 0.D0) THEN
          UCF     = SQRT( T4%R(I) / XMVE)
          T7%R(I) = XWC / ( KARMAN * UCF )
!         AUX     = 1.D0 + KARMAN*SQRT(2.D0/MAX(CF%R(I),ZERO))
!         T8%R(I) = 30.D0*EXP(-AUX)
          T8%R(I) = MAX(KSR%R(I),KSP%R(I))/MAX(HN%R(I),ZERO)
        ELSE
          T7%R(I)= 100001.D0
          T8%R(I)= 100001.D0
        ENDIF
      ENDDO
      ! ************************************ !
      ! V - EINSTEIN INTEGRAL                !
      ! ************************************ !
      CALL INTEG_GAIA(T7%R, T8%R, T9%R, NPOIN)
      ! ************************************** !
      ! VI - TRANSPORT BY SUSPENSION           !
      ! ************************************** !
      CALL OS('X=YZ    ', X=QSS, Y=T9, Z=QSC)
!======================================================================!
!     SOLID DISCHARGE IS TRANSFORMED IN [kg/(m*s)]
!
      CALL OS('X=CX    ', X=QSC, C=XMVS)
      CALL OS('X=CX    ', X=QSS, C=XMVS)
!======================================================================!
      RETURN
      END SUBROUTINE BEDLOAD_BIJKER_GAIA
