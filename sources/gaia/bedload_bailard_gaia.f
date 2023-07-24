!                   *******************************
                    SUBROUTINE BEDLOAD_BAILARD_GAIA
!                   *******************************
     &(U2D,V2D,UNORM,TOB,TOBW,THETAW,UW,FW,CF,NPOIN,PI,
     & XMVE,GRAV,DENS,XWC,ALPHAW,QSCX,QSCY,QSSX,QSSY,
     & UC3X,UC3Y,US4X,US4Y,T10,FCW,T13,QSC,QSS,HOULE,XMVS,THETAC)
!                    *******************************
!
!***********************************************************************
! GAIA
!***********************************************************************
!
!>@brief    Bailard formulation.
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!>@param[in]     U2D    Depth-averaged velocity x-direction
!>@param[in]     V2D    Depth-averaged velocity y-direction
!>@param[in]     UNORM  Norm of the mean flow velocity
!>@param[in]     TOB    Bed shear stress (total friction)
!>@param[in]     TOBW   Wave induced shear stress
!>@param[in]     THETAW Wave direction (deg wrt ox axis)
!>@param[in]     UW     Orbital wave velocity
!>@param[in]     FW     Quadratic friction coefficient (wave)
!>@param[in]     CF     Quadratic friction coefficient
!>@param[in]     NPOIN  Number of points
!>@param[in]     PI     Pi
!>@param[in]     XMVE   Fluid density
!>@param[in]     GRAV   Acceleration of gravity
!>@param[in]     DENS   Relative density of sediment
!>@param[in]     XWC    Settling velocity
!>@param[in,out] ALPHAW difference of Angle between waves and current
!>@param[in,out] QSCX   Bedload transport rate in the x-direction
!>@param[in,out] QSCY   Bedload transport rate in the y-direction
!>@param[in,out] QSSX   Suspended load transport rate in the x-direction
!>@param[in,out] QSSY   Suspended load transport rate in the y-direction
!>@param[in,out] UC3X   Work array
!>@param[in,out] UC3Y   Work array
!>@param[in,out] US4X   Work array
!>@param[in,out] US4Y   Work array
!>@param[in,out] T10    Work bief_obj structure
!>@param[in,out] FCW    Wave-current friction angle
!>@param[in,out] T13    Work bief_obj structure
!>@param[in,out] QSC    Bedload transport rate
!>@param[in,out] QSS    Suspended load transport rate
!>@param[in]     HOULE  Logical, for wave effects
!>@param[in]     XMVS   Sediment density
!>@param[in]     THETAC Current angle to the x direction
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE INTERFACE_GAIA,EX_BEDLOAD_BAILARD => BEDLOAD_BAILARD_GAIA
      USE BIEF
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      TYPE(BIEF_OBJ),   INTENT(IN)    :: U2D,V2D,UNORM, TOB
      TYPE(BIEF_OBJ),   INTENT(IN)    :: TOBW, THETAW, UW, FW, CF
      TYPE(BIEF_OBJ),   INTENT(IN)    :: THETAC
      INTEGER,          INTENT(IN)    :: NPOIN
      LOGICAL,          INTENT(IN)    :: HOULE
      DOUBLE PRECISION, INTENT(IN)    :: PI, XMVE, GRAV, DENS, XWC,XMVS
      TYPE(BIEF_OBJ),   INTENT(INOUT) :: ALPHAW
      TYPE(BIEF_OBJ),   INTENT(INOUT) :: QSCX, QSCY
      TYPE(BIEF_OBJ),   INTENT(INOUT) :: QSSX, QSSY
      TYPE(BIEF_OBJ),   INTENT(INOUT) :: UC3X, UC3Y
      TYPE(BIEF_OBJ),   INTENT(INOUT) :: US4X, US4Y
      TYPE(BIEF_OBJ),   INTENT(INOUT) :: FCW, T10, T13
      TYPE(BIEF_OBJ),   INTENT(INOUT) :: QSC, QSS
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
!
!     LOCAL VARIABLES
!
      INTEGER                     :: I
      DOUBLE PRECISION            :: C3, C4, PHI
      DOUBLE PRECISION, PARAMETER :: EPSC = 0.21D0   ! BEDLOAD
      DOUBLE PRECISION, PARAMETER :: EPSS = 0.025D0  ! SUSPENSION
      DOUBLE PRECISION            :: U3X, U3Y, NUM
!
!======================================================================!
!======================================================================!
!                               PROGRAM                                !
!======================================================================!
!======================================================================!
!
!     CASE WITH WAVES
!
      IF(HOULE) THEN
!
!     ANGLE OF VELOCITY WITH OX (IN RADIANS)
!     ANGLE OF WAVES WITH OX (IN RADIANS)
!     ALPHAW: ANGLE DIFFERNECE (IN RADIANS)
      DO I=1,NPOIN
!       T10 IS THETAC_RAD
        T10%R(I)=THETAC%R(I)*PI/180.D0
!       T13 IS THETAW IN RADIANS
        T13%R(I)=THETAW%R(I)*PI/180.D0
        ALPHAW%R(I)=T13%R(I)-T10%R(I)
      ENDDO
!
!     US4X AND US4Y ARE WORK ARRAYS, THEIR STRUCTURE IS GIVEN HERE
!     THE STRUCTURE OF THETAC (CATHERINE DON'T REMOVE THIS PLEASE)
      CALL CPSTVC(THETAC,US4X)
      CALL CPSTVC(THETAC,US4Y)
!
      DO I = 1, NPOIN
        ! ********************* !
        ! I - CURRENT REFERENCE SYSTEM !
        ! ********************* !
        U3X = UNORM%R(I)**3
     &      + UNORM%R(I)*UW%R(I)**2 * (1 + COS(2.D0*ALPHAW%R(I))/ 2.D0)
        U3Y = UNORM%R(I)*UW%R(I)**2 * SIN(2.D0*ALPHAW%R(I)) / 2.D0
        ! ********************************************** !
        ! II - 3RD ORDER MOMENTUM (LINEAR WAVE THEORY)   !
        ! ********************************************** !
        UC3X%R(I) = U3X * COS(T10%R(I)) - U3Y * SIN(T10%R(I))
        UC3Y%R(I) = U3X * SIN(T10%R(I)) + U3Y * COS(T10%R(I))
        ! ************************************************************ !
        ! III -  4TH ORDER MOMENTUM (COLINEAR WAVES AND CURRENTS)      !
        ! ************************************************************ !
        NUM = ( 8.D0*UNORM%R(I)**4 + 3.D0*UW%R(I)**4
     &          + 24.D0*(UNORM%R(I)**2)*(UW%R(I)**2) )*0.125D0
        US4X%R(I) = NUM * COS(T10%R(I))
        US4Y%R(I) = NUM * SIN(T10%R(I))
      ENDDO
      ! *********************************************** !
      ! IV -  FRICTION COEFFICIENT WAVE + CURRENT       !
      ! *********************************************** !
      CALL BEDLOAD_INTERACT_GAIA
     &     (UNORM,TOBW,TOB,ALPHAW,FW,CF,UW,NPOIN,XMVE,FCW)
      ! ******************************** !
      ! V - TRANSPORT RATES              !
      ! ******************************** !
      PHI = PI   / 6.D0  ! FRICTION ANGLE
      C3  = EPSC / (GRAV*DENS*TAN(PHI))
      C4  = EPSS / (GRAV*DENS*XWC)
      CALL OS('X=CYZ   ', X=QSCX, Y=FCW,  Z=UC3X, C=C3)
      CALL OS('X=CYZ   ', X=QSCY, Y=FCW,  Z=UC3Y, C=C3)
      CALL OS('X=CYZ   ', X=QSSX, Y=FCW,  Z=US4X, C=C4)
      CALL OS('X=CYZ   ', X=QSSY, Y=FCW,  Z=US4Y, C=C4)
!
!     CASE WITHOUT WAVES
!
      ELSE
!
        WRITE(LU,*) 'BAILARD WITHOUT WAVES NOT PROGRAMMED'
        CALL PLANTE(1)
        STOP
!
      ENDIF
!
!     NORMS
!
      CALL OS('X=N(Y,Z)', X=QSC,  Y=QSCX, Z=QSCY)
      CALL OS('X=N(Y,Z)', X=QSS,  Y=QSSX, Z=QSSY)
!======================================================================!
!     SOLID DISCHARGE IS TRANSFORMED IN [kg/(m*s)]
!
      CALL OS('X=CX    ', X=QSC, C=XMVS)
      CALL OS('X=CX    ', X=QSS, C=XMVS)
!======================================================================!
      RETURN
      END
