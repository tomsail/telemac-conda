!                   ****************************
                    SUBROUTINE BEDLOAD_BAILARD !
!                   ****************************
!
     &(U2D,V2D,UCMOY,TOB,TOBW,THETAW,UW,FW,CF,NPOIN,PI,
     & XMVE,GRAV,DENS,XWC,ALPHAW,QSCX,QSCY,QSSX,QSSY,
     & UC3X,UC3Y,US4X,US4Y,THETAC,FCW,QSC,QSS,HOULE)
!
!***********************************************************************
! SISYPHE   V6P1                                   21/07/2011
!***********************************************************************
!
!brief    BAILARD FORMULATION.
!
!history  C. VILLARET (LNHE)
!+        01/10/2003
!+
!+
!
!history  JMH
!+        21/12/2006
!+        V6P0
!+   BEDLOAD_CALCBAIL DELETED; 'HOULE' ARGUMENT ADDED
!
!history  N.DURAND (HRW), S.E.BOURBAN (HRW)
!+        13/07/2010
!+        V6P0
!+   Translation of French comments within the FORTRAN sources into
!+   English comments
!
!history  N.DURAND (HRW), S.E.BOURBAN (HRW)
!+        21/08/2010
!+        V6P0
!+   Creation of DOXYGEN tags for automated documentation and
!+   cross-referencing of the FORTRAN sources
!
!history  C.VILLARET (EDF-LNHE), P.TASSI (EDF-LNHE)
!+        19/07/2011
!+        V6P1
!+  Name of variables
!+
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| ALPHAW         |<->| ANGLE OF WAVES WITH OX
!| CF             |-->| QUADRATIC FRICTION COEFFICIENT
!| DENS           |-->| RELATIVE SENSITY OF SEDIMENT
!| FCW            |---| WAVE-CURRENT FRICTION ANGLE
!| FW             |-->| WAVE FRICTION FACTOR
!| GRAV           |-->| ACCELERATION OF GRAVITY
!| HOULE          |-->| LOGICAL, FOR WAVE EFFECTS
!| NPOIN          |-->| NUMBER OF POINTS
!| PI             |-->| PI
!| QSC            |<->| BEDLOAD TRANSPORT RATE
!| QSCX           |<->| BEDLOAD TRANSPORT RATE IN THE X-DIRECTION
!| QSCY           |<->| BEDLOAD TRANSPORT RATE IN THE Y-DIRECTION
!| QSS            |<->| SUSPENDED LOAD TRANSPORT RATE
!| QSSX           |<->| SUSPENDED LOAD TRANSPORT RATE IN THE X-DIRECTION
!| QSSY           |<->| SUSPENDED LOAD TRANSPORT RATE IN THE Y-DIRECTION
!| THETAC         |<->| CURRENT ANGLE TO THE X DIRECTION
!| THETAW         |-->| ANGLE BETWEEN WAVE AND CURRENT
!| TOB            |<->| BED SHEAR STRESS (TOTAL FRICTION)
!| TOBW           |-->| WAVE INDUCED SHEAR STRESS
!| U2D            |<->| MEAN FLOW VELOCITY X-DIRECTION
!| UC3X           |<->| WORK ARRAY
!| UC3Y           |<->| WORK ARRAY
!| UCMOY          |-->| MEAN CURRENT
!| US4X           |<->| WORK ARRAY
!| US4Y           |<->| WORK ARRAY
!| UW             |-->| ORBITAL WAVE VELOCITY
!| V2D            |<->| MEAN FLOW VELOCITY Y-DIRECTION
!| XMVE           |-->| FLUID DENSITY
!| XWC            |-->| SETTLING VELOCITY
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE INTERFACE_SISYPHE,EX_BEDLOAD_BAILARD => BEDLOAD_BAILARD
      USE BIEF
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
! 2/ GLOBAL VARIABLES
!
      TYPE(BIEF_OBJ),   INTENT(IN)    :: U2D,V2D,UCMOY, TOB
      TYPE(BIEF_OBJ),   INTENT(IN)    :: TOBW, THETAW, UW, FW, CF
      INTEGER,          INTENT(IN)    :: NPOIN
      LOGICAL,          INTENT(IN)    :: HOULE
      DOUBLE PRECISION, INTENT(IN)    :: PI, XMVE, GRAV, DENS, XWC
      TYPE(BIEF_OBJ),   INTENT(INOUT) :: ALPHAW        ! WORK ARRAY T1
      TYPE(BIEF_OBJ),   INTENT(INOUT) :: QSCX, QSCY    ! WORK ARRAY T2 AND T3
      TYPE(BIEF_OBJ),   INTENT(INOUT) :: QSSX, QSSY    ! WORK ARRAY T4 AND T5
      TYPE(BIEF_OBJ),   INTENT(INOUT) :: UC3X, UC3Y    ! WORK ARRAY T6 AND T7
      TYPE(BIEF_OBJ),   INTENT(INOUT) :: US4X, US4Y    ! WORK ARRAY T8 AND T9
      TYPE(BIEF_OBJ),   INTENT(INOUT) :: THETAC, FCW   ! WORK ARRAY T10 AND T11
      TYPE(BIEF_OBJ),   INTENT(INOUT) :: QSC, QSS
!
! 3/ LOCAL VARIABLES
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
!
      CALL BEDLOAD_DIRECTION(U2D,V2D,NPOIN,PI,THETAC)
!
!     ANGLE OF WAVES WITH OX (IN RADIANS)
!
      CALL OS('X=CY    ', X=ALPHAW, Y=THETAW, C=-PI/180.D0)
      CALL OS('X=X+C   ', X=ALPHAW, C=0.5D0*PI)
      CALL OS('X=Y-Z   ', X=ALPHAW, Y=ALPHAW, Z=THETAC)
!
!     PARAMETERS  ,
!
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
        U3X = UCMOY%R(I)**3
     &      + UCMOY%R(I)*UW%R(I)**2 * (1 + COS(2.D0*ALPHAW%R(I))/ 2.D0)
        U3Y = UCMOY%R(I)*UW%R(I)**2 * SIN(2.D0*ALPHAW%R(I)) / 2.D0
        ! ********************************************** !
        ! II - 3RD ORDER MOMENTUM (LINEAR WAVE THEORY)   !
        ! ********************************************** !
        UC3X%R(I) = U3X * COS(THETAC%R(I)) - U3Y * SIN(THETAC%R(I))
        UC3Y%R(I) = U3X * SIN(THETAC%R(I)) + U3Y * COS(THETAC%R(I))
        ! ************************************************************ !
        ! III -  4TH ORDER MOMENTUM (COLINEAR WAVES AND CURRENTS)      !
        ! ************************************************************ !
        NUM = ( 8.D0*UCMOY%R(I)**4 + 3.D0*UW%R(I)**4
     &          + 24.D0*(UCMOY%R(I)**2)*(UW%R(I)**2) )*0.125D0
        US4X%R(I) = NUM * COS(THETAC%R(I))
        US4Y%R(I) = NUM * SIN(THETAC%R(I))
      ENDDO
      ! *********************************************** !
      ! IV -  FRICTION COEFFICIENT WAVE + CURRENT       !
      ! *********************************************** !
      CALL BEDLOAD_INTERACT
     &     (UCMOY,TOBW,TOB,ALPHAW,FW,CF,UW,NPOIN,XMVE,FCW)
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
!======================================================================!
      RETURN
      END
