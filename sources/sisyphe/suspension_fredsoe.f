!                   *****************************
                    SUBROUTINE SUSPENSION_FREDSOE
!                   *****************************
!
     &(DM,TAUP,NPOIN,GRAV,XMVE,XMVS,AC,CSTAEQ)
!
!***********************************************************************
! SISYPHE   V6P1                                   21/07/2011
!***********************************************************************
!
!brief    COMPUTES THE REFERENCE CONCENTRATION AT Z= 2*D50
!+                ACCORDING TO ZYSERMAN AND FREDSOE FORMULATION (1994).
!
!history  C. VILLARET
!+        14/04/2004
!+        V5P5
!+
!
!history  F. HUVELIN
!+        04/01/2005
!+        V5P6
!+
!
!history  JMH
!+        13/06/2008
!+
!+   FORMULATION OPTIMISED WITH AUX
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
!+
!+
!history  C. VILLARET
!+        20/03/2011
!+        V6P1
!+   Send DM instead of array ACLADM
!
!history  C.VILLARET (EDF-LNHE), P.TASSI (EDF-LNHE)
!+        19/07/2011
!+        V6P1
!+   Name of variables
!+
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| AC             |<->| CRITICAL SHIELDS PARAMETER
!| DM             |-->| SEDIMENT GRAIN DIAMETER
!| CSTAEQ         |<->| EQUILIBRIUM CONCENTRATION
!| GRAV           |-->| ACCELERATION OF GRAVITY
!| NPOIN          |-->| NUMBER OF POINTS
!| TAUP           |-->| CRITICAL SHEAR STRESS
!| XMVE           |-->| FLUID DENSITY
!| XMVS           |-->| WATER DENSITY
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE INTERFACE_SISYPHE,EX_SUSPENSION_FREDSOE => SUSPENSION_FREDSOE
      USE BIEF
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!
      ! 2/ GLOBAL VARIABLES
      ! -------------------
      TYPE(BIEF_OBJ),   INTENT(IN)    ::  TAUP
      INTEGER,          INTENT(IN)    :: NPOIN
      DOUBLE PRECISION, INTENT(IN)    :: GRAV, XMVE, XMVS
      DOUBLE PRECISION, INTENT(IN)    :: AC,DM
      TYPE(BIEF_OBJ),   INTENT(INOUT) :: CSTAEQ


      ! 3/ LOCAL VARIABLES
      ! ------------------
      INTEGER            :: I
      DOUBLE PRECISION   ::  TETAP,AUX
!
!     MAXIMUM CONCENTRATION CORRESPONDING TO DENSE PACKING
!
      DOUBLE PRECISION, PARAMETER :: CMAX = 0.6D0
!!
!======================================================================!
!======================================================================!
!                               PROGRAM                                !
!======================================================================!
!======================================================================!
!
      ! ******************************** !
      !    I - CRITICAL SHIELDS PARAMETER!
      ! ******************************** !
!
      DO I=1,NPOIN
!
        ! ****************** !
        ! II - SKIN FRICTION !
        ! ****************** !
!
        TETAP = TAUP%R(I) / (GRAV*(XMVS-XMVE)*DM)
!
        ! ***************** !
        ! IV - EROSION FLUX ! (_IMP_)
        ! ***************** !
        ! CONCENTRATION INCREASED BY AVA BECAUSE IT IS COMPUTED
        ! ONLY WITH ONE CLASS OF SEDIMENT (ASSUMPTION)
!
        IF(TETAP.GT.AC) THEN
          AUX=(TETAP-AC)**1.75D0
          CSTAEQ%R(I) = 0.331D0*AUX/(1.D0+0.72D0*AUX)
          CSTAEQ%R(I) = MIN(CSTAEQ%R(I),CMAX)
        ELSE
          CSTAEQ%R(I) = 0.D0
        ENDIF
!
      ENDDO
!
!======================================================================!
!======================================================================!
!
      RETURN
      END
