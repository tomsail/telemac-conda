!                   **************************
                    SUBROUTINE BEDLOAD_VANRIJN
!                   **************************
!
     &(TETAP,NPOIN, DM, DENS, GRAV, DSTAR, AC, QSC)
!
!***********************************************************************
! SISYPHE   V6P1                                   21/07/2011
!***********************************************************************
!
!brief    VAN RIJN BEDLOAD TRANSPORT FORMULATION.
!
!history  BUI MINH DUC
!+        **/10/2001
!+        V5P2
!+
!
!history  C. VILLARET
!+        **/**/2004
!+        V5P4
!+
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
!| AC             |<->| CRITICAL SHIELDS PARAMETER
!| DENS           |-->| RELATIVE DENSITY
!| DM             |-->| SEDIMENT GRAIN DIAMETER
!| DSTAR          |-->| NON-DIMENSIONAL DIAMETER
!| GRAV           |-->| ACCELERATION OF GRAVITY
!| NPOIN          |-->| NUMBER OF POINTS
!| QSC            |<->| BEDLOAD TRANSPORT RATE
!| TETAP          |-->| ADIMENSIONAL SKIN FRICTION
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE INTERFACE_SISYPHE,EX_BEDLOAD_VANRIJN => BEDLOAD_VANRIJN
      USE BIEF
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
      ! 2/ GLOBAL VARIABLES
      ! -------------------
      TYPE(BIEF_OBJ),   INTENT(IN)  :: TETAP
      INTEGER,          INTENT(IN)  :: NPOIN
      DOUBLE PRECISION, INTENT(IN)  :: DM, DENS, GRAV, DSTAR, AC
      TYPE(BIEF_OBJ),   INTENT(INOUT) :: QSC
      ! 3/ LOCAL VARIABLES
      ! ------------------
      INTEGER          :: I
      DOUBLE PRECISION :: C2, T
!======================================================================!
!======================================================================!
!                               PROGRAM                                !
!======================================================================!
!======================================================================!
      C2 = 0.053D0 * SQRT(DM**3*DENS*GRAV) * DSTAR**(-0.3D0)
      DO I = 1, NPOIN
        ! ****************************** !
        ! I - TRANSPORT STAGE PARAMETER  !
        ! ****************************** !
        IF(TETAP%R(I) .LE. AC) THEN
          T = 0.D0
        ELSE
          T = (TETAP%R(I)-AC)/MAX(AC,1.D-06)
        ENDIF
!
        ! ***************************** !
        ! II - BEDLOAD TRANSPORT RATE   !
        ! ***************************** !
        QSC%R(I) = C2 * T**2.1D0
      ENDDO
!======================================================================!
!======================================================================!
      RETURN
      END SUBROUTINE BEDLOAD_VANRIJN
