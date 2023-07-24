!                   ************************
                    SUBROUTINE BEDLOAD_EINST
!                   ************************
!
     &(TETAP, NPOIN, DENS, GRAV, DM, DSTAR, QSC)
!
!***********************************************************************
! SISYPHE   V6P1                                   21/07/2011
!***********************************************************************
!
!brief    EINSTEIN-BROWN BEDLOAD TRANSPORT FORMULATION.
!
!history  E. PELTIER; C. LENORMANT; J.-M. HERVOUET
!+        11/09/1995
!+        V5P1
!+
!
!history  C.VILLARET
!+        **/10/2003
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
!| DENS           |-->| RELATIVE SENSITY OF SEDIMENT
!| DM             |-->| SEDIMENT GRAIN DIAMETER
!| DSTAR          |-->| NON-DIMENSIONAL DIAMETER
!| GRAV           |-->| ACCELERATION OF GRAVITY
!| NPOIN          |-->| NUMBER OF POINTS
!| QSC            |<->| BED LOAD TRANSPORT
!| TETAP          |-->| ADIMENSIONAL SKIN FRICTION
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE INTERFACE_SISYPHE, EX_BEDLOAD_EINST => BEDLOAD_EINST
      USE BIEF
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      TYPE(BIEF_OBJ),   INTENT(IN)    :: TETAP
      INTEGER,          INTENT(IN)    :: NPOIN
      DOUBLE PRECISION, INTENT(IN)    :: DENS, GRAV, DM, DSTAR
      TYPE(BIEF_OBJ),   INTENT(INOUT)   :: QSC
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER          :: I
      DOUBLE PRECISION :: CEINST
!
!======================================================================!
!                               PROGRAM                                !
!======================================================================!
!
!     BEDLOAD TRANSPORT
!
      CEINST = 36.D0/(DSTAR**3)
      CEINST = SQRT(2.D0/3.D0+CEINST) -  SQRT(CEINST)
      CEINST = CEINST * SQRT(DENS*GRAV*(DM**3))
      DO I = 1, NPOIN
        IF(TETAP%R(I) < 2.5D-3) THEN
          QSC%R(I) = 0.D0
        ELSE IF (TETAP%R(I) < 0.2D0) THEN
          QSC%R(I) = 2.15D0* CEINST * EXP(-0.391D0/TETAP%R(I))
        ELSE
          QSC%R(I) = 40.D0 * CEINST * (TETAP%R(I)**3.D0)
        ENDIF
      ENDDO
!
!=======================================================================
!
      RETURN
      END
