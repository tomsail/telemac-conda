!                   ***************************
                    SUBROUTINE BEDLOAD_ENGEL_CC
!                   ***************************
!
     &(TETAP,CF,NPOIN,GRAV,DM,DENS,TETA,QSC)
!
!***********************************************************************
! SISYPHE   V7P3                                   10/01/2018
!***********************************************************************
!
!brief    ENGELUND-HANSEN BEDLOAD TRANSPORT FORMULATION.
!
!warning  FORMULATION IS DIFFERENT FROM THAT IN BEDLOAD_ENGEL
!
!history  E. PELTIER; C. LENORMANT; J.-M. HERVOUET
!+        11/09/1995
!+        V5P1
!+
!
!history  C.VILLARET
!+        **/11/2003
!+        V5P4
!+
!
!history  J-M HERVOUET
!+        11/07/2007
!+        V5P8
!+   DELETED OS REFERENCES
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
!history  P.TASSI (EDF-LNHE)
!+        10/01/2018
!+        V7P3
!+ Correction of the coefficient of the Engelund and Hansen formula
!+ (thanks to Alexander Breugem)
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| CF             |-->| QUADRATIC FRICTION COEFFICIENT
!| DENS           |-->| RELATIVE DENSITY
!| DM             |-->| SEDIMENT GRAIN DIAMETER
!| GRAV           |-->| ACCELERATION OF GRAVITY
!| NPOIN          |-->| NUMBER OF POINTS
!| QSC            |<->| BED LOAD TRANSPORT
!| TETA           |<->| DIMENSIONLESS BED SHEAR STRESS
!| TETAP          |-->| ADIMENSIONAL SKIN FRICTION
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE INTERFACE_SISYPHE,EX_BEDLOAD_ENGEL_CC => BEDLOAD_ENGEL_CC
      USE BIEF
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      TYPE(BIEF_OBJ),   INTENT(IN)    :: TETAP,CF
      INTEGER,          INTENT(IN)    :: NPOIN
      DOUBLE PRECISION, INTENT(IN)    :: GRAV, DM, DENS
      TYPE(BIEF_OBJ),   INTENT(INOUT) :: TETA! WORK ARRAY T1
      TYPE(BIEF_OBJ),   INTENT(INOUT) :: QSC
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER          :: I
      DOUBLE PRECISION :: CENGEL
!
      INTRINSIC SQRT
!
!======================================================================!
!======================================================================!
!                               PROGRAM                                !
!======================================================================!
!======================================================================!
!
!     ADIMENSIONAL SKIN STRESS: TETAP
!
!     ADIMENSIONAL TOTAL STRESS
!
      DO I = 1, NPOIN
        IF(TETAP%R(I) <= 0.06D0) THEN
          TETA%R(I) = 0.D0
        ELSEIF(TETAP%R(I) <  0.384D0) THEN
          TETA%R(I) = SQRT( 2.5D0 * (TETAP%R(I) - 0.06D0))
        ELSEIF(TETAP%R(I) <  1.080D0) THEN
          TETA%R(I) = 1.066D0 * TETAP%R(I)**0.176D0
        ELSE
          TETA%R(I) = TETAP%R(I)
        ENDIF
      ENDDO
!
!     BEDLOAD TRANSPORT
!
      CENGEL = 0.05D0*SQRT(DENS*GRAV*DM**3)
      DO I=1,NPOIN
        QSC%R(I)=CENGEL*SQRT(TETA%R(I)**5)/MAX(CF%R(I),1.D-6)
      ENDDO
!
!-----------------------------------------------------------------------
!
      RETURN
      END
