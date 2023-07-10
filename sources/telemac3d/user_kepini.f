!                   **********************
                    SUBROUTINE USER_KEPINI
!                   **********************
!
     &(AK,EP,U,V,Z,ZF,NPOIN2,NPLAN,DNUVIH,DNUVIV,KARMAN,CMU,KMIN,EMIN)
!
!***********************************************************************
! TELEMAC3D
!***********************************************************************
!
!brief    USER INITIALISES K AND EPSILON.
!
!history  JACEK A. JANKOWSKI PINXIT
!+        **/03/99
!+
!+   FORTRAN95 VERSION
!
!history  V. BOYER UMIST
!+
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
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| AK             |<->| TURBULENT ENERGY
!| CMU            |-->| CONSTANT FOR MODELE K-EPSILON MODEL
!| DNUVIH         |-->| COEFFICIENT FOR HORIZONTAL DIFFUSION OF VELOCITIES
!| DNUVIV         |-->| COEFFICIENT FOR VERTICAL DIFFUSION OF VELOCITIES
!| EMIN           |-->| MINIMUM VALUE FOR EPSILON WHEN CLIPPING
!| EP             |<->| TURBULENT DISSIPATION
!| KARMAN         |-->| KARMAN CONSTANT
!| KMIN           |-->| MINIMUM VALUE FOR K WHEN CLIPPING
!| NPLAN          |-->| NUMBER OF PLANES IN THE 3D MESH OF PRISMS
!| NPOIN2         |-->| NUMBER OF POINTS IN 2D
!| U              |-->| COMPONENT OF VELOCITY
!| V              |-->| COMPONENT OF VELOCITY
!| Z              |-->| ELEVATION OF REAL 3D MESH POINTS
!| ZF             |-->| ELEVATION OF BOTTOM
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)            :: NPOIN2,NPLAN
      DOUBLE PRECISION, INTENT(INOUT):: AK(NPOIN2,NPLAN)
      DOUBLE PRECISION, INTENT(INOUT):: EP(NPOIN2,NPLAN)
      DOUBLE PRECISION, INTENT(IN)   :: U(NPOIN2,NPLAN), V(NPOIN2,NPLAN)
      DOUBLE PRECISION, INTENT(IN)   :: Z(NPOIN2,NPLAN)
      DOUBLE PRECISION, INTENT(IN)   :: ZF(NPOIN2)
      DOUBLE PRECISION, INTENT(IN)   :: KARMAN, DNUVIH, DNUVIV
      DOUBLE PRECISION, INTENT(IN)   :: CMU
      DOUBLE PRECISION, INTENT(IN)   :: KMIN, EMIN
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER IPOIN2,IPLAN
!
      INTRINSIC LOG, SQRT, MAX
!
      DOUBLE PRECISION, PARAMETER :: FICTIFEPS = 2.D0
      DOUBLE PRECISION :: DIST
!
!-----------------------------------------------------------------------
!
      IF(.FALSE.) THEN
!
!     A THEORY BY VINCENT BOYER MODIFIED BY MARTIN FERRAND
!
        DO IPOIN2 = 1,NPOIN2
          DO IPLAN = 1,NPLAN

!           ARBITRARY COMPUTATION OF K EXPRESSED AS A PERCENTAGE OF SPEED

            AK(IPOIN2,IPLAN) = 1.D-3*U(IPOIN2,IPLAN)**2
            AK(IPOIN2,IPLAN) = MAX(AK(IPOIN2,IPLAN),KMIN)

!           COMPUTATION OF EPSILON

!           EP INITIALISED ACCORDING TO UETOIL**3/KAPPA/Y
!           WHERE UETOIL IS CALCULATED FROM THE VALUE OF K AT THE WALL

            IF(IPLAN.EQ.1) THEN
              DIST = (Z(IPOIN2,2)-ZF(IPOIN2))/FICTIFEPS
            ELSE
              DIST = Z(IPOIN2,IPLAN)-ZF(IPOIN2)
            ENDIF
            EP(IPOIN2,IPLAN)=
     &           CMU**0.75*SQRT(AK(IPOIN2,1)**3)/KARMAN/DIST
            EP(IPOIN2,IPLAN)=MAX(EP(IPOIN2,IPLAN),EMIN)
          ENDDO
        ENDDO
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
