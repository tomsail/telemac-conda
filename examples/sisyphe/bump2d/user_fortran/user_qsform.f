!                   **********************
                    SUBROUTINE USER_QSFORM
!                   **********************
!
     &(U2D, V2D, TOB, HN, XMVE, TETAP, MU, NPOIN, DM,
     & DENS, GRAV, DSTAR, AC, QSC, QSS)
!
!***********************************************************************
! SISYPHE   V6P2                                   21/07/2011
!***********************************************************************
!
!brief    ALLOWS THE USER TO CODE THEIR OWN BEDLOAD TRANSPORT
!+                FORMULATION, BEST SUITED TO THEIR APPLICATION.
!
!history  F. HUVELIN
!+        **/11/2003
!+        V5P4
!+   MODIFIED
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE INTERFACE_SISYPHE, EX_USER_QSFORM => USER_QSFORM
      USE BIEF
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      TYPE(BIEF_OBJ),   INTENT(IN)    :: U2D,V2D,TOB,HN,TETAP,MU
      TYPE(BIEF_OBJ),   INTENT(INOUT) :: QSC, QSS
      INTEGER,          INTENT(IN)    :: NPOIN
      DOUBLE PRECISION, INTENT(IN)    :: XMVE, DM, DENS, GRAV, DSTAR, AC
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER          :: I
      DOUBLE PRECISION :: UNORM
      ! Sediment transport param (m^2s^-1)
      DOUBLE PRECISION, PARAMETER :: ACOEFF = 0.00167D0
!
!-----------------------------------------------------------------------
!
!     GRASS (1981) TYPE
!     later u/sqrt(u**2 + v**2)
!     later v/sqrt(u**2 + v**2)
!     qsc_u = QSC *  u/sqrt(u**2 + v**2)
!     qsc_v = QSC *  v/sqrt(u**2 + v**2)
!
      DO I = 1, NPOIN
        UNORM = SQRT(U2D%R(I)**2+V2D%R(I)**2)
        ! Grass (1981) type bedload (total load)
        QSC%R(I) = ACOEFF * (U2D%R(I)**2+V2D%R(I)**2) * UNORM
        QSS%R(I) = 0.D0 ! Zero suspended load

      END DO
!
!-----------------------------------------------------------------------
!
      RETURN
      END
