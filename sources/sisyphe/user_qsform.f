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
      USE INTERFACE_SISYPHE, EX_USER_QSFROM => USER_QSFORM
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
!
!-----------------------------------------------------------------------
!
!
!     EXAMPLE BY VAN RIJN
!
!     C1 = DENS * GRAV * DM
!     C2 = 0.053D0 * SQRT(DM**3*DENS*GRAV) * DSTAR**(-0.3D0)
!
!     DO I = 1, NPOIN
!
!       TRANSPORT STAGE PARAMETER
!
!       IF(TETAP%R(I) .LE. AC) THEN
!         T = 0.D0
!       ELSE
!         T = (TETAP%R(I)-AC)/MAX(AC,1.D-06)
!       ENDIF
!
!       BEDLOAD TRANSPORT RATE
!
!       QSC%R(I) = C2 * T**2.1D0
!       QSS%R(I) = 0.D0
!
!     ENDDO
!
!  FOLLOWING LINES NEED TO BE COMMENTED OUT
!
      WRITE(LU,53)
!
53    FORMAT(/,1X,'SISYPHE IS STOPPED : ',/
     &      ,1X,' SAND TRANSPORT MUST BE CALCULATED IN USER_QSFORM')
      CALL PLANTE(1)
      STOP
!
!-----------------------------------------------------------------------
!
      RETURN
      END
