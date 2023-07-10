!                   ***********************
                    SUBROUTINE PLANE_BOTTOM
!                   ***********************
!
     &(IPBOT,Z,NPOIN2,NPLAN,SIGMAG,OPTBAN)
!
!***********************************************************************
! TELEMAC3D   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    FOR EVERY 2D POINT, FINDS THE LAST PLANE WITH NO
!+                NORMAL HEIGHT ABOVE, I.E. DELTA(Z) EQUAL TO ZERO.
!+                IF NO PROBLEM IPBOT=0.
!+
!+            IF TIDAL FLAT IPBOT=NPLAN-1, SO PLANE IPBOT+1
!+                ALWAYS EXISTS AND HAS THE FIRST FREE POINT, UNLESS
!+                THERE IS NO DEPTH.
!
!history  J-M HERVOUET (LNHE)
!+        21/08/09
!+        V6P0
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
!history  J-M HERVOUET (LNHE)
!+        04/04/2012
!+        V6P2
!+   Argument HN suppressed.
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| IPBOT          |<->| PLANE NUMBER OF LAST CRUSHED PLANE (0 IF NONE)
!| NPLAN          |-->| NUMBER OF HORIZONTAL PLANES
!| NPOIN2         |-->| NUMBER OF 2D POINTS
!| OPTBAN         |-->| OPTION FOR TIDAL FLATS, IF 1, FREE SURFACE
!|                |   | MODIFIED AND PIECE-WISE LINEAR
!| SIGMAG         |-->| LOGICAL FOR GENERALISED SIGMA TRANSFORMATION
!| Z              |-->| ELEVATION OF POINTS IN THE MESH
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)          :: NPOIN2,NPLAN,OPTBAN
      INTEGER, INTENT(INOUT)       :: IPBOT(NPOIN2)
      DOUBLE PRECISION, INTENT(IN) :: Z(NPOIN2,NPLAN)
      LOGICAL, INTENT(IN)          :: SIGMAG
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER IPOIN2,IPLAN
!
      DOUBLE PRECISION, PARAMETER :: CHOUIA = 1.D-4
!
!-----------------------------------------------------------------------
!
      IF(SIGMAG.OR.OPTBAN.EQ.1) THEN
!
        DO IPOIN2=1,NPOIN2
          IPBOT(IPOIN2)=0
          DO IPLAN=1,NPLAN-1
            IF(Z(IPOIN2,IPLAN+1)-Z(IPOIN2,IPLAN).LT.CHOUIA) THEN
              IPBOT(IPOIN2)=IPLAN
            ENDIF
          ENDDO
        ENDDO
!
      ELSE
!
        DO IPOIN2=1,NPOIN2
          IPBOT(IPOIN2)=0
        ENDDO
!
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
