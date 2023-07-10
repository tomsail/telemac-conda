!                   ******************
                    SUBROUTINE AIRWIK3
!                   ******************
!
     & (LIHBOR,U,V,XNEBOR,YNEBOR,NBOR,NPTFR,NPLAN,NPOIN2,KLOG)
!
!***********************************************************************
! TELEMAC3D   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    ENSURES THE CONDITION U . N = 0  (U AND N ARE VECTORS).
!+
!+           (FOR A LATERAL SOLID BOUNDARY, DUPLICATES THE NORMAL
!+                COMPONENT OF THE VELOCITY, COMPUTED BY TELEMAC, ON THE VERTICAL).
!+
!+            ALSO ENSURES THE DIRICHLET CONDITIONS.
!
!history  JM HERVOUET (LNHE)
!+        29/02/08
!+        V5P9
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
!| KLOG           |-->| CONVENTION FOR SOLID BOUNDARY
!| LIHBOR         |-->| TYPE OF BOUNDARY CONDITIONS ON DEPTH
!| NBOR           |-->| GLOBAL NUMBER OF BOUNDARY POINTS
!| NPLAN          |-->| NUMBER OF PLANES IN THE 3D MESH OF PRISMS
!| NPOIN2         |-->| NUMBER OF POINTS IN 2D
!| NPTFR          |-->| NUMBER OF BOUNDARY POINTS IN 2D
!| XNEBOR         |-->| X COMPONENT OF NORMAL VECTOR FOR BOUNDARY POINTS
!| YNEBOR         |-->| Y COMPONENT OF NORMAL VECTOR FOR BOUNDARY POINTS
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!----------------------------------------------------------------------
!
      INTEGER, INTENT(IN) :: NPTFR,NPLAN,NPOIN2,KLOG
      INTEGER, INTENT(IN) :: LIHBOR(NPTFR),NBOR(NPTFR)
!
      DOUBLE PRECISION, INTENT(IN)    :: XNEBOR(NPTFR),YNEBOR(NPTFR)
      DOUBLE PRECISION, INTENT(INOUT) :: U(NPOIN2,NPLAN),V(NPOIN2,NPLAN)
!
!-----------------------------------------------------------------------
!
      INTEGER IPTFR,IPLAN,IPOIN
      DOUBLE PRECISION PSCAL
!
!***********************************************************************
!
! BOUNDARY CONDITIONS
!
! ENFORCES BC'S ON THE 3D FIELDS
!
      DO IPLAN=1,NPLAN
        DO IPTFR=1,NPTFR
          IF(LIHBOR(IPTFR).EQ.KLOG) THEN
            IPOIN=NBOR(IPTFR)
            PSCAL = U(IPOIN,IPLAN)*XNEBOR(IPTFR)
     &            + V(IPOIN,IPLAN)*YNEBOR(IPTFR)
            U(IPOIN,IPLAN) = U(IPOIN,IPLAN) - PSCAL*XNEBOR(IPTFR)
            V(IPOIN,IPLAN) = V(IPOIN,IPLAN) - PSCAL*YNEBOR(IPTFR)
          ENDIF
        ENDDO
      ENDDO
!
!-----------------------------------------------------------------------
!
      RETURN
      END
