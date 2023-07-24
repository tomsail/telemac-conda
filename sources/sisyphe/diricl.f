!                   *****************
                    SUBROUTINE DIRICL
!                   *****************
!
     &( ZF1 , ZF , EBOR , LIEBOR , NBOR , NPOIN  , NPTFR  , KENT )
!
!***********************************************************************
! SISYPHE   V6P1                                   21/07/2011
!***********************************************************************
!
!brief    DETERMINES THE BOUNDARY CONDITIONS ON E
!+                FOR DIRICHLET POINTS.
!
!history  C.LE NORMANT
!+        **/10/97
!+        V5P1
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
!| EBOR           |<->| EVOLUTION OF BOUNDARY POINTS
!| KENT           |-->| CONVENTION FOR LIQUID INPUT WITH PRESCRIBED VALUE
!| LIEBOR         |<->| BOUNDARY CONDITION TYPE OVER E
!| NBOR           |-->| GLOBAL NUMBER OF BOUNDARY POINT
!| NPOIN          |-->| NUMBER OF POINTS
!| NPTFR          |-->| NUMBER OF BOUNDARY POINTS
!| ZF             |-->| ELEVATION OF BOTTOM
!| ZF1            |<->| ELEVATION OF BOTTOM
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN):: KENT,NPOIN,NPTFR
      INTEGER, INTENT(IN):: NBOR(NPTFR)
      INTEGER, INTENT(IN):: LIEBOR(NPTFR)
!
      DOUBLE PRECISION, INTENT(IN)::  ZF(NPOIN)
      DOUBLE PRECISION, INTENT(INOUT):: ZF1(NPOIN)
      DOUBLE PRECISION, INTENT(IN) :: EBOR(NPTFR)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER K, N
!
!-----------------------------------------------------------------------
!
      DO K=1,NPTFR
!
        N = NBOR(K)
!
        IF (LIEBOR(K).EQ.KENT) THEN
          ZF1(N)   = EBOR(K)+ZF(N)
        ENDIF
!
      ENDDO
!
!-----------------------------------------------------------------------
!
      RETURN
      END SUBROUTINE DIRICL
