!                   *****************
                    SUBROUTINE MASKAB
!                   *****************
!
     &(HN , Q , QU , QV , NPOIN)
!
!***********************************************************************
! SISYPHE   V6P1                                   21/07/2011
!***********************************************************************
!
!brief    ELIMINATES NEGATIVE WATER DEPTHS.
!
!history  E. PELTIER; C. LENORMANT; J.-M. HERVOUET
!+        11/09/95
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
!| HN             |<->| WATER DEPTH
!| NPOIN          |-->| NUMBER OF POINTS
!| Q              |-->| LIQUID DISCHARGE
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN):: NPOIN
!
      DOUBLE PRECISION, INTENT(IN)    :: HN(NPOIN)
      DOUBLE PRECISION, INTENT(INOUT) :: Q(NPOIN),QU(NPOIN),QV(NPOIN)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER I
!
!-----------------------------------------------------------------------
!
!
! CAPS WATER DEPTHS
!
      DO I=1,NPOIN
!
!  TREATS NEGATIVE VALUES IN THE DOMAIN
!
        IF(HN(I).LE.0.D0) THEN
          Q(I)  = 0.D0
          QU(I) = 0.D0
          QV(I) = 0.D0
        ENDIF
      ENDDO

      CALL USER_MASKAB (HN , Q , QU , QV , NPOIN)
!
!-----------------------------------------------------------------------
!
      RETURN
      END SUBROUTINE MASKAB
