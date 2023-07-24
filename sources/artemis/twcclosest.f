!                   *********************
                    SUBROUTINE TWCCLOSEST
!                   *********************
!
!***********************************************************************
! ARTEMIS   V7P4                                     Nov 2017
!***********************************************************************
!
!brief    IDENTIFIES ARTEMIS BOUNDARY NODE CLOSEST TO REFERENCE POINT
!+               (ONLY REQUIRED FOR CALL TO ENTART, artemis.f line 489)
!
!history  N.DURAND (HRW)
!+        Nov 2017
!+        V7P4
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_ARTEMIS
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!-----------------------------------------------------------------------
!
      INTEGER                       :: IPTFR
      DOUBLE PRECISION              :: EPS, DIST
!
!=======================================================================
!
      EPS = 3000.D0
      IPTFR_REF = 0
!
      DO IPTFR = 1 , NPTFR
        DIST = DSQRT((X(MESH%NBOR%I(IPTFR))-X_SFREF)**2 +
     &               (Y(MESH%NBOR%I(IPTFR))-Y_SFREF)**2 )
        IF(DIST.LT.EPS) THEN
          EPS = DIST
          IPTFR_REF = IPTFR
        ENDIF
      ENDDO
!
      IF(IPTFR_REF.EQ.0) THEN
        WRITE(LU,201) X_SFREF, Y_SFREF
!       CALL PLANTE(1)
!       STOP
      ENDIF
!
!-----------------------------------------------------------------------
!
!     PRINTOUT FORMATS:
!
201   FORMAT(/,1X,'CLOSEST : PLEASE REVIEW COORDINATES FOR THE',
     &       ' REFERENCE POINT (',1F9.2,';',1F9.2,')')
!
!-----------------------------------------------------------------------
!
      RETURN
      END
