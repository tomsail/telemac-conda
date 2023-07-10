!                   **********************
                    SUBROUTINE USER_CORFON
!                   **********************
!
!
!***********************************************************************
! TELEMAC2D
!***********************************************************************
!
!brief    MODIFIES THE BOTTOM TOPOGRAPHY.
!
!warning  USER SUBROUTINE
!
!history  Y AUDOUIN (LNHE)
!+        20/09/2018
!+        V8P0
!+
!
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_TELEMAC2D
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER          N,I,FIRST,LAST
!
!-----------------------------------------------------------------------
!
!  SUPPRESS TIDAL FLATS ON MARITIME BOUNDARIES
!  PARAMETERS FIRST AND LAST THAT MAY BE CHANGED!!!
      FIRST = 250
      LAST  = 339
!
      DO N=1,MESH%NPTFR
        I=BOUNDARY_COLOUR%I(N)
        IF(I.GE.FIRST.AND.I.LE.LAST) THEN
          ZF%R(MESH%NBOR%I(N)) = MIN(0.D0,ZF%R(MESH%NBOR%I(N)))
        ENDIF
      ENDDO
      RETURN
      END
