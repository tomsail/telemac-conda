!                   **************************
                    SUBROUTINE USER_INIT_COMPO
!                   **************************
!
     &(NCOUCHES)
!
!***********************************************************************
! SISYPHE
!***********************************************************************
!
!brief    USER INITIAL FRACTION DISTRIBUTION, STRATIFICATION,
!+                VARIATION IN SPACE.
!
!history  MATTHIEU GONZALES DE LINARES
!+        2002
!+        V6P0
!+
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| NCOUCHES       |-->| NUMBER OF LAYER FOR EACH POINT
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_TELEMAC
      USE DECLARATIONS_SISYPHE
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT (INOUT)::NCOUCHES(*)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER J
!
!-----------------------------------------------------------------------
!
      DO J=1,NPOIN
!
        NCOUCHES(J) = 1
!
        IF (ZF%R(J)>454.5D0) THEN
!
          AVAIL(J,1,1) = 0.065
          AVAIL(J,1,2) = 0.045
          AVAIL(J,1,3) = 0.145
          AVAIL(J,1,4) = 0.285
          AVAIL(J,1,5) = 0.345
          AVAIL(J,1,6) = 0.115
!
        ELSEIF(ZF%R(J)<453.8D0) THEN
!
          AVAIL(J,1,1) = 0.065D0
          AVAIL(J,1,2) = 0.
          AVAIL(J,1,3) = 0.
          AVAIL(J,1,4) = 0.
          AVAIL(J,1,5) = 0.
          AVAIL(J,1,6) = 0.935D0
!
        ELSEIF(ZF%R(J)<454.5D0.AND.ZF%R(J)>453.8D0) THEN
!         DGRA = DMAX + (ZF%R(J)-453.8D0)*(0.022D0-DMAX)/(454.5D0-453.8D0)
!
          AVAIL(J,1,1) = 0.065
          AVAIL(J,1,2) = 0.045*(ZF%R(J)-453.8D0)/(454.5D0-453.8D0)
          AVAIL(J,1,3) = 0.145*(ZF%R(J)-453.8D0)/(454.5D0-453.8D0)
          AVAIL(J,1,4) = 0.285*(ZF%R(J)-453.8D0)/(454.5D0-453.8D0)
          AVAIL(J,1,5) = 0.345*(ZF%R(J)-453.8D0)/(454.5D0-453.8D0)
          AVAIL(J,1,6) = 1.-(AVAIL(J,1,1)+AVAIL(J,1,2)+AVAIL(J,1,3)
     &                        +AVAIL(J,1,4)+AVAIL(J,1,5))
!
        ENDIF
      ENDDO
!
!-----------------------------------------------------------------------
!
      RETURN
      END
