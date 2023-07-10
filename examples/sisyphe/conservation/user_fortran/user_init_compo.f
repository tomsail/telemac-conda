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
      INTEGER I,J,K
!
!-----------------------------------------------------------------------
!
      DO J=1,NPOIN
!
        NCOUCHES(J) = NOMBLAY
        DO K=1,NOMBLAY
          DO I = 1, NSICLA
            AVAIL(J,K,I) = AVA0(I)
          ENDDO
        ENDDO
        ES(J,1) = ZF%R(J)-ZR%R(J)
!       LAST LAYER IS ZERO IN ORDER TO HAVE ONLY 1 LAYER
        ES(J,2) = 0.D0
!
      ENDDO
!
!-----------------------------------------------------------------------
!
      RETURN
      END
