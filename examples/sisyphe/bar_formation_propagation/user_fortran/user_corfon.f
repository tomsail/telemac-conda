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
      INTEGER I
      REAL YR
!
!-----------------------------------------------------------------------
!
      DO I=1,NPOIN
        CALL RANDOM_NUMBER(HARVEST=YR)
        ZF%R(I)=0.531D0-0.00452D0*X(I)+(YR*0.005D0-0.005D0)
      ENDDO
!
!     IN PARALLEL, ZF MUST BE THE SAME IN ALL PROCESSORS
!
      IF(NCSIZE.GT.1) CALL PARCOM(ZF,3,MESH)
!
      RETURN
      END
