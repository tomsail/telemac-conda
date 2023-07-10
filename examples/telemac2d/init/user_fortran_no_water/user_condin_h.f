!                   ************************
                    SUBROUTINE USER_CONDIN_H
!                   ************************
!
!
!***********************************************************************
! TELEMAC2D   V7P3
!***********************************************************************
!
!brief    USER INITIALISES THE PHYSICAL PARAMETERS U, V
!
!history  J-M HERVOUET (LNHE)
!+        30/08/2007
!+        V6P0
!+
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_TELEMAC
      USE DECLARATIONS_TELEMAC2D
      USE TPXO
      USE OKADA
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      DOUBLE PRECISION COT
      INTEGER LIST_POINT(13)
      INTEGER I,ID
!
!-----------------------------------------------------------------------
!
      CALL OS('X=0     ', X=H)
      CALL OS('X=X-Y   ', X=H, Y=ZF)
      COT = 260.D0
      LIST_POINT = (/14,23,114,258,274,80,255,207,130,229,117,176,29/)
      DO I=1,13
        ID = LIST_POINT(I)
        IF(NCSIZE.GT.1) ID = GLOBAL_TO_LOCAL_POINT(ID,MESH)
        IF(ID.NE.0) H%R(ID) = COT-ZF%R(ID)
      ENDDO
!
!-----------------------------------------------------------------------
!
      RETURN
      END
