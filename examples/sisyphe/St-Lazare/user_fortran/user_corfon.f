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
! CCV MODIFICATION DU FOND FRONTIERE 2
! ++++++++++++++++++++++++++++++++++++
      I = GLOBAL_TO_LOCAL_POINT(61, MESH)
      IF(I.NE.0) ZF%R(I) = 460.5
      I = GLOBAL_TO_LOCAL_POINT(323, MESH)
      IF(I.NE.0) ZF%R(I) = 459.5
      I = GLOBAL_TO_LOCAL_POINT(565, MESH)
      IF(I.NE.0) ZF%R(I) = 459.5
      I = GLOBAL_TO_LOCAL_POINT(363, MESH)
      IF(I.NE.0) ZF%R(I) = 460.5
!
!-----------------------------------------------------------------------
!
      RETURN
      END
