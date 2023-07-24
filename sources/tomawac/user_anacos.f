!                   **********************
                    SUBROUTINE USER_ANACOS
!                   **********************
!***********************************************************************
! TOMAWAC
!***********************************************************************
!
!brief    SPECIFIES A ! STATIONARY ! ANALYTICAL CURRENT.
!
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| NPOIN2         |-->| NUMBER OF POINTS IN 2D
!| UC             |<--| CURRENT VELOCITY ALONG X AT THE MESH POINTS
!| VC             |<--| CURRENT VELOCITY ALONG Y AT THE MESH POINTS
!| X              |-->| ABSCISSAE OF POINTS IN THE MESH
!| Y              |-->| ORDINATES OF POINTS IN THE MESH
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE DECLARATIONS_SPECIAL
!     USE DECLARATIONS_TOMAWAC, ONLY : UC, VC, X, Y, NPOIN2
      USE INTERFACE_TOMAWAC, EX_USER_ANACOS => USER_ANACOS
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
!
!-----------------------------------------------------------------------
!
!
!-----------------------------------------------------------------------
!
      RETURN
      END
