!                   **********************
                    SUBROUTINE USER_ANAVEN
!                   **********************
!
!
!***********************************************************************
! TOMAWAC
!***********************************************************************
!
!brief    USER SPECIFIES AN ANALYTICAL WIND
!+               (CAN BE VARIABLE IN TIME).
!
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| AT             |-->| COMPUTATION TIME
!| DDC            |-->| DATE OF COMPUTATION BEGINNING
!| NPOIN2         |-->| NUMBER OF POINTS IN 2D
!| UV             |<--| WIND VELOCITY ALONG X AT THE MESH POINTS
!| VV             |<--| WIND VELOCITY ALONG Y AT THE MESH POINTS
!| VX_CTE         |-->| WIND ALONG X (CONSTANT VALUE IN STEERING FILE)
!| VY_CTE         |-->| WIND ALONG Y (CONSTANT VALUE IN STEERING FILE)
!| X              |-->| ABSCISSAE OF POINTS IN THE MESH
!| Y              |-->| ORDINATES OF POINTS IN THE MESH
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE DECLARATIONS_SPECIAL
!     USE DECLARATIONS_TOMAWAC, ONLY :  NPOIN2, AT    , DDC   ,
!    &                                  VX_CTE, VY_CTE, X, Y,UV,VV
!     USE METEO_TELEMAC, ONLY : WINDX,WINDY
      USE INTERFACE_TOMAWAC, EX_USER_ANAVEN => USER_ANAVEN
      IMPLICIT NONE

!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
!
!-----------------------------------------------------------------------
!
!     EXAMPLE IF COUPLING WITH TELEMAC-2D
!     READING TOMAWAC WIND IN TELEMAC-2D SHOULD BE POSSIBLE ALSO
!
!     DO IP=1,NPOIN2
!       UV(IP)=WINDX%R(IP)
!       VV(IP)=WINDY%R(IP)
!     ENDDO
!
!-----------------------------------------------------------------------
!
      RETURN
      END
