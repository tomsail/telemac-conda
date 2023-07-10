!                   *****************
                    SUBROUTINE ANAVEN
!                   *****************
!
!***********************************************************************
! TOMAWAC   V6P3                                   08/06/2011
!***********************************************************************
!
!brief    SPECIFIES AN ANALYTICAL WIND
!+               (CAN BE VARIABLE IN TIME).
!
!history  M. BENOIT (LNH)
!+        07/06/95
!+        V1P0
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
!history  G.MATTAROLO (EDF - LNHE)
!+        08/06/2011
!+        V6P1
!+   Translation of French names of the variables in argument
!
!history  J-M HERVOUET (EDF R&D, LNHE)
!+        26/11/2013
!+        V6P3
!+   Example added to copy the wind in Telemac-2D memory.
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| AT             |-->| COMPUTATION TIME
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
      USE DECLARATIONS_TOMAWAC, ONLY :  NPOIN2,
     &                                  VX_CTE, VY_CTE, UV,VV
      USE INTERFACE_TOMAWAC, EX_ANAVEN => ANAVEN
      IMPLICIT NONE
!
!     WHEN COUPLING WITH TELEMAC-2D, THIS WILL ALLOW A COPY OF WIND
!     IN TELEMAC-2D
!
!     USE METEO_TELEMAC, ONLY : WINDX,WINDY
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER  IP
!
!     STANDARD CASE (CONSTANT VALUES IN THE STEERING FILE)
!
      DO IP=1,NPOIN2
        UV(IP)=VX_CTE
        VV(IP)=VY_CTE
      ENDDO
      ! USER FORTRAN
      CALL USER_ANAVEN
!
!-----------------------------------------------------------------------
!
      RETURN
      END
