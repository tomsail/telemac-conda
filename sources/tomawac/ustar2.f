!                   *****************
                    SUBROUTINE USTAR2
!                   *****************
!
     &( USTAR, NPOIN2)
!
!***********************************************************************
! TOMAWAC   V6P1                                   29/06/2011
!***********************************************************************
!
!brief    COMPUTES THE FRICTION VELOCITY U*
!+                FOR ALL THE NODES IN THE 2D MESH.
!+
!+            USES A DRAG COEFFICIENT WHICH VARIES LINEARLY WITH
!+                WIND SPEED. THE FORMULATION IS IDENTICAL TO THAT
!+                USED IN WAM-CYCLE 3 (WAMDI GROUP, 1988).
!
!reference  WAMDI GROUP (1988) :
!+                     "A THIRD GENERATION OCEAN WAVE PREDICTION MODEL".
!+                      JPO, VOL 18, PP 1775-1810.
!
!history  M. BENOIT (EDF/DER/LNH)
!+        26/03/96
!+        V1P1
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
!+        29/06/2011
!+        V6P1
!+   Translation of French names of the variables in argument
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| NPOIN2         |-->| NUMBER OF POINTS IN 2D MESH
!| USTAR          |<--| FRICTION VELOCITY
!| UV             |-->| WIND VELOCITY ALONG X AT THE MESH POINTS
!| VV             |-->| WIND VELOCITY ALONG Y AT THE MESH POINTS
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE DECLARATIONS_TOMAWAC, ONLY : UV, VV
      USE INTERFACE_TOMAWAC, EX_USTAR2 => USTAR2
      IMPLICIT NONE
!
!.....VARIABLES IN ARGUMENT
!     """"""""""""""""""""
      INTEGER, INTENT(IN)    ::  NPOIN2
      DOUBLE PRECISION, INTENT(INOUT) :: USTAR(NPOIN2)
!
!.....LOCAL VARIABLES
!     """""""""""""""""
      INTEGER  IP
      DOUBLE PRECISION UVENT , CDRAG
!
!
!.....MAIN LOOP ON THE NODES OF THE 2D MESH
!     """""""""""""""""""""""""""""""""""""""""""""""""""""
      DO IP=1,NPOIN2
!
!.......COMPUTES THE WIND SPEED 10 METERS ABOVE WATER
!       """""""""""""""""""""""""""""""""""""""""
        UVENT=SQRT(UV(IP)**2+VV(IP)**2)
!
!.......COMPUTES THE DRAG COEFFICIENT
!       """""""""""""""""""""""""""""""""
        CDRAG = 6.5D-5*UVENT + 8.D-4
        IF (UVENT.LT.7.5D0) CDRAG = 1.2875D-3
!
!.......COMPUTES THE FRICTION VELOCITY
!       """""""""""""""""""""""""""""""""""
        USTAR(IP)=SQRT(CDRAG)*UVENT
!
      ENDDO ! IP
!
      RETURN
      END
