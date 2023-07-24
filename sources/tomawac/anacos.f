!                   *****************
                    SUBROUTINE ANACOS
!                   *****************
!***********************************************************************
! TOMAWAC   V6P1                                   09/06/2011
!***********************************************************************
!
!brief    SPECIFIES A ! STATIONARY ! ANALYTICAL CURRENT.
!
!history
!+        07/06/2001
!+        V5P2
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
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| NPOIN2         |-->| NUMBER OF POINTS IN 2D
!| UC             |<--| CURRENT VELOCITY ALONG X AT THE MESH POINTS
!| VC             |<--| CURRENT VELOCITY ALONG Y AT THE MESH POINTS
!| X              |-->| ABSCISSAE OF POINTS IN THE MESH
!| Y              |-->| ORDINATES OF POINTS IN THE MESH
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE DECLARATIONS_SPECIAL
      USE DECLARATIONS_TOMAWAC, ONLY : UC, VC, NPOIN2
      USE INTERFACE_TOMAWAC, EX_ANACOS => ANACOS
      IMPLICIT NONE
!
!.....LOCAL VARIABLES
!     """""""""""""""""
      INTEGER  IP
      DOUBLE PRECISION UCONST, VCONST
!
!
      UCONST=0.D0
      VCONST=0.D0
!
      DO IP=1,NPOIN2
        UC(IP)=UCONST
        VC(IP)=VCONST
      ENDDO ! IP
!
      ! USER FUNCTION
      CALL USER_ANACOS
      RETURN
      END
