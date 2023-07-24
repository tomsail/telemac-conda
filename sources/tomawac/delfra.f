!                   ***************
                    FUNCTION DELFRA
!                   ***************
!
     &( SS    )
!
!***********************************************************************
! TOMAWAC   V6P1                                   14/06/2011
!***********************************************************************
!
!brief    COMPUTES THE COEFFICIENT THAT NORMALISES THE DIRECTIONAL
!+                SPREADING FUNCTION IN COS **2.S (TETA-TETA0).
!code
!+                               GAMMA( SS + 0.5)
!+        DELFRA(SS) = SQRT(PI)  ----------------
!+                               GAMMA( SS + 1. )
!
!history  M. BENOIT
!+        15/11/95
!+        V1P0
!+   CREATED
!
!history  M. BENOIT
!+        07/11/96
!+        V1P2
!+   MODIFIED
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
!+        15/06/2011
!+        V6P1
!+   Translation of French names of the variables in argument
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| DEUPI          |-->| 2.PI
!| SS             |-->| EXPONENT OF THE DIRECTIONAL SPREADING FUNCTION
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE INTERFACE_TOMAWAC, EX_DELFRA => DELFRA
      USE DECLARATIONS_TOMAWAC, ONLY : DEUPI
      IMPLICIT NONE
!
!.....VARIABLES IN ARGUMENT
!     """""""""""""""""""""
      DOUBLE PRECISION DELFRA, SS
!
!.....EXTERNAL FUNCTIONS
!     """"""""""""""""""
!      DOUBLE PRECISION GAMMLN
!      EXTERNAL         GAMMLN
!
!
      DELFRA=SQRT(DEUPI/2.D0)
     &      *EXP(GAMMLN(SS+0.5D0,DEUPI)-GAMMLN(SS+1.D0,DEUPI))
!
      RETURN
      END
