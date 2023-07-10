!                       ******************
                        SUBROUTINE FILT_SA
!                       ******************
!
!***********************************************************************
! TOMAWAC   V6P2                                   25/06/2012
!***********************************************************************
!
!brief    NUMERICAL FILTER TO SMOOTH THE WAVE AMPLITUDES OF
!+            DIRECTIONAL SPECTRA
!
!history  E. KRIEZI (LNH)
!+        04/12/2006
!+        V5P5
!+
!
!
!history  G.MATTAROLO (EDF - LNHE)
!+        23/06/2012
!+        V6P2
!+   Modification for V6P2
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
!
      USE BIEF
      USE DECLARATIONS_TELEMAC
      USE DECLARATIONS_TOMAWAC
!
      USE INTERFACE_TOMAWAC, EX_FILT_SA => FILT_SA
      IMPLICIT NONE
!
      LISFON =1
!
      CALL FILTER(SA,.TRUE.,ST1,ST2,AM1,'MATMAS          ',
     &            1.D0,ST1,ST1,ST1,ST1,ST1,ST1,MESH,.FALSE.,ST1,LISFON)
!
!-----------------------------------------------------------------------
!
      RETURN
      END
