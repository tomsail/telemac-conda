!                   *************************
                    SUBROUTINE VITCHU_SISYPHE
!                   *************************
!
     & ( WS , DENS , DM , GRAV , VCE )
!
!***********************************************************************
! SISYPHE   V6P1                                   21/07/2011
!***********************************************************************
!
!brief    COMPUTES THE FALL VELOCITY.
!
!history
!+        20/05/96
!+        V5P1
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
!history  C.VILLARET (EDF-LNHE), P.TASSI (EDF-LNHE)
!+        19/07/2011
!+        V6P1
!+   Name of variables
!+
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| DENS           |-->| POIDS DEJAUGE
!| DM             |-->| MEAN DIAMETER OF THE SEDIMENT
!| GRAV           |-->| ACCELERATION OF GRAVITY
!| VCE            |-->| FLOW VISCOSITY
!| WS             |-->| SETTLING/FALL VELOCITY
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      DOUBLE PRECISION, INTENT(IN)    :: DENS,  DM,  GRAV, VCE
      DOUBLE PRECISION, INTENT(INOUT) :: WS
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
!
! FALL VELOCITY
! ================
!
      IF (DM.LT.1.D-4) THEN
        WS = DENS * DM * DM * GRAV / ( 18.D0 * VCE )
      ELSEIF (DM.LT.1D-3) THEN
        WS = 10.D0 * VCE / DM * (SQRT( 1.D0 + 0.01D0* DENS * GRAV *
     &       DM**3.D0 / (VCE*VCE) ) -1.D0 )
      ELSE
        WS = 1.1D0 * SQRT( DENS * GRAV * DM )
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END SUBROUTINE VITCHU_SISYPHE
