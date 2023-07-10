!                   ******************
                    SUBROUTINE CALUEB2
!                   ******************
!
!
!***********************************************************************
! ARTEMIS   V7P3                                     Aug 2017
!***********************************************************************
!
!brief    COMPUTES AN EFFECTIVE SPEED UE FOR THE ESTIMATION
!+        OF THE FRICTION DISSIPATION UNDER IRREGULAR SEA STATES
!+
!
!history  C.PEYRARD (LNHE)
!+        06/2014
!+        V7P0
!+
!
!history  N.DURAND (HRW)
!+        August 2017
!+        V7P3
!+   PI now defined in DECLARATIONS_ARTEMIS
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_TELEMAC
      USE DECLARATIONS_ARTEMIS
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
      INTEGER I
!
      DOUBLE PRECISION DEUKD,KD,UI
!
!-----------------------------------------------------------------------
!
      DOUBLE PRECISION, PARAMETER :: ZERO = 1.D-10
!
!=======================================================================
! CASE 1 : USE OF BOTTOM FRICTION COMPUTED FROM THE POTENTIAL
!=======================================================================
! VELOCITY IN Z=-D FOR RANDOM WAVES (see TOMAWAC vitfon.f)
!    UE^2 = UE^2 + 2 * Sp(f,teta) df dteta * K g / SINH(DEUKD) = UE^2 + Ai²*K g / SINH(DEUKD)
!    Ue :Ai = Hi/2 = (HHO/2)
!    But here we look to RMS value : Ue => Ue/sqrt(2) and we write Ai²=HHO/8
!=======================================================================
!
!=======================================================================
! CASE 2 : USE OF BOTTOM FRICTION COMPUTED FROM THE STOKES LINEAR THEORY
!          + linear hypothesis abs(U) * U = 8/3PI U
!=======================================================================
! VELOCITY IN Z=-D FOR RANDOM WAVES
!   Ui = H/2 * (gk/w)*1/ch(kD)
!=======================================================================
!
      IF (FORMFR.EQ.1) THEN
        DO I=1,NPOIN
          DEUKD=2D0*K%R(I)*H%R(I)
!         UPDATE OF UEB WITH ACTUAL WAVE COMPONENT
          UEB%R(I)=UEB%R(I)+HHO%R(I)**2/8D0*K%R(I)*GRAV/SINH(DEUKD)
        ENDDO
      ELSEIF (FORMFR.EQ.2) THEN
        DO I=1,NPOIN
          KD=K%R(I)*H%R(I)
!         UPDATE OF UEB WITH ACTUAL WAVE COMPONENT
          UI      =(8D0/(3D0*PI))*(HHO%R(I)/2D0)*GRAV*K%R(I)
          UI      =UI/(OMEGA*COSH(KD))
          UEB%R(I)=UEB%R(I)+UI**2
        ENDDO
      ELSE
        WRITE(LU,*) 'YOUR OPTION FOR BOTTOM FRICTION IS NOT CORRECT'
        WRITE(LU,*) 'CALUEB2 : PLEASE USE FORMFR = 1 OR FORMFR = 2 '
        WRITE(LU,*) 'THE CODE IS GOING TO STOP.....................'
        STOP
      ENDIF
!
      RETURN
      END SUBROUTINE
