!                   ****************************
                    SUBROUTINE ARTEMIS_CONSTANTS
!                   ****************************
!
!
!***********************************************************************
! ARTEMIS   V7P3                                     Aug 2017
!***********************************************************************
!
!brief    Sets a number of constants used by ARTEMIS, like PI, etc.
!
!history  N.DURAND (HRW)
!+        August 2017
!+        V7P3
!+   Taken from that for TOMAWAC (JMH, 27/11/2012, V6P3)
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!|                |---|
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE DECLARATIONS_ARTEMIS
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTRINSIC ATAN
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
!     VARIOUS CONSTANTS LINKED TO PI
!
!     for ALGORITHMIC DIFFERENTIATION using the formula below
      PI = 4.D0 * ATAN( 1.D0 )
      DEUPI  = 2.D0*PI
      PISUR2 = PI/2.D0
      USDPI  = 1.D0/DEUPI
      RADDEG = 180.D0/PI
      DEGRAD = PI/180.D0
!
!-----------------------------------------------------------------------
!
      RETURN
      END
