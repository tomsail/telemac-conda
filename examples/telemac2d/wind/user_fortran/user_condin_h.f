!                   ************************
                    SUBROUTINE USER_CONDIN_H
!                   ************************
!
!
!***********************************************************************
! TELEMAC2D   V7P3
!***********************************************************************
!
!brief    USER INITIALISES THE PHYSICAL PARAMETERS U, V
!
!history  J-M HERVOUET (LNHE)
!+        30/08/2007
!+        V6P0
!+
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_TELEMAC
      USE DECLARATIONS_TELEMAC2D
      USE TPXO
      USE OKADA
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      DOUBLE PRECISION FAIR1, WIND, FVENT, HINI, LCANAL
      COMMON/FORFUN/FVENT,LCANAL,HINI
!
!-----------------------------------------------------------------------
!
      FAIR1  = 1.2615D-3
      WIND   = 5.D0
      FVENT  = FAIR1*WIND*WIND
      HINI   = -ZF%R(1)
      LCANAL = 500.D0

      CALL EXACTE(H%R,X,Y,NPOIN,ZF%R)
!
!-----------------------------------------------------------------------
!
      RETURN
      END
