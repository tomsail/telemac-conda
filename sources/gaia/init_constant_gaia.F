!                   ************************
                    SUBROUTINE INIT_CONSTANT_GAIA
!                   ************************
!
     &(KARIM_HOLLY_YANG,KARMAN,PI)
!
!***********************************************************************
! GAIA
!***********************************************************************
!
!>@brief Sets the constants used by gaia.
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!>@param[in,out] KARIM_HOLLY_YAN KARIM, HOLLY & YANG constant
!>@param[in,out] KARMAN          VON KARMAN constant
!>@param[in,out] PI              pi
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!!-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      DOUBLE PRECISION, INTENT(INOUT) :: KARIM_HOLLY_YANG
      DOUBLE PRECISION, INTENT(INOUT) :: KARMAN
      DOUBLE PRECISION, INTENT(INOUT) :: PI
!
!!-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
! KARIM, HOLLY & YANG CONSTANT
! ----------------------------
!
      KARIM_HOLLY_YANG = 0.85D0
!
! VON KARMAN CONSTANT
! -------------------
!
      KARMAN = 0.4D0
!
! PARTHENIADES CONSTANT : EXPRESSED IN M/S
! ---------------------
! THIS VALUE IS NOW GIVEN IN USER_KRONE_PART
!      PARTHENIADES = 2.D-5/XMVS
!
! PI
! --
!
      PI = 4.D0 * ATAN( 1.D0 )
!
!======================================================================!
!
      RETURN
      END
