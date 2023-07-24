!                   ************************
                    SUBROUTINE INIT_CONSTANT
!                   ************************
!
     &(KARIM_HOLLY_YANG,KARMAN,PI)
!
!***********************************************************************
! SISYPHE   V7P1
!***********************************************************************
!
!brief    SETS THE CONSTANTS USED BY SISYPHE.
!
!history  F. HUVELIN
!+        11/01/2004
!+        V5P7
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
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| KARIM_HOLLY_YAN|-->| KARIM, HOLLY & YANG CONSTANT
!| KARMAN         |-->| VON KARMAN CONSTANT
!| PI             |-->| PI
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      DOUBLE PRECISION, INTENT(INOUT) :: KARIM_HOLLY_YANG
      DOUBLE PRECISION, INTENT(INOUT) :: KARMAN
      DOUBLE PRECISION, INTENT(INOUT) :: PI
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
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
