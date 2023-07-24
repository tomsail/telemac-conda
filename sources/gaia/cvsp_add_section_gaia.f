!                   ********************************
                    SUBROUTINE CVSP_ADD_SECTION_GAIA
!                   ********************************
!
     &(J)
!
!***********************************************************************
! GAIA   V8P1                                   14/03/2013
!***********************************************************************
!
!>@brief    ADDS A SECTION TO THE VERTICAL SORTING PROFILE
!!        WITH 0 STRENGTH
!
!>@history  UWE MERKEL
!!        2011
!!        V6P2
!
!>@history  P. A. TASSI (EDF R&D, LNHE)
!!        12/03/2013
!!        V6P3
!!   Cleaning, cosmetic
!
!>@history  R. KOPMANN (BAW)
!!        25/02/2019 
!!        V7P2
!!   Removing 1/NSICLA 
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!>@param[in] J Index of a point in mesh
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE DECLARATIONS_GAIA
!
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN) :: J
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER  I
!
!-----------------------------------------------------------------------
!
      PRO_MAX(J)  = PRO_MAX(J) + 2
!
      DO I=1,NSICLA
        PRO_D(J,PRO_MAX(J),I) = PRO_D(J,PRO_MAX(J)-2,I)
        PRO_F(J,PRO_MAX(J),I) = 0.D0
        PRO_D(J,PRO_MAX(J)-1,I) = PRO_D(J,PRO_MAX(J)-2,I)
        PRO_F(J,PRO_MAX(J)-1,I) = 0.D0
      ENDDO
!
!-----------------------------------------------------------------------
!
      RETURN
      END SUBROUTINE CVSP_ADD_SECTION_GAIA
