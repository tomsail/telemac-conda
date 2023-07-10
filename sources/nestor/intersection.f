!*********************************************************************************************
!*********************************************************************************************
!***                                              ********************************************
!***                                              ********************************************
      SUBROUTINE  Intersection                   !********************************************
!***                                              ********************************************
!***                                              ********************************************
!
     &( xA1,yA1,xA2,yA2,  xB1,yB1,xB2,yB2,  xS,yS )
!
      USE m_TypeDefs_Nestor, ONLY : R8
!
      IMPLICIT NONE
!
      REAL (KIND=R8),INTENT(IN)  :: xA1,yA1, xA2,yA2 ! points: A1,A2
      REAL (KIND=R8),INTENT(IN)  :: xB1,yB1, xB2,yB2 ! points: B1,B2
      REAL (KIND=R8),INTENT(OUT) :: xS,yS            ! point:  S  (intesection)
!
#ifndef NESTOR_INTERFACES
      !--------------------- local variables ---------------
      REAL (KIND=R8)::  AAx, AAy, AAC ! equation coefficients   A1A2
      REAL (KIND=R8)::  BBx, BBy, BBC ! equBtion coefficients   B1B2
      REAL (KIND=R8)::  D, Dx, Dy     ! determinats
!
!      dbug WRITE(6,*)'?>-------  SR Intersection -----------------'
!
      !       \       /
      !        \     * A2    we have two lines each given by two points
      !      B2 *   /        but we need the equation of a line in
      !          \ /         general (or standard) form which is   "Ax + By = C"
      !           S          thus we calculate the coefficients    "A    B    C"
      !          / \
      !         /   \
      !     A1 *     \
      !       /       * B1
      !      /         \
!
      AAx = yA1 - yA2             ! coefficient  "A" of line A1A2
      AAy = xA2 - xA1             ! coefficient  "B" of line A1A2
      AAC = xA2*yA1 - xA1*yA2     ! coefficient  "C" of line A1A2
!
      BBx = yB1 - yB2             ! coefficient  "A" of line B1B2
      BBy = xB2 - xB1             ! coefficient  "B" of line B1B2
      BBC = xB2*yB1 - xB1*yB2     ! coefficient  "C" of line B1B2
!
      ! we write the system of equations
      ! AAx + AAy = AAC   determinant D: |AAx AAy|  Dx: |AAC AAy|  Dy: |AAx AAC|
      ! BBx + BBy = BBC                  |BBx BBy|      |BBC BBy|      |BBx BBC|
      D  = AAx*BBy - AAy*BBx
      Dx = AAC*BBy - AAy*BBC
      Dy = AAx*BBC - AAC*BBx
!
      xS = Dx / D    ! x coordinate of intersection  S
      yS = Dy / D    ! y coordinate of intersection  S
!
!      dbug WRITE(6,*)'?>-------  SR Intersection End -------------'
      RETURN

#endif
      END SUBROUTINE Intersection                !********************************************
!***                                              ********************************************
!***                                              ********************************************
!*********************************************************************************************
!*********************************************************************************************