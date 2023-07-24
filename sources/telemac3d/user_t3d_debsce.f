!                   **************************
                    SUBROUTINE USER_T3D_DEBSCE
!                   **************************
!
     &( T3D_DEBSCE, TIME , I , DISCE )
!
!***********************************************************************
! TELEMAC3D
!***********************************************************************
!
!brief    PRESCRIBES THE DISCHARGE FOR EVERY SOURCE POINT
!+               (CAN BE A FUNCTION OF TIME AND SPACE/DEPTH).
!
!note     NOMVEF AND NVEF ARE THE NAME AND LOGICAL UNIT OF THE SOURCE
!+         FILE IN TELEMAC-2D AND 3D.
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| DISCE          |-->| ARRAY OF DISCHARGES OF SOURCES.
!|                |   | READ IN THE PARAMETER FILE.
!|                |   | NAME OF DISCE IS QSCE IN TELEMAC-3D.
!| I              |-->| NUMBER OF THE SOURCE
!| TIME           |-->| TIME
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_SPECIAL
      USE DECLARATIONS_TELEMAC3D
!
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      DOUBLE PRECISION, INTENT(INOUT) :: T3D_DEBSCE
      DOUBLE PRECISION, INTENT(IN) :: TIME,DISCE(*)
      INTEGER         , INTENT(IN) :: I
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
!
!-----------------------------------------------------------------------
!
!
!-----------------------------------------------------------------------
!
      RETURN
      END
