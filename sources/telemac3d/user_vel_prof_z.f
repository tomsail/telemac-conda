!                   **************************
                    SUBROUTINE USER_VEL_PROF_Z
!                   **************************
!
     &( VEL_PROF_Z, I , IPOIN2 , IPLAN , IOPT)
!
!***********************************************************************
! TELEMAC3D
!***********************************************************************
!
!brief    GIVES THE VELOCITY VERTICAL PROFILE AT ENTRANCES.
!+
!+            THIS PROFILE IS LOGARITHMIC AND DESIGNED SO THAT THE
!+                INTEGRAL ON THE VERTICAL EQUALS THE DEPTH.
!
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| I              |-->| NUMBER OF THE LIQUID BOUNDARY
!| IOPT           |-->| OPTION FOR VELOCITY PROFILE
!| IPLAN          |-->| PLAN NUMBER
!| IPOIN2         |-->| 2D GLOBAL NUMBER OF POINT CONSIDERED
!| ITRAC          |-->| TRACER NUMBER
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_TELEMAC
      USE DECLARATIONS_TELEMAC3D
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      DOUBLE PRECISION, INTENT(INOUT) :: VEL_PROF_Z
      INTEGER          , INTENT(IN) :: I,IPOIN2,IPLAN,IOPT
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
!
!-----------------------------------------------------------------------
!
!
      WRITE(LU,*) 'USER DEFINE PROFILE MISSING IN USER_VEL_PROF_Z'
      CALL PLANTE(1)
      STOP
!
!-----------------------------------------------------------------------
!
      RETURN
      END

