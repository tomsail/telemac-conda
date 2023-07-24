!                       ************************
                        LOGICAL FUNCTION CVSP_DB
!                       ************************
!
     &(J_GLOBAL, TIMESTAMP)
!
!***********************************************************************
! SISYPHE   V7P2                                   16/05/2017
!***********************************************************************
!
!brief    Checks if a certain Point should be debugged and printed for this timestep
!+        Routine for USER DEFINED SUPERVISION OF POINTS
!+
!
!history  UWE MERKEL, R. KOPMANN (BAW)
!+        2016, 2017
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| J_GLOBAL              |<--| POINT ID
!| TIMESTAMP             |<--| TIMESTEP ID
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
!     USE DECLARATIONS_SISYPHE, ONLY: MESH, CVSM_OUT, LT, ENTET
!
      IMPLICIT NONE
!
      INTEGER, INTENT(IN)    :: J_GLOBAL
      INTEGER, INTENT(IN)    :: TIMESTAMP
!
      CVSP_DB = .FALSE.

      CALL USER_CVSP_DB(CVSP_DB,J_GLOBAL, TIMESTAMP)
!
      END FUNCTION CVSP_DB
