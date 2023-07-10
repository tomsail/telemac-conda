!                       *****************************
                        LOGICAL FUNCTION CVSP_DB_GAIA
!                       *****************************
!
     &(J_GLOBAL, TIMESTAMP)
!
!***********************************************************************
! GAIA   V8P1                                   16/05/2017
!***********************************************************************
!
!>@brief    Checks if a certain Point should be debugged and printed for this timestep
!!        Routine for USER DEFINED SUPERVISION OF POINTS
!
!>@history  UWE MERKEL, R. KOPMANN (BAW)
!!        2016, 2017
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!>@param[in] J_GLOBAL Point id
!>@param[in] TIMESTAMP Timestep id
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
!     USE DECLARATIONS_GAIA, ONLY: MESH, CVSM_OUT, LT, ENTET
!
      IMPLICIT NONE
!
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)    :: J_GLOBAL
      INTEGER, INTENT(IN)    :: TIMESTAMP
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
!
      CVSP_DB_GAIA = .FALSE.

      !CALL USER_CVSP_DB_GAIA(CVSP_DB,J_GLOBAL, TIMESTAMP)
!
      END FUNCTION CVSP_DB_GAIA
