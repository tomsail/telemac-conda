!                       ***********************
                        SUBROUTINE USER_CVSP_DB
!                       ***********************
!
     &(CVSP_DB,J_GLOBAL, TIMESTAMP)
!
!***********************************************************************
! SISYPHE
!***********************************************************************
!
!brief    Checks if a certain Point should be debugged and printed for this timestep
!+        Routine for USER DEFINED SUPERVISION OF POINTS
!+
!
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| CVSP_DB               |-->| DEBUG INFO
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
      LOGICAL, INTENT(INOUT) :: CVSP_DB
!
!-----------------------------------------------------------------------
!
      INTEGER J
!
!-----------------------------------------------------------------------
!

      J = J_GLOBAL

!      IF (J.EQ.175712) CVSP_DB = .TRUE.

!USE THIS TO LIMIT OUTPUT TO CAS FILE DEFINED TIMESTEPS
!      IF(CVSM_OUT) THEN
!         IF ( (J.QE.502) ) CVSP_DB = .TRUE.
!      ENDIF
!
!USE THIS TO SET AN OUTPUT FOR A CERTAIN TIMESTEP,
!INDEPENDENT OF POINT NUMBER AND CAS FILE
!      IF ( (J.EQ.-1).AND.(T.GE.-1)) CVSP_DB = .TRUE.
!
! USE THIS TO SET AN OUTPUT FOR A CERTAIN NODE, WITH TIMESTEP RULES
!      IF( (J.EQ.948).AND.(AT.GE.0.D0)) CVSP_DB = .TRUE.
!
! USE THIS TO SET AN OUTPUT FOR A CERTAIN NODE, AT A SPECIFIC TIME STEP
!      IF( (J.EQ.27).AND.(LT.GE.TimeStamp)) CVSP_DB = .TRUE.
!
!-----------------------------------------------------------------------
!
      RETURN
      END SUBROUTINE
