!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!>@brief error handling functions for the api
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      MODULE API_HANDLE_ERROR

      IMPLICIT NONE
      !> name of error string length
      INTEGER, PARAMETER :: ERROR_LEN = 50
      !> error message string length
      INTEGER, PARAMETER :: ERROR_MESS_LEN = 250
      ! ERROR HANDLING FLAGS
      INTEGER, PARAMETER :: NO_ERROR=0
      INTEGER, PARAMETER :: UNALLOCATED_ARRAY_ERROR=1
      INTEGER, PARAMETER :: FILE_NOT_FOUND_ERROR=2
      INTEGER, PARAMETER :: CALL_POSITION_ERROR=3
      INTEGER, PARAMETER :: OVERTIME_ERROR=4
      INTEGER, PARAMETER :: MAX_INSTANCE_ERROR=5
      INTEGER, PARAMETER :: INVALID_INSTANCE_NUM_ERROR=6
      INTEGER, PARAMETER :: UNUSED_INSTANCE_ERROR=7
      INTEGER, PARAMETER :: UNKNOWN_VAR_ERROR=8
      INTEGER, PARAMETER :: INCREASE_NB_VAR_T2D_ERROR=9
      INTEGER, PARAMETER :: INCREASE_NB_VAR_T3D_ERROR=10
      INTEGER, PARAMETER :: INCREASE_NB_VAR_ART_ERROR=11
      INTEGER, PARAMETER :: INCREASE_NB_VAR_WAC_ERROR=12
      INTEGER, PARAMETER :: INCREASE_NB_VAR_SIS_ERROR=13
      INTEGER, PARAMETER :: INDEX_BLOCK_MISSING=14
      INTEGER, PARAMETER :: UNKNOWN_MODULE=15
!
      ! POSITION FOR A CALL FUNCTION
      INTEGER, PARAMETER :: NO_POSITION=-1
      INTEGER, PARAMETER :: RUN_SET_CONFIG_POS=0
      INTEGER, PARAMETER :: RUN_READ_CASE_POS=1
      INTEGER, PARAMETER :: RUN_ALLOCATION_POS=2
      INTEGER, PARAMETER :: RUN_INIT_POS=3
      INTEGER, PARAMETER :: RUN_TIMESTEP_POS=4
      INTEGER, PARAMETER :: RUN_FINALIZE_POS=5
      !> List of positions names
      CHARACTER(LEN=32) :: POS_NAME(6)
      PARAMETER ( POS_NAME = (/
     &            'RUN_SET_CONFIG                  ',
     &            'RUN_READ_CASE                   ',
     &            'RUN_ALLOCATION                  ',
     &            'RUN_INIT                        ',
     &            'RUN_TIMESTEP                    ',
     &            'RUN_FINALIZE                    ' /) )
      !> Error message
      CHARACTER(LEN=ERROR_MESS_LEN) :: ERR_MESS
!
      CONTAINS
        !
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !>@brief Check that the flag for postion call_position
      !!     is between 'before' and 'after'
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !>@param[in] CALL_POSITION Id of the current position
      !>@param[in] FNAME Name of the function
      !>@param[in] PREV_POS Id of the postion the function
      !!                         must be called after
      !>@param[in] NEXT_POS Id of the postion the function
      !!                         must be called before
      !>@param[out] IERR 0 if subroutine successfull,
      !!                         error id otherwise
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      SUBROUTINE CHECK_CALL_POSITION
     &     (CALL_POSITION,FNAME,PREV_POS,NEXT_POS,IERR)
        INTEGER :: CALL_POSITION
        CHARACTER(*), INTENT(IN) :: FNAME
        INTEGER, INTENT(IN) :: PREV_POS, NEXT_POS
        INTEGER, INTENT(OUT) :: IERR
!
        IERR = 0
!
        IF((NEXT_POS.NE.NO_POSITION).AND.
     &     (CALL_POSITION.GT.NEXT_POS)) THEN
          IERR = CALL_POSITION_ERROR
          ERR_MESS = "THE FUNCTION :'"//TRIM(FNAME)//
     &      "' MUST BE CALLED BEFORE "//POS_NAME(NEXT_POS+1)
        ENDIF
        IF((PREV_POS.NE.NO_POSITION).AND.
     &     (CALL_POSITION.LT.PREV_POS)) THEN
          IERR = CALL_POSITION_ERROR
          ERR_MESS = "THE FUNCTION :'"//TRIM(FNAME)//
     &      "' MUST BE CALLED AFTER "//POS_NAME(PREV_POS+1)
        ENDIF
      END SUBROUTINE CHECK_CALL_POSITION
!
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !>@brief Return the error message of the last error
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !>@param[in] ierr id of the error
      !>@param[out] message the error message
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      SUBROUTINE GET_ERROR_TYPE(IERR,MESSAGE)
        INTEGER, INTENT(IN) :: IERR
        CHARACTER(*), INTENT(OUT) :: MESSAGE
!
        IF(IERR.EQ.UNALLOCATED_ARRAY_ERROR) THEN
          MESSAGE = 'ERROR OF TYPE: UNALLOCATED_ARRAY_ERROR'
        ELSE IF(IERR.EQ.FILE_NOT_FOUND_ERROR) THEN
          MESSAGE = 'ERROR OF TYPE: FILE_NOT_FOUND_ERROR'
        ELSE IF(IERR.EQ.CALL_POSITION_ERROR) THEN
          MESSAGE = 'ERROR OF TYPE: CALL_POSITION_ERROR'
        ELSE IF(IERR.EQ.MAX_INSTANCE_ERROR) THEN
          MESSAGE = 'ERROR OF TYPE: MAX_INSTANCE_ERROR'
        ELSE IF(IERR.EQ.INVALID_INSTANCE_NUM_ERROR) THEN
          MESSAGE = 'ERROR OF TYPE: INVALID_INSTANCE_NUM_ERROR'
        ELSE IF(IERR.EQ.UNUSED_INSTANCE_ERROR) THEN
          MESSAGE = 'ERROR OF TYPE: UNUSED_INSTANCE_ERROR'
        ELSE IF(IERR.EQ.UNKNOWN_VAR_ERROR) THEN
          MESSAGE = 'ERROR OF TYPE: UNKNOWN_VAR_ERROR'
        ELSE IF(IERR.EQ.INDEX_BLOCK_MISSING) THEN
          MESSAGE = 'ERROR OF TYPE: INDEX_BLOCK_MISSING'
        ELSE IF(IERR.EQ.UNKNOWN_MODULE) THEN
          MESSAGE = 'MODULE GIVEN AS ARGUMENT IS UNKNOWN'
        ELSE
          MESSAGE = 'ERROR OF UNKNOWN TYPE'
        ENDIF
      END SUBROUTINE GET_ERROR_TYPE
!
      END MODULE API_HANDLE_ERROR
