!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!>@brief function to control tomawac execution
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      MODULE API_RUN_WAC
!
      USE API_HANDLE_ERROR
      USE API_HANDLE_VAR_WAC
      USE API_INSTANCE_WAC
      USE INTERFACE_TOMAWAC, ONLY : WAC, LECDON_TOMAWAC
      USE BIEF, ONLY: BIEF_OPEN_FILES, BIEF_INIT, INCLUS, DEALL_BIEF,
     &                IPID
      USE DECLARATIONS_PARALLEL, ONLY : COMM
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
      PRIVATE
      ! COMMON VALUES TO DEFINE OUTPUT + LANGUAGE
!
      INTEGER TDEB(8),TFIN(8),NCAR,NIT_ORI
      DOUBLE PRECISION :: DT_ORI
!
      CHARACTER(LEN=24), PARAMETER :: CODE='TOMAWAC                 '
      CHARACTER(LEN=250) FILE_DESC(4,MAXKEYWORD)
!
      CHARACTER(LEN=MAXLENTMPDIR) PATH
!
! List the public subroutines
!
      PUBLIC :: RUN_SET_CONFIG_WAC_D
      PUBLIC :: RUN_READ_CASE_WAC_D
      PUBLIC :: RUN_ALLOCATION_WAC_D
      PUBLIC :: RUN_INIT_WAC_D
      PUBLIC :: RUN_TIMESTEP_WAC_D
      PUBLIC :: RUN_FINALIZE_WAC_D
      SAVE
!
      CONTAINS
      !
      ! SET THE LU AND LNG VALUES
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !>@brief Initialise the instance and set the output
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !>@param[in,out] INST The instance
      !>@param[in] U_LU Output stream id
      !>@param[in] U_LNG Output kanguage 2 english 1 french
      !>@param[in] U_COMM The mpi communicator (-1 if none)
      !>@param[in] U_STD_OUTPUT If false listing in file
      !>@param[out] IERR 0 if subroutine successfull,
      !!                      error id otherwise
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      SUBROUTINE RUN_SET_CONFIG_WAC_D(INST, U_LU, U_LNG, U_COMM,
     &                                U_STD_OUTPUT, IERR)
        TYPE(INSTANCE_WAC),  INTENT(INOUT) :: INST
        INTEGER,             INTENT(IN) :: U_LU, U_LNG, U_COMM
        LOGICAL,             INTENT(IN) :: U_STD_OUTPUT
        INTEGER,             INTENT(OUT) :: IERR
!
        IERR = 0
!
        LU = U_LU
        LNG = U_LNG
        COMM = U_COMM
        STD_OUTPUT = U_STD_OUTPUT
!
      END SUBROUTINE RUN_SET_CONFIG_WAC_D
!
!!!!!!! FUNCTION HANDLING THE EXECUTION OF THE SIMULATION
!
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !>@brief Reads the case file
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !>@param[in,out] INST The instance
      !>@param[in] CAS_FILE Path to the case file
      !>@param[in] DICO_FILE Path to the dictionary file
      !>@param[in] INIT If true p_init is called
      !>@param[out] IERR 0 if subroutine successfull,
      !!                        error id otherwise
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      SUBROUTINE RUN_READ_CASE_WAC_D(INST,CAS_FILE, DICO_FILE,INIT,IERR)
!
        TYPE(INSTANCE_WAC), INTENT(INOUT) :: INST
        CHARACTER(LEN=250), INTENT(IN) :: CAS_FILE
        CHARACTER(LEN=250), INTENT(IN) :: DICO_FILE
        LOGICAL,            INTENT(IN) :: INIT
        INTEGER,            INTENT(OUT) :: IERR
!
        IERR = 0
!
        CALL BIEF_INIT(PATH,NCAR,INIT)
!
!     INITIAL TIME FOR COMPUTATION DURATION
!
        CALL DATE_AND_TIME(VALUES=TDEB)
!
!     PRINTS BANNER TO LISTING
!
      CALL PRINT_HEADER(CODE,'                        ')
!
!-----------------------------------------------------------------------
!
!     READS THE STEERING FILE
        CALL LECDON_TOMAWAC(FILE_DESC,
     &                        PATH,NCAR,CAS_FILE,DICO_FILE,PART=-1)
!
!-----------------------------------------------------------------------
!
!     OPENS THE FILES FOR TELEMAC2D
!
      END SUBROUTINE RUN_READ_CASE_WAC_D
!
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !>@brief Allocate all of telemac2d variables
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !>@param[in,out] INST The instance
      !>@param[out] IERR 0 if subroutine successfull,
      !!                        error id otherwise
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      SUBROUTINE RUN_ALLOCATION_WAC_D(INST,IERR)
        TYPE(INSTANCE_WAC), INTENT(INOUT) :: INST
        INTEGER,            INTENT(OUT) :: IERR
!
        INTEGER :: IFLOT
!
        IERR = 0
!
        IFLOT = 0

        CALL BIEF_OPEN_FILES(CODE,INST%WAC_FILES,
     &                       INST%MAXLU_WAC,
     &                       PATH,NCAR,
     &                       1,.TRUE.)
!
!-----------------------------------------------------------------------
!
!     ALLOCATES MEMORY
!
        CALL POINT_TOMAWAC
!
!-----------------------------------------------------------------------
!
!
      END SUBROUTINE RUN_ALLOCATION_WAC_D
!
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !>@brief Initialise the telemac2d variables
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !>@param[in,out] INST The instance
      !>@param[out] IERR 0 if subroutine successfull,
      !!                        error id otherwise
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      SUBROUTINE RUN_INIT_WAC_D(INST,IERR)
!
        TYPE(INSTANCE_WAC), INTENT(INOUT) :: INST
        INTEGER,            INTENT(OUT) :: IERR
!
        IERR = 0
!
        ! RUN THE INITIAL TIME STEP
        NIT_ORI = INST%NIT
        DT_ORI = INST%DT
        CALL WAC(PART=2,
     &           NIT_ORI=NIT_ORI)
!
      END SUBROUTINE RUN_INIT_WAC_D
!
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !>@brief Run a timestep in telemac2d
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !>@param[in,out] INST The instance
      !>@param[out] IERR 0 if subroutine successfull,
      !!                        error id otherwise
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      SUBROUTINE RUN_TIMESTEP_WAC_D(INST,IERR)
!
        TYPE(INSTANCE_WAC), INTENT(INOUT) :: INST
        INTEGER,            INTENT(OUT) :: IERR
!
        IERR = 0
!
        INST%NIT = 1
        CALL WAC(PART=3,
     &       NIT_ORI=NIT_ORI)
      END SUBROUTINE RUN_TIMESTEP_WAC_D
!
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !>@brief Finalize a telemac2d run
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !>@param[in,out] INST The instance
      !>@param[out] IERR 0 if subroutine successfull,
      !!                        error id otherwise
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      SUBROUTINE RUN_FINALIZE_WAC_D(INST,IERR)
!
        TYPE(INSTANCE_WAC), INTENT(INOUT) :: INST
        INTEGER,            INTENT(OUT) :: IERR
!
        IERR = 0
!
        CALL BIEF_CLOSE_FILES(INST%WAC_FILES,
     &                        INST%MAXLU_WAC,.FALSE.)
!
!       DEALLOCATE ALL OF BIEF AND TELEMAC2D ARRAYS
        CALL DEALL_TOMAWAC()
        CALL DEALL_BIEF()
!
        WRITE(LU,11)
11      FORMAT(1X,///,1X,'CORRECT END OF RUN',///)
!
!     TIME OF END OF COMPUTATION
!
        CALL DATE_AND_TIME(VALUES=TFIN)
        CALL ELAPSE(TDEB,TFIN)
        ! Closing log files
        IF(IPID.NE.0) THEN
          CLOSE(LU)
        ELSE
          IF(.NOT.STD_OUTPUT) CLOSE(LU)
        ENDIF
!
      END SUBROUTINE RUN_FINALIZE_WAC_D
!
      END MODULE API_RUN_WAC
