!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!>@brief $function to control telemac2d execution
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      MODULE API_RUN_T2D
!
      USE API_HANDLE_ERROR
      USE API_HANDLE_VAR_T2D
      USE API_INSTANCE_T2D
      USE INTERFACE_TELEMAC2D, ONLY : TELEMAC2D, LECDON_TELEMAC2D,
     &                                DEALL_TELEMAC2D
      USE BIEF, ONLY : BIEF_OPEN_FILES, BIEF_INIT, INCLUS, DEALL_BIEF,
     &                 IPID
      USE DECLARATIONS_PARALLEL, ONLY : COMM
      USE BIEF_DEF, ONLY : IPID
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
      PRIVATE
      ! COMMON VALUES TO DEFINE OUTPUT + LANGUAGE
!
      INTEGER TDEB(8),TFIN(8),NCAR,NIT_ORI
      DOUBLE PRECISION :: DT_ORI
!
      CHARACTER(LEN=24), PARAMETER :: CODE1='TELEMAC2D               '
      CHARACTER(LEN=24), PARAMETER :: CODE2='SISYPHE                 '
      CHARACTER(LEN=24), PARAMETER :: CODE3='TOMAWAC                 '
      CHARACTER(LEN=24), PARAMETER :: CODE6='GAIA                    '
!
      CHARACTER(LEN=MAXLENTMPDIR) PATH
      LOGICAL CPL_GAIA

!
! List the public subroutines
!
      PUBLIC :: RUN_SET_CONFIG_T2D_D
      PUBLIC :: RUN_READ_CASE_T2D_D
      PUBLIC :: RUN_ALLOCATION_T2D_D
      PUBLIC :: RUN_INIT_T2D_D
      PUBLIC :: RUN_TIMESTEP_COMPUTE_T2D_D
      PUBLIC :: RUN_TIMESTEP_RES_T2D_D
      PUBLIC :: RUN_FINALIZE_T2D_D
      PUBLIC :: RUN_WRITE_T2D_D
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
      SUBROUTINE RUN_SET_CONFIG_T2D_D(INST, U_LU, U_LNG, U_COMM,
     &                                U_STD_OUTPUT, IERR)
        TYPE(INSTANCE_T2D),  INTENT(INOUT) :: INST
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
      END SUBROUTINE RUN_SET_CONFIG_T2D_D
!
!!!!!!! FUNCTION HANDLING THE EXECUTION OF THE SIMULATION
!
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !>@brief Reads the case file
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !>@param[in,out] INST The instance
      !>@param[in] CAS_FILE Path to the case file
      !>@param[in] DICO_FILE Path to the dictionary file
      !>@param[in] INIT If true p_init is called
      !>@param[out] IERR 0 if subroutine successfull,
      !!                        error id otherwise
      !>@param[in] GAIA_CAS Path to the gaia case file
      !>@param[in] GAIA_DICO Path to the gaia dictionary file
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      SUBROUTINE RUN_READ_CASE_T2D_D(INST,CAS_FILE, DICO_FILE,INIT,
     &                               IERR,GAIA_CAS,GAIA_DICO)
!
        TYPE(INSTANCE_T2D), INTENT(INOUT) :: INST
        CHARACTER(LEN=250), INTENT(IN) :: CAS_FILE
        CHARACTER(LEN=250), INTENT(IN) :: DICO_FILE
        LOGICAL,            INTENT(IN) :: INIT
        INTEGER,            INTENT(OUT) :: IERR
        CHARACTER(LEN=250), INTENT(IN) :: GAIA_CAS
        CHARACTER(LEN=250), INTENT(IN) :: GAIA_DICO
!
        CHARACTER(LEN=250) MOTCAR(INST%MAXKEYWORD)
        CHARACTER(LEN=250) FILE_DESC(4,INST%MAXKEYWORD)
!
        IERR = 0
!
        CALL BIEF_INIT(PATH,NCAR,INIT)
!
!       INITIAL TIME FOR COMPUTATION DURATION
!
        CALL DATE_AND_TIME(VALUES=TDEB)
!
!       PRINTS BANNER TO LISTING
!
        CALL PRINT_HEADER(CODE1,'                        ')
!
!-----------------------------------------------------------------------
!
!       READS THE STEERING FILE
        CALL LECDON_TELEMAC2D(MOTCAR,FILE_DESC,
     &                        PATH,NCAR,CAS_FILE,DICO_FILE,
     &                        CAS_FILE_GAIA=GAIA_CAS,
     &                        DICO_FILE_GAIA=GAIA_DICO)
!
!-----------------------------------------------------------------------
!
        CPL_GAIA = INCLUS(INST%COUPLING, 'GAIA')

        IF(CPL_GAIA) THEN
!
          CALL PRINT_HEADER(CODE6,CODE1)
!
          CALL LECDON_GAIA(MOTCAR,FILE_DESC,PATH,NCAR,CODE1,
     &                     GAIA_CAS,GAIA_DICO)
        ENDIF
      END SUBROUTINE RUN_READ_CASE_T2D_D
!
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !>@brief Allocate all of telemac2d variables
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !>@param[in,out] INST The instance
      !>@param[out] IERR 0 if subroutine successfull,
      !!                        error id otherwise
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      SUBROUTINE RUN_ALLOCATION_T2D_D(INST,IERR)
        TYPE(INSTANCE_T2D), INTENT(INOUT) :: INST
        INTEGER,            INTENT(OUT) :: IERR
!
        IERR = 0
!
        CALL BIEF_OPEN_FILES(CODE1,INST%T2D_FILES,
     &                       INST%MAXLU_T2D,
     &                       PATH,NCAR,
     &                       1,.TRUE.)
!
!-----------------------------------------------------------------------
!
!       ALLOCATES MEMORY
!
        CALL POINT_TELEMAC2D
!
!-----------------------------------------------------------------------
!
        IF(CPL_GAIA) THEN
          CALL BIEF_OPEN_FILES(CODE6,INST%GAI_FILES,
     &                         INST%MAXLU_GAI,
     &                         PATH,NCAR,
     &                         2,.TRUE.)
!
!         RESETS TELEMAC2D CONFIGURATION
!
          CALL CONFIG_CODE(1)
!
!         MEMORY ORGANISATION
!
          CALL POINT_GAIA
!
        ENDIF
!
      END SUBROUTINE RUN_ALLOCATION_T2D_D
!
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !>@brief Initialise the telemac2d variables
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !>@param[in,out] INST The instance
      !>@param[out] IERR 0 if subroutine successfull,
      !!                        error id otherwise
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      SUBROUTINE RUN_INIT_T2D_D(INST,IERR)
!
        TYPE(INSTANCE_T2D), INTENT(INOUT) :: INST
        INTEGER,            INTENT(OUT) :: IERR
!
        IERR = 0
!
        ! RUN THE INITIAL TIME STEP
        NIT_ORI = INST%NIT
        DT_ORI = INST%DT
        CALL TELEMAC2D(PASS=0,ATDEP=0.D0,NITER=0,CODE='       ',
     &                  NITERORI=NIT_ORI)
!
      END SUBROUTINE RUN_INIT_T2D_D
!
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !>@brief Run a timestep in telemac2d no writting results
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !>@param[in,out] INST The instance
      !>@param[out] IERR 0 if subroutine successfull,
      !!                        error id otherwise
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      SUBROUTINE RUN_TIMESTEP_COMPUTE_T2D_D(INST,IERR)
!
        TYPE(INSTANCE_T2D), INTENT(INOUT) :: INST
        INTEGER,            INTENT(OUT) :: IERR
!
        IERR = 0
!
        INST%NIT = INST%LT
        CALL TELEMAC2D(PASS=1,ATDEP=0.D0,NITER=INST%LT,
     &                 CODE='       ',NITERORI=NIT_ORI)
      END SUBROUTINE RUN_TIMESTEP_COMPUTE_T2D_D
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !>@brief Run a timestep in telemac2d only writting results
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !>@param[in,out] INST The instance
      !>@param[out] IERR 0 if subroutine successfull,
      !!                        error id otherwise
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      SUBROUTINE RUN_TIMESTEP_RES_T2D_D(INST,IERR)
!
        TYPE(INSTANCE_T2D), INTENT(INOUT) :: INST
        INTEGER,            INTENT(OUT) :: IERR
!
        IERR = 0
!
        CALL TELEMAC2D(PASS=2,ATDEP=0.D0,NITER=INST%LT,
     &                 CODE='       ',NITERORI=NIT_ORI)
      END SUBROUTINE RUN_TIMESTEP_RES_T2D_D
!
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !>@brief Write output in telemac2d
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !>@param[in,out] INST The instance
      !>@param[out] IERR 0 if subroutine successfull,
      !!                        error id otherwise
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      SUBROUTINE RUN_WRITE_T2D_D(INST,IERR)
!
        TYPE(INSTANCE_T2D), INTENT(INOUT) :: INST
        INTEGER,            INTENT(OUT) :: IERR
!
        IERR = 0
!
        INST%NIT = INST%LT
        CALL TELEMAC2D(PASS=2,ATDEP=0.D0,NITER=INST%LT,
     &       CODE='       ',NITERORI=NIT_ORI)
      END SUBROUTINE RUN_WRITE_T2D_D
!
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !>@brief Finalize a telemac2d run
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !>@param[in,out] INST The instance
      !>@param[out] IERR 0 if subroutine successfull,
      !!                        error id otherwise
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      SUBROUTINE RUN_FINALIZE_T2D_D(INST,IERR)
!
        TYPE(INSTANCE_T2D), INTENT(INOUT) :: INST
        INTEGER,            INTENT(OUT) :: IERR
!
        IERR = 0
!
        CALL BIEF_CLOSE_FILES(INST%T2D_FILES,
     &                        INST%MAXLU_T2D,.FALSE.)
!
        IF(CPL_GAIA) THEN
          CALL CONFIG_CODE(6)
          CALL BIEF_CLOSE_FILES(INST%GAI_FILES,INST%MAXLU_GAI,.FALSE.)
          CALL DEALL_GAIA
        ENDIF

!       DEALLOCATE ALL OF BIEF AND TELEMAC2D ARRAYS
        CALL DEALL_TELEMAC2D(.TRUE.)
        CALL DEALL_BIEF()
!
        WRITE(LU,11)
11      FORMAT(1X,///,1X,'CORRECT END OF RUN',///)
!
!       TIME OF END OF COMPUTATION
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
      END SUBROUTINE RUN_FINALIZE_T2D_D
!
      END MODULE API_RUN_T2D
