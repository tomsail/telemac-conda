      PROGRAM HOMERE_API
      USE API_INTERFACE, EX_LU => LU
      USE API_INSTANCE_SIS
      USE API_INSTANCE_T2D
      USE API_INSTANCE_T3D
      USE API_INSTANCE_ART
      USE API_INSTANCE_WAC
      USE DECLARATIONS_PARTEL, ONLY : CODE
      USE DECLARATIONS_SPECIAL, ONLY : PATH_LEN
!
#if defined HAVE_MPI
!
#  if defined HAVE_MPI_MOD
        USE MPI
        IMPLICIT NONE
#  else
        IMPLICIT NONE
        INCLUDE 'mpif.h'
#  endif
#endif
        INTEGER ::  I, IERR, ID
        INTEGER :: NTIME_STEPS
        INTEGER :: LNG, LU
        CHARACTER(LEN=PATH_LEN) :: GEO_FILE
        CHARACTER(LEN=PATH_LEN) :: CLI_FILE
        CHARACTER(LEN=PATH_LEN) :: RES_FILE
        CHARACTER(LEN=PATH_LEN) :: DUMMY
        CHARACTER(LEN=PATH_LEN) :: CAS_FILE, DICO_FILE
        CHARACTER(LEN=PATH_LEN) :: GAI_CAS_FILE, GAI_DICO_FILE
        CHARACTER(LEN=8) :: FFORMAT = 'SERAFIN '
        CHARACTER(LEN=9) :: TEL_MODULE
        CHARACTER(LEN=T2D_VAR_LEN) :: VARNAME
        CHARACTER(LEN=250) :: DUMMY_MESSAGE
        INTEGER NPLAN
        INTEGER PARALLEL
        INTEGER RANK,NCSIZE,PMETHOD,COMM
        COMM = 0
        DUMMY = REPEAT(' ',PATH_LEN)
        GEO_FILE = REPEAT(' ',PATH_LEN)
        CLI_FILE = REPEAT(' ',PATH_LEN)
        RES_FILE = REPEAT(' ',PATH_LEN)
        NPLAN = 0

!     OUTPUT FOR WRITING
        LU = 6
        LNG = 1
        ID = 0
        !READING THE FILES NAMES
        OPEN(UNIT=12,FILE='param_api')
        READ(12,*) TEL_MODULE
        READ(12,'(A)') DICO_FILE
        READ(12,*) CAS_FILE
        READ(12,'(A)') GAI_DICO_FILE
        READ(12,*) GAI_CAS_FILE
        READ(12,*) GEO_FILE
        READ(12,*) CLI_FILE
        READ(12,*) PARALLEL
        CLOSE(12)

        PRINT*, "DICO: ",DICO_FILE
        PRINT*, "CAS : ",CAS_FILE
        PRINT*, "GEO : ",GEO_FILE
        PRINT*, "CLI : ",CLI_FILE
        IF(PARALLEL.LE.1) PRINT*,"MPI : SEQUENTIAL RUN"
        IF(PARALLEL.GT.1) PRINT*,"MPI : PARALLEL RUN"
        PRINT*
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
#if defined HAVE_MPI
        IF(PARALLEL.GE.1) THEN
          ! Partitioning method to use 1: metis
          PMETHOD=1
          ! Initialising mpi
          CALL MPI_INIT(IERR)
          ! Getting rank
          CALL MPI_COMM_RANK(MPI_COMM_WORLD,RANK,IERR)
          ! Getting the number of process
          CALL MPI_COMM_SIZE(MPI_COMM_WORLD,NCSIZE,IERR)
          COMM = MPI_COMM_WORLD
          IF(NCSIZE.NE.PARALLEL) THEN
            WRITE(LU,*) 'Error in number of process '
            WRITE(LU,*) 'In file:',PARALLEL
            WRITE(LU,*) 'From MPI:',NCSIZE
            CALL PLANTE(1)
            STOP
          ENDIF
          CALL MPI_BARRIER(COMM,IERR)
          IF(PARALLEL.GT.1) THEN
            CALL GET_SHORT
            CALL SPLIT_GEO
          ENDIF
        ENDIF
#else
        RANK = -1
        NCSIZE = 0
#endif
!

        IF(TEL_MODULE(1:9).EQ.'TELEMAC2D') THEN
          CALL RUN_T2D
          CALL RUN_T2D
        ELSE IF (TEL_MODULE(1:9).EQ.'TELEMAC3D') THEN
          CALL RUN_T3D
          CALL RUN_T3D
        ELSE IF (TEL_MODULE(1:7).EQ.'SISYPHE') THEN
          CALL RUN_SIS
        ELSE IF (TEL_MODULE(1:7).EQ.'ARTEMIS') THEN
          CALL RUN_ART
        ELSE IF (TEL_MODULE(1:7).EQ.'TOMAWAC') THEN
          CALL RUN_WAC
        ELSE
          WRITE(LU,*) "UNKBOWN MODULE", TEL_MODULE
          CALL PLANTE(1)
        ENDIF

#if defined HAVE_MPI
        IF(PARALLEL.GE.1) THEN
          CALL MPI_BARRIER(COMM,IERR)
          IF(PARALLEL.GT.1) THEN
            CALL MERGE_RES
          ENDIF
          CALL MPI_FINALIZE(IERR)
        ENDIF
#endif
        ! Boggus call to compile some functions
        IF(1.EQ.0) THEN
          CALL TRANSFER_GROUP_INFO(GEO_FILE,I,I,I,I,(/1/),1,1,
     &                             .FALSE.,.FALSE.,I)
          CALL GET_DATA_VAR_LIST2(GEO_FILE,I,1,(/''/),(/''/),I)
          CALL EXAMPLE_CPL_T2D_SIS()
          CALL GET_ERROR_MESSAGE(1,CODE, I,DUMMY_MESSAGE)
          CALL GET_ENDIANESS(FFORMAT,I,DUMMY_MESSAGE(1:13),I)
          CALL SET_ENDIANESS(FFORMAT,I,DUMMY_MESSAGE(1:13),I)
          CALL GET_FILE_FORMAT(FFORMAT,I,FFORMAT,I)
        ENDIF
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        CONTAINS
          ! Get the short name associated with the module
          SUBROUTINE GET_SHORT()
            IF(TEL_MODULE(1:9).EQ.'TELEMAC2D') THEN
              CODE = 'T2D'
            ELSE IF (TEL_MODULE(1:9).EQ.'TELEMAC3D') THEN
              CODE = 'T3D'
            ELSE IF (TEL_MODULE(1:7).EQ.'SISYPHE') THEN
              CODE = 'SIS'
            ELSE IF (TEL_MODULE(1:7).EQ.'ARTEMIS') THEN
              CODE = 'ART'
            ELSE IF (TEL_MODULE(1:7).EQ.'TOMAWAC') THEN
              CODE = 'WAC'
            ENDIF
          END SUBROUTINE

          ! Splitting geometry file
          SUBROUTINE SPLIT_GEO
            ! The partitioning is done sequentially
            IF(RANK.EQ.0) THEN
              ! PARITIONING THE GEOMETRY FILE
              CALL PARTEL(GEO_FILE,CLI_FILE,NCSIZE,PMETHOD,
     &                     FFORMAT,DUMMY,DUMMY,DUMMY)

              ! Additional serafin/med files
!              CALL PARRES(GEO_FILE,PRE_FILE,
!    &                     NCSIZE,'SERAFIN ','SERAFIN ')
            ENDIF
          END SUBROUTINE

          ! Merging result file
          SUBROUTINE MERGE_RES
            ! Mergin step
            IF(RANK.EQ.0) THEN
              CALL GRETEL_AUTOP(GEO_FILE,FFORMAT,CLI_FILE,RES_FILE,
     &                          FFORMAT, NCSIZE, NPLAN)
            ENDIF
          END SUBROUTINE

          ! Run a telemac2d job
          SUBROUTINE RUN_T2D
            CALL RUN_SET_CONFIG_T2D(ID,LU,LNG,COMM,IERR)
            CALL CHECK_CALL(IERR,'RUN_SET_CONFIG_T2D')

            CALL RUN_READ_CASE_T2D(ID,CAS_FILE,DICO_FILE,.TRUE.,
     &                             IERR,GAI_CAS_FILE,GAI_DICO_FILE)
            CALL CHECK_CALL(IERR,'RUN_READ_CASE_T2D')
            ! Get the name of the result file
            VARNAME = 'MODEL.RESULTFILE'
            CALL GET_STRING(ID, 'T2D', VARNAME, RES_FILE,
     &                           PATH_LEN, 0, 0, IERR)
            CALL CHECK_CALL(IERR,'GET_STRING_T2D')

            CALL RUN_ALLOCATION_T2D(ID,IERR)
            CALL CHECK_CALL(IERR,'RUN_ALLOCATION_T2D')

            CALL RUN_INIT_T2D(ID,IERR)
            CALL CHECK_CALL(IERR,'RUN_INIT_T2D')

            !Get the number of timesteps
            VARNAME = 'MODEL.NTIMESTEPS'
            CALL GET_INTEGER(ID, 'T2D', VARNAME, NTIME_STEPS,
     &                           0, 0, 0, IERR)
            CALL CHECK_CALL(IERR,'GET_INTEGER_T2D')
            ! Time loop
            DO I=1,NTIME_STEPS
              CALL RUN_TIMESTEP_T2D(ID,IERR)
              CALL CHECK_CALL(IERR,'RUN_TIMESTEP_T2D')
            ENDDO

            CALL RUN_FINALIZE_T2D(ID,IERR)
            CALL CHECK_CALL(IERR,'RUN_FINALIZE_T2D')
          END SUBROUTINE

          SUBROUTINE RUN_T3D
            CALL RUN_SET_CONFIG_T3D(ID,LU,LNG,COMM,IERR)
            CALL CHECK_CALL(IERR,'RUN_SET_CONFIG_T3D')

            CALL RUN_READ_CASE_T3D(ID,CAS_FILE,DICO_FILE,.TRUE.,IERR,
     &                  DUMMY,DUMMY,DUMMY,DUMMY)
            CALL CHECK_CALL(IERR,'RUN_READ_CASE_T3D')
            ! Get the name of the result file
            VARNAME = 'MODEL.RESULTFILE'
            CALL GET_STRING(ID, 'T3D', VARNAME, RES_FILE,
     &                           PATH_LEN, 0, 0, IERR)
            CALL CHECK_CALL(IERR,'GET_STRING_T3D')

            CALL RUN_ALLOCATION_T3D(ID,IERR)
            CALL CHECK_CALL(IERR,'RUN_ALLOCATION_T3D')

            CALL RUN_INIT_T3D(ID,IERR)
            CALL CHECK_CALL(IERR,'RUN_INIT_T3D')

            !Get the number of timesteps
            VARNAME = 'MODEL.NTIMESTEPS'
            CALL GET_INTEGER(ID, 'T3D', VARNAME, NTIME_STEPS,
     &                           0, 0, 0, IERR)
            CALL CHECK_CALL(IERR,'GET_INTEGER_T3D')
            ! Time loop
            DO I=1,NTIME_STEPS
              CALL RUN_TIMESTEP_T3D(ID,IERR)
              CALL CHECK_CALL(IERR,'RUN_TIMESTEP_T3D')
            ENDDO

            CALL RUN_FINALIZE_T3D(ID,IERR)
            CALL CHECK_CALL(IERR,'RUN_FINALIZE_T3D')
          END SUBROUTINE

          SUBROUTINE RUN_SIS
            CALL RUN_SET_CONFIG_SIS(ID,LU,LNG,COMM,IERR)
            CALL CHECK_CALL(IERR,'RUN_SET_CONFIG_SIS')

            CALL RUN_READ_CASE_SIS(ID,'SISYPHE                 ',
     &                 CAS_FILE,DICO_FILE,.TRUE.,IERR)
            CALL CHECK_CALL(IERR,'RUN_READ_CASE_SIS')
            ! Get the name of the result file
            VARNAME = 'MODEL.RESULTFILE'
            CALL GET_STRING(ID, 'SIS', VARNAME, RES_FILE,
     &                           PATH_LEN, 0, 0, IERR)
            CALL CHECK_CALL(IERR,'GET_STRING_SIS')

            CALL RUN_ALLOCATION_SIS(ID,IERR)
            CALL CHECK_CALL(IERR,'RUN_ALLOCATION_SIS')

            CALL RUN_INIT_SIS(ID,IERR)
            CALL CHECK_CALL(IERR,'RUN_INIT_SIS')

            !Get the number of timesteps
            VARNAME = 'MODEL.NTIMESTEPS'
            CALL GET_INTEGER(ID, 'SIS', VARNAME, NTIME_STEPS,
     &                           0, 0, 0, IERR)
            CALL CHECK_CALL(IERR,'GET_INTEGER_SIS')
            ! Time loop
            DO I=1,NTIME_STEPS
              CALL RUN_TIMESTEP_SIS(ID,IERR)
              CALL CHECK_CALL(IERR,'RUN_TIMESTEP_SIS')
            ENDDO

            CALL RUN_FINALIZE_SIS(ID,IERR)
            CALL CHECK_CALL(IERR,'RUN_FINALIZE_SIS')
          END SUBROUTINE

          SUBROUTINE RUN_ART
            CALL RUN_SET_CONFIG_ART(ID,LU,LNG,COMM,IERR)
            CALL CHECK_CALL(IERR,'RUN_SET_CONFIG_ART')

            CALL RUN_READ_CASE_ART(ID,CAS_FILE,DICO_FILE,.TRUE.,IERR)
            CALL CHECK_CALL(IERR,'RUN_READ_CASE_ART')
            ! Get the name of the result file
            VARNAME = 'MODEL.RESULTFILE'
            CALL GET_STRING(ID, 'ART', VARNAME, RES_FILE,
     &                           PATH_LEN, 0, 0, IERR)
            CALL CHECK_CALL(IERR,'GET_STRING_ART')

            CALL RUN_ALLOCATION_ART(ID,IERR)
            CALL CHECK_CALL(IERR,'RUN_ALLOCATION_ART')

            CALL RUN_INIT_ART(ID,IERR)
            CALL CHECK_CALL(IERR,'RUN_INIT_ART')

            !Get the number of timesteps
            VARNAME = 'MODEL.NTIMESTEPS'
            CALL GET_INTEGER(ID, 'ART', VARNAME, NTIME_STEPS,
     &                           0, 0, 0, IERR)
            CALL CHECK_CALL(IERR,'GET_INTEGER_ART')
            ! Time loop
            DO I=1,NTIME_STEPS
              CALL RUN_TIMESTEP_ART(ID,IERR)
              CALL CHECK_CALL(IERR,'RUN_TIMESTEP_ART')
            ENDDO

            CALL RUN_FINALIZE_ART(ID,IERR)
            CALL CHECK_CALL(IERR,'RUN_FINALIZE_ART')
          END SUBROUTINE

          SUBROUTINE RUN_WAC
            CALL RUN_SET_CONFIG_WAC(ID,LU,LNG,COMM,IERR)
            CALL CHECK_CALL(IERR,'RUN_SET_CONFIG_WAC')

            CALL RUN_READ_CASE_WAC(ID,CAS_FILE,DICO_FILE,.TRUE.,IERR)
            CALL CHECK_CALL(IERR,'RUN_READ_CASE_WAC')
            ! Get the name of the result file
            VARNAME = 'MODEL.RESULTFILE'
            CALL GET_STRING(ID, 'WAC', VARNAME, RES_FILE,
     &                           PATH_LEN, 0, 0, IERR)
            CALL CHECK_CALL(IERR,'GET_STRING_WAC')

            CALL RUN_ALLOCATION_WAC(ID,IERR)
            CALL CHECK_CALL(IERR,'RUN_ALLOCATION_WAC')

            CALL RUN_INIT_WAC(ID,IERR)
            CALL CHECK_CALL(IERR,'RUN_INIT_WAC')

            !Get the number of timesteps
            VARNAME = 'MODEL.NTIMESTEPS'
            CALL GET_INTEGER(ID, 'WAC', VARNAME, NTIME_STEPS,
     &                           0, 0, 0, IERR)
            CALL CHECK_CALL(IERR,'GET_INTEGER_WAC')
            ! Time loop
            DO I=1,NTIME_STEPS
              CALL RUN_TIMESTEP_WAC(ID,IERR)
              CALL CHECK_CALL(IERR,'RUN_TIMESTEP_WAC')
            ENDDO

            CALL RUN_FINALIZE_WAC(ID,IERR)
            CALL CHECK_CALL(IERR,'RUN_FINALIZE_WAC')
          END SUBROUTINE
        END PROGRAM HOMERE_API
