      SUBROUTINE EXAMPLE_CPL_T2D_SIS()
      USE API_INTERFACE, EX_LU => LU
      USE API_INSTANCE_SIS
      USE API_INSTANCE_T2D
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
        INTEGER ::  I, IERR, ID_SIS, ID_T2D, IDUM, IDUM1
        INTEGER :: CPL_PERIOD
        INTEGER :: NTIME_STEPS_T2D
        CHARACTER(LEN=PATH_LEN) :: GEO_FILE,PRE_FILE
        CHARACTER(LEN=PATH_LEN) :: REF_FILE_T2D, REF_FILE_SIS
        CHARACTER(LEN=PATH_LEN) :: CLI_FILE_T2D, CLI_FILE_SIS
        CHARACTER(LEN=PATH_LEN) :: RES_FILE_T2D, RES_FILE_SIS
        CHARACTER(LEN=PATH_LEN) :: CAS_FILE_T2D, DICO_FILE_T2D
        CHARACTER(LEN=PATH_LEN) :: CAS_FILE_SIS, DICO_FILE_SIS
        CHARACTER(LEN=PATH_LEN) :: DUMMY
        CHARACTER(LEN=SIS_VAR_LEN) :: VARNAME
        INTEGER LU,LNG,NPLAN,PARALLEL,REFFILE,PREFILE
        INTEGER RANK,NCSIZE,PMETHOD,VAR_SIZE,COMM
        DUMMY = ' '
        COMM = 0

!     OUTPUT FOR WRITING
        LU=6
        ! 1 FOR FRENCH 2 FOR ENGLISH
        LNG=2
        ID_SIS = 0
        ID_T2D = 0
        !READING THE FILES NAMES
        OPEN(UNIT=12,FILE='param_api')
        READ(12,'(A)') DICO_FILE_T2D
        READ(12,'(A)') DICO_FILE_SIS
        READ(12,*) CAS_FILE_T2D
        READ(12,*) CAS_FILE_SIS
        READ(12,*) CPL_PERIOD
        READ(12,*) RES_FILE_T2D
        READ(12,*) RES_FILE_SIS
        READ(12,*) GEO_FILE
        READ(12,*) CLI_FILE_T2D
        READ(12,*) CLI_FILE_SIS
        READ(12,*) REFFILE
        IF (REFFILE.EQ.1) THEN
          READ(12,*) REF_FILE_T2D
          READ(12,*) REF_FILE_SIS
        ELSE
          READ(12,*)
          READ(12,*)
        ENDIF
        READ(12,*) PREFILE
        IF (PREFILE.EQ.1) THEN
          READ(12,*) PRE_FILE
        ELSE
          READ(12,*)
        ENDIF
        READ(12,*) PARALLEL
        CLOSE(12)

        PRINT*, "DICO T2D : ",DICO_FILE_T2D
        PRINT*, "DICO SIS : ",DICO_FILE_SIS
        PRINT*, "CAS  T2D : ",CAS_FILE_T2D
        PRINT*, "CAS  SIS : ",CAS_FILE_SIS
        PRINT*, "COUPLING PERIOD  : ", CPL_PERIOD
        PRINT*, "RES  T2D : ",RES_FILE_T2D
        PRINT*, "RES  SIS : ",RES_FILE_SIS
        PRINT*, "GEO  : ",GEO_FILE
        PRINT*, "CLI  T2D : ",CLI_FILE_T2D
        PRINT*, "CLI  SIS : ",CLI_FILE_SIS
        IF(REFFILE.EQ.1) PRINT*,"REFERENCE FILES PRESENT : ",
     &                           REF_FILE_T2D, REF_FILE_SIS
        IF(PREFILE.EQ.1) PRINT*,"PREVIOUS CALCULATIONS FILE PRESENT : ",
     &                                                         PRE_FILE
        IF(PARALLEL.EQ.0) PRINT*,"MPI : SEQUENTIAL RUN"
        IF(PARALLEL.EQ.1) PRINT*,"MPI : PARALLEL RUN"
        PRINT*
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        IF(PARALLEL.EQ.1) THEN
          ! Partitioning method to use 1: metis
          PMETHOD=1
#if defined HAVE_MPI
          ! Initialising mpi
!! COMPAD-DCO-MPICHECK  BEGIN  JR2016
#  if defined COMPAD
          WRITE(LU,*) '(AD) COMPAD :: HOMERE_API.F : DIRECT CALL OF ',
     &         'MPI_INIT NOT AD-READY'
          WRITE(LU,*) '  PLEASE CONTACT JR @ ADJOINTWARE'
          CALL PLANTE(1)
          STOP
#  endif
!! COMPAD-DCO-MPICHECK  END  JR2016
          CALL MPI_INIT(IERR)
          ! Getting rank
          CALL MPI_COMM_RANK(MPI_COMM_WORLD,RANK,IERR)
          ! Getting the number of process
          CALL MPI_COMM_SIZE(MPI_COMM_WORLD,NCSIZE,IERR)
          COMM = MPI_COMM_WORLD
#else
        RANK = -1
#endif
!
          CODE = 'SIS'
          ! The partitioning is done sequentially
          IF(RANK.EQ.0) THEN
            ! PARITIONING THE GEOMETRY FILE
            CALL PARTEL(GEO_FILE,CLI_FILE_SIS,NCSIZE,PMETHOD,
     &                  'SERAFIN ',' ',' ')
            IF(REFFILE.EQ.1)   CALL PARRES(GEO_FILE,REF_FILE_SIS,
     &                  NCSIZE,'SERAFIN ','SERAFIN ')
          ENDIF
          CODE = 'T2D'
          ! The partitioning is done sequentially
          IF(RANK.EQ.0) THEN
            ! PARITIONING THE GEOMETRY FILE
            CALL PARTEL(GEO_FILE,CLI_FILE_T2D,NCSIZE,PMETHOD,
     &                  'SERAFIN ',' ',' ')
            IF(REFFILE.EQ.1)   CALL PARRES(GEO_FILE,REF_FILE_T2D,
     &                  NCSIZE,'SERAFIN ','SERAFIN ')
            IF(PREFILE.EQ.1)   CALL PARRES(GEO_FILE,PRE_FILE,
     &                  NCSIZE,'SERAFIN ','SERAFIN ')
          ENDIF
        ENDIF
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!TELEMAC 2D!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        CALL RUN_SET_CONFIG_T2D(ID_T2D,LU,LNG,COMM,IERR)

        CALL RUN_READ_CASE_T2D(ID_T2D,CAS_FILE_T2D,DICO_FILE_T2D,
     &                         .TRUE.,IERR,DUMMY,DUMMY)

        ! Changing the name of the result file
        VARNAME = 'MODEL.RESULTFILE'
        CALL GET_VAR_SIZE(ID_T2D,'T2D',VARNAME,VAR_SIZE,
     &                        IDUM,IDUM1,IERR)
        PRINT *, 'rank:',RANK,'VAR_SIZE:',VAR_SIZE
        CALL SET_STRING(ID_T2D,'T2D',VARNAME,RES_FILE_T2D,VAR_SIZE,
     &                      0,0,IERR)

        CALL RUN_ALLOCATION_T2D(ID_T2D,IERR)
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!SISYPHE!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        CALL RUN_SET_CONFIG_SIS(ID_SIS,LU,LNG,COMM,IERR)

        CALL RUN_READ_CASE_SIS(ID_SIS,'TELEMAC2D               ',
     &                         CAS_FILE_SIS,DICO_FILE_SIS,
     &                         .FALSE.,IERR)

        ! Changing the name of the result file
        VARNAME = 'MODEL.RESULTFILE'
        CALL GET_VAR_SIZE(ID_SIS,'SIS',VARNAME,VAR_SIZE,IDUM,IDUM1,IERR)
        PRINT *, 'rank:',RANK,'VAR_SIZE:',VAR_SIZE
        CALL SET_STRING(ID_SIS,'SIS',VARNAME,RES_FILE_SIS,VAR_SIZE,
     &                      0,0,IERR)

        CALL RUN_ALLOCATION_SIS(ID_SIS,IERR)
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!TIME LOOP!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        CALL RUN_INIT_T2D(ID_T2D,IERR)
        VARNAME = 'MODEL.CPL_PERIOD'
        CALL SET_INTEGER(ID_T2D, 'T2D', VARNAME, CPL_PERIOD,
     &                       0, 0, 0, IERR)
        CALL CPL_INIT(ID_T2D,IERR)
        CALL SET_VAR_SIS(ID_T2D, ID_SIS, 0, IERR)
        CALL RUN_INIT_SIS(ID_SIS,IERR)
        CALL SET_VAR_T2D(ID_T2D, ID_SIS, IERR)
        CALL SAVE_CHARR_SUSP(ID_T2D, ID_SIS, IERR)

        !Get the number of timesteps
        VARNAME = 'MODEL.NTIMESTEPS'
        CALL GET_INTEGER(ID_T2D, 'T2D', VARNAME, NTIME_STEPS_T2D,
     &                       0, 0, 0, IERR)

        !Get the coupling period
        VARNAME = 'MODEL.CPL_PERIOD'
        CALL GET_INTEGER(ID_T2D,'T2D',VARNAME,CPL_PERIOD, 0, 0, 0, IERR)

        DO I=1,NTIME_STEPS_T2D
          CALL RUN_TIMESTEP_COMPUTE_T2D(ID_T2D,IERR)
          !COUPLAGE
          IF(CPL_PERIOD*(I/CPL_PERIOD).EQ.I) THEN
            CALL RUN_TIMESTEP_SIS_CPL(ID_T2D, ID_SIS, IERR)
          ENDIF
          CALL RUN_TIMESTEP_RES_T2D(ID_T2D,IERR)
        ENDDO
!


!!!!!!!!!!!!!!!!!!!!!!!!!!!FINALISATION!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        CALL RUN_FINALIZE_T2D(ID_T2D,IERR)
        CALL RUN_FINALIZE_SIS(ID_SIS,IERR)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        IF(PARALLEL.EQ.1) THEN
!         Mergin step
          IF(RANK.EQ.0) THEN
            CALL GRETEL_AUTOP(GEO_FILE,'SERAFIN ',RES_FILE_T2D,
     &                        'SERAFIN ', NCSIZE,NPLAN)
            CALL GRETEL_AUTOP(GEO_FILE,'SERAFIN ',RES_FILE_SIS,
     &                        'SERAFIN ', NCSIZE,NPLAN)

          ENDIF
#if defined HAVE_MPI
#  if defined COMPAD
          WRITE(LU,*) '(AD) COMPAD :: HOMERE_API.F : DIRECT CALL OF ',
     &         'MPI_FINALIZE NOT AD-READY'
          WRITE(LU,*) '  PLEASE CONTACT JR @ ADJOINTWARE'
          CALL PLANTE(1)
          STOP
#  endif
          CALL MPI_FINALIZE(IERR)
#endif
        ENDIF
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        END SUBROUTINE EXAMPLE_CPL_T2D_SIS
