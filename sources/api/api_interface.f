!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!>@brief USER API FUNCTIONS
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      MODULE API_INTERFACE
!
      USE API_HANDLE_VAR_T2D
      USE API_INSTANCE_T2D
      USE API_RUN_T2D
      USE API_HANDLE_VAR_T3D
      USE API_INSTANCE_T3D
      USE API_RUN_T3D
      USE API_HANDLE_VAR_SIS
      USE API_INSTANCE_SIS
      USE API_RUN_SIS
      USE API_HANDLE_VAR_ART
      USE API_INSTANCE_ART
      USE API_RUN_ART
      USE API_HANDLE_VAR_WAC
      USE API_INSTANCE_WAC
      USE API_RUN_WAC
      USE API_COUPLING
      USE DECLARATIONS_PARTEL, PAR_CODE => CODE
      USE BIEF_DEF,ONLY:EX_NCSIZE=>NCSIZE
      USE DECLARATIONS_SPECIAL, ONLY: STD_OUTPUT, LU, PARTEL_CONCAT

      IMPLICIT NONE
      INTEGER, EXTERNAL :: GLOBAL_TO_LOCAL_POINT
      INTEGER, PARAMETER :: STR_LEN = 250
!
      CONTAINS
!
!***********************************************************************
!     PARTEL/GRETEL
!***********************************************************************
!
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !>@brief Run partel
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !>@param[in] CODE Short name of code for wich running partel
      !!               (T2D, T3D...)
      !>@param[in] NAMEINP Name of the geometry file
      !>@param[in] NAMECLI Name of the boundary conditions file
      !>@param[in] NPARTS  Number of partitions
      !>@param[in] PMETHOD 1: for metis 2: for scotch
      !>@param[in] FFORMAT Format of the geometry file
      !>@param[in] NAMESEC Name of the section file ' ' if there are none
      !>@param[in] NAMEZFI Name of the friction zone file ' ' if there are none
      !>@param[in] NAMESEU Name of the weir file ' ' if there are none
      !>@param[out] ierr 0 if subroutine successfull,
      !!                        error id otherwise
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      SUBROUTINE RUN_PARTEL(CODE,NAMEINP, NAMECLI, NPARTS, PMETHOD,
     &  FFORMAT,NAMESEC, NAMEZFI,NAMESEU, IERR)
!
!
        CHARACTER(LEN=3),   INTENT(IN) :: CODE
        INTEGER,            INTENT(OUT):: IERR
        CHARACTER(LEN=250), INTENT(IN) :: NAMEINP
        CHARACTER(LEN=250), INTENT(IN) :: NAMECLI
        INTEGER, INTENT(IN) :: NPARTS
        INTEGER, INTENT(IN) :: PMETHOD
        CHARACTER(LEN=8), INTENT(INOUT) :: FFORMAT
        CHARACTER(LEN=250), INTENT(IN) :: NAMESEC
        CHARACTER(LEN=250), INTENT(IN) :: NAMEZFI
        CHARACTER(LEN=250), INTENT(IN) :: NAMESEU
!
        INTEGER :: TMP_LU
!
        IERR = 0
        PAR_CODE = CODE
        ! PARTEL might change lu value
        TMP_LU = LU
        ! If ask for writing in a log file
        IF(.NOT. STD_OUTPUT) THEN
          CALL GET_FREE_ID(LU)
          OPEN(UNIT=LU,FILE='PARTEL_'//TRIM(NAMEINP)//'.LOG',
     &         FORM='FORMATTED', STATUS='UNKNOWN')
        ENDIF
        ! The partitioning is done sequentially
        ! PARITIONING THE GEOMETRY FILE
        CALL PARTEL(NAMEINP, NAMECLI, NPARTS, PMETHOD, FFORMAT,
     &  NAMESEC, NAMEZFI, NAMESEU)
        IF(PARTEL_CONCAT) THEN
          WRITE(LU,*) 'PARTEL-CONCAT NOT HANDLED BY API'
          CALL PLANTE(1)
          STOP
        ENDIF
        IF(.NOT. STD_OUTPUT) THEN
          CLOSE(LU)
        ENDIF
        LU=TMP_LU
!
      END SUBROUTINE RUN_PARTEL
!
!
!
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !>@brief Run parres
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !>@param[in] CODE Short name of code for wich running partel
      !!               (T2D, T3D...)
      !>@param[in] NAMEGEO Name of the geometry file
      !>@param[in] NAMEINP Name of the file to be partitionned
      !>@param[in] NPARTS Number of partitions
      !>@param[in] GEOFORMAT Format of the geometry file
      !>@param[in] INPFORMAT Format of the file to be partitioned
      !>@param[out] ierr 0 if subroutine successfull,
      !!                        error id otherwise
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      SUBROUTINE RUN_PARRES(CODE,NAMEGEO, NAMEINP, NPARTS, GEOFORMAT,
     &     INPFORMAT,IERR)
!
        CHARACTER(LEN=3),   INTENT(IN) :: CODE
        INTEGER,            INTENT(OUT):: IERR
        CHARACTER(LEN=250), INTENT(IN) :: NAMEGEO
        CHARACTER(LEN=250), INTENT(IN) :: NAMEINP
        INTEGER, INTENT(IN) :: NPARTS
        CHARACTER(LEN=8), INTENT(INOUT) :: GEOFORMAT
        CHARACTER(LEN=8), INTENT(INOUT) :: INPFORMAT
!
        INTEGER :: TMP_LU
!
        IERR = 0
        PAR_CODE = CODE
        TMP_LU = LU
        IF(.NOT. STD_OUTPUT) THEN
          CALL GET_FREE_ID(LU)
          OPEN(UNIT=LU,FILE='PARTEL_'//TRIM(NAMEINP)//'.LOG',
     &         FORM='FORMATTED', STATUS='UNKNOWN')
        ENDIF
        ! The partitioning is done sequentially
        ! PARITIONING THE GEOMETRY FILE
        CALL PARRES(NAMEGEO, NAMEINP, NPARTS, GEOFORMAT,INPFORMAT)
        IF(.NOT. STD_OUTPUT) THEN
          CLOSE(LU)
        ENDIF
        LU = TMP_LU
!
      END SUBROUTINE RUN_PARRES
!
!
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !>@brief Running gretel
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !>@param[in] CODE Short name of code for wich running partel
      !!               (T2D, T3D...)
      !>@param[in] GEO Name of the geometry file
      !>@param[in,out] GEOFORMAT Format of the geometry file
      !>@param[in] BND Name of the boudnary file
      !>@param[in] RES Name of the result file
      !>@param[in,out] RESFORMAT Format of the result file
      !>@param[in] NPROC Number of processors
      !>@param[in] NPLAN_RES Number of planes for the result file
      !>@param[in] METHOD Method to merge data information
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      SUBROUTINE RUN_GRETEL(CODE,GEO,GEOFORMAT,BND,RES,RESFORMAT,NPROC,
     &     NPLAN_RES,METHOD)
!
        CHARACTER(LEN=3),  INTENT(IN) :: CODE
        CHARACTER(LEN=250), INTENT(IN) :: GEO
        CHARACTER(LEN=250), INTENT(IN) :: RES
        CHARACTER(LEN=250), INTENT(IN) :: BND
        CHARACTER(LEN=8),   INTENT(INOUT) :: GEOFORMAT,RESFORMAT
        INTEGER,            INTENT(IN) :: NPROC
        INTEGER,            INTENT(INOUT) :: NPLAN_RES
        INTEGER,            INTENT(IN) :: METHOD
!
        INTEGER :: TMP_LU
!
        TMP_LU = LU
        IF(.NOT. STD_OUTPUT) THEN
          CALL GET_FREE_ID(LU)
          OPEN(UNIT=LU,FILE='GRETEL_'//trim(RES)//'.LOG',
     &           FORM='FORMATTED', STATUS='UNKNOWN')
        ENDIF
        ! The partitioning is done sequentially
        ! PARITIONING THE GEOMETRY FILE
        CALL GRETEL_AUTOP(GEO,GEOFORMAT,BND,RES,RESFORMAT,NPROC,
     &                    NPLAN_RES,METHOD)
        IF(.NOT. STD_OUTPUT) THEN
          CLOSE(LU)
        ENDIF
        LU = TMP_LU

      END SUBROUTINE RUN_GRETEL
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !>@brief Check that the instance exist
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !>@param[in] ID Id of the instance
      !>@param[in] TAG Short of the module to use (t2d,sis...)
      !>@param[out] IERR 0 if subroutine successfull,
      !!                        error id otherwise
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      SUBROUTINE CHECK_INSTANCE(ID, TAG, IERR)
        INTEGER,                    INTENT(IN)    :: ID
        CHARACTER(LEN=3),           INTENT(IN)    :: TAG
        INTEGER,                    INTENT(OUT)   :: IERR
!
        IF (TAG == 'T2D') THEN
          CALL CHECK_INSTANCE_T2D(ID,IERR)
        ELSE IF (TAG == 'T3D') THEN
          CALL CHECK_INSTANCE_T3D(ID,IERR)
        ELSE IF (TAG == 'SIS') THEN
          CALL CHECK_INSTANCE_SIS(ID,IERR)
        ELSE IF (TAG == 'ART') THEN
          CALL CHECK_INSTANCE_ART(ID,IERR)
        ELSE IF (TAG == 'WAC') THEN
          CALL CHECK_INSTANCE_WAC(ID,IERR)
        ELSE
          IERR = UNKNOWN_MODULE
        ENDIF
!
        END SUBROUTINE
!
!***********************************************************************
! Get/set
!***********************************************************************
!
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !>@brief Get a double array
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !>@param[in] ID Id of the instance
      !>@param[in] TAG Short of the module to use (t2d,sis...)
      !>@param[in] VARNAME Name of the variable to read
      !>@param[in,out] VALEUR Contaings the read value
      !>@param[in] DIM1 Dimension of the array
      !>@param[out] IERR 0 if subroutine successfull,
      !!                        error id otherwise
      !>@param[in] BLOCK_INDEX Used for bief bloc represent index of the
      !!                      array in the bloc to extract
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      SUBROUTINE GET_DOUBLE_ARRAY
     &   (ID, TAG, VARNAME, VALEUR, DIM1, IERR, BLOCK_INDEX)
!
        INTEGER,                    INTENT(IN)    :: ID
        CHARACTER(LEN=3),           INTENT(IN)    :: TAG
        CHARACTER(LEN=T2D_VAR_LEN), INTENT(IN)    :: VARNAME
        INTEGER,                    INTENT(IN)    :: DIM1
        DOUBLE PRECISION, DIMENSION(DIM1), INTENT(INOUT)   :: VALEUR
        INTEGER,                    INTENT(OUT)   :: IERR
        INTEGER, OPTIONAL,           INTENT(IN) :: BLOCK_INDEX
!
        CALL CHECK_INSTANCE(ID,TAG,IERR)
        IF(IERR.NE.0) RETURN
!

        IF (TAG == 'T2D') THEN
          IF(PRESENT(BLOCK_INDEX))THEN
            CALL GET_DOUBLE_ARRAY_T2D_D(
     &           INSTANCE_LIST_T2D(ID), VARNAME, VALEUR, DIM1,
     &           IERR, BLOCK_INDEX)
          ELSE
            CALL GET_DOUBLE_ARRAY_T2D_D(
     &           INSTANCE_LIST_T2D(ID), VARNAME, VALEUR, DIM1,
     &           IERR)
          ENDIF
        ELSE IF (TAG == 'T3D') THEN
          IF(PRESENT(BLOCK_INDEX))THEN
            CALL GET_DOUBLE_ARRAY_T3D_D(
     &           INSTANCE_LIST_T3D(ID), VARNAME, VALEUR, DIM1,
     &           IERR, BLOCK_INDEX)
          ELSE
            CALL GET_DOUBLE_ARRAY_T3D_D(
     &           INSTANCE_LIST_T3D(ID), VARNAME, VALEUR, DIM1,
     &           IERR)
          ENDIF
        ELSE IF (TAG == 'SIS') THEN
          IF(PRESENT(BLOCK_INDEX))THEN
            CALL GET_DOUBLE_ARRAY_SIS_D(
     &           INSTANCE_LIST_SIS(ID), VARNAME, VALEUR, DIM1,
     &           IERR, BLOCK_INDEX)
          ELSE
            CALL GET_DOUBLE_ARRAY_SIS_D(
     &           INSTANCE_LIST_SIS(ID), VARNAME, VALEUR, DIM1,
     &           IERR)
          ENDIF
        ELSE IF (TAG == 'ART') THEN
          IF(PRESENT(BLOCK_INDEX))THEN
            CALL GET_DOUBLE_ARRAY_ART_D(
     &           INSTANCE_LIST_ART(ID), VARNAME, VALEUR, DIM1,
     &           IERR, BLOCK_INDEX)
          ELSE
            CALL GET_DOUBLE_ARRAY_ART_D(
     &           INSTANCE_LIST_ART(ID), VARNAME, VALEUR, DIM1,
     &           IERR)
          ENDIF
        ELSE IF (TAG == 'WAC') THEN
          IF(PRESENT(BLOCK_INDEX))THEN
            CALL GET_DOUBLE_ARRAY_WAC_D(
     &           INSTANCE_LIST_WAC(ID), VARNAME, VALEUR, DIM1,
     &           IERR, BLOCK_INDEX)
          ELSE
            CALL GET_DOUBLE_ARRAY_WAC_D(
     &           INSTANCE_LIST_WAC(ID), VARNAME, VALEUR, DIM1,
     &           IERR)
          ENDIF
        ELSE
          IERR = UNKNOWN_MODULE
          RETURN
        ENDIF
!
      END SUBROUTINE GET_DOUBLE_ARRAY
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !>@brief Set a double array
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !>@param[in] ID Id of the instance
      !>@param[in] TAG Short of the module to use (t2d,sis...)
      !>@param[in] VARNAME Name of the variable to read
      !>@param[in,out] VALEUR Containis the read value
      !>@param[in] DIM1 Dimension of the array
      !>@param[out] IERR 0 if subroutine successfull,
      !!                        error id otherwise
      !>@param[in] BLOCK_INDEX Used for bief bloc represent index of the
      !!                      array in the bloc to extract
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      SUBROUTINE SET_DOUBLE_ARRAY
     &   (ID, TAG, VARNAME, VALEUR, DIM1, IERR, BLOCK_INDEX)
!
        INTEGER,                    INTENT(IN)    :: ID
        CHARACTER(LEN=3),           INTENT(IN)    :: TAG
        CHARACTER(LEN=T2D_VAR_LEN), INTENT(IN)    :: VARNAME
        INTEGER,                    INTENT(IN)    :: DIM1
        DOUBLE PRECISION, DIMENSION(DIM1), INTENT(IN)   :: VALEUR
        INTEGER,                    INTENT(OUT)   :: IERR
        INTEGER, OPTIONAL,          INTENT(IN)    :: BLOCK_INDEX
!
        CALL CHECK_INSTANCE(ID,TAG,IERR)
        IF(IERR.NE.0) RETURN
!
        IF (TAG == 'T2D') THEN
          IF(PRESENT(BLOCK_INDEX))THEN
            CALL SET_DOUBLE_ARRAY_T2D_D(
     &           INSTANCE_LIST_T2D(ID), VARNAME, VALEUR, DIM1,
     &           IERR, BLOCK_INDEX)
          ELSE
            CALL SET_DOUBLE_ARRAY_T2D_D(
     &           INSTANCE_LIST_T2D(ID), VARNAME, VALEUR, DIM1,
     &           IERR)
          END IF
        ELSE IF (TAG == 'T3D') THEN
          IF(PRESENT(BLOCK_INDEX))THEN
            CALL SET_DOUBLE_ARRAY_T3D_D(
     &           INSTANCE_LIST_T3D(ID), VARNAME, VALEUR, DIM1,
     &           IERR, BLOCK_INDEX)
          ELSE
            CALL SET_DOUBLE_ARRAY_T3D_D(
     &           INSTANCE_LIST_T3D(ID), VARNAME, VALEUR, DIM1,
     &           IERR)
          END IF
        ELSE IF (TAG == 'SIS') THEN
          IF(PRESENT(BLOCK_INDEX))THEN
            CALL SET_DOUBLE_ARRAY_SIS_D(
     &           INSTANCE_LIST_SIS(ID), VARNAME, VALEUR, DIM1,
     &           IERR, BLOCK_INDEX)
          ELSE
            CALL SET_DOUBLE_ARRAY_SIS_D(
     &           INSTANCE_LIST_SIS(ID), VARNAME, VALEUR, DIM1,
     &           IERR)
          END IF
        ELSE IF (TAG == 'ART') THEN
          IF(PRESENT(BLOCK_INDEX))THEN
            CALL SET_DOUBLE_ARRAY_ART_D(
     &           INSTANCE_LIST_ART(ID), VARNAME, VALEUR, DIM1,
     &           IERR, BLOCK_INDEX)
          ELSE
            CALL SET_DOUBLE_ARRAY_ART_D(
     &           INSTANCE_LIST_ART(ID), VARNAME, VALEUR, DIM1,
     &           IERR)
          END IF
        ELSE IF (TAG == 'WAC') THEN
          IF(PRESENT(BLOCK_INDEX))THEN
            CALL SET_DOUBLE_ARRAY_WAC_D(
     &           INSTANCE_LIST_WAC(ID), VARNAME, VALEUR, DIM1,
     &           IERR, BLOCK_INDEX)
          ELSE
            CALL SET_DOUBLE_ARRAY_WAC_D(
     &           INSTANCE_LIST_WAC(ID), VARNAME, VALEUR, DIM1,
     &           IERR)
          END IF
        ELSE
          IERR = UNKNOWN_MODULE
          RETURN
        ENDIF
!
      END SUBROUTINE SET_DOUBLE_ARRAY
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !>@brief Get a integer array
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !>@param[in] ID Id of the instance
      !>@param[in] TAG Short of the module to use (t2d,sis...)
      !>@param[in] VARNAME Name of the variable to read
      !>@param[in,out] VALEUR Containis the read value
      !>@param[in] DIM1 Dimension of the array
      !>@param[out] IERR 0 if subroutine successfull,
      !!                        error id otherwise
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      SUBROUTINE GET_INTEGER_ARRAY
     &   (ID, TAG, VARNAME, VALEUR, DIM1, IERR)
!
        INTEGER,                    INTENT(IN)    :: ID
        CHARACTER(LEN=3),           INTENT(IN)    :: TAG
        CHARACTER(LEN=T2D_VAR_LEN), INTENT(IN)    :: VARNAME
        INTEGER,                    INTENT(IN)    :: DIM1
        INTEGER, DIMENSION(DIM1), INTENT(INOUT)   :: VALEUR
        INTEGER,                    INTENT(OUT)   :: IERR
!
        CALL CHECK_INSTANCE(ID,TAG,IERR)
        IF(IERR.NE.0) RETURN
!
        IF (TAG == 'T2D') THEN
          CALL GET_INTEGER_ARRAY_T2D_D(
     &            INSTANCE_LIST_T2D(ID), VARNAME, VALEUR, DIM1,
     &            IERR)
        ELSE IF (TAG == 'T3D') THEN
          CALL GET_INTEGER_ARRAY_T3D_D(
     &            INSTANCE_LIST_T3D(ID), VARNAME, VALEUR, DIM1,
     &            IERR)
        ELSE IF (TAG == 'SIS') THEN
          CALL GET_INTEGER_ARRAY_SIS_D(
     &            INSTANCE_LIST_SIS(ID), VARNAME, VALEUR, DIM1,
     &            IERR)
        ELSE IF (TAG == 'ART') THEN
          CALL GET_INTEGER_ARRAY_ART_D(
     &            INSTANCE_LIST_ART(ID), VARNAME, VALEUR, DIM1,
     &            IERR)
        ELSE IF (TAG == 'WAC') THEN
          CALL GET_INTEGER_ARRAY_WAC_D(
     &            INSTANCE_LIST_WAC(ID), VARNAME, VALEUR, DIM1,
     &            IERR)
        ELSE
          IERR = UNKNOWN_MODULE
          RETURN
        ENDIF
!
      END SUBROUTINE GET_INTEGER_ARRAY
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !>@brief Set a integer array
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !>@param[in] ID Id of the instance
      !>@param[in] TAG Short of the module to use (t2d,sis...)
      !>@param[in] VARNAME Name of the variable to read
      !>@param[in,out] VALEUR Containis the read value
      !>@param[in] DIM1 Dimension of the array
      !>@param[out] IERR 0 if subroutine successfull,
      !!                        error id otherwise
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      SUBROUTINE SET_INTEGER_ARRAY
     &   (ID, TAG, VARNAME, VALEUR, DIM1, IERR)
!
        INTEGER,                    INTENT(IN)    :: ID
        CHARACTER(LEN=3),           INTENT(IN)    :: TAG
        CHARACTER(LEN=T2D_VAR_LEN), INTENT(IN)    :: VARNAME
        INTEGER,                    INTENT(IN)    :: DIM1
        INTEGER, DIMENSION(DIM1), INTENT(IN)   :: VALEUR
        INTEGER,                    INTENT(OUT)   :: IERR
!
        CALL CHECK_INSTANCE(ID,TAG,IERR)
        IF(IERR.NE.0) RETURN
!
        IF (TAG == 'T2D') THEN
          CALL SET_INTEGER_ARRAY_T2D_D(
     &            INSTANCE_LIST_T2D(ID), VARNAME, VALEUR, DIM1,
     &            IERR)
        ELSE IF (TAG == 'T3D') THEN
          CALL SET_INTEGER_ARRAY_T3D_D(
     &            INSTANCE_LIST_T3D(ID), VARNAME, VALEUR, DIM1,
     &            IERR)
        ELSE IF (TAG == 'SIS') THEN
          CALL SET_INTEGER_ARRAY_SIS_D(
     &            INSTANCE_LIST_SIS(ID), VARNAME, VALEUR, DIM1,
     &            IERR)
        ELSE IF (TAG == 'ART') THEN
          CALL SET_INTEGER_ARRAY_ART_D(
     &            INSTANCE_LIST_ART(ID), VARNAME, VALEUR, DIM1,
     &            IERR)
        ELSE IF (TAG == 'WAC') THEN
          CALL SET_INTEGER_ARRAY_WAC_D(
     &            INSTANCE_LIST_WAC(ID), VARNAME, VALEUR, DIM1,
     &            IERR)
        ELSE
          IERR = UNKNOWN_MODULE
          RETURN
        ENDIF
!
      END SUBROUTINE SET_INTEGER_ARRAY
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !>@brief Get a double variable
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !>@param[in] ID Id of the instance
      !>@param[in] TAG Short of the module to use (t2d,sis...)
      !>@param[in] VARNAME Name of the variable to read
      !>@param[out] VALEUR Containis the read value
      !>@param[in] INDEX1 Index on the first dimension
      !>@param[in] INDEX2 Index on the second dimension
      !>@param[in] INDEX3 Index on the third dimension
      !>@param[out] IERR 0 if subroutine successfull,
      !!                        error id otherwise
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      SUBROUTINE GET_DOUBLE
     &   (ID, TAG, VARNAME, VALEUR, INDEX1, INDEX2, INDEX3, IERR)
!
        INTEGER,                    INTENT(IN)    :: ID
        CHARACTER(LEN=3),           INTENT(IN)    :: TAG
        CHARACTER(LEN=T2D_VAR_LEN), INTENT(IN)    :: VARNAME
        DOUBLE PRECISION,           INTENT(OUT)   :: VALEUR
        INTEGER,                    INTENT(INOUT) :: INDEX1
        INTEGER,                    INTENT(INOUT) :: INDEX2
        INTEGER,                    INTENT(INOUT) :: INDEX3
        INTEGER,                    INTENT(OUT)   :: IERR
!
        CALL CHECK_INSTANCE(ID,TAG,IERR)
        IF(IERR.NE.0) RETURN
!
        VALEUR = 0.D0
        IF (TAG == 'T2D') THEN
          CALL GET_DOUBLE_T2D_D(INSTANCE_LIST_T2D(ID), VARNAME, VALEUR,
     &                         INDEX1, INDEX2, INDEX3, IERR)
        ELSE IF (TAG == 'T3D') THEN
          CALL GET_DOUBLE_T3D_D(INSTANCE_LIST_T3D(ID), VARNAME, VALEUR,
     &                         INDEX1, INDEX2, INDEX3, IERR)
        ELSE IF (TAG == 'SIS') THEN
          CALL GET_DOUBLE_SIS_D(INSTANCE_LIST_SIS(ID), VARNAME, VALEUR,
     &                         INDEX1, INDEX2, INDEX3, IERR)
        ELSE IF (TAG == 'ART') THEN
          CALL GET_DOUBLE_ART_D(INSTANCE_LIST_ART(ID), VARNAME, VALEUR,
     &                         INDEX1, INDEX2, INDEX3, IERR)
        ELSE IF (TAG == 'WAC') THEN
          CALL GET_DOUBLE_WAC_D(INSTANCE_LIST_WAC(ID), VARNAME, VALEUR,
     &                         INDEX1, INDEX2, INDEX3, IERR)
        ELSE
          IERR = UNKNOWN_MODULE
          RETURN
        ENDIF
!
      END SUBROUTINE GET_DOUBLE
!
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !>@brief Defines the value of a double variable
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !>@param[in] ID Id of the instance
      !>@param[in] TAG Short of the module to use (t2d,sis...)
      !>@param[in] VARNAME Name of the variable to write
      !>@param[in] VALEUR The value to write in the variable
      !>@param[in] INDEX1 Index on the first dimension
      !>@param[in] INDEX2 Index on the second dimension
      !>@param[in] INDEX3 Index on the third dimension
      !>@param[out] IERR 0 if subroutine successfull,
      !!                        error id otherwise
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      SUBROUTINE SET_DOUBLE
     &   (ID, TAG, VARNAME, VALEUR, INDEX1, INDEX2, INDEX3, IERR)
!
        INTEGER,                    INTENT(IN)    :: ID
        CHARACTER(LEN=3),           INTENT(IN)    :: TAG
        CHARACTER(LEN=T2D_VAR_LEN), INTENT(IN)    :: VARNAME
        DOUBLE PRECISION,           INTENT(IN)    :: VALEUR
        INTEGER,                    INTENT(IN) :: INDEX1
        INTEGER,                    INTENT(IN) :: INDEX2
        INTEGER,                    INTENT(IN) :: INDEX3
        INTEGER,                    INTENT(OUT)   :: IERR
!
        CALL CHECK_INSTANCE(ID,TAG,IERR)
        IF(IERR.NE.0) RETURN
!
        IF (TAG == 'T2D') THEN
          CALL SET_DOUBLE_T2D_D(INSTANCE_LIST_T2D(ID), VARNAME, VALEUR,
     &                         INDEX1,INDEX2, INDEX3, IERR)
        ELSE IF (TAG == 'T3D') THEN
          CALL SET_DOUBLE_T3D_D(INSTANCE_LIST_T3D(ID), VARNAME, VALEUR,
     &                         INDEX1,INDEX2, INDEX3, IERR)
        ELSE IF (TAG == 'SIS') THEN
          CALL SET_DOUBLE_SIS_D(INSTANCE_LIST_SIS(ID), VARNAME, VALEUR,
     &                         INDEX1,INDEX2, INDEX3, IERR)
        ELSE IF (TAG == 'ART') THEN
          CALL SET_DOUBLE_ART_D(INSTANCE_LIST_ART(ID), VARNAME, VALEUR,
     &                         INDEX1,INDEX2, INDEX3, IERR)
        ELSE IF (TAG == 'WAC') THEN
          CALL SET_DOUBLE_WAC_D(INSTANCE_LIST_WAC(ID), VARNAME, VALEUR,
     &                         INDEX1,INDEX2, INDEX3, IERR)
        ELSE
          IERR = UNKNOWN_MODULE
          RETURN
        ENDIF
!
      END SUBROUTINE SET_DOUBLE
!
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !>@brief Get an integer variable
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !>@param[in] ID Id of the instance
      !>@param[in] TAG Short of the module to use (t2d,sis...)
      !>@param[in] VARNAME Name of the variable to read
      !>@param[out] VALEUR Containis the read value
      !>@param[in] INDEX1 Index on the first dimension
      !>@param[in] INDEX2 Index on the second dimension
      !>@param[in] INDEX3 Index on the third dimension
      !>@param[out] IERR 0 if subroutine successfull,
      !!                        error id otherwise
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      SUBROUTINE GET_INTEGER(ID, TAG, VARNAME, VALEUR,
     &             INDEX1, INDEX2, INDEX3, IERR)
!
        INTEGER,               INTENT(IN) :: ID
        CHARACTER(LEN=3),           INTENT(IN)    :: TAG
        CHARACTER(LEN=T2D_VAR_LEN), INTENT(IN)  :: VARNAME
        INTEGER,               INTENT(OUT) :: VALEUR
        INTEGER,               INTENT(IN) :: INDEX1
        INTEGER,               INTENT(IN) :: INDEX2
        INTEGER,               INTENT(IN) :: INDEX3
        INTEGER,               INTENT(OUT) :: IERR
!
        CALL CHECK_INSTANCE(ID,TAG,IERR)
        IF(IERR.NE.0) RETURN
!
        IF (TAG == 'T2D') THEN
          CALL GET_INTEGER_T2D_D(INSTANCE_LIST_T2D(ID), VARNAME, VALEUR,
     &                       INDEX1, INDEX2, INDEX3, IERR)
        ELSE IF (TAG == 'T3D') THEN
          CALL GET_INTEGER_T3D_D(INSTANCE_LIST_T3D(ID), VARNAME, VALEUR,
     &                       INDEX1, INDEX2, INDEX3, IERR)
        ELSE IF (TAG == 'SIS') THEN
          CALL GET_INTEGER_SIS_D(INSTANCE_LIST_SIS(ID), VARNAME, VALEUR,
     &                       INDEX1, INDEX2, INDEX3, IERR)
        ELSE IF (TAG == 'ART') THEN
          CALL GET_INTEGER_ART_D(INSTANCE_LIST_ART(ID), VARNAME, VALEUR,
     &                       INDEX1, INDEX2, INDEX3, IERR)
        ELSE IF (TAG == 'WAC') THEN
          CALL GET_INTEGER_WAC_D(INSTANCE_LIST_WAC(ID), VARNAME, VALEUR,
     &                       INDEX1, INDEX2, INDEX3, IERR)
        ELSE
          IERR = UNKNOWN_MODULE
          RETURN
        ENDIF
!
      END SUBROUTINE GET_INTEGER
!
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !>@brief Defines the value of an integer variable
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !>@param[in] ID Id of the instance
      !>@param[in] TAG Short of the module to use (t2d,sis...)
      !>@param[in] VARNAME Name of the variable to write
      !>@param[in] VALEUR The value to write in the variable
      !>@param[in] INDEX1 Index on the first dimension
      !>@param[in] INDEX2 Index on the second dimension
      !>@param[in] INDEX3 Index on the third dimension
      !>@param[out] IERR 0 if subroutine successfull,
      !!                        error id otherwise
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      SUBROUTINE SET_INTEGER(ID, TAG, VARNAME, VALEUR,
     &             INDEX1, INDEX2, INDEX3, IERR)
!
        INTEGER,               INTENT(IN) :: ID
        CHARACTER(LEN=3),           INTENT(IN)    :: TAG
        CHARACTER(LEN=T2D_VAR_LEN), INTENT(IN)  :: VARNAME
        INTEGER,               INTENT(IN) :: VALEUR
        INTEGER,               INTENT(IN) :: INDEX1
        INTEGER,               INTENT(IN) :: INDEX2
        INTEGER,               INTENT(IN) :: INDEX3
        INTEGER,               INTENT(OUT) :: IERR
!
        CALL CHECK_INSTANCE(ID,TAG,IERR)
        IF(IERR.NE.0) RETURN
!
        IF (TAG == 'T2D') THEN
          CALL SET_INTEGER_T2D_D(INSTANCE_LIST_T2D(ID), VARNAME, VALEUR,
     &                        INDEX1, INDEX2, INDEX3, IERR)
        ELSE IF (TAG == 'T3D') THEN
          CALL SET_INTEGER_T3D_D(INSTANCE_LIST_T3D(ID), VARNAME, VALEUR,
     &                        INDEX1, INDEX2, INDEX3, IERR)
        ELSE IF (TAG == 'SIS') THEN
          CALL SET_INTEGER_SIS_D(INSTANCE_LIST_SIS(ID), VARNAME, VALEUR,
     &                        INDEX1, INDEX2, INDEX3, IERR)
        ELSE IF (TAG == 'ART') THEN
          CALL SET_INTEGER_ART_D(INSTANCE_LIST_ART(ID), VARNAME, VALEUR,
     &                        INDEX1, INDEX2, INDEX3, IERR)
        ELSE IF (TAG == 'WAC') THEN
          CALL SET_INTEGER_WAC_D(INSTANCE_LIST_WAC(ID), VARNAME, VALEUR,
     &                        INDEX1, INDEX2, INDEX3, IERR)
        ELSE
          IERR = UNKNOWN_MODULE
          RETURN
        ENDIF
!
      END SUBROUTINE SET_INTEGER
!
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !>@brief Get a string variable
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !>@param[in] ID Id of the instance
      !>@param[in] TAG Short of the module to use (t2d,sis...)
      !>@param[in] VARNAME Name of the variable to read
      !>@param[out] VALEUR Containis the read value
      !>@param[in] VALUELEN Length of the string
      !>@param[in] INDEX1 Index on the first dimension
      !>@param[in] INDEX2 Index on the second dimension
      !>@param[out] IERR 0 if subroutine successfull,
      !!                        error id otherwise
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      SUBROUTINE GET_STRING(ID, TAG, VARNAME, VALEUR,
     &             VALUELEN, INDEX1, INDEX2, IERR)
!
        INTEGER,               INTENT(IN) :: ID
        CHARACTER(LEN=3),           INTENT(IN)    :: TAG
        CHARACTER(LEN=T2D_VAR_LEN), INTENT(IN)  :: VARNAME
        INTEGER,               INTENT(IN) :: VALUELEN
        INTEGER,               INTENT(IN) :: INDEX1
        INTEGER,               INTENT(IN) :: INDEX2
        ! Harcoded 250 forced by intel (otherwise get string does not
        ! work...)
        CHARACTER(LEN=250), INTENT(OUT) :: VALEUR
        INTEGER,               INTENT(OUT) :: IERR
!
        ! Temporary array with a good length
        CHARACTER :: TMP(VALUELEN)
        INTEGER I
        CALL CHECK_INSTANCE(ID,TAG,IERR)
        IF(IERR.NE.0) RETURN
!
        VALEUR = REPEAT(' ', 250)

        IF (TAG == 'T2D') THEN
          CALL GET_STRING_T2D_D(INSTANCE_LIST_T2D(ID),VARNAME, TMP,
     &                          VALUELEN, INDEX1, INDEX2, IERR)
        ELSE IF (TAG == 'T3D') THEN
          CALL GET_STRING_T3D_D(INSTANCE_LIST_T3D(ID),VARNAME, TMP,
     &                          VALUELEN, INDEX1, INDEX2, IERR)
        ELSE IF (TAG == 'SIS') THEN
          CALL GET_STRING_SIS_D(INSTANCE_LIST_SIS(ID),VARNAME, TMP,
     &                          VALUELEN, INDEX1, INDEX2, IERR)
        ELSE IF (TAG == 'ART') THEN
          CALL GET_STRING_ART_D(INSTANCE_LIST_ART(ID),VARNAME, TMP,
     &                          VALUELEN, INDEX1, INDEX2, IERR)
        ELSE IF (TAG == 'WAC') THEN
          CALL GET_STRING_WAC_D(INSTANCE_LIST_WAC(ID),VARNAME, TMP,
     &                          VALUELEN, INDEX1, INDEX2, IERR)
        ELSE
          IERR = UNKNOWN_MODULE
          RETURN
        ENDIF
        DO I=1,VALUELEN
          VALEUR(I:I) = TMP(I)
        ENDDO
!
      END SUBROUTINE GET_STRING
!
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !>@brief Defines the value of a string variable
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !>@param[in] ID Id of the instance
      !>@param[in] TAG Short of the module to use (t2d,sis...)
      !>@param[in] VARNAME Name of the variable to write
      !>@param[in] VALEUR The value to write in the variable
      !>@param[in] VALUELEN Length of the string
      !>@param[in] INDEX1 Index on the first dimension
      !>@param[in] INDEX2 Index on the second dimension
      !>@param[out] IERR 0 if subroutine successfull,
      !!                        error id otherwise
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      SUBROUTINE SET_STRING(ID, TAG, VARNAME, VALEUR,
     &             VALUELEN, INDEX1, INDEX2, IERR)
!
        INTEGER,               INTENT(IN) :: ID
        CHARACTER(LEN=3),           INTENT(IN)    :: TAG
        CHARACTER(LEN=T2D_VAR_LEN), INTENT(IN)  :: VARNAME
        INTEGER,               INTENT(IN) :: VALUELEN
        INTEGER,               INTENT(IN) :: INDEX1
        INTEGER,               INTENT(IN) :: INDEX2
        CHARACTER,             INTENT(IN) :: VALEUR(VALUELEN)
        INTEGER,               INTENT(OUT) :: IERR
!
        CALL CHECK_INSTANCE(ID,TAG,IERR)
        IF(IERR.NE.0) RETURN
!
        IF (TAG == 'T2D') THEN
          CALL SET_STRING_T2D_D(INSTANCE_LIST_T2D(ID), VARNAME, VALEUR,
     &                          VALUELEN, INDEX1, INDEX2, IERR)
        ELSE IF (TAG == 'T3D') THEN
          CALL SET_STRING_T3D_D(INSTANCE_LIST_T3D(ID), VARNAME, VALEUR,
     &                          VALUELEN, INDEX1, INDEX2, IERR)
        ELSE IF (TAG == 'SIS') THEN
          CALL SET_STRING_SIS_D(INSTANCE_LIST_SIS(ID), VARNAME, VALEUR,
     &                          VALUELEN, INDEX1, INDEX2, IERR)
        ELSE IF (TAG == 'ART') THEN
          CALL SET_STRING_ART_D(INSTANCE_LIST_ART(ID), VARNAME, VALEUR,
     &                          VALUELEN, INDEX1, INDEX2, IERR)
        ELSE IF (TAG == 'WAC') THEN
          CALL SET_STRING_WAC_D(INSTANCE_LIST_WAC(ID), VARNAME, VALEUR,
     &                          VALUELEN, INDEX1, INDEX2, IERR)
        ELSE
          IERR = UNKNOWN_MODULE
          RETURN
        ENDIF
!
      END SUBROUTINE SET_STRING
!
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !>@brief Get a boolean variable
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !>@param[in] ID Id of the instance
      !>@param[in] TAG Short of the module to use (t2d,sis...)
      !>@param[in] VARNAME Name of the variable to read
      !>@param[out] VALEUR Containis the read value
      !>@param[in] INDEX1 Index on the first dimension
      !>@param[in] INDEX2 Index on the second dimension
      !>@param[in] INDEX3 Index on the third dimension
      !>@param[out] IERR 0 if subroutine successfull,
      !!                        error id otherwise
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      SUBROUTINE GET_BOOLEAN
     &     (ID, TAG, VARNAME, VALEUR, INDEX1, INDEX2, INDEX3, IERR)
!
        INTEGER,               INTENT(IN) :: ID
        CHARACTER(LEN=3),           INTENT(IN)    :: TAG
        CHARACTER(LEN=T2D_VAR_LEN), INTENT(IN)  :: VARNAME
        INTEGER,               INTENT(OUT) :: VALEUR
        INTEGER,               INTENT(IN) :: INDEX1
        INTEGER,               INTENT(IN) :: INDEX2
        INTEGER,               INTENT(IN) :: INDEX3
        INTEGER,               INTENT(OUT) :: IERR
!
        CALL CHECK_INSTANCE(ID,TAG,IERR)
        IF(IERR.NE.0) RETURN
!
        IF (TAG == 'T2D') THEN
          CALL GET_BOOLEAN_T2D_D(INSTANCE_LIST_T2D(ID), VARNAME, VALEUR,
     &                        INDEX1, INDEX2, INDEX3, IERR)
        ELSE IF (TAG == 'T3D') THEN
          CALL GET_BOOLEAN_T3D_D(INSTANCE_LIST_T3D(ID), VARNAME, VALEUR,
     &                        INDEX1, INDEX2, INDEX3, IERR)
        ELSE IF (TAG == 'SIS') THEN
          CALL GET_BOOLEAN_SIS_D(INSTANCE_LIST_SIS(ID), VARNAME, VALEUR,
     &                        INDEX1, INDEX2, INDEX3, IERR)
        ELSE IF (TAG == 'ART') THEN
          CALL GET_BOOLEAN_ART_D(INSTANCE_LIST_ART(ID), VARNAME, VALEUR,
     &                        INDEX1, INDEX2, INDEX3, IERR)
        ELSE IF (TAG == 'WAC') THEN
          CALL GET_BOOLEAN_WAC_D(INSTANCE_LIST_WAC(ID), VARNAME, VALEUR,
     &                        INDEX1, INDEX2, INDEX3, IERR)
        ELSE
          IERR = UNKNOWN_MODULE
          RETURN
        ENDIF
!
      END SUBROUTINE GET_BOOLEAN
!
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !>@brief Defines the value of a boolean variable
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !>@param[in] ID Id of the instance
      !>@param[in] TAG Short of the module to use (t2d,sis...)
      !>@param[in] VARNAME Name of the variable to write
      !>@param[in] VALEUR The value to write in the variable
      !>@param[in] INDEX1 Index on the first dimension
      !>@param[in] INDEX2 Index on the second dimension
      !>@param[in] INDEX3 Index on the third dimension
      !>@param[out] IERR 0 if subroutine successfull,
      !!                        error id otherwise
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      SUBROUTINE SET_BOOLEAN
     &     (ID, TAG, VARNAME, VALEUR, INDEX1, INDEX2, INDEX3, IERR)
!
        INTEGER,               INTENT(IN) :: ID
        CHARACTER(LEN=3),           INTENT(IN)    :: TAG
        CHARACTER(LEN=T2D_VAR_LEN), INTENT(IN)  :: VARNAME
        INTEGER,               INTENT(IN) :: VALEUR
        INTEGER,               INTENT(IN) :: INDEX1
        INTEGER,               INTENT(IN) :: INDEX2
        INTEGER,               INTENT(IN) :: INDEX3
        INTEGER,               INTENT(OUT) :: IERR
!
        CALL CHECK_INSTANCE(ID,TAG,IERR)
        IF(IERR.NE.0) RETURN
!
        IF (TAG == 'T2D') THEN
          CALL SET_BOOLEAN_T2D_D(INSTANCE_LIST_T2D(ID), VARNAME, VALEUR,
     &                        INDEX1, INDEX2, INDEX3, IERR)
        ELSE IF (TAG == 'T3D') THEN
          CALL SET_BOOLEAN_T3D_D(INSTANCE_LIST_T3D(ID), VARNAME, VALEUR,
     &                        INDEX1, INDEX2, INDEX3, IERR)
        ELSE IF (TAG == 'SIS') THEN
          CALL SET_BOOLEAN_SIS_D(INSTANCE_LIST_SIS(ID), VARNAME, VALEUR,
     &                        INDEX1, INDEX2, INDEX3, IERR)
        ELSE IF (TAG == 'ART') THEN
          CALL SET_BOOLEAN_ART_D(INSTANCE_LIST_ART(ID), VARNAME, VALEUR,
     &                        INDEX1, INDEX2, INDEX3, IERR)
        ELSE IF (TAG == 'WAC') THEN
          CALL SET_BOOLEAN_WAC_D(INSTANCE_LIST_WAC(ID), VARNAME, VALEUR,
     &                        INDEX1, INDEX2, INDEX3, IERR)
        ELSE
          IERR = UNKNOWN_MODULE
          RETURN
        ENDIF
!
      END SUBROUTINE SET_BOOLEAN
!
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !>@brief Get informations on a variable
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !>@param[in] TAG Short of the module to use (t2d,sis...)
      !>@param[in] VARNAME Name of the variable
      !>@param[out] VARTYPE Type of the variable
      !!                        (INTEGER, DOUBLE, STRING, BOOLEAN)
      !>@param[out] READONLY 0 if the variable is read only
      !!                        1 IF IT IS WRITTABLE
      !>@param[out] NDIM Number of dimension
      !!                        (0 IF IT IS NOT AN ARRAY)
      !>@param[out] IENT 1 if the numbering is on point
      !>@param[out] JENT 1 if the numbering is on point
      !>@param[out] KENT 1 if the numbering is on point
      !>@param[out] GETPOS 1 if the numbering is on point
      !>@param[out] SETPOS 1 if the numbering is on point
      !>@param[out] IERR 0 if subroutine successfull,
      !!                        error id otherwise
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      SUBROUTINE GET_VAR_TYPE
     &        (TAG, VARNAME, VARTYPE, READONLY, NDIM,IENT,JENT,KENT,
     &         GETPOS,SETPOS,IERR)
        CHARACTER(LEN=3),           INTENT(IN)    :: TAG
        CHARACTER(LEN=T2D_VAR_LEN), INTENT(IN)  :: VARNAME
        CHARACTER(LEN=T2D_TYPE_LEN),     INTENT(OUT) :: VARTYPE
        LOGICAL,               INTENT(OUT) :: READONLY
        INTEGER,               INTENT(OUT) :: NDIM
        INTEGER,               INTENT(OUT) :: IERR
        INTEGER,               INTENT(OUT) :: IENT
        INTEGER,               INTENT(OUT) :: JENT
        INTEGER,               INTENT(OUT) :: KENT
        INTEGER,               INTENT(OUT) :: GETPOS
        INTEGER,               INTENT(OUT) :: SETPOS
!
        IF (TAG == 'T2D') THEN
          CALL GET_VAR_TYPE_T2D_D
     &          (VARNAME, VARTYPE, READONLY, NDIM,IENT,JENT,KENT,
     &           GETPOS, SETPOS, IERR)
        ELSE IF (TAG == 'T3D') THEN
          CALL GET_VAR_TYPE_T3D_D
     &          (VARNAME, VARTYPE, READONLY, NDIM,IENT,JENT,KENT,
     &           GETPOS, SETPOS, IERR)
        ELSE IF (TAG == 'SIS') THEN
          CALL GET_VAR_TYPE_SIS_D
     &          (VARNAME, VARTYPE, READONLY, NDIM,IENT,JENT,KENT,
     &           GETPOS, SETPOS, IERR)
        ELSE IF (TAG == 'ART') THEN
          CALL GET_VAR_TYPE_ART_D
     &          (VARNAME, VARTYPE, READONLY, NDIM,IENT,JENT,KENT,
     &           GETPOS, SETPOS, IERR)
        ELSE IF (TAG == 'WAC') THEN
          CALL GET_VAR_TYPE_WAC_D
     &          (VARNAME, VARTYPE, READONLY, NDIM,IENT,JENT,KENT,
     &           GETPOS, SETPOS, IERR)
        ELSE
          IERR = UNKNOWN_MODULE
          RETURN
        ENDIF
      END SUBROUTINE GET_VAR_TYPE
!
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !>@brief Get the size of each dimension of a varaible
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !>@param[in] ID Id of the instance
      !>@param[in] TAG Short of the module to use (t2d,sis...)
      !>@param[in] VARNAME Name of the varaible
      !>@param[out] DIM1 Size of the first dimension
      !>@param[out] DIM2 Size of the second dimension
      !>@param[out] DIM3 Size of the third dimension
      !>@param[out] IERR 0 if subroutine successfull,
      !!                        error id otherwise
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      SUBROUTINE GET_VAR_SIZE(ID, TAG, VARNAME, DIM1, DIM2, DIM3, IERR)
        INTEGER,               INTENT(IN) :: ID
        CHARACTER(LEN=3),           INTENT(IN)    :: TAG
        CHARACTER(LEN=T2D_VAR_LEN), INTENT(IN)  :: VARNAME
        INTEGER,               INTENT(OUT) :: DIM1
        INTEGER,               INTENT(OUT) :: DIM2
        INTEGER,               INTENT(OUT) :: DIM3
        INTEGER,               INTENT(OUT) :: IERR
!
        CALL CHECK_INSTANCE(ID,TAG,IERR)
        IF(IERR.NE.0) RETURN
!
        IF (TAG == 'T2D') THEN
          CALL GET_VAR_SIZE_T2D_D(INSTANCE_LIST_T2D(ID), VARNAME,
     &                            DIM1, DIM2, DIM3, IERR)
        ELSE IF (TAG == 'T3D') THEN
          CALL GET_VAR_SIZE_T3D_D(INSTANCE_LIST_T3D(ID), VARNAME,
     &                            DIM1, DIM2, DIM3, IERR)
        ELSE IF (TAG == 'SIS') THEN
          CALL GET_VAR_SIZE_SIS_D(INSTANCE_LIST_SIS(ID), VARNAME,
     &                            DIM1, DIM2, DIM3, IERR)
        ELSE IF (TAG == 'ART') THEN
          CALL GET_VAR_SIZE_ART_D(INSTANCE_LIST_ART(ID), VARNAME,
     &                            DIM1, DIM2, DIM3, IERR)
        ELSE IF (TAG == 'WAC') THEN
          CALL GET_VAR_SIZE_WAC_D(INSTANCE_LIST_WAC(ID), VARNAME,
     &                            DIM1, DIM2, DIM3, IERR)
        ELSE
          IERR = UNKNOWN_MODULE
          RETURN
        ENDIF
!
      END SUBROUTINE GET_VAR_SIZE
!
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !>@brief Get a description of each variable
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !>@param[in] TAG Short of the module to use (t2d,sis...)
      !>@param[out] VARNAME List of all the variables
      !>@param[out] VARINFO List of all the descriptions
      !>@param[out] IERR 0 if subroutine successfull,
      !!                        error id otherwise
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      SUBROUTINE GET_VAR_LIST(TAG, VARNAME, VARINFO, IERR)
!
        CHARACTER(LEN=3),           INTENT(IN)    :: TAG
        CHARACTER(LEN=T2D_VAR_LEN),  INTENT(INOUT) :: VARNAME(*)
        CHARACTER(LEN=T2D_INFO_LEN), INTENT(INOUT) :: VARINFO(*)
        INTEGER, INTENT(OUT) :: IERR
!
        INTEGER I

        IERR = 0
        IF (TAG == 'T2D') THEN
          DO I=1,NB_VAR_T2D
            VARNAME(I) = VNAME_T2D(I)
            VARINFO(I) = VINFO_T2D(I)
          ENDDO
        ELSE IF (TAG == 'T3D') THEN
          DO I=1,NB_VAR_T3D
            VARNAME(I) = VNAME_T3D(I)
            VARINFO(I) = VINFO_T3D(I)
          ENDDO
        ELSE IF (TAG == 'SIS') THEN
          DO I=1,NB_VAR_SIS
            VARNAME(I) = VNAME_SIS(I)
            VARINFO(I) = VINFO_SIS(I)
          ENDDO
        ELSE IF (TAG == 'ART') THEN
          DO I=1,NB_VAR_ART
            VARNAME(I) = VNAME_ART(I)
            VARINFO(I) = VINFO_ART(I)
          ENDDO
        ELSE IF (TAG == 'WAC') THEN
          DO I=1,NB_VAR_WAC
            VARNAME(I) = VNAME_WAC(I)
            VARINFO(I) = VINFO_WAC(I)
          ENDDO
        ELSE
          IERR = UNKNOWN_MODULE
          RETURN
        ENDIF
!
      END SUBROUTINE
!
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !>@brief Returns the error message of the instance
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !>@param[in] ID Id of the instance
      !>@param[in] TAG Short of the module to use (t2d,sis...)
      !>@param[in] IERR Error code
      !>@param[out] MESS The error message
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      SUBROUTINE GET_ERROR_MESSAGE(ID,TAG,IERR,MESS)
        INTEGER, INTENT(IN) :: ID
        CHARACTER(LEN=3),           INTENT(IN)    :: TAG
        INTEGER, INTENT(IN) :: IERR
        CHARACTER(LEN=ERROR_MESS_LEN), INTENT(OUT) :: MESS
!
        CHARACTER(LEN=T2D_INFO_LEN) :: INST_MESS
        CHARACTER(LEN=50) :: ERR_TYPE
        INTEGER :: IERR2
!
        CALL CHECK_INSTANCE(ID,TAG,IERR2)
        IF(IERR2.NE.0) THEN
          MESS = TRIM(ERR_MESS)
        ELSE
          IF (TAG == 'T2D') THEN
            CALL GET_INSTANCE_ERROR_T2D(ID,INST_MESS)
          ELSE IF (TAG == 'T3D') THEN
            CALL GET_INSTANCE_ERROR_T3D(ID,INST_MESS)
          ELSE IF (TAG == 'SIS') THEN
            CALL GET_INSTANCE_ERROR_SIS(ID,INST_MESS)
          ELSE IF (TAG == 'ART') THEN
            CALL GET_INSTANCE_ERROR_ART(ID,INST_MESS)
          ELSE IF (TAG == 'WAC') THEN
            CALL GET_INSTANCE_ERROR_WAC(ID,INST_MESS)
          ELSE
            RETURN
          ENDIF
          CALL GET_ERROR_TYPE(IERR,ERR_TYPE)
          MESS = TRIM(ERR_TYPE) // '\n' // INST_MESS
        ENDIF
!
      END SUBROUTINE GET_ERROR_MESSAGE

!
!***********************************************************************
!     TELEMAC2D
!***********************************************************************
!
!
! EXECUTION FUNCTIONS
!
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !>@brief Initialise the instance and set the output
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !>@param[out] ID Id of the instance
      !>@param[in] LU Output stream id
      !>@param[in] LNG Output language 2 english 1 french
      !>@param[in] COMM Mpi communicator
      !>@param[out] IERR 0 if subroutine successfull,
      !!                   error id otherwise
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      SUBROUTINE RUN_SET_CONFIG_T2D(ID,LU,LNG,COMM,IERR)
!
        INTEGER,  INTENT(OUT) :: ID
        INTEGER,  INTENT(IN) :: LU, LNG, COMM
        INTEGER, INTENT(OUT) :: IERR
!
        INTEGER :: EXEC_POS
!
        CALL CREATE_INSTANCE_T2D(ID,IERR)
        IF(IERR.NE.0) RETURN
!
        EXEC_POS = INSTANCE_LIST_T2D(ID)%MYPOSITION
        CALL CHECK_CALL_POSITION(EXEC_POS,'RUN_SET_CONFIG_T2D',
     &                      NO_POSITION,
     &                      RUN_READ_CASE_POS,IERR)
        IF(IERR.NE.0) RETURN
!
        INSTANCE_LIST_T2D(ID)%MYPOSITION = RUN_SET_CONFIG_POS
!
        CALL RUN_SET_CONFIG_T2D_D(INSTANCE_LIST_T2D(ID),LU,LNG,
     &                            COMM,LU.NE.0,IERR)
        IF(IERR.NE.0) RETURN
        CALL SET_VAR_LIST_T2D_D(IERR)
!
      END SUBROUTINE RUN_SET_CONFIG_T2D
!
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !>@brief Initializes variables for TELEMAC2D in case of coupling
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !>@param[in] ID Id of the telemac2d instance
      !>@param[out] IERR 0 if subroutine successfull,
      !!                   error id otherwise
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      SUBROUTINE CPL_INIT(ID,IERR)
!
        INTEGER,  INTENT(IN) :: ID
        INTEGER, INTENT(OUT) :: IERR
!
        IERR = 0
        CALL CHECK_INSTANCE_T2D(ID,IERR)
        IF(IERR.NE.0) RETURN

        CALL CPL_INIT_T2D(ID,IERR)

      END SUBROUTINE CPL_INIT
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !>@brief Reads the case file
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !>@param[in] ID Id of the instance
      !>@param[in] CAS_FILE Path to the case file
      !>@param[in] DICO_FILE Path to the dictionary file
      !>@param[in] INIT If true p_init is called
      !>@param[out] IERR 0 if subroutine successfull,
      !!                        error id otherwise
      !>@param[in] GAIA_CAS Path to the gaia case file
      !>@param[in] GAIA_DICO Path to the gaia dictionary file
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      SUBROUTINE RUN_READ_CASE_T2D(ID,CAS_FILE, DICO_FILE,INIT,IERR,
     &                             GAIA_CAS,GAIA_DICO)
!
          INTEGER,            INTENT(IN) :: ID
          CHARACTER(LEN=250), INTENT(IN) :: CAS_FILE
          CHARACTER(LEN=250), INTENT(IN) :: DICO_FILE
          LOGICAL,            INTENT(IN) :: INIT
          INTEGER,            INTENT(OUT) :: IERR
          CHARACTER(LEN=250), INTENT(IN) :: GAIA_CAS
          CHARACTER(LEN=250), INTENT(IN) :: GAIA_DICO
!
        INTEGER :: EXEC_POS
!
        CALL CHECK_INSTANCE_T2D(ID,IERR)
        IF(IERR.NE.0) RETURN
!
        EXEC_POS = INSTANCE_LIST_T2D(ID)%MYPOSITION
        CALL CHECK_CALL_POSITION(EXEC_POS,'RUN_READ_CASE_T2D',
     &                      RUN_SET_CONFIG_POS,
     &                      RUN_ALLOCATION_POS,IERR)
        IF(IERR.NE.0) RETURN
!
        INSTANCE_LIST_T2D(ID)%MYPOSITION = RUN_READ_CASE_POS
!
        CALL RUN_READ_CASE_T2D_D(INSTANCE_LIST_T2D(ID),CAS_FILE,
     &                           DICO_FILE, INIT, IERR,
     &                           GAIA_CAS,
     &                           GAIA_DICO)
!
      END SUBROUTINE RUN_READ_CASE_T2D
!
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !>@brief Allocate all of telemac2d variables
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !>@param[in] ID Id of the instance
      !>@param[out] IERR 0 if subroutine successfull,
      !!                        error id otherwise
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      SUBROUTINE RUN_ALLOCATION_T2D(ID,IERR)
!
        INTEGER,            INTENT(IN) :: ID
        INTEGER,            INTENT(OUT) :: IERR
!
        INTEGER :: EXEC_POS
!
        CALL CHECK_INSTANCE_T2D(ID,IERR)
        IF(IERR.NE.0) RETURN
!
        EXEC_POS = INSTANCE_LIST_T2D(ID)%MYPOSITION
        CALL CHECK_CALL_POSITION(EXEC_POS,'RUN_ALLOCATION_T2D',
     &                           RUN_READ_CASE_POS,
     &                           RUN_INIT_POS,IERR)
        IF(IERR.NE.0) RETURN
        INSTANCE_LIST_T2D(ID)%MYPOSITION = RUN_ALLOCATION_POS
!
        CALL RUN_ALLOCATION_T2D_D(INSTANCE_LIST_T2D(ID),IERR)
!
      END SUBROUTINE RUN_ALLOCATION_T2D
!
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !>@brief Initialise the telemac2d variables
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !>@param[in] ID Id of the instance
      !>@param[out] IERR 0 if subroutine successfull,
      !!                        error id otherwise
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      SUBROUTINE RUN_INIT_T2D(ID,IERR)
!
        INTEGER,            INTENT(IN) :: ID
        INTEGER,            INTENT(OUT) :: IERR
!
        INTEGER :: EXEC_POS
!
        CALL CHECK_INSTANCE_T2D(ID,IERR)
        IF(IERR.NE.0) RETURN
!
        EXEC_POS = INSTANCE_LIST_T2D(ID)%MYPOSITION
        CALL CHECK_CALL_POSITION(EXEC_POS,'RUN_INIT_T2D',
     &                           RUN_ALLOCATION_POS,
     &                           RUN_TIMESTEP_POS,IERR)
        INSTANCE_LIST_T2D(ID)%MYPOSITION = RUN_INIT_POS
!
        CALL RUN_INIT_T2D_D(INSTANCE_LIST_T2D(ID),IERR)
      END SUBROUTINE RUN_INIT_T2D
!
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !>@brief Run a timestep in telemac2d
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !>@param[in] ID Id of the instance
      !>@param[out] IERR 0 if subroutine successfull,
      !!                        error id otherwise
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      SUBROUTINE RUN_TIMESTEP_T2D(ID,IERR)
!
        INTEGER,            INTENT(IN) :: ID
        INTEGER,            INTENT(OUT) :: IERR
!
        CALL RUN_TIMESTEP_COMPUTE_T2D(ID, IERR)
        CALL RUN_TIMESTEP_RES_T2D(ID, IERR)
!
      END SUBROUTINE RUN_TIMESTEP_T2D
!
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !>@brief Run a timestep in telemac2d without writing results
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !>@param[in] ID Id of the instance
      !>@param[out] IERR 0 if subroutine successfull,
      !!                        error id otherwise
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      SUBROUTINE RUN_TIMESTEP_COMPUTE_T2D(ID,IERR)
!
        INTEGER,            INTENT(IN) :: ID
        INTEGER,            INTENT(OUT) :: IERR
!
        INTEGER :: EXEC_POS
!
        CALL CHECK_INSTANCE_T2D(ID,IERR)
        IF(IERR.NE.0) RETURN
!
        EXEC_POS = INSTANCE_LIST_T2D(ID)%MYPOSITION
        CALL CHECK_CALL_POSITION(EXEC_POS,'RUN_TIMESTEP_T2D',
     &          RUN_INIT_POS,RUN_FINALIZE_POS,IERR)
        IF(IERR.NE.0) RETURN
!
        CALL RUN_TIMESTEP_COMPUTE_T2D_D(INSTANCE_LIST_T2D(ID),IERR)
!
      END SUBROUTINE RUN_TIMESTEP_COMPUTE_T2D
!
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !>@brief Run a timestep in telemac2d only writing results
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !>@param[in] ID Id of the instance
      !>@param[out] IERR 0 if subroutine successfull,
      !!                        error id otherwise
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      SUBROUTINE RUN_TIMESTEP_RES_T2D(ID,IERR)
!
        INTEGER,            INTENT(IN) :: ID
        INTEGER,            INTENT(OUT) :: IERR
!
        INTEGER :: EXEC_POS
!
        CALL CHECK_INSTANCE_T2D(ID,IERR)
        IF(IERR.NE.0) RETURN
!
        EXEC_POS = INSTANCE_LIST_T2D(ID)%MYPOSITION
        CALL CHECK_CALL_POSITION(EXEC_POS,'RUN_TIMESTEP_T2D',
     &          RUN_INIT_POS,RUN_FINALIZE_POS,IERR)
        IF(IERR.NE.0) RETURN
        INSTANCE_LIST_T2D(ID)%MYPOSITION = RUN_TIMESTEP_POS
!
        CALL RUN_TIMESTEP_RES_T2D_D(INSTANCE_LIST_T2D(ID),IERR)
!
      END SUBROUTINE RUN_TIMESTEP_RES_T2D
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !>@brief Finalize a telemac2d run
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !>@param[in] ID Id of the instance
      !>@param[out] IERR 0 if subroutine successfull,
      !!                        error id otherwise
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      SUBROUTINE RUN_FINALIZE_T2D(ID,IERR)
!
        INTEGER :: EXEC_POS
!
        INTEGER,            INTENT(IN) :: ID
        INTEGER,            INTENT(OUT) :: IERR
!
        CALL CHECK_INSTANCE_T2D(ID,IERR)
        IF(IERR.NE.0) RETURN
!
        EXEC_POS = INSTANCE_LIST_T2D(ID)%MYPOSITION
        CALL CHECK_CALL_POSITION(EXEC_POS,'RUN_FINALIZE_T2D',
     &        RUN_TIMESTEP_POS,NO_POSITION,IERR)
        IF(IERR.NE.0) RETURN
        INSTANCE_LIST_T2D(ID)%MYPOSITION = RUN_FINALIZE_POS
!
        CALL RUN_FINALIZE_T2D_D(INSTANCE_LIST_T2D(ID),IERR)
        IF(IERR.NE.0) RETURN
        CALL DELETE_INSTANCE_T2D(ID,IERR)
        DEALLOCATE(VNAME_T2D)
        DEALLOCATE(VINFO_T2D)
!
      END SUBROUTINE RUN_FINALIZE_T2D
!
!***********************************************************************
!     TELEMAC3D
!***********************************************************************
!
!
! EXECUTION FUNCTIONS
!
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !>@brief Initialise the instance and set the output
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !>@param[out] ID Id of the instance
      !>@param[in] LU Output stream id
      !>@param[in] LNG Output language 2 english 1 french
      !>@param[in] COMM Mpi communicator
      !>@param[out] IERR 0 if subroutine successfull,
      !!                   error id otherwise
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      SUBROUTINE RUN_SET_CONFIG_T3D(ID,LU,LNG,COMM,IERR)
!
        INTEGER,  INTENT(OUT) :: ID
        INTEGER,  INTENT(IN) :: LU, LNG, COMM
        INTEGER, INTENT(OUT) :: IERR
!
        INTEGER :: EXEC_POS
!
        CALL CREATE_INSTANCE_T3D(ID,IERR)
        IF(IERR.NE.0) RETURN
!
        EXEC_POS = INSTANCE_LIST_T3D(ID)%MYPOSITION
        CALL CHECK_CALL_POSITION(EXEC_POS,'RUN_SET_CONFIG_T3D',
     &                      NO_POSITION,
     &                      RUN_READ_CASE_POS,IERR)
        IF(IERR.NE.0) RETURN
!
        INSTANCE_LIST_T3D(ID)%MYPOSITION = RUN_SET_CONFIG_POS
!
        CALL RUN_SET_CONFIG_T3D_D(INSTANCE_LIST_T3D(ID),LU,LNG,
     &                            COMM,LU.NE.0,IERR)
        IF(IERR.NE.0) RETURN
        CALL SET_VAR_LIST_T3D_D(IERR)
!
      END SUBROUTINE RUN_SET_CONFIG_T3D
!
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !>@brief Reads the case file
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !>@param[in] ID Id of the instance
      !>@param[in] CAS_FILE Path to the case file
      !>@param[in] DICO_FILE Path to the dictionary file
      !>@param[in] INIT If true p_init is called
      !>@param[out] IERR 0 if subroutine successfull,
      !!                        error id otherwise
      !>@param[in] WAQ_CAS_FILE Path to the gaia case file
      !>@param[in] WAQ_DICO_FILE Path to the gaia dictionary file
      !>@param[in] GAIA_CAS_FILE Path to the gaia case file
      !>@param[in] GAIA_DICO_FILE Path to the gaia dictionary file
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      SUBROUTINE RUN_READ_CASE_T3D(ID,CAS_FILE, DICO_FILE, INIT, IERR,
     &                             WAQ_CAS_FILE, WAQ_DICO_FILE,
     &                             GAIA_CAS_FILE, GAIA_DICO_FILE)
!
          INTEGER,            INTENT(IN) :: ID
          CHARACTER(LEN=250), INTENT(IN) :: CAS_FILE
          CHARACTER(LEN=250), INTENT(IN) :: DICO_FILE
          LOGICAL,            INTENT(IN) :: INIT
          INTEGER,            INTENT(OUT) :: IERR
          CHARACTER(LEN=250), INTENT(IN) :: WAQ_CAS_FILE
          CHARACTER(LEN=250), INTENT(IN) :: WAQ_DICO_FILE
          CHARACTER(LEN=250), INTENT(IN) :: GAIA_CAS_FILE
          CHARACTER(LEN=250), INTENT(IN) :: GAIA_DICO_FILE
!
        INTEGER :: EXEC_POS
!
        CALL CHECK_INSTANCE_T3D(ID,IERR)
        IF(IERR.NE.0) RETURN
!
        EXEC_POS = INSTANCE_LIST_T3D(ID)%MYPOSITION
        CALL CHECK_CALL_POSITION(EXEC_POS,'RUN_READ_CASE_T3D',
     &                      RUN_SET_CONFIG_POS,
     &                      RUN_ALLOCATION_POS,IERR)
        IF(IERR.NE.0) RETURN
!
        INSTANCE_LIST_T3D(ID)%MYPOSITION = RUN_READ_CASE_POS
!
        CALL RUN_READ_CASE_T3D_D(INSTANCE_LIST_T3D(ID),CAS_FILE,
     &                           DICO_FILE, INIT, IERR,
     &                           WAQ_CAS_FILE, WAQ_DICO_FILE,
     &                           GAIA_CAS_FILE, GAIA_DICO_FILE)
!
      END SUBROUTINE RUN_READ_CASE_T3D
!
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !>@brief Allocate all of telemac3d variables
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !>@param[in] ID Id of the instance
      !>@param[out] IERR 0 if subroutine successfull,
      !!                        error id otherwise
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      SUBROUTINE RUN_ALLOCATION_T3D(ID,IERR)
!
        INTEGER,            INTENT(IN) :: ID
        INTEGER,            INTENT(OUT) :: IERR
!
        INTEGER :: EXEC_POS
!
        CALL CHECK_INSTANCE_T3D(ID,IERR)
        IF(IERR.NE.0) RETURN
!
        EXEC_POS = INSTANCE_LIST_T3D(ID)%MYPOSITION
        CALL CHECK_CALL_POSITION(EXEC_POS,'RUN_ALLOCATION_T3D',
     &                           RUN_READ_CASE_POS,
     &                           RUN_INIT_POS,IERR)
        IF(IERR.NE.0) RETURN
        INSTANCE_LIST_T3D(ID)%MYPOSITION = RUN_ALLOCATION_POS
!
        CALL RUN_ALLOCATION_T3D_D(INSTANCE_LIST_T3D(ID),IERR)
!
      END SUBROUTINE RUN_ALLOCATION_T3D
!
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !>@brief Initialise the telemac3d variables
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !>@param[in] ID Id of the instance
      !>@param[out] IERR 0 if subroutine successfull,
      !!                        error id otherwise
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      SUBROUTINE RUN_INIT_T3D(ID,IERR)
!
        INTEGER,            INTENT(IN) :: ID
        INTEGER,            INTENT(OUT) :: IERR
!
        INTEGER :: EXEC_POS
!
        CALL CHECK_INSTANCE_T3D(ID,IERR)
        IF(IERR.NE.0) RETURN
!
        EXEC_POS = INSTANCE_LIST_T3D(ID)%MYPOSITION
        CALL CHECK_CALL_POSITION(EXEC_POS,'RUN_INIT_T3D',
     &                           RUN_ALLOCATION_POS,
     &                           RUN_TIMESTEP_POS,IERR)
        INSTANCE_LIST_T3D(ID)%MYPOSITION = RUN_INIT_POS
!
        CALL RUN_INIT_T3D_D(INSTANCE_LIST_T3D(ID),IERR)
      END SUBROUTINE RUN_INIT_T3D
!
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !>@brief Run a timestep in telemac3d
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !>@param[in] ID Id of the instance
      !>@param[out] IERR 0 if subroutine successfull,
      !!                        error id otherwise
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      SUBROUTINE RUN_TIMESTEP_T3D(ID,IERR)
!
        INTEGER,            INTENT(IN) :: ID
        INTEGER,            INTENT(OUT) :: IERR
!
        INTEGER :: EXEC_POS
!
        CALL CHECK_INSTANCE_T3D(ID,IERR)
        IF(IERR.NE.0) RETURN
!
        EXEC_POS = INSTANCE_LIST_T3D(ID)%MYPOSITION
        CALL CHECK_CALL_POSITION(EXEC_POS,'RUN_TIMESTEP_T3D',
     &          RUN_INIT_POS,RUN_FINALIZE_POS,IERR)
        IF(IERR.NE.0) RETURN
        INSTANCE_LIST_T3D(ID)%MYPOSITION = RUN_TIMESTEP_POS
!
        CALL RUN_TIMESTEP_T3D_D(INSTANCE_LIST_T3D(ID),IERR)
!
      END SUBROUTINE RUN_TIMESTEP_T3D
!
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !>@brief Finalize a telemac3d run
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !>@param[in] ID Id of the instance
      !>@param[out] IERR 0 if subroutine successfull,
      !!                        error id otherwise
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      SUBROUTINE RUN_FINALIZE_T3D(ID,IERR)
!
        INTEGER :: EXEC_POS
!
        INTEGER,            INTENT(IN) :: ID
        INTEGER,            INTENT(OUT) :: IERR
!
        CALL CHECK_INSTANCE_T3D(ID,IERR)
        IF(IERR.NE.0) RETURN
!
        EXEC_POS = INSTANCE_LIST_T3D(ID)%MYPOSITION
        CALL CHECK_CALL_POSITION(EXEC_POS,'RUN_FINALIZE_T3D',
     &        RUN_TIMESTEP_POS,NO_POSITION,IERR)
        IF(IERR.NE.0) RETURN
        INSTANCE_LIST_T3D(ID)%MYPOSITION = RUN_FINALIZE_POS
!
        CALL RUN_FINALIZE_T3D_D(INSTANCE_LIST_T3D(ID),IERR)
        IF(IERR.NE.0) RETURN
        CALL DELETE_INSTANCE_T3D(ID,IERR)
        DEALLOCATE(VNAME_T3D)
        DEALLOCATE(VINFO_T3D)
!
      END SUBROUTINE RUN_FINALIZE_T3D
!
!***********************************************************************
!     SISYPHE
!***********************************************************************
!
! EXECUTION FUNCTIONS
!
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !>@brief Initialise the instance and set the output
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !>@param[out] ID Id of the instance
      !>@param[in] LU Output stream id
      !>@param[in] LNG Output language 2 english 1 french
      !>@param[in] COMM Mpi communicator
      !>@param[out] IERR 0 if subroutine successfull,
      !!                   error id otherwise
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      SUBROUTINE RUN_SET_CONFIG_SIS(ID,LU,LNG,COMM,IERR)
!
        INTEGER,  INTENT(OUT) :: ID
        INTEGER,  INTENT(IN) :: LU, LNG,COMM
        INTEGER, INTENT(OUT) :: IERR
!
        INTEGER :: EXEC_POS
!
        CALL CREATE_INSTANCE_SIS(ID,IERR)
        IF(IERR.NE.0) RETURN
!
        EXEC_POS = INSTANCE_LIST_SIS(ID)%MYPOSITION
        CALL CHECK_CALL_POSITION(EXEC_POS,'RUN_SET_CONFIG_SIS',
     &                      NO_POSITION,
     &                      RUN_READ_CASE_POS,IERR)
        IF(IERR.NE.0) RETURN
!
        INSTANCE_LIST_SIS(ID)%MYPOSITION = RUN_SET_CONFIG_POS
!
        CALL RUN_SET_CONFIG_SIS_D(INSTANCE_LIST_SIS(ID),LU,LNG,COMM,
     &                            LU.NE.0,IERR)
        CALL SET_VAR_LIST_SIS_D(IERR)
!
      END SUBROUTINE RUN_SET_CONFIG_SIS
!
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !>@brief Reads the case file
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !>@param[in] ID Id of the instance
      !>@param[in] CODE Code for coupled call
      !>@param[in] CAS_FILE Path to the case file
      !>@param[in] DICO_FILE Path to the dictionary file
      !>@param[in] INIT If true p_init is called
      !>@param[out] IERR 0 if subroutine successfull,
      !!                        error id otherwise
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      SUBROUTINE RUN_READ_CASE_SIS(ID,CODE,CAS_FILE, DICO_FILE, INIT,
     &                             IERR)
!
        INTEGER,            INTENT(IN) :: ID
        CHARACTER(LEN=24),  INTENT(IN) :: CODE
        CHARACTER(LEN=250), INTENT(IN) :: CAS_FILE
        CHARACTER(LEN=250), INTENT(IN) :: DICO_FILE
        LOGICAL,            INTENT(IN) :: INIT
        INTEGER,            INTENT(OUT) :: IERR
!
        INTEGER :: EXEC_POS
!
        CALL CHECK_INSTANCE_SIS(ID,IERR)
        IF(IERR.NE.0) RETURN
!
        EXEC_POS = INSTANCE_LIST_SIS(ID)%MYPOSITION
        CALL CHECK_CALL_POSITION(EXEC_POS,'RUN_READ_CASE_SIS',
     &                      RUN_SET_CONFIG_POS,
     &                      RUN_ALLOCATION_POS,IERR)
        IF(IERR.NE.0) RETURN
!
        INSTANCE_LIST_SIS(ID)%MYPOSITION = RUN_READ_CASE_POS
!
        CALL RUN_READ_CASE_SIS_D(INSTANCE_LIST_SIS(ID),CODE,CAS_FILE,
     &                           DICO_FILE, INIT, IERR)
!
      END SUBROUTINE RUN_READ_CASE_SIS
!
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !>@brief Allocate all of sisyphe variables
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !>@param[in] ID Id of the instance
      !>@param[out] IERR 0 if subroutine successfull,
      !!                        error id otherwise
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      SUBROUTINE RUN_ALLOCATION_SIS(ID,IERR)
!
        INTEGER,            INTENT(IN) :: ID
        INTEGER,            INTENT(OUT) :: IERR
!
        INTEGER :: EXEC_POS
!
        CALL CHECK_INSTANCE_SIS(ID,IERR)
        IF(IERR.NE.0) RETURN
!
        EXEC_POS = INSTANCE_LIST_SIS(ID)%MYPOSITION
        CALL CHECK_CALL_POSITION(EXEC_POS,'RUN_ALLOCATION_SIS',
     &                           RUN_READ_CASE_POS,
     &                           RUN_INIT_POS,IERR)
        IF(IERR.NE.0) RETURN
        INSTANCE_LIST_SIS(ID)%MYPOSITION = RUN_ALLOCATION_POS
!
        CALL RUN_ALLOCATION_SIS_D(INSTANCE_LIST_SIS(ID),IERR)
!
      END SUBROUTINE RUN_ALLOCATION_SIS
!
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !>@brief Initialise the sisyphe variables
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !>@param[in] ID Id of the instance
      !>@param[out] IERR 0 if subroutine successfull,
      !!                        error id otherwise
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      SUBROUTINE RUN_INIT_SIS(ID,IERR)
!
        INTEGER,            INTENT(IN) :: ID
        INTEGER,            INTENT(OUT) :: IERR
!
        INTEGER :: EXEC_POS
!
        CALL CHECK_INSTANCE_SIS(ID,IERR)
        IF(IERR.NE.0) RETURN
!
        EXEC_POS = INSTANCE_LIST_SIS(ID)%MYPOSITION
        CALL CHECK_CALL_POSITION(EXEC_POS,'RUN_INIT_SIS',
     &                           RUN_ALLOCATION_POS,
     &                           RUN_TIMESTEP_POS,IERR)
        INSTANCE_LIST_SIS(ID)%MYPOSITION = RUN_INIT_POS
!
        CALL RUN_INIT_SIS_D(INSTANCE_LIST_SIS(ID),IERR)
      END SUBROUTINE RUN_INIT_SIS
!
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !>@brief Run a timestep in sisyphe
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !>@param[in] ID Id of the instance
      !>@param[out] IERR 0 if subroutine successfull,
      !!                        error id otherwise
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      SUBROUTINE RUN_TIMESTEP_SIS(ID,IERR)
!
        INTEGER,            INTENT(IN) :: ID
        INTEGER,            INTENT(OUT) :: IERR
!
        INTEGER :: EXEC_POS
!
        CALL CHECK_INSTANCE_SIS(ID,IERR)
        IF(IERR.NE.0) RETURN
!
        EXEC_POS = INSTANCE_LIST_SIS(ID)%MYPOSITION
        CALL CHECK_CALL_POSITION(EXEC_POS,'RUN_TIMESTEP_SIS',
     &          RUN_INIT_POS,RUN_FINALIZE_POS,IERR)
        IF(IERR.NE.0) RETURN
        INSTANCE_LIST_SIS(ID)%MYPOSITION = RUN_TIMESTEP_POS
!
        CALL RUN_TIMESTEP_SIS_D(INSTANCE_LIST_SIS(ID),IERR)
!
      END SUBROUTINE RUN_TIMESTEP_SIS
!
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !>@brief Finalize a sisyphe run
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !>@param[in] ID Id of the instance
      !>@param[out] IERR 0 if subroutine successfull,
      !!                        error id otherwise
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      SUBROUTINE RUN_FINALIZE_SIS(ID,IERR)
!
        INTEGER :: EXEC_POS
!
        INTEGER,            INTENT(IN) :: ID
        INTEGER,            INTENT(OUT) :: IERR
!
        CALL CHECK_INSTANCE_SIS(ID,IERR)
        IF(IERR.NE.0) RETURN
!
        EXEC_POS = INSTANCE_LIST_SIS(ID)%MYPOSITION
        CALL CHECK_CALL_POSITION(EXEC_POS,'RUN_FINALIZE_SIS',
     &        RUN_TIMESTEP_POS,NO_POSITION,IERR)
        IF(IERR.NE.0) RETURN
        INSTANCE_LIST_SIS(ID)%MYPOSITION = RUN_FINALIZE_POS
!
        CALL RUN_FINALIZE_SIS_D(INSTANCE_LIST_SIS(ID),IERR)
        IF(IERR.NE.0) RETURN
        CALL DELETE_INSTANCE_SIS(ID,IERR)
!
      END SUBROUTINE RUN_FINALIZE_SIS
!
!***********************************************************************
!     COUPLING T2D_SIS
!***********************************************************************
!
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !>@brief Saves original charr and susp values after first sisyphe call
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !>@param[in] ID_T2D Id of the telemac2d instance
      !>@param[in] ID_SIS Id of the sisyphe instance
      !>@param[out] IERR 0 if subroutine successfull,
      !!                   error id otherwise
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      SUBROUTINE SAVE_CHARR_SUSP(ID_T2D, ID_SIS,IERR)
!
        INTEGER,  INTENT(IN) :: ID_T2D
        INTEGER,  INTENT(IN) :: ID_SIS
        INTEGER, INTENT(OUT) :: IERR
!
        IERR = 0
        CALL CHECK_INSTANCE_T2D(ID_T2D,IERR)
        IF(IERR.NE.0) RETURN
        CALL CHECK_INSTANCE_SIS(ID_SIS,IERR)
        IF(IERR.NE.0) RETURN

        CALL SAVE_CHARR_SUSP_CPL(INSTANCE_LIST_SIS(ID_SIS),
     &                           INSTANCE_LIST_T2D(ID_T2D), IERR)

      END SUBROUTINE SAVE_CHARR_SUSP


      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !>@brief Deals with cases : BEDLOAD OF SUSPENSION
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !>@param[in] ID_T2D Id of the telemac2d instance
      !>@param[in] ID_SIS Id of the sisyphe instance
      !>@param[out] CHARR_SUSP Defines which sisyphe call
      !!                             = 1 Means Bedload
      !!                             = 2 Means Suspension
      !!                             = 3 Means Both
      !>@param[out] IERR 0 if subroutine successfull,
      !!                   error id otherwise
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      SUBROUTINE CHARR_OR_SUSP(ID_T2D, ID_SIS, CHARR_SUSP, IERR)
!
        INTEGER,  INTENT(IN) :: ID_T2D
        INTEGER,  INTENT(IN) :: ID_SIS
        INTEGER, INTENT(OUT) :: CHARR_SUSP
        INTEGER, INTENT(OUT) :: IERR
!
        IERR = 0
        CALL CHECK_INSTANCE_T2D(ID_T2D,IERR)
        IF(IERR.NE.0) RETURN
        CALL CHECK_INSTANCE_SIS(ID_SIS,IERR)
        IF(IERR.NE.0) RETURN

        CALL CHARR_OR_SUSP_CPL(INSTANCE_LIST_SIS(ID_SIS),
     &                         INSTANCE_LIST_T2D(ID_T2D),
     &                         CHARR_SUSP,IERR)

      END SUBROUTINE CHARR_OR_SUSP



      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !>@brief Sets loop variables for sisyphe in case of coupling
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !>@param[in] ID_T2D Id of the telemac2d instance
      !>@param[in] ID_SIS Id of the sisyphe instance
      !>@param[in] CALL_TYPE Defines which sisyphe call
      !!                             = 0 Means Initializing
      !!                             = 1 Means Bedload CALL
      !!                             = 2 Means Suspension CALL
      !>@param[out] IERR 0 if subroutine successfull,
      !!                             error id otherwise
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      SUBROUTINE SET_VAR_SIS(ID_T2D,ID_SIS, CALL_TYPE,IERR)
!
        INTEGER,  INTENT(IN) :: ID_T2D
        INTEGER,  INTENT(IN) :: ID_SIS
        INTEGER, INTENT(IN) :: CALL_TYPE
        INTEGER, INTENT(OUT) :: IERR
!
        IERR = 0
        CALL CHECK_INSTANCE_T2D(ID_T2D,IERR)
        IF(IERR.NE.0) RETURN
        CALL CHECK_INSTANCE_SIS(ID_SIS,IERR)
        IF(IERR.NE.0) RETURN

        CALL SET_VAR_SIS_CPL(INSTANCE_LIST_T2D(ID_T2D), CALL_TYPE,
     &                                  INSTANCE_LIST_SIS(ID_SIS),IERR)

      END SUBROUTINE SET_VAR_SIS

      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !>@brief Sends variables to telemac2d after sisyphe call
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !>@param[in] ID_T2D Id of the telemac2d instance
      !>@param[in] ID_SIS Id of the sisyphe instance
      !>@param[out] IERR 0 if subroutine successfull,
      !!                             error id otherwise
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      SUBROUTINE SET_VAR_T2D(ID_T2D, ID_SIS, IERR)
!
        INTEGER,  INTENT(IN) :: ID_T2D
        INTEGER,  INTENT(IN) :: ID_SIS
        INTEGER, INTENT(OUT) :: IERR
!
        IERR = 0
        CALL CHECK_INSTANCE_T2D(ID_T2D,IERR)
        IF(IERR.NE.0) RETURN
        CALL CHECK_INSTANCE_SIS(ID_SIS,IERR)
        IF(IERR.NE.0) RETURN

        CALL SET_VAR_T2D_CPL(INSTANCE_LIST_SIS(ID_SIS),
     &                                INSTANCE_LIST_T2D(ID_T2D),IERR)

      END SUBROUTINE SET_VAR_T2D
!
!***********************************************************************
!     ARTEMIS
!***********************************************************************
!
!
! EXECUTION FUNCTIONS
!
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !>@brief Initialise the instance and set the output
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !>@param[out] ID Id of the instance
      !>@param[in] LU Output stream id
      !>@param[in] LNG Output language 2 english 1 french
      !>@param[in] COMM Mpi communicator
      !>@param[out] IERR 0 if subroutine successfull,
      !!                   error id otherwise
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      SUBROUTINE RUN_SET_CONFIG_ART(ID,LU,LNG,COMM,IERR)
!
        INTEGER,  INTENT(OUT) :: ID
        INTEGER,  INTENT(IN) :: LU, LNG, COMM
        INTEGER, INTENT(OUT) :: IERR
!
        INTEGER :: EXEC_POS
!
        CALL CREATE_INSTANCE_ART(ID,IERR)
        IF(IERR.NE.0) RETURN
!
        EXEC_POS = INSTANCE_LIST_ART(ID)%MYPOSITION
        CALL CHECK_CALL_POSITION(EXEC_POS,'RUN_SET_CONFIG_ART',
     &                      NO_POSITION,
     &                      RUN_READ_CASE_POS,IERR)
        IF(IERR.NE.0) RETURN
!
        INSTANCE_LIST_ART(ID)%MYPOSITION = RUN_SET_CONFIG_POS
!
        CALL RUN_SET_CONFIG_ART_D(INSTANCE_LIST_ART(ID),LU,LNG,
     &                            COMM,LU.NE.0,IERR)
        IF(IERR.NE.0) RETURN
        CALL SET_VAR_LIST_ART_D(IERR)
!
      END SUBROUTINE RUN_SET_CONFIG_ART
!
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !>@brief Reads the case file
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !>@param[in] ID Id of the instance
      !>@param[in] CAS_FILE Path to the case file
      !>@param[in] DICO_FILE Path to the dictionary file
      !>@param[in] INIT If true p_init is called
      !>@param[out] IERR 0 if subroutine successfull,
      !!                        error id otherwise
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      SUBROUTINE RUN_READ_CASE_ART(ID,CAS_FILE, DICO_FILE, INIT,IERR)
!
          INTEGER,            INTENT(IN) :: ID
          CHARACTER(LEN=250), INTENT(IN) :: CAS_FILE
          CHARACTER(LEN=250), INTENT(IN) :: DICO_FILE
          LOGICAL,            INTENT(IN) :: INIT
          INTEGER,            INTENT(OUT) :: IERR
!
        INTEGER :: EXEC_POS
!
        CALL CHECK_INSTANCE_ART(ID,IERR)
        IF(IERR.NE.0) RETURN
!
        EXEC_POS = INSTANCE_LIST_ART(ID)%MYPOSITION
        CALL CHECK_CALL_POSITION(EXEC_POS,'RUN_READ_CASE_ART',
     &                      RUN_SET_CONFIG_POS,
     &                      RUN_ALLOCATION_POS,IERR)
        IF(IERR.NE.0) RETURN
!
        INSTANCE_LIST_ART(ID)%MYPOSITION = RUN_READ_CASE_POS
!
        CALL RUN_READ_CASE_ART_D(INSTANCE_LIST_ART(ID),CAS_FILE,
     &                           DICO_FILE, INIT, IERR)
!
      END SUBROUTINE RUN_READ_CASE_ART
!
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !>@brief Allocate all of artemis variables
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !>@param[in] ID Id of the instance
      !>@param[out] IERR 0 if subroutine successfull,
      !!                        error id otherwise
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      SUBROUTINE RUN_ALLOCATION_ART(ID,IERR)
!
        INTEGER,            INTENT(IN) :: ID
        INTEGER,            INTENT(OUT) :: IERR
!
        INTEGER :: EXEC_POS
!
        CALL CHECK_INSTANCE_ART(ID,IERR)
        IF(IERR.NE.0) RETURN
!
        EXEC_POS = INSTANCE_LIST_ART(ID)%MYPOSITION
        CALL CHECK_CALL_POSITION(EXEC_POS,'RUN_ALLOCATION_ART',
     &                           RUN_READ_CASE_POS,
     &                           RUN_INIT_POS,IERR)
        IF(IERR.NE.0) RETURN
        INSTANCE_LIST_ART(ID)%MYPOSITION = RUN_ALLOCATION_POS
!
        CALL RUN_ALLOCATION_ART_D(INSTANCE_LIST_ART(ID),IERR)
!
      END SUBROUTINE RUN_ALLOCATION_ART
!
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !>@brief Initialise the artemis variables
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !>@param[in] ID Id of the instance
      !>@param[out] IERR 0 if subroutine successfull,
      !!                        error id otherwise
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      SUBROUTINE RUN_INIT_ART(ID,IERR)
!
        INTEGER,            INTENT(IN) :: ID
        INTEGER,            INTENT(OUT) :: IERR
!
        INTEGER :: EXEC_POS
!
        CALL CHECK_INSTANCE_ART(ID,IERR)
        IF(IERR.NE.0) RETURN
!
        EXEC_POS = INSTANCE_LIST_ART(ID)%MYPOSITION
        CALL CHECK_CALL_POSITION(EXEC_POS,'RUN_INIT_ART',
     &                           RUN_ALLOCATION_POS,
     &                           RUN_TIMESTEP_POS,IERR)
        INSTANCE_LIST_ART(ID)%MYPOSITION = RUN_INIT_POS
!
        CALL RUN_INIT_ART_D(INSTANCE_LIST_ART(ID),IERR)
      END SUBROUTINE RUN_INIT_ART
!
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !>@brief RUN A TIMESTEP IN ARTEMIS
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !>@param[in] ID Id of the instance
      !>@param[out] IERR 0 if subroutine successfull,
      !!                        error id otherwise
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      SUBROUTINE RUN_TIMESTEP_ART(ID,IERR)
!
        INTEGER,            INTENT(IN) :: ID
        INTEGER,            INTENT(OUT) :: IERR
!
        INTEGER :: EXEC_POS
!
        CALL CHECK_INSTANCE_ART(ID,IERR)
        IF(IERR.NE.0) RETURN
!
        EXEC_POS = INSTANCE_LIST_ART(ID)%MYPOSITION
        CALL CHECK_CALL_POSITION(EXEC_POS,'RUN_TIMESTEP_ART',
     &          RUN_INIT_POS,RUN_FINALIZE_POS,IERR)
        IF(IERR.NE.0) RETURN
        INSTANCE_LIST_ART(ID)%MYPOSITION = RUN_TIMESTEP_POS
!
        CALL RUN_TIMESTEP_ART_D(INSTANCE_LIST_ART(ID),IERR)
!
      END SUBROUTINE RUN_TIMESTEP_ART
!
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !>@brief Finalize a artemis run
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !>@param[in] ID Id of the instance
      !>@param[out] IERR 0 if subroutine successfull,
      !!                        error id otherwise
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      SUBROUTINE RUN_FINALIZE_ART(ID,IERR)
!
        INTEGER :: EXEC_POS
!
        INTEGER,            INTENT(IN) :: ID
        INTEGER,            INTENT(OUT) :: IERR
!
        CALL CHECK_INSTANCE_ART(ID,IERR)
        IF(IERR.NE.0) RETURN
!
        EXEC_POS = INSTANCE_LIST_ART(ID)%MYPOSITION
        CALL CHECK_CALL_POSITION(EXEC_POS,'RUN_FINALIZE_ART',
     &        RUN_TIMESTEP_POS,NO_POSITION,IERR)
        IF(IERR.NE.0) RETURN
        INSTANCE_LIST_ART(ID)%MYPOSITION = RUN_FINALIZE_POS
!
        CALL RUN_FINALIZE_ART_D(INSTANCE_LIST_ART(ID),IERR)
        IF(IERR.NE.0) RETURN
        CALL DELETE_INSTANCE_ART(ID,IERR)
        DEALLOCATE(VNAME_ART)
        DEALLOCATE(VINFO_ART)
!
      END SUBROUTINE RUN_FINALIZE_ART
!
!***********************************************************************
!     tomawac
!***********************************************************************
!
!
! EXECUTION FUNCTIONS
!
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !>@brief Initialise the instance and set the output
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !>@param[out] ID Id of the instance
      !>@param[in] LU Output stream id
      !>@param[in] LNG Output language 2 english 1 french
      !>@param[in] COMM Mpi communicator
      !>@param[out] IERR 0 if subroutine successfull,
      !!                   error id otherwise
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      SUBROUTINE RUN_SET_CONFIG_WAC(ID,LU,LNG,COMM,IERR)
!
        INTEGER,  INTENT(OUT) :: ID
        INTEGER,  INTENT(IN) :: LU, LNG, COMM
        INTEGER, INTENT(OUT) :: IERR
!
        INTEGER :: EXEC_POS
!
        CALL CREATE_INSTANCE_WAC(ID,IERR)
        IF(IERR.NE.0) RETURN
!
        EXEC_POS = INSTANCE_LIST_WAC(ID)%MYPOSITION
        CALL CHECK_CALL_POSITION(EXEC_POS,'RUN_SET_CONFIG_WAC',
     &                      NO_POSITION,
     &                      RUN_READ_CASE_POS,IERR)
        IF(IERR.NE.0) RETURN
!
        INSTANCE_LIST_WAC(ID)%MYPOSITION = RUN_SET_CONFIG_POS
!
        CALL RUN_SET_CONFIG_WAC_D(INSTANCE_LIST_WAC(ID),LU,LNG,
     &                            COMM,LU.NE.0,IERR)
        IF(IERR.NE.0) RETURN
        CALL SET_VAR_LIST_WAC_D(IERR)
!
      END SUBROUTINE RUN_SET_CONFIG_WAC
!
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !>@brief Reads the case file
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !>@param[in] ID Id of the instance
      !>@param[in] CAS_FILE Path to the case file
      !>@param[in] DICO_FILE Path to the dictionary file
      !>@param[in] INIT If true p_init is called
      !>@param[out] IERR 0 if subroutine successfull,
      !!                        error id otherwise
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      SUBROUTINE RUN_READ_CASE_WAC(ID,CAS_FILE, DICO_FILE, INIT,IERR)
!
          INTEGER,            INTENT(IN) :: ID
          CHARACTER(LEN=250), INTENT(IN) :: CAS_FILE
          CHARACTER(LEN=250), INTENT(IN) :: DICO_FILE
          LOGICAL,            INTENT(IN) :: INIT
          INTEGER,            INTENT(OUT) :: IERR
!
        INTEGER :: EXEC_POS
!
        CALL CHECK_INSTANCE_WAC(ID,IERR)
        IF(IERR.NE.0) RETURN
!
        EXEC_POS = INSTANCE_LIST_WAC(ID)%MYPOSITION
        CALL CHECK_CALL_POSITION(EXEC_POS,'RUN_READ_CASE_WAC',
     &                      RUN_SET_CONFIG_POS,
     &                      RUN_ALLOCATION_POS,IERR)
        IF(IERR.NE.0) RETURN
!
        INSTANCE_LIST_WAC(ID)%MYPOSITION = RUN_READ_CASE_POS
!
        CALL RUN_READ_CASE_WAC_D(INSTANCE_LIST_WAC(ID),CAS_FILE,
     &                           DICO_FILE, INIT,IERR)
!
      END SUBROUTINE RUN_READ_CASE_WAC
!
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !>@brief Allocate all of tomawac variables
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !>@param[in] ID Id of the instance
      !>@param[out] IERR 0 if subroutine successfull,
      !!                        error id otherwise
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      SUBROUTINE RUN_ALLOCATION_WAC(ID,IERR)
!
        INTEGER,            INTENT(IN) :: ID
        INTEGER,            INTENT(OUT) :: IERR
!
        INTEGER :: EXEC_POS
!
        CALL CHECK_INSTANCE_WAC(ID,IERR)
        IF(IERR.NE.0) RETURN
!
        EXEC_POS = INSTANCE_LIST_WAC(ID)%MYPOSITION
        CALL CHECK_CALL_POSITION(EXEC_POS,'RUN_ALLOCATION_WAC',
     &                           RUN_READ_CASE_POS,
     &                           RUN_INIT_POS,IERR)
        IF(IERR.NE.0) RETURN
        INSTANCE_LIST_WAC(ID)%MYPOSITION = RUN_ALLOCATION_POS
!
        CALL RUN_ALLOCATION_WAC_D(INSTANCE_LIST_WAC(ID),IERR)
!
      END SUBROUTINE RUN_ALLOCATION_WAC
!
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !>@brief Initialise the tomawac variables
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !>@param[in] ID Id of the instance
      !>@param[out] IERR 0 if subroutine successfull,
      !!                        error id otherwise
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      SUBROUTINE RUN_INIT_WAC(ID,IERR)
!
        INTEGER,            INTENT(IN) :: ID
        INTEGER,            INTENT(OUT) :: IERR
!
        INTEGER :: EXEC_POS
!
        CALL CHECK_INSTANCE_WAC(ID,IERR)
        IF(IERR.NE.0) RETURN
!
        EXEC_POS = INSTANCE_LIST_WAC(ID)%MYPOSITION
        CALL CHECK_CALL_POSITION(EXEC_POS,'RUN_INIT_WAC',
     &                           RUN_ALLOCATION_POS,
     &                           RUN_TIMESTEP_POS,IERR)
        INSTANCE_LIST_WAC(ID)%MYPOSITION = RUN_INIT_POS
!
        CALL RUN_INIT_WAC_D(INSTANCE_LIST_WAC(ID),IERR)
      END SUBROUTINE RUN_INIT_WAC
!
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !>@brief Run a timestep in tomawac
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !>@param[in] ID Id of the instance
      !>@param[out] IERR 0 if subroutine successfull,
      !!                        error id otherwise
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      SUBROUTINE RUN_TIMESTEP_WAC(ID,IERR)
!
        INTEGER,            INTENT(IN) :: ID
        INTEGER,            INTENT(OUT) :: IERR
!
        INTEGER :: EXEC_POS
!
        CALL CHECK_INSTANCE_WAC(ID,IERR)
        IF(IERR.NE.0) RETURN
!
        EXEC_POS = INSTANCE_LIST_WAC(ID)%MYPOSITION
        CALL CHECK_CALL_POSITION(EXEC_POS,'RUN_TIMESTEP_WAC',
     &          RUN_INIT_POS,RUN_FINALIZE_POS,IERR)
        IF(IERR.NE.0) RETURN
        INSTANCE_LIST_WAC(ID)%MYPOSITION = RUN_TIMESTEP_POS
!
        CALL RUN_TIMESTEP_WAC_D(INSTANCE_LIST_WAC(ID),IERR)
!
      END SUBROUTINE RUN_TIMESTEP_WAC
!
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !>@brief Finalize a tomawac run
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !>@param[in] ID Id of the instance
      !>@param[out] IERR 0 if subroutine successfull,
      !!                        error id otherwise
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      SUBROUTINE RUN_FINALIZE_WAC(ID,IERR)
!
        INTEGER :: EXEC_POS
!
        INTEGER,            INTENT(IN) :: ID
        INTEGER,            INTENT(OUT) :: IERR
!
        CALL CHECK_INSTANCE_WAC(ID,IERR)
        IF(IERR.NE.0) RETURN
!
        EXEC_POS = INSTANCE_LIST_WAC(ID)%MYPOSITION
        CALL CHECK_CALL_POSITION(EXEC_POS,'RUN_FINALIZE_WAC',
     &        RUN_TIMESTEP_POS,NO_POSITION,IERR)
        IF(IERR.NE.0) RETURN
        INSTANCE_LIST_WAC(ID)%MYPOSITION = RUN_FINALIZE_POS
!
        CALL RUN_FINALIZE_WAC_D(INSTANCE_LIST_WAC(ID),IERR)
        IF(IERR.NE.0) RETURN
        CALL DELETE_INSTANCE_WAC(ID,IERR)
        DEALLOCATE(VNAME_WAC)
        DEALLOCATE(VINFO_WAC)
!
      END SUBROUTINE RUN_FINALIZE_WAC
!
!***********************************************************************
!     T2D/SIS
!***********************************************************************
!
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !>@brief Run sisyphe in case of coupling : bedload vs suspension
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !>@param[in] ID_T2D The instance
      !>@param[in] ID_SIS The instance
      !>@param[out] IERR 0 if subroutine successfull,
      !!                      error id otherwise
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      SUBROUTINE RUN_TIMESTEP_SIS_CPL(ID_T2D, ID_SIS, IERR)
        INTEGER,             INTENT(IN) :: ID_T2D, ID_SIS
        INTEGER,             INTENT(OUT) :: IERR

        INTEGER CHARR_SUSP
        TYPE(INSTANCE_T2D) :: T2D


        IERR = 0
        CALL CHECK_INSTANCE_T2D(ID_T2D,IERR)
        IF(IERR.NE.0) RETURN
        CALL CHECK_INSTANCE_SIS(ID_SIS,IERR)
        IF(IERR.NE.0) RETURN

        T2D = INSTANCE_LIST_T2D(ID_T2D)

        CALL PRERES_TELEMAC2D()
        IF(T2D%LEO.AND.T2D%EQUA(1:15).NE.'SAINT-VENANT VF') THEN
          T2D%COMPLEO = T2D%COMPLEO-1
        ENDIF

        CALL CHARR_OR_SUSP(ID_T2D, ID_SIS, CHARR_SUSP, IERR)
        IF(CHARR_SUSP.EQ.1.OR.CHARR_SUSP.EQ.3) THEN
          CALL SET_VAR_SIS(ID_T2D, ID_SIS, 1, IERR)
          CALL RUN_TIMESTEP_SIS(ID_SIS,IERR)
        END IF
        IF(CHARR_SUSP.EQ.2.OR.CHARR_SUSP.EQ.3) THEN
          CALL SET_VAR_SIS(ID_T2D, ID_SIS, 2, IERR)
          CALL RUN_TIMESTEP_SIS(ID_SIS,IERR)
        END IF
        CALL SET_VAR_T2D(ID_T2D, ID_SIS, IERR)

      END SUBROUTINE RUN_TIMESTEP_SIS_CPL
!
!***********************************************************************
! TOOLS
!***********************************************************************
!
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !>@brief Identify the liquid boundaries
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !>@param[in] IKLES Connectivity array 1d form
      !>@param[in] DIM_MESH Dimension of the mesh
      !>@param[in] NPTFR Number of boundary points
      !>@param[in] NPOIN Number of points
      !>@param[in,out] NELEM2 Number of 2d elements
      !>@param[in] LIUBOR Boundary value for velocity
      !>@param[in] LIHBOR Boundary value for height
      !>@param[in,out] NBOR Boundary numbering array
      !>@param[in] COORD Coordinates
      !>@param[out] NELBOR Number of boundary elements
      !>@param[out] IFABOR Array for boundaries
      !>@param[out] KP1BOR Neigbouring boundary nodes array
      !>@param[out] NUMLIQ Array for liquid boundaries
      !>@param[in] LISTIN If true display boundaries info
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      SUBROUTINE IDENTIFY_LIQ_BND(
     &  IKLES, DIM_MESH, NPTFR, NPOIN, NELEM2,
     &  LIUBOR, LIHBOR, NBOR, COORD,
     &  NELBOR, IFABOR, KP1BOR, NUMLIQ, LISTIN)
!
        USE MOD_NUMBERING_OPEN_BOUNDARIES
        USE DECLARATIONS_PARTEL
        USE DECLARATIONS_SPECIAL
        USE BIEF, ONLY: NCSIZE
!
        IMPLICIT NONE
!
        INTEGER, INTENT(IN) :: DIM_MESH, NPOIN
        INTEGER, INTENT(IN) :: NPTFR, NELEM2
        INTEGER, INTENT(INOUT) :: NBOR(NPTFR)
        DOUBLE PRECISION, INTENT(IN) :: COORD(NPOIN*2)
        INTEGER, INTENT(IN) :: IKLES(NELEM2*3)
        INTEGER, INTENT(IN) :: LIUBOR(NPTFR), LIHBOR(NPTFR)
        INTEGER, INTENT(OUT) :: NELBOR(NPTFR),
     &    KP1BOR(NPTFR,2), IFABOR(NELEM2,3), NUMLIQ(NPTFR)
        LOGICAL, INTENT(IN) :: LISTIN
!
        CHARACTER(LEN=PATH_LEN) :: NAMEINP
        INTEGER, ALLOCATABLE :: IKLE(:,:)
        INTEGER, ALLOCATABLE :: LNELBOR(:),
     &    LKP1BOR(:,:), LIFABOR(:,:), LNUMLIQ(:)
        INTEGER :: LNPTFR, LNELEM2

        NAMEINP = REPEAT(' ', PATH_LEN)
        LU = 6
        LNG = 2
        ! SET NCSIZE TO 1 TO USE VOISIN AND READ_MESH_INFO IN SERIAL MODE
        NCSIZE = 1
        LNPTFR = NPTFR
        LNELEM2 = NELEM2

        CALL NUMBERING_OPEN_BOUNDARIES(
     &      NAMEINP, IKLE, IKLES, LKP1BOR, LNUMLIQ, DIM_MESH, NPOIN,
     &      LNPTFR, NPOIN, LNELEM2, LNELBOR, LIUBOR, LIHBOR, NBOR,
     &      LIFABOR, COORD, LISTIN)

        NELBOR = LNELBOR
        KP1BOR = LKP1BOR
        IFABOR = LIFABOR
        NUMLIQ = LNUMLIQ

        DEALLOCATE(IKLE)
        DEALLOCATE(LNELBOR)
        DEALLOCATE(LKP1BOR)
        DEALLOCATE(LIFABOR)
        DEALLOCATE(LNUMLIQ)


      END SUBROUTINE IDENTIFY_LIQ_BND

      END MODULE API_INTERFACE
