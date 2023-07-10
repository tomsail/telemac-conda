!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!>@brief Module handling all the instance function
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      MODULE API_INSTANCE_WAC
!
      USE API_HANDLE_ERROR
      USE BIEF
      USE DECLARATIONS_SPECIAL, ONLY : MAXKEYWORD
      USE DECLARATIONS_TOMAWAC
      IMPLICIT NONE

      PRIVATE

      PUBLIC :: CREATE_INSTANCE_WAC
      PUBLIC :: DELETE_INSTANCE_WAC
      PUBLIC :: CHECK_INSTANCE_WAC
      PUBLIC :: GET_INSTANCE_ERROR_WAC
      PUBLIC :: INSTANCE_WAC
      PUBLIC :: INSTANCE_LIST_WAC
!
      TYPE INSTANCE_WAC
        ! RUN POSITION
        INTEGER MYPOSITION
        ! ERROR MESSAGE
        CHARACTER(LEN=200) :: ERROR_MESSAGE
        INTEGER :: MAXKEYWORD
        ! LIST OF ALL THE VARIABLE FOR MODEL
!
        TYPE(BIEF_MESH), POINTER :: MESH
!
        INTEGER, POINTER :: NIT
        INTEGER, POINTER :: LT
        DOUBLE PRECISION, POINTER :: AT
        DOUBLE PRECISION, POINTER :: DT
!
        TYPE(BIEF_FILE), POINTER :: WAC_FILES(:)
        INTEGER :: MAXLU_WAC
        INTEGER, POINTER :: WACRES
        INTEGER, POINTER :: WACGEO
        INTEGER, POINTER :: WACCLI
!
        INTEGER, POINTER :: DEBUG
        DOUBLE PRECISION, POINTER :: ZF(:)

        !VARIANCE
        DOUBLE PRECISION, POINTER :: VARIAN(:)
        !PEAK FREQUECY (READ 5)
        DOUBLE PRECISION, POINTER :: PFREAD5(:)
        !PEAK FREQUECY (READ 5)
        DOUBLE PRECISION, POINTER :: PFREAD8(:)
        !ZERO CROSSING WAVE PERIOD
        DOUBLE PRECISION, POINTER :: TM02(:)
        !MEAN WAVE PERIOD
        DOUBLE PRECISION, POINTER :: TM01(:)
        ! WIND IN X DIRECTION
        DOUBLE PRECISION, POINTER :: WINDX(:)
        ! WIND IN Y DIRECTION
        DOUBLE PRECISION, POINTER :: WINDY(:)

        INTEGER         :: NBMAXNSHARE
        INTEGER,        POINTER :: NPTIR

        DOUBLE PRECISION, POINTER :: BETAM
        DOUBLE PRECISION, POINTER :: VX_CTE
        DOUBLE PRECISION, POINTER :: VY_CTE
        
      ! <new_var>
!
      END TYPE ! MODEL_WAC
!
      INTEGER, PARAMETER :: MAX_INSTANCES=10
      TYPE(INSTANCE_WAC), POINTER :: INSTANCE_LIST_WAC(:)
      LOGICAL, ALLOCATABLE :: USED_INSTANCE(:)
!
      CONTAINS
!
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !>@brief Creates a tomawac instance
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !>@param[out] ID Id of the new instance
      !>@param[out] IERR 0 if subroutine successfull,
      !!                   error id otherwise
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      SUBROUTINE CREATE_INSTANCE_WAC(ID,IERR)
      ! initialise instance for tomawac
        INTEGER, INTENT(OUT) :: ID
        INTEGER, INTENT(OUT) :: IERR
!
        INTEGER :: I
        ID = 0
        IERR = 0
        ! If first time createing an instance allocating the instance array
        IF(.NOT. ALLOCATED(USED_INSTANCE)) THEN
          ALLOCATE(USED_INSTANCE(MAX_INSTANCES),STAT=IERR)
          IF(IERR.NE.0) THEN
            ERR_MESS = 'ERROR WHILE ALLOCATING USED INSTANCE ARRAY'
            RETURN
          ENDIF
          USED_INSTANCE = .FALSE.
          ALLOCATE(INSTANCE_LIST_WAC(MAX_INSTANCES),STAT=IERR)
          IF(IERR.NE.0) THEN
            ERR_MESS = 'ERROR WHILE ALLOCATING INSTANCE ARRAY'
            RETURN
          ENDIF
        ENDIF
!
        ! look for the first instance available
        I = 1
        DO WHILE(USED_INSTANCE(I).AND.I.LE.MAX_INSTANCES)
          I = I + 1
        ENDDO
        ID = I
        USED_INSTANCE(ID) = .TRUE.
!
        ! if still equals 0 no available instance was found then we crash
        IF(ID.EQ.(MAX_INSTANCES+1))THEN
          IERR = MAX_INSTANCE_ERROR
          ERR_MESS = "MAX INSTANCE REACHED "
          RETURN
        ENDIF
        !
        INSTANCE_LIST_WAC(ID)%MYPOSITION = NO_POSITION
!       Link with tomawac variables
        CALL UPDATE_INSTANCE_WAC(ID,IERR)

      END SUBROUTINE CREATE_INSTANCE_WAC
!
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !>@brief Updates a tomawac instance
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !>@param[out] ID Id of the new instance
      !>@param[out] IERR 0 if subroutine successfull,
      !!                   error id otherwise
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      SUBROUTINE UPDATE_INSTANCE_WAC(ID,IERR)
      ! initialise instance for tomawac
        INTEGER, INTENT(IN) :: ID
        INTEGER, INTENT(OUT) :: IERR
!
        IERR = 0
!       Link with tomawac variables
        INSTANCE_LIST_WAC(ID)%MESH   => MESH
        INSTANCE_LIST_WAC(ID)%MAXLU_WAC = MAXLU_WAC
!
        INSTANCE_LIST_WAC(ID)%NIT    => NIT
        INSTANCE_LIST_WAC(ID)%LT     => LT
        INSTANCE_LIST_WAC(ID)%AT     => AT
        INSTANCE_LIST_WAC(ID)%DT     => DT
        INSTANCE_LIST_WAC(ID)%BETAM  => BETAM
        INSTANCE_LIST_WAC(ID)%VX_CTE  => VX_CTE
        INSTANCE_LIST_WAC(ID)%VY_CTE  => VY_CTE
!
        INSTANCE_LIST_WAC(ID)%WAC_FILES => WAC_FILES
        INSTANCE_LIST_WAC(ID)%WACRES => WACRES
        INSTANCE_LIST_WAC(ID)%WACGEO => WACGEO
        INSTANCE_LIST_WAC(ID)%WACCLI => WACCLI
        INSTANCE_LIST_WAC(ID)%MAXKEYWORD = MAXKEYWORD
        INSTANCE_LIST_WAC(ID)%ZF => ZF

        INSTANCE_LIST_WAC(ID)%NPTIR => NPTIR
        INSTANCE_LIST_WAC(ID)%NBMAXNSHARE = NBMAXNSHARE
!       IMDC, ABR: List of new variables
        INSTANCE_LIST_WAC(ID)%VARIAN => VARIAN
        INSTANCE_LIST_WAC(ID)%TM01 => PTM01
        INSTANCE_LIST_WAC(ID)%TM02 => PTM02
        INSTANCE_LIST_WAC(ID)%PFREAD5 => FREA5
        INSTANCE_LIST_WAC(ID)%PFREAD8 => FREA8
        INSTANCE_LIST_WAC(ID)%WINDX => SUV%R
        INSTANCE_LIST_WAC(ID)%WINDY => SVV%R
        ! <new_link>
!
      END SUBROUTINE UPDATE_INSTANCE_WAC
!
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !>@brief Deletes a tomawac instance
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !>@param[in] ID Id of the instance
      !>@param[out] IERR 0 if subroutine successfull,
      !!                   error id otherwise
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      SUBROUTINE DELETE_INSTANCE_WAC(ID,IERR)
        INTEGER, INTENT(IN) :: ID
        INTEGER, INTENT(OUT) :: IERR
!
        IERR = 0
        !
        CALL CHECK_INSTANCE_WAC(ID,IERR)
        IF(IERR.NE.0) RETURN
        USED_INSTANCE(ID) = .FALSE.
      END SUBROUTINE DELETE_INSTANCE_WAC
!
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !>@brief Check if the id is following convention
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !>@param[in] ID Id of the instance
      !>@param[out] IERR 0 if subroutine successfull,
      !!                   error id otherwise
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      SUBROUTINE CHECK_INSTANCE_WAC(ID,IERR)
        INTEGER, INTENT(IN) :: ID
        INTEGER, INTENT(OUT) :: IERR
!
        IERR = 0
        IF(ID.LE.0 .OR. ID.GT.MAX_INSTANCES) THEN
          IERR = INVALID_INSTANCE_NUM_ERROR
          ERR_MESS = 'INVALID INSTANCE NUMBER'
          RETURN
        ENDIF
        IF(.NOT.USED_INSTANCE(ID)) THEN
          IERR = UNUSED_INSTANCE_ERROR
          ERR_MESS = 'INSTANCE NUMBER WAS NOT CREATED'
          RETURN
        ENDIF
        CALL UPDATE_INSTANCE_WAC(ID,IERR)
      END SUBROUTINE CHECK_INSTANCE_WAC
!
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !>@brief Returns the error message of the instance
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !>@param[in] ID Id of the instance
      !>@param[out] MESS The error message
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      SUBROUTINE GET_INSTANCE_ERROR_WAC(ID,MESS)
        INTEGER, INTENT(IN) :: ID
        CHARACTER(LEN=200), INTENT(OUT) :: MESS
!
        MESS = INSTANCE_LIST_WAC(ID)%ERROR_MESSAGE
!
      END SUBROUTINE GET_INSTANCE_ERROR_WAC
      END MODULE API_INSTANCE_WAC
