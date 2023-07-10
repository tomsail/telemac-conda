!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!>@brief module handling all the instance function
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      MODULE API_INSTANCE_ART
!
      USE API_HANDLE_ERROR
      USE BIEF
      USE DECLARATIONS_SPECIAL, ONLY : MAXKEYWORD
      USE DECLARATIONS_ARTEMIS
      IMPLICIT NONE

      PRIVATE

      PUBLIC :: CREATE_INSTANCE_ART
      PUBLIC :: DELETE_INSTANCE_ART
      PUBLIC :: CHECK_INSTANCE_ART
      PUBLIC :: GET_INSTANCE_ERROR_ART
      PUBLIC :: INSTANCE_ART
      PUBLIC :: INSTANCE_LIST_ART
!

      TYPE INSTANCE_ART
        ! RUN POSITION
        INTEGER MYPOSITION
        ! ERROR MESSAGE
        CHARACTER(LEN=200) :: ERROR_MESSAGE
        ! LIST OF ALL THE VARIABLE FOR MODEL
!
        TYPE(BIEF_MESH), POINTER :: MESH
!
        TYPE(BIEF_OBJ), POINTER :: LIHBOR
        TYPE(BIEF_OBJ), POINTER :: LIUBOR
        TYPE(BIEF_OBJ), POINTER :: LIVBOR
        TYPE(BIEF_OBJ), POINTER :: NUMLIQ
!
        INTEGER         :: NIT
!
        TYPE(BIEF_FILE), POINTER :: ART_FILES(:)
        INTEGER :: MAXLU_ART
        INTEGER :: MAXKEYWORD
        INTEGER, POINTER :: ARTRES
        INTEGER, POINTER :: ARTGEO
        INTEGER, POINTER :: ARTCLI

        INTEGER         :: NBMAXNSHARE
        INTEGER,        POINTER :: NPTIR
!
        INTEGER, POINTER :: DEBUG
        TYPE(BIEF_OBJ), POINTER :: PHAS
        !<new_var>
!
      END TYPE ! MODEL_ART
!
      INTEGER, PARAMETER :: MAX_INSTANCES=10
      TYPE(INSTANCE_ART), POINTER :: INSTANCE_LIST_ART(:)
      LOGICAL, ALLOCATABLE :: USED_INSTANCE(:)
!
      CONTAINS
!
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !>@brief Creates a telemac2d instance
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !>@param[out] ID Id of the new instance
      !>@param[out] IERR 0 if subroutine successfull,
      !!                   error id otherwise
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      SUBROUTINE CREATE_INSTANCE_ART(ID,IERR)
      ! initialise instance for telemac2d
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
          ALLOCATE(INSTANCE_LIST_ART(MAX_INSTANCES),STAT=IERR)
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
        INSTANCE_LIST_ART(ID)%MYPOSITION = NO_POSITION
!       Link with telemac2d variables
        CALL UPDATE_INSTANCE_ART(ID,IERR)

      END SUBROUTINE CREATE_INSTANCE_ART
!
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !>@brief Updates a telemac2d instance
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !>@param[out] ID Id of the new instance
      !>@param[out] IERR 0 if subroutine successfull,
      !!                   error id otherwise
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      SUBROUTINE UPDATE_INSTANCE_ART(ID,IERR)
      ! initialise instance for telemac2d
        INTEGER, INTENT(IN) :: ID
        INTEGER, INTENT(OUT) :: IERR
!
        IERR = 0
!       Link with telemac2d variables
!
        INSTANCE_LIST_ART(ID)%MESH   => MESH
        INSTANCE_LIST_ART(ID)%LIHBOR => LIHBOR
        INSTANCE_LIST_ART(ID)%LIUBOR => LIUBOR
        INSTANCE_LIST_ART(ID)%LIVBOR => LIVBOR
        INSTANCE_LIST_ART(ID)%NUMLIQ => NUMLIQ
!
        INSTANCE_LIST_ART(ID)%ART_FILES => ART_FILES
        INSTANCE_LIST_ART(ID)%ARTRES => ARTRES
        INSTANCE_LIST_ART(ID)%ARTGEO => ARTGEO
        INSTANCE_LIST_ART(ID)%ARTCLI => ARTCLI
        INSTANCE_LIST_ART(ID)%MAXLU_ART = MAXLU_ART
        INSTANCE_LIST_ART(ID)%MAXKEYWORD = MAXKEYWORD
        INSTANCE_LIST_ART(ID)%PHAS => PHAS

        INSTANCE_LIST_ART(ID)%NPTIR => NPTIR
        INSTANCE_LIST_ART(ID)%NBMAXNSHARE = NBMAXNSHARE
        !<new_link>
!

      END SUBROUTINE UPDATE_INSTANCE_ART
!
!
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !>@brief Deletes a telemac2d instance
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !>@param[in] ID Id of the instance
      !>@param[out] IERR 0 if subroutine successfull,
      !!                   error id otherwise
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      SUBROUTINE DELETE_INSTANCE_ART(ID,IERR)
        INTEGER, INTENT(IN) :: ID
        INTEGER, INTENT(OUT) :: IERR
!
        IERR = 0
        !
        CALL CHECK_INSTANCE_ART(ID,IERR)
        IF(IERR.NE.0) RETURN
        USED_INSTANCE(ID) = .FALSE.
      END SUBROUTINE DELETE_INSTANCE_ART
!
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !>@brief Check if the id is following convention
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !>@param[in] ID Id of the instance
      !>@param[out] IERR 0 if subroutine successfull,
      !!                   error id otherwise
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      SUBROUTINE CHECK_INSTANCE_ART(ID,IERR)
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
        CALL UPDATE_INSTANCE_ART(ID,IERR)
      END SUBROUTINE CHECK_INSTANCE_ART
!
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !>@brief Returns the error message of the instance
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !>@param[in] ID Id of the instance
      !>@param[out] MESS The error message
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      SUBROUTINE GET_INSTANCE_ERROR_ART(ID,MESS)
        INTEGER, INTENT(IN) :: ID
        CHARACTER(LEN=200), INTENT(OUT) :: MESS
!
        MESS = INSTANCE_LIST_ART(ID)%ERROR_MESSAGE
!
      END SUBROUTINE GET_INSTANCE_ERROR_ART
      END MODULE API_INSTANCE_ART
