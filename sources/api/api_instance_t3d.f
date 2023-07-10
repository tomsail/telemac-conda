!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!>@brief module handling all the instance function
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      MODULE API_INSTANCE_T3D
!
      USE API_HANDLE_ERROR
      USE BIEF
      USE DECLARATIONS_TELEMAC, ONLY : COUPLING
      USE DECLARATIONS_SPECIAL, ONLY : MAXKEYWORD
      USE DECLARATIONS_TELEMAC3D
      USE DECLARATIONS_WAQTEL,  ONLY : WAQ_FILES,MAXLU_WAQ,
     &                                 CP_EAU,C_ATMOS
      USE DECLARATIONS_GAIA, ONLY : GAI_FILES,MAXLU_GAI,E,Q,DCLA,AC,
     &             XWC,QBOR,EBOR,TOB,CLU,CLV,LIQBOR,
     &             NSICLA,NOMBLAY,PARTHENIADES,LIEBOR
      USE METEO_TELEMAC !, ONLY: WINDX,WINDY,TAIR
      IMPLICIT NONE

      PRIVATE

      PUBLIC :: CREATE_INSTANCE_T3D
      PUBLIC :: DELETE_INSTANCE_T3D
      PUBLIC :: CHECK_INSTANCE_T3D
      PUBLIC :: GET_INSTANCE_ERROR_T3D
      PUBLIC :: INSTANCE_T3D
      PUBLIC :: INSTANCE_LIST_T3D
!

      TYPE INSTANCE_T3D
        ! RUN POSITION
        INTEGER MYPOSITION
        ! ERROR MESSAGE
        CHARACTER(LEN=200) :: ERROR_MESSAGE
        ! LIST OF ALL THE VARIABLE FOR MODEL
        TYPE(BIEF_OBJ), POINTER :: HBOR
        TYPE(BIEF_OBJ), POINTER :: UBOR2D
        TYPE(BIEF_OBJ), POINTER :: VBOR2D
!
        TYPE(BIEF_OBJ), POINTER :: UBORF
        TYPE(BIEF_OBJ), POINTER :: VBORF
        TYPE(BIEF_OBJ), POINTER :: WBORF
!
        TYPE(BIEF_OBJ), POINTER :: UBORL
        TYPE(BIEF_OBJ), POINTER :: VBORL
        TYPE(BIEF_OBJ), POINTER :: WBORL
!
        TYPE(BIEF_OBJ), POINTER :: UBORS
        TYPE(BIEF_OBJ), POINTER :: VBORS
        TYPE(BIEF_OBJ), POINTER :: WBORS
!
        TYPE(BIEF_OBJ), POINTER :: U
        TYPE(BIEF_OBJ), POINTER :: V
        TYPE(BIEF_OBJ), POINTER :: W

        TYPE(BIEF_OBJ), POINTER :: AK
        TYPE(BIEF_OBJ), POINTER :: EP
        TYPE(BIEF_OBJ), POINTER :: AKN
        TYPE(BIEF_OBJ), POINTER :: EPN

        TYPE(BIEF_OBJ), POINTER :: RUGOF

        DOUBLE PRECISION, POINTER :: FLUX_BOUNDARIES(:)
        DOUBLE PRECISION, POINTER :: COTIMP(:)
        DOUBLE PRECISION, POINTER :: DEBIMP(:)
        DOUBLE PRECISION, POINTER :: VITIMP(:)
!
        DOUBLE PRECISION, POINTER :: QSCE(:)
        DOUBLE PRECISION, POINTER :: USCE(:)
        DOUBLE PRECISION, POINTER :: VSCE(:)
        DOUBLE PRECISION, POINTER :: WSCE(:)
        DOUBLE PRECISION, POINTER :: XSCE(:)
        DOUBLE PRECISION, POINTER :: YSCE(:)
        DOUBLE PRECISION, POINTER :: ZSCE(:)
        DOUBLE PRECISION, POINTER :: TASCE(:,:)
!
        DOUBLE PRECISION, POINTER :: BETAC(:)
        DOUBLE PRECISION, POINTER :: T0AC(:)
        DOUBLE PRECISION, POINTER :: TRACER(:)
        DOUBLE PRECISION, POINTER :: TRAC0(:)
!
        TYPE(BIEF_MESH), POINTER :: MESH2D
        TYPE(BIEF_MESH), POINTER :: MESH3D
!
        TYPE(BIEF_OBJ), POINTER :: LIHBOR
!
        TYPE(BIEF_OBJ), POINTER :: LIUBOF
        TYPE(BIEF_OBJ), POINTER :: LIVBOF
        TYPE(BIEF_OBJ), POINTER :: LIWBOF
!
        TYPE(BIEF_OBJ), POINTER :: LIUBOL
        TYPE(BIEF_OBJ), POINTER :: LIVBOL
        TYPE(BIEF_OBJ), POINTER :: LIWBOL
!
        TYPE(BIEF_OBJ), POINTER :: LIUBOS
        TYPE(BIEF_OBJ), POINTER :: LIVBOS
        TYPE(BIEF_OBJ), POINTER :: LIWBOS
!
        TYPE(BIEF_OBJ), POINTER :: NUMLIQ
        TYPE(BIEF_OBJ), POINTER :: BOUNDARY_COLOUR
!
        INTEGER,        POINTER :: NIT
        INTEGER,        POINTER :: LT
        DOUBLE PRECISION,POINTER :: AT
        DOUBLE PRECISION,POINTER :: RHO0
        DOUBLE PRECISION,POINTER :: DUREE
        DOUBLE PRECISION,POINTER :: DT
!
        INTEGER,        POINTER :: GRAPRD
        INTEGER,        POINTER :: LISPRD
!
        TYPE(BIEF_FILE), POINTER :: T3D_FILES(:)
        INTEGER :: MAXLU_T3D
!
!       WAQTEL VARIABLES
!
        TYPE(BIEF_FILE), POINTER :: WAQ_FILES(:)
        INTEGER :: MAXLU_WAQ
        DOUBLE PRECISION, POINTER :: C_ATMOS
        TYPE(BIEF_OBJ), POINTER   :: TAIR
        DOUBLE PRECISION, POINTER :: CP_EAU
!
!       GAIA VARIABLES
!
        TYPE(BIEF_FILE), POINTER :: GAI_FILES(:)
        INTEGER :: MAXLU_GAI
!
        INTEGER :: MAXKEYWORD
        INTEGER, POINTER :: T3DRES
        INTEGER, POINTER :: T3DHYD
        INTEGER, POINTER :: T3DGEO
        INTEGER, POINTER :: T3DCLI
!
        CHARACTER(LEN=PATH_LEN), POINTER :: COUPLING
        CHARACTER(LEN=20), POINTER :: EQUA
!
        TYPE(BIEF_OBJ), POINTER :: H
        TYPE(BIEF_OBJ), POINTER :: DH
        TYPE(BIEF_OBJ), POINTER :: TA
        TYPE(BIEF_OBJ), POINTER :: WINDX,WINDY
!
        INTEGER, POINTER :: DEBUG
        ! LIST OF ALL THE VARIABLE FOR STATE
        INTEGER, POINTER :: BND_TIDE(:)
        DOUBLE PRECISION, POINTER :: CTIDE
        DOUBLE PRECISION, POINTER :: CTIDEV
        DOUBLE PRECISION, POINTER :: MSL

        DOUBLE PRECISION, POINTER :: PRANDTL

        INTEGER         :: NBMAXNSHARE
        INTEGER,        POINTER :: NPTIR
        INTEGER,        POINTER :: NPLAN

        INTEGER,        POINTER :: NTRAC
        INTEGER,        POINTER :: MAXTRA
        INTEGER,        POINTER :: MAXSCE
        TYPE(BIEF_OBJ), POINTER :: E
        TYPE(BIEF_OBJ), POINTER :: ZF
        TYPE(BIEF_OBJ), POINTER :: Q
        DOUBLE PRECISION, POINTER :: DCLA(:)
        DOUBLE PRECISION, POINTER :: AC(:)
        DOUBLE PRECISION, POINTER :: XWC(:)
        TYPE(BIEF_OBJ), POINTER :: QBOR
        TYPE(BIEF_OBJ), POINTER :: EBOR
        TYPE(BIEF_OBJ), POINTER :: FLBOR
        TYPE(BIEF_OBJ), POINTER :: TOB
        TYPE(BIEF_OBJ), POINTER :: CLU
        TYPE(BIEF_OBJ), POINTER :: CLV
        TYPE(BIEF_OBJ), POINTER :: LIQBOR
        TYPE(BIEF_OBJ), POINTER :: LIEBOR
        INTEGER, POINTER :: NSICLA
        INTEGER, POINTER :: NOMBLAY
        DOUBLE PRECISION, POINTER :: PARTHENIADES(:,:)
        !<new_var>
!

      END TYPE ! MODEL_T3D
!
      INTEGER, PARAMETER :: MAX_INSTANCES=10
      TYPE(INSTANCE_T3D), POINTER :: INSTANCE_LIST_T3D(:)
      LOGICAL, ALLOCATABLE :: USED_INSTANCE(:)
!
      CONTAINS
!
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !>@brief Creates a TELEMAC3D instance
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !>@param[out] ID Id of the new instance
      !>@param[out] IERR 0 if subroutine successfull,
      !!                   error id otherwise
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      SUBROUTINE CREATE_INSTANCE_T3D(ID,IERR)
      ! initialise instance for TELEMAC3D
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
          ALLOCATE(INSTANCE_LIST_T3D(MAX_INSTANCES),STAT=IERR)
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
        INSTANCE_LIST_T3D(ID)%MYPOSITION = NO_POSITION
!       Link with TELEMAC3D variables
        CALL UPDATE_INSTANCE_T3D(ID,IERR)

      END SUBROUTINE CREATE_INSTANCE_T3D
!
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !>@brief Updates a TELEMAC3D instance
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !>@param[out] ID Id of the new instance
      !>@param[out] IERR 0 if subroutine successfull,
      !!                   error id otherwise
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      SUBROUTINE UPDATE_INSTANCE_T3D(ID,IERR)
      ! initialise instance for TELEMAC3D
        INTEGER, INTENT(IN) :: ID
        INTEGER, INTENT(OUT) :: IERR
!
        IERR = 0
!       Link with TELEMAC3D variables
        INSTANCE_LIST_T3D(ID)%HBOR   => HBOR
        INSTANCE_LIST_T3D(ID)%UBOR2D  =>  UBOR2D
        INSTANCE_LIST_T3D(ID)%VBOR2D  =>  VBOR2D
        INSTANCE_LIST_T3D(ID)%UBORF   =>  UBORF
        INSTANCE_LIST_T3D(ID)%VBORF   =>  VBORF
        INSTANCE_LIST_T3D(ID)%WBORF   =>  WBORF
        INSTANCE_LIST_T3D(ID)%UBORL   =>  UBORL
        INSTANCE_LIST_T3D(ID)%VBORL   =>  VBORL
        INSTANCE_LIST_T3D(ID)%WBORL   =>  WBORL
        INSTANCE_LIST_T3D(ID)%UBORS   =>  UBORS
        INSTANCE_LIST_T3D(ID)%VBORS   =>  VBORS
        INSTANCE_LIST_T3D(ID)%WBORS   =>  WBORS
        INSTANCE_LIST_T3D(ID)%H      =>  H
        INSTANCE_LIST_T3D(ID)%WINDX   =>  WINDX
        INSTANCE_LIST_T3D(ID)%WINDY   =>  WINDY
        INSTANCE_LIST_T3D(ID)%DH     =>  DH
        INSTANCE_LIST_T3D(ID)%U      =>  U
        INSTANCE_LIST_T3D(ID)%V      =>  V
        INSTANCE_LIST_T3D(ID)%W      =>  W
        INSTANCE_LIST_T3D(ID)%AK      =>  AK
        INSTANCE_LIST_T3D(ID)%EP      =>  EP
        INSTANCE_LIST_T3D(ID)%AK      =>  AKN
        INSTANCE_LIST_T3D(ID)%EP      =>  EPN
        INSTANCE_LIST_T3D(ID)%RUGOF   =>  RUGOF
        ! For allocatable arrays nag crashes if we try to point towards
        ! an unallocated array
        IF(ALLOCATED(FLUX_BOUNDARIES)) THEN
          INSTANCE_LIST_T3D(ID)%FLUX_BOUNDARIES => FLUX_BOUNDARIES
        ENDIF
        IF(ALLOCATED(COTIMP)) THEN
          INSTANCE_LIST_T3D(ID)%COTIMP => COTIMP
        ENDIF
        IF(ALLOCATED(DEBIMP)) THEN
          INSTANCE_LIST_T3D(ID)%DEBIMP => DEBIMP
        ENDIF
        INSTANCE_LIST_T3D(ID)%VITIMP => VITIMP
!
        IF(ALLOCATED(QSCE)) THEN
          INSTANCE_LIST_T3D(ID)%QSCE => QSCE
        ENDIF
        IF(ALLOCATED(USCE)) THEN
          INSTANCE_LIST_T3D(ID)%USCE => USCE
        ENDIF
        IF(ALLOCATED(VSCE)) THEN
          INSTANCE_LIST_T3D(ID)%VSCE => VSCE
        ENDIF
        IF(ALLOCATED(WSCE)) THEN
          INSTANCE_LIST_T3D(ID)%WSCE => WSCE
        ENDIF
        IF(ALLOCATED(XSCE)) THEN
          INSTANCE_LIST_T3D(ID)%XSCE => XSCE
        ENDIF
        IF(ALLOCATED(YSCE)) THEN
          INSTANCE_LIST_T3D(ID)%YSCE => YSCE
        ENDIF
        IF(ALLOCATED(ZSCE)) THEN
          INSTANCE_LIST_T3D(ID)%ZSCE => ZSCE
        ENDIF
        IF(ALLOCATED(TASCE)) THEN
          INSTANCE_LIST_T3D(ID)%TASCE => TASCE
        ENDIF
        IF(ALLOCATED(BETAC)) THEN
          INSTANCE_LIST_T3D(ID)%BETAC => BETAC
        ENDIF
        IF(ALLOCATED(T0AC)) THEN
          INSTANCE_LIST_T3D(ID)%T0AC => T0AC
        ENDIF
        IF(ALLOCATED(TRACER)) THEN
          INSTANCE_LIST_T3D(ID)%TRACER => TRACER
        ENDIF
        IF(ALLOCATED(TRAC0)) THEN
          INSTANCE_LIST_T3D(ID)%TRAC0 => TRAC0
        ENDIF
!
        INSTANCE_LIST_T3D(ID)%GAI_FILES => GAI_FILES
        INSTANCE_LIST_T3D(ID)%MAXLU_GAI = MAXLU_GAI
!
        INSTANCE_LIST_T3D(ID)%WAQ_FILES => WAQ_FILES
        INSTANCE_LIST_T3D(ID)%MAXLU_WAQ = MAXLU_WAQ
        INSTANCE_LIST_T3D(ID)%C_ATMOS => C_ATMOS
        INSTANCE_LIST_T3D(ID)%TAIR => TAIR
        INSTANCE_LIST_T3D(ID)%CP_EAU => CP_EAU
!
        INSTANCE_LIST_T3D(ID)%MESH2D => MESH2D
        INSTANCE_LIST_T3D(ID)%MESH3D => MESH3D
        INSTANCE_LIST_T3D(ID)%LIHBOR => LIHBOR
        INSTANCE_LIST_T3D(ID)%LIUBOF => LIUBOF
        INSTANCE_LIST_T3D(ID)%LIVBOF => LIVBOF
        INSTANCE_LIST_T3D(ID)%LIWBOF => LIWBOF
        INSTANCE_LIST_T3D(ID)%LIUBOL => LIUBOL
        INSTANCE_LIST_T3D(ID)%LIVBOL => LIVBOL
        INSTANCE_LIST_T3D(ID)%LIWBOL => LIWBOL
        INSTANCE_LIST_T3D(ID)%LIUBOS => LIUBOS
        INSTANCE_LIST_T3D(ID)%LIVBOS => LIVBOS
        INSTANCE_LIST_T3D(ID)%LIWBOS => LIWBOS
        INSTANCE_LIST_T3D(ID)%NUMLIQ => NUMLIQ
        INSTANCE_LIST_T3D(ID)%BOUNDARY_COLOUR => BOUNDARY_COLOUR
        INSTANCE_LIST_T3D(ID)%MAXKEYWORD = MAXKEYWORD

        INSTANCE_LIST_T3D(ID)%NIT    => NIT
        INSTANCE_LIST_T3D(ID)%LT    => LT
        INSTANCE_LIST_T3D(ID)%TA     => TA
        INSTANCE_LIST_T3D(ID)%AT     => AT
        INSTANCE_LIST_T3D(ID)%RHO0   => RHO0
        INSTANCE_LIST_T3D(ID)%DT     => DT
        INSTANCE_LIST_T3D(ID)%DUREE   => DUREE
        INSTANCE_LIST_T3D(ID)%EQUA   => EQUA
!
        INSTANCE_LIST_T3D(ID)%GRAPRD => GRAPRD
        INSTANCE_LIST_T3D(ID)%LISPRD => LISPRD
!
        INSTANCE_LIST_T3D(ID)%T3D_FILES => T3D_FILES
        INSTANCE_LIST_T3D(ID)%T3DRES => T3DRES
        INSTANCE_LIST_T3D(ID)%T3DHYD => T3DHYD
        INSTANCE_LIST_T3D(ID)%T3DGEO => T3DGEO
        INSTANCE_LIST_T3D(ID)%T3DCLI => T3DCLI
        INSTANCE_LIST_T3D(ID)%MAXLU_T3D = MAXLU_T3D
        INSTANCE_LIST_T3D(ID)%MAXKEYWORD = MAXKEYWORD
!
        INSTANCE_LIST_T3D(ID)%DEBUG  => DEBUG
        IF(ALLOCATED(BND_TIDE)) THEN
          INSTANCE_LIST_T3D(ID)%BND_TIDE  => BND_TIDE
        ENDIF
        INSTANCE_LIST_T3D(ID)%CTIDE  => CTIDE
        INSTANCE_LIST_T3D(ID)%CTIDEV => CTIDEV
        INSTANCE_LIST_T3D(ID)%MSL  => MSL

        INSTANCE_LIST_T3D(ID)%PRANDTL  => PRANDTL

        INSTANCE_LIST_T3D(ID)%NPTIR => NPTIR
        INSTANCE_LIST_T3D(ID)%NBMAXNSHARE = NBMAXNSHARE
        INSTANCE_LIST_T3D(ID)%NPLAN => NPLAN
        INSTANCE_LIST_T3D(ID)%NTRAC => NTRAC
        INSTANCE_LIST_T3D(ID)%MAXTRA => MAXTRA
        INSTANCE_LIST_T3D(ID)%MAXSCE => MAXSCE

        INSTANCE_LIST_T3D(ID)%COUPLING => COUPLING
        INSTANCE_LIST_T3D(ID)%E => E
        INSTANCE_LIST_T3D(ID)%ZF => ZF
        INSTANCE_LIST_T3D(ID)%Q => Q
        INSTANCE_LIST_T3D(ID)%DCLA => DCLA
        INSTANCE_LIST_T3D(ID)%AC => AC
        INSTANCE_LIST_T3D(ID)%XWC => XWC
        INSTANCE_LIST_T3D(ID)%QBOR => QBOR
        INSTANCE_LIST_T3D(ID)%EBOR => EBOR
        INSTANCE_LIST_T3D(ID)%FLBOR => FLBOR
        INSTANCE_LIST_T3D(ID)%TOB => TOB
        INSTANCE_LIST_T3D(ID)%CLU => CLU
        INSTANCE_LIST_T3D(ID)%CLV => CLV
        INSTANCE_LIST_T3D(ID)%LIQBOR => LIQBOR
        INSTANCE_LIST_T3D(ID)%LIEBOR => LIEBOR
        INSTANCE_LIST_T3D(ID)%NSICLA => NSICLA
        INSTANCE_LIST_T3D(ID)%NOMBLAY => NOMBLAY
        INSTANCE_LIST_T3D(ID)%PARTHENIADES => PARTHENIADES
        ! <new_link>
!
      END SUBROUTINE UPDATE_INSTANCE_T3D
!

      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !>@brief Deletes a TELEMAC3D instance
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !>@param[in] ID Id of the instance
      !>@param[out] IERR 0 if subroutine successfull,
      !!                   error id otherwise
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      SUBROUTINE DELETE_INSTANCE_T3D(ID,IERR)
        INTEGER, INTENT(IN) :: ID
        INTEGER, INTENT(OUT) :: IERR
!
        IERR = 0
        !
        CALL CHECK_INSTANCE_T3D(ID,IERR)
        IF(IERR.NE.0) RETURN
        USED_INSTANCE(ID) = .FALSE.
      END SUBROUTINE DELETE_INSTANCE_T3D
!
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !>@brief Check if the id is following convention
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !>@param[in] ID Id of the instance
      !>@param[out] IERR 0 if subroutine successfull,
      !!                   error id otherwise
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      SUBROUTINE CHECK_INSTANCE_T3D(ID,IERR)
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
        CALL UPDATE_INSTANCE_T3D(ID,IERR)
      END SUBROUTINE CHECK_INSTANCE_T3D
!
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !>@brief Returns the error message of the instance
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !>@param[in] ID Id of the instance
      !>@param[out] MESS The error message
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      SUBROUTINE GET_INSTANCE_ERROR_T3D(ID,MESS)
        INTEGER, INTENT(IN) :: ID
        CHARACTER(LEN=200), INTENT(OUT) :: MESS
!
        MESS = INSTANCE_LIST_T3D(ID)%ERROR_MESSAGE
!
      END SUBROUTINE GET_INSTANCE_ERROR_T3D
      END MODULE API_INSTANCE_T3D
