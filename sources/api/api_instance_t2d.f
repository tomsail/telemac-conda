!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!>@brief Module handling all the instance function
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      MODULE API_INSTANCE_T2D
!
      USE API_HANDLE_ERROR
      USE BIEF
      USE DECLARATIONS_TELEMAC, ONLY : COUPLING
      USE DECLARATIONS_SPECIAL, ONLY : MAXKEYWORD
      USE DECLARATIONS_TELEMAC2D
      USE DECLARATIONS_GAIA, ONLY : GAI_FILES,MAXLU_GAI,Q,DCLA,AC,XWC,
     &              Z,TOB,LIQBOR,LIEBOR,NSICLA,E,PARTHENIADES,NOMBLAY,
     &              MPM,BETA2,PHISED,ALPHA,XKV0,KSPRATIO
      IMPLICIT NONE

      PRIVATE

      PUBLIC :: CREATE_INSTANCE_T2D
      PUBLIC :: DELETE_INSTANCE_T2D
      PUBLIC :: CHECK_INSTANCE_T2D
      PUBLIC :: GET_INSTANCE_ERROR_T2D
      PUBLIC :: INSTANCE_T2D
      PUBLIC :: INSTANCE_LIST_T2D
      PUBLIC :: CPL_INIT_T2D
!

      ! TYPE FOR API COUPLED CALL
      TYPE SISYPHE_CPL
        INTEGER, POINTER :: LT, LEOPRD, LISPRD, NIT
        TYPE(BIEF_OBJ), POINTER :: U, V, H, HN, HPROP
        TYPE(BIEF_OBJ), POINTER :: ZF, CF, CHESTR
        TYPE(API_CPL), POINTER :: SIS_CPL
        INTEGER, POINTER :: PERCOU
        DOUBLE PRECISION, POINTER :: AT
        TYPE(BIEF_OBJ), POINTER :: VISC
        DOUBLE PRECISION, POINTER :: DT
        TYPE(BIEF_OBJ), POINTER :: FLBOR,DM1
        INTEGER, POINTER :: SOLSYS
        TYPE(BIEF_OBJ), POINTER :: USIS, VSIS, ZCONV
        TYPE(BIEF_OBJ), POINTER :: DIRMOY, HM0, TPR5, ORBVEL
      END TYPE SISYPHE_CPL


      TYPE INSTANCE_T2D
        ! RUN POSITION
        INTEGER MYPOSITION
        ! ERROR MESSAGE
        CHARACTER(LEN=200) :: ERROR_MESSAGE
        ! LIST OF ALL THE VARIABLE FOR MODEL
        TYPE(BIEF_OBJ), POINTER :: HBOR
        TYPE(BIEF_OBJ), POINTER :: UBOR
        TYPE(BIEF_OBJ), POINTER :: VBOR
        TYPE(BIEF_OBJ), POINTER :: U
        TYPE(BIEF_OBJ), POINTER :: V
        TYPE(BIEF_OBJ), POINTER :: CHESTR
        DOUBLE PRECISION, POINTER :: FLUX_BOUNDARIES(:)
        DOUBLE PRECISION, POINTER :: COTE(:)
        DOUBLE PRECISION, POINTER :: DEBIT(:)
!
        TYPE(BIEF_MESH), POINTER :: MESH
!
        TYPE(BIEF_OBJ), POINTER :: LIHBOR
        TYPE(BIEF_OBJ), POINTER :: LIUBOR
        TYPE(BIEF_OBJ), POINTER :: LIVBOR
        TYPE(BIEF_OBJ), POINTER :: NUMLIQ
!
        INTEGER,        POINTER :: NIT
        INTEGER,        POINTER :: LT
        INTEGER         :: NBMAXNSHARE
        INTEGER,        POINTER :: COMPLEO
        LOGICAL,        POINTER :: LEO
        INTEGER,        POINTER :: NPTIR
        DOUBLE PRECISION,POINTER :: AT
        DOUBLE PRECISION,POINTER :: TMAX
        DOUBLE PRECISION,POINTER :: DT
        DOUBLE PRECISION,POINTER :: FAIR
!
        TYPE(BIEF_FILE), POINTER :: T2D_FILES(:)
        INTEGER :: MAXLU_T2D
        INTEGER :: MAXKEYWORD
        INTEGER, POINTER :: T2DRES
        INTEGER, POINTER :: T2DGEO
        INTEGER, POINTER :: T2DCLI
        INTEGER, POINTER :: T2ATMB
        INTEGER, POINTER :: T2DFO2
        INTEGER, POINTER :: T2DIMP
        INTEGER, POINTER :: T2DPRE

!
        CHARACTER(LEN=PATH_LEN), POINTER :: COUPLING
        CHARACTER(LEN=20), POINTER :: EQUA
!

        TYPE(BIEF_OBJ), POINTER :: TE5
        TYPE(BIEF_OBJ), POINTER :: ZF
        TYPE(BIEF_OBJ), POINTER :: H
        TYPE(BIEF_OBJ), POINTER :: DH
        TYPE(BIEF_OBJ), POINTER :: DU
        TYPE(BIEF_OBJ), POINTER :: DV
        TYPE(BIEF_OBJ), POINTER :: DHN
        INTEGER, POINTER :: IORDRH
        INTEGER, POINTER :: IORDRU
!
        INTEGER, POINTER :: DEBUG
        ! LIST OF ALL THE VARIABLE FOR STATE
        INTEGER :: TRUC
        INTEGER, POINTER :: BND_TIDE(:)
        DOUBLE PRECISION, POINTER :: CTIDE
        DOUBLE PRECISION, POINTER :: CTIDEV
        DOUBLE PRECISION, POINTER :: MSL
!
        TYPE(BIEF_OBJ), POINTER :: AK
        TYPE(BIEF_OBJ), POINTER :: EP
        TYPE(BIEF_OBJ), POINTER :: VISCSA
        DOUBLE PRECISION, POINTER :: PROPNU
        INTEGER,        POINTER :: PTINIG
        INTEGER, POINTER :: ITURB

        LOGICAL, POINTER :: SECCURRENTS
        TYPE(BIEF_OBJ), POINTER :: SEC_R
        LOGICAL, POINTER :: NESTOR
        TYPE(BIEF_OBJ), POINTER :: ZRL

        INTEGER, POINTER :: RSTPRD
        INTEGER, POINTER :: RESTART_RECORD

        !VARIABLES FOR SISYPHE CALL, NECESSARY FOR THE COUPLING
        TYPE(SISYPHE_CPL) :: SIS

        !TEMPORARY FOR SISYPHE CALL
        LOGICAL :: SUSP1, CHARR_TEL
        LOGICAL :: CHARR_SIS, SUSP_SIS
        INTEGER :: LEOPRD_CHARR

        TYPE(BIEF_OBJ), POINTER :: H0
        TYPE(BIEF_OBJ), POINTER :: T
        INTEGER, POINTER :: NTRAC
        !
        ! Element of system solved in propag
        !
        TYPE(BIEF_OBJ), POINTER :: AM1
        TYPE(BIEF_OBJ), POINTER :: AM2
        TYPE(BIEF_OBJ), POINTER :: AM3
        TYPE(BIEF_OBJ), POINTER :: BM1
        TYPE(BIEF_OBJ), POINTER :: BM2
        TYPE(BIEF_OBJ), POINTER :: CM1
        TYPE(BIEF_OBJ), POINTER :: CM2
        TYPE(BIEF_OBJ), POINTER :: A23
        TYPE(BIEF_OBJ), POINTER :: A32
        TYPE(BIEF_OBJ), POINTER :: CV1
        TYPE(BIEF_OBJ), POINTER :: CV2
        TYPE(BIEF_OBJ), POINTER :: CV3
        !
        ! Gaia parameters
        !
        TYPE(BIEF_FILE), POINTER :: GAI_FILES(:)
        INTEGER :: MAXLU_GAI
        TYPE(BIEF_OBJ), POINTER :: Q
        DOUBLE PRECISION, POINTER :: DCLA(:)
        DOUBLE PRECISION, POINTER :: AC(:)
        DOUBLE PRECISION, POINTER :: XWC(:)
        TYPE(BIEF_OBJ), POINTER :: Z
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
        TYPE(BIEF_OBJ), POINTER :: E
        DOUBLE PRECISION, POINTER :: PARTHENIADES(:,:)
        INTEGER, POINTER :: MARDAT(:)
        INTEGER, POINTER :: MARTIM(:)
        LOGICAL, POINTER :: RAZTIM
        INTEGER, POINTER :: START_RECORD
        TYPE(BIEF_OBJ), POINTER :: VOLU2D
        DOUBLE PRECISION, POINTER :: MPM
        DOUBLE PRECISION, POINTER :: BETA2
        DOUBLE PRECISION, POINTER :: PHISED
        DOUBLE PRECISION, POINTER :: ALPHA
        DOUBLE PRECISION, POINTER :: XKV0(:)
        DOUBLE PRECISION, POINTER :: KSPRATIO
        ! <new_var>
      END TYPE ! MODEL_T2D
!
      INTEGER, PARAMETER :: MAX_INSTANCES=10
      TYPE(INSTANCE_T2D), POINTER :: INSTANCE_LIST_T2D(:)
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
      SUBROUTINE CREATE_INSTANCE_T2D(ID,IERR)
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
          ALLOCATE(INSTANCE_LIST_T2D(MAX_INSTANCES),STAT=IERR)
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
        INSTANCE_LIST_T2D(ID)%MYPOSITION = NO_POSITION
!       Link with telemac2d variables
        CALL UPDATE_INSTANCE_T2D(ID,IERR)

      END SUBROUTINE CREATE_INSTANCE_T2D
!
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !>@brief Updates a telemac2d instance
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !>@param[out] ID Id of the new instance
      !>@param[out] IERR 0 if subroutine successfull,
      !!                   error id otherwise
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      SUBROUTINE UPDATE_INSTANCE_T2D(ID,IERR)
      ! initialise instance for telemac2d
        INTEGER, INTENT(IN) :: ID
        INTEGER, INTENT(OUT) :: IERR
!
        IERR = 0
!       Link with telemac2d variables
        INSTANCE_LIST_T2D(ID)%HBOR   => HBOR
        INSTANCE_LIST_T2D(ID)%UBOR   =>  UBOR
        INSTANCE_LIST_T2D(ID)%VBOR   =>  VBOR
        INSTANCE_LIST_T2D(ID)%H      =>  H
        INSTANCE_LIST_T2D(ID)%DH     =>  DH
        INSTANCE_LIST_T2D(ID)%U      =>  U
        INSTANCE_LIST_T2D(ID)%V      =>  V
        INSTANCE_LIST_T2D(ID)%DU     =>  DU
        INSTANCE_LIST_T2D(ID)%DV     =>  DV
        INSTANCE_LIST_T2D(ID)%DHN    =>  DHN
        INSTANCE_LIST_T2D(ID)%IORDRH =>  IORDRH
        INSTANCE_LIST_T2D(ID)%IORDRU =>  IORDRU
        INSTANCE_LIST_T2D(ID)%CHESTR =>  CHESTR
        ! For allocatable arrays nag crashes if we try to point towards
        ! an unallocated array
        IF(ALLOCATED(FLUX_BOUNDARIES)) THEN
          INSTANCE_LIST_T2D(ID)%FLUX_BOUNDARIES => FLUX_BOUNDARIES
        ENDIF
        IF(ALLOCATED(COTE)) THEN
          INSTANCE_LIST_T2D(ID)%COTE => COTE
        ENDIF
        IF(ALLOCATED(DEBIT)) THEN
          INSTANCE_LIST_T2D(ID)%DEBIT  => DEBIT
        ENDIF
!
        INSTANCE_LIST_T2D(ID)%MESH   => MESH
        INSTANCE_LIST_T2D(ID)%LIHBOR => LIHBOR
        INSTANCE_LIST_T2D(ID)%LIUBOR => LIUBOR
        INSTANCE_LIST_T2D(ID)%LIVBOR => LIVBOR
        INSTANCE_LIST_T2D(ID)%NUMLIQ => NUMLIQ
        INSTANCE_LIST_T2D(ID)%MAXLU_T2D = MAXLU_T2D
        INSTANCE_LIST_T2D(ID)%MAXKEYWORD = MAXKEYWORD
!
        INSTANCE_LIST_T2D(ID)%NIT    => NIT
        INSTANCE_LIST_T2D(ID)%LT     => LT
        INSTANCE_LIST_T2D(ID)%NBMAXNSHARE = NBMAXNSHARE
        INSTANCE_LIST_T2D(ID)%COMPLEO => COMPLEO
        INSTANCE_LIST_T2D(ID)%LEO => LEO
        INSTANCE_LIST_T2D(ID)%NPTIR => NPTIR
        INSTANCE_LIST_T2D(ID)%AT     => AT
        INSTANCE_LIST_T2D(ID)%DT     => DT
        INSTANCE_LIST_T2D(ID)%TMAX     => TMAX
        INSTANCE_LIST_T2D(ID)%FAIR     => FAIR
!
        INSTANCE_LIST_T2D(ID)%T2D_FILES => T2D_FILES
        INSTANCE_LIST_T2D(ID)%T2DRES => T2DRES
        INSTANCE_LIST_T2D(ID)%T2DGEO => T2DGEO
        INSTANCE_LIST_T2D(ID)%T2DCLI => T2DCLI
        INSTANCE_LIST_T2D(ID)%T2ATMB => T2ATMB
        INSTANCE_LIST_T2D(ID)%T2DFO2 => T2DFO2
        INSTANCE_LIST_T2D(ID)%T2DIMP => T2DIMP
        INSTANCE_LIST_T2D(ID)%T2DPRE => T2DPRE
        INSTANCE_LIST_T2D(ID)%MAXLU_T2D = MAXLU_T2D
        INSTANCE_LIST_T2D(ID)%MAXKEYWORD = MAXKEYWORD
!
        INSTANCE_LIST_T2D(ID)%COUPLING => COUPLING
        INSTANCE_LIST_T2D(ID)%EQUA => EQUA
!
        INSTANCE_LIST_T2D(ID)%AK => AK
        INSTANCE_LIST_T2D(ID)%EP => EP
        INSTANCE_LIST_T2D(ID)%VISCSA => VISCSA
        INSTANCE_LIST_T2D(ID)%PROPNU  => PROPNU
        INSTANCE_LIST_T2D(ID)%ITURB  => ITURB
        INSTANCE_LIST_T2D(ID)%PTINIG  => PTINIG
        INSTANCE_LIST_T2D(ID)%SECCURRENTS => SECCURRENTS
        INSTANCE_LIST_T2D(ID)%SEC_R => SEC_R
        INSTANCE_LIST_T2D(ID)%NESTOR => NESTOR
        INSTANCE_LIST_T2D(ID)%ZRL => ZRL
        INSTANCE_LIST_T2D(ID)%T => T
        INSTANCE_LIST_T2D(ID)%NTRAC  => NTRAC
!
        INSTANCE_LIST_T2D(ID)%TE5    => TE5
        INSTANCE_LIST_T2D(ID)%ZF     => ZF
        INSTANCE_LIST_T2D(ID)%H      => H
!
        INSTANCE_LIST_T2D(ID)%DEBUG  => DEBUG
        IF(ALLOCATED(DEBIT)) THEN
          INSTANCE_LIST_T2D(ID)%BND_TIDE  => BND_TIDE
        ENDIF
        INSTANCE_LIST_T2D(ID)%CTIDE  => CTIDE
        INSTANCE_LIST_T2D(ID)%CTIDEV  => CTIDEV
        INSTANCE_LIST_T2D(ID)%MSL  => MSL

        INSTANCE_LIST_T2D(ID)%RSTPRD => RSTPRD
        INSTANCE_LIST_T2D(ID)%RESTART_RECORD => RESTART_RECORD

        ! INITIALISATIONS POUR UN CAS SANS COUPLAGE
        INSTANCE_LIST_T2D(ID)%SIS%LT => LT
        INSTANCE_LIST_T2D(ID)%SIS%LEOPRD => LEOPRD
        INSTANCE_LIST_T2D(ID)%SIS%LISPRD => LISPRD
        INSTANCE_LIST_T2D(ID)%SIS%NIT => NIT
        INSTANCE_LIST_T2D(ID)%SIS%U => U
        INSTANCE_LIST_T2D(ID)%SIS%V => V
        INSTANCE_LIST_T2D(ID)%SIS%H => H
        INSTANCE_LIST_T2D(ID)%SIS%HN => HN
        INSTANCE_LIST_T2D(ID)%SIS%HPROP => HPROP
        INSTANCE_LIST_T2D(ID)%SIS%ZF => ZF
        INSTANCE_LIST_T2D(ID)%SIS%CF => CF
        INSTANCE_LIST_T2D(ID)%SIS%CHESTR => CHESTR
        INSTANCE_LIST_T2D(ID)%SIS%SIS_CPL => SIS_CPL
        INSTANCE_LIST_T2D(ID)%SIS%PERCOU => PERCOU
        INSTANCE_LIST_T2D(ID)%SIS%AT => AT
        INSTANCE_LIST_T2D(ID)%SIS%VISC => VISC
        INSTANCE_LIST_T2D(ID)%SIS%DT => DT
        INSTANCE_LIST_T2D(ID)%SIS%FLBOR => FLBOR
        INSTANCE_LIST_T2D(ID)%SIS%SOLSYS => SOLSYS
        INSTANCE_LIST_T2D(ID)%SIS%DM1 => DM1
        !USIS et VSIS modified of SOLSYS.EQ.2 after initialization
        IF(INSTANCE_LIST_T2D(ID)%SIS%SOLSYS.EQ.2) THEN
          INSTANCE_LIST_T2D(ID)%SIS%USIS => UDEL
          INSTANCE_LIST_T2D(ID)%SIS%VSIS => VDEL
        ELSE
          INSTANCE_LIST_T2D(ID)%SIS%USIS => UCONV
          INSTANCE_LIST_T2D(ID)%SIS%VSIS => VCONV
        END IF
        INSTANCE_LIST_T2D(ID)%SIS%ZCONV => ZCONV
        INSTANCE_LIST_T2D(ID)%SIS%DIRMOY => DIRMOY
        INSTANCE_LIST_T2D(ID)%SIS%HM0 => HM0
        INSTANCE_LIST_T2D(ID)%SIS%TPR5 => TPR5
        INSTANCE_LIST_T2D(ID)%SIS%ORBVEL => ORBVEL
        INSTANCE_LIST_T2D(ID)%H0 => H0
        INSTANCE_LIST_T2D(ID)%T => T
        INSTANCE_LIST_T2D(ID)%NTRAC => NTRAC
        !Matrix of solved system in propag
        INSTANCE_LIST_T2D(ID)%AM1 => AM1
        INSTANCE_LIST_T2D(ID)%AM2 => AM2
        INSTANCE_LIST_T2D(ID)%AM3 => AM3
        INSTANCE_LIST_T2D(ID)%BM1 => BM1
        INSTANCE_LIST_T2D(ID)%BM2 => BM2
        INSTANCE_LIST_T2D(ID)%CM1 => CM1
        INSTANCE_LIST_T2D(ID)%CM2 => CM2
        INSTANCE_LIST_T2D(ID)%A23 => A23
        INSTANCE_LIST_T2D(ID)%A32 => A32
        INSTANCE_LIST_T2D(ID)%CV1 => CV1
        INSTANCE_LIST_T2D(ID)%CV2 => CV2
        INSTANCE_LIST_T2D(ID)%CV3 => CV3
        ! GAIA
        INSTANCE_LIST_T2D(ID)%GAI_FILES => GAI_FILES
        INSTANCE_LIST_T2D(ID)%MAXLU_GAI = MAXLU_GAI
        INSTANCE_LIST_T2D(ID)%Q => Q
        INSTANCE_LIST_T2D(ID)%DCLA => DCLA
        INSTANCE_LIST_T2D(ID)%AC => AC
        INSTANCE_LIST_T2D(ID)%XWC => XWC
        INSTANCE_LIST_T2D(ID)%Z => Z
        INSTANCE_LIST_T2D(ID)%QBOR => QBOR
        INSTANCE_LIST_T2D(ID)%EBOR => EBOR
        INSTANCE_LIST_T2D(ID)%FLBOR => FLBOR
        INSTANCE_LIST_T2D(ID)%TOB => TOB
        INSTANCE_LIST_T2D(ID)%CLU => CLU
        INSTANCE_LIST_T2D(ID)%CLV => CLV
        INSTANCE_LIST_T2D(ID)%LIQBOR => LIQBOR
        INSTANCE_LIST_T2D(ID)%LIEBOR => LIEBOR
        INSTANCE_LIST_T2D(ID)%NSICLA => NSICLA
        INSTANCE_LIST_T2D(ID)%NOMBLAY => NOMBLAY
        INSTANCE_LIST_T2D(ID)%E => E
        INSTANCE_LIST_T2D(ID)%PARTHENIADES => PARTHENIADES
        INSTANCE_LIST_T2D(ID)%MARDAT => MARDAT
        INSTANCE_LIST_T2D(ID)%MARTIM => MARTIM
        INSTANCE_LIST_T2D(ID)%RAZTIM => RAZTIM
        INSTANCE_LIST_T2D(ID)%START_RECORD => START_RECORD
        INSTANCE_LIST_T2D(ID)%VOLU2D => VOLU2D
        INSTANCE_LIST_T2D(ID)%PROPNU => PROPNU
        INSTANCE_LIST_T2D(ID)%MPM => MPM
        INSTANCE_LIST_T2D(ID)%BETA2 => BETA2
        INSTANCE_LIST_T2D(ID)%PHISED => PHISED
        INSTANCE_LIST_T2D(ID)%ALPHA => ALPHA
        INSTANCE_LIST_T2D(ID)%XKV0 => XKV0
        INSTANCE_LIST_T2D(ID)%KSPRATIO => KSPRATIO
        ! <new_link>

      END SUBROUTINE UPDATE_INSTANCE_T2D
!
!
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !>@brief Initializes variables for TELEMAC2D in case of coupling
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !>@param[in] ID Id for telemac2d instance
      !>@param[out] IERR 0 if subroutine successfull,
      !!                             ERROR ID OTHERWISE
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      SUBROUTINE CPL_INIT_T2D(ID,IERR)
        INTEGER, INTENT(IN) :: ID
        INTEGER,             INTENT(OUT) :: IERR
      !
        IERR = 0
        IF(INSTANCE_LIST_T2D(ID)%SIS%SOLSYS.EQ.2) THEN
          INSTANCE_LIST_T2D(ID)%SIS%USIS => UDEL
          INSTANCE_LIST_T2D(ID)%SIS%VSIS => VDEL
        ELSE
          INSTANCE_LIST_T2D(ID)%SIS%USIS => UCONV
          INSTANCE_LIST_T2D(ID)%SIS%VSIS => VCONV
        END IF
      END SUBROUTINE CPL_INIT_T2D

      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !>@brief Deletes a telemac2d instance
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !>@param[in] ID Id of the instance
      !>@param[out] IERR 0 if subroutine successfull,
      !!                   error id otherwise
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      SUBROUTINE DELETE_INSTANCE_T2D(ID,IERR)
        INTEGER, INTENT(IN) :: ID
        INTEGER, INTENT(OUT) :: IERR
!
        IERR = 0
        !
        CALL CHECK_INSTANCE_T2D(ID,IERR)
        IF(IERR.NE.0) RETURN
        USED_INSTANCE(ID) = .FALSE.
      END SUBROUTINE DELETE_INSTANCE_T2D
!
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !>@brief Check if the id is following convention
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !>@param[in] ID Id of the instance
      !>@param[out] IERR 0 if subroutine successfull,
      !!                   error id otherwise
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      SUBROUTINE CHECK_INSTANCE_T2D(ID,IERR)
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
        CALL UPDATE_INSTANCE_T2D(ID,IERR)
      END SUBROUTINE CHECK_INSTANCE_T2D
!
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !>@brief Returns the error message of the instance
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !>@param[in] ID Id of the instance
      !>@param[out] MESS The error message
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      SUBROUTINE GET_INSTANCE_ERROR_T2D(ID,MESS)
        INTEGER, INTENT(IN) :: ID
        CHARACTER(LEN=200), INTENT(OUT) :: MESS
!
        MESS = INSTANCE_LIST_T2D(ID)%ERROR_MESSAGE
!
      END SUBROUTINE GET_INSTANCE_ERROR_T2D
      END MODULE API_INSTANCE_T2D
