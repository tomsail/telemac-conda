!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!>@brief Getter/setter of sisyphe variables
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      MODULE API_HANDLE_VAR_SIS

        USE API_HANDLE_ERROR
        USE API_INSTANCE_SIS
        IMPLICIT NONE
        !> Size of the string containing the name of a variable
        INTEGER, PARAMETER :: SIS_VAR_LEN=40
        !> Size of the string containing the type of a variable
        INTEGER, PARAMETER :: SIS_TYPE_LEN=12
        !> Size of the string containing the information about a variable
        INTEGER, PARAMETER :: SIS_INFO_LEN=200
        !> The maximum number of variable
        INTEGER, PARAMETER :: NB_VAR_SIS=42
        !> List of variable names
        CHARACTER(LEN=SIS_VAR_LEN),ALLOCATABLE :: VNAME_SIS(:)
        !> List of variable info
        CHARACTER(LEN=SIS_INFO_LEN),ALLOCATABLE :: VINFO_SIS(:)
!
      CONTAINS
!
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !>@brief Get a double array
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      SUBROUTINE GET_DOUBLE_ARRAY_SIS_D
     &     (INST, VARNAME, VALEUR, DIM1, IERR , BLOCK_INDEX)
!
        TYPE(INSTANCE_SIS),         INTENT(IN) :: INST
        CHARACTER(LEN=SIS_VAR_LEN), INTENT(IN) :: VARNAME
        INTEGER,                    INTENT(IN) :: DIM1
        INTEGER, OPTIONAL,          INTENT(IN) :: BLOCK_INDEX
        DOUBLE PRECISION,           INTENT(OUT):: VALEUR(DIM1)
        INTEGER,                    INTENT(OUT):: IERR
!
        IERR = 0
        VALEUR = 0.0
!
        IF(TRIM(VARNAME).EQ.'MODEL.FLOWRATEQ') THEN
          VALEUR(1:INST%Q%DIM1) = INST%Q%R(1:INST%Q%DIM1)
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.EVOLUTION') THEN
          VALEUR(1:INST%E%DIM1) = INST%E%R(1:INST%E%DIM1)
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.Z') THEN
          VALEUR(1:INST%Z%DIM1) = INST%Z%R(1:INST%Z%DIM1)
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.BOTTOMELEVATION') THEN
          VALEUR(1:INST%ZF%DIM1) = INST%ZF%R(1:INST%ZF%DIM1)
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.ZF_C') THEN
          VALEUR(1:INST%ZF_C%DIM1) = INST%ZF_C%R(1:INST%ZF_C%DIM1)
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.FLBOR') THEN
          VALEUR(1:INST%FLBOR%DIM1) = INST%FLBOR%R(1:INST%FLBOR%DIM1)
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.FLBOR_SIS') THEN
          VALEUR(1:INST%FLBOR_SIS%DIM1) =
     &    INST%FLBOR_SIS%R(1:INST%FLBOR_SIS%DIM1)
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.X') THEN
          VALEUR(1:INST%MESH%X%DIM1) = INST%MESH%X%R(1:INST%MESH%X%DIM1)
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.Y') THEN
          VALEUR(1:INST%MESH%Y%DIM1) = INST%MESH%Y%R(1:INST%MESH%Y%DIM1)
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.XNEBOR') THEN
          VALEUR(1:INST%MESH%XNEBOR%DIM1) =
     &    INST%MESH%XNEBOR%R(1:INST%MESH%XNEBOR%DIM1)
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.YNEBOR') THEN
          VALEUR(1:INST%MESH%YNEBOR%DIM1) =
     &    INST%MESH%YNEBOR%R(1:INST%MESH%YNEBOR%DIM1)
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.TOB') THEN
          VALEUR(1:INST%TOB%DIM1) = INST%TOB%R(1:INST%TOB%DIM1)
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.CHESTR') THEN
          VALEUR(1:INST%CHESTR%DIM1) = INST%CHESTR%R(1:INST%CHESTR%DIM1)
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.CONCENTRATION') THEN
          IF(PRESENT(BLOCK_INDEX))THEN
            VALEUR(1:INST%CS%ADR(BLOCK_INDEX)%P%DIM1) =
     &   INST%CS%ADR(BLOCK_INDEX)%P%R(1:INST%CS%ADR(BLOCK_INDEX)%P%DIM1)
          ELSE
            IERR = INDEX_BLOCK_MISSING
            ERR_MESS = 'THE BOCK NUMBER IS MISSING FOR'//TRIM(VARNAME)
          END IF
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.QBOR') THEN
          IF(PRESENT(BLOCK_INDEX))THEN
            VALEUR(1:INST%QBOR%ADR(BLOCK_INDEX)%P%DIM1) =
     &    INST%QBOR%ADR(BLOCK_INDEX)%P%R(
     &            1:INST%QBOR%ADR(BLOCK_INDEX)%P%DIM1)
          ELSE
            IERR = INDEX_BLOCK_MISSING
            ERR_MESS = 'THE BOCK NUMBER IS MISSING FOR'//TRIM(VARNAME)
          END IF
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.CBOR') THEN
          IF(PRESENT(BLOCK_INDEX))THEN
            VALEUR(1:INST%CBOR%ADR(BLOCK_INDEX)%P%DIM1) =
     &    INST%CBOR%ADR(BLOCK_INDEX)%P%R(
     &            1:INST%CBOR%ADR(BLOCK_INDEX)%P%DIM1)
          ELSE
            IERR = INDEX_BLOCK_MISSING
            ERR_MESS = 'THE BOCK NUMBER IS MISSING FOR'//TRIM(VARNAME)
          END IF
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.EBOR') THEN
          IF(PRESENT(BLOCK_INDEX))THEN
            VALEUR(1:INST%EBOR%ADR(BLOCK_INDEX)%P%DIM1) =
     &   INST%EBOR%ADR(BLOCK_INDEX)%P%R(
     &            1:INST%EBOR%ADR(BLOCK_INDEX)%P%DIM1)
          ELSE
            IERR = INDEX_BLOCK_MISSING
            ERR_MESS = 'THE BOCK NUMBER IS MISSING FOR'//TRIM(VARNAME)
          END IF
        ! <get_double_array>
        ELSE
          IERR = UNKNOWN_VAR_ERROR
          ERR_MESS = 'UNKNOWN VARIABLE NAME : '//TRIM(VARNAME)
        ENDIF
!
      END SUBROUTINE GET_DOUBLE_ARRAY_SIS_D

      !
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !>@brief Defines the value of a double array
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      SUBROUTINE SET_DOUBLE_ARRAY_SIS_D
     &     (INST, VARNAME, VALEUR, DIM1, IERR, BLOCK_INDEX)
!
        TYPE(INSTANCE_SIS),       INTENT(INOUT) :: INST
        CHARACTER(LEN=SIS_VAR_LEN), INTENT(IN)  :: VARNAME
        INTEGER, OPTIONAL,           INTENT(IN) :: BLOCK_INDEX
        INTEGER,                     INTENT(IN) :: DIM1
        DOUBLE PRECISION,            INTENT(IN) :: VALEUR(DIM1)
        INTEGER,                     INTENT(OUT) :: IERR
!
        IERR = 0

!
        IF(TRIM(VARNAME).EQ.'MODEL.FLOWRATEQ') THEN
          INST%Q%R(1:INST%Q%DIM1) = VALEUR(1:INST%Q%DIM1)
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.EVOLUTION') THEN
          INST%E%R(1:INST%E%DIM1) = VALEUR(1:INST%E%DIM1)
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.Z') THEN
          INST%Z%R(1:INST%Z%DIM1) = VALEUR(1:INST%Z%DIM1)
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.BOTTOMELEVATION') THEN
          INST%ZF%R(1:INST%ZF%DIM1) = VALEUR(1:INST%ZF%DIM1)
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.ZF_C') THEN
          INST%ZF_C%R(1:INST%ZF_C%DIM1) = VALEUR(1:INST%ZF_C%DIM1)
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.FLBOR') THEN
          INST%FLBOR%R(1:INST%FLBOR%DIM1) = VALEUR(1:INST%FLBOR%DIM1)
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.FLBOR_SIS') THEN
          INST%FLBOR_SIS%R(1:INST%FLBOR_SIS%DIM1) =
     &    VALEUR(1:INST%FLBOR_SIS%DIM1)
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.X') THEN
          INST%MESH%X%R(1:INST%MESH%X%DIM1) = VALEUR(1:INST%MESH%X%DIM1)
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.Y') THEN
          INST%MESH%Y%R(1:INST%MESH%Y%DIM1) = VALEUR(1:INST%MESH%Y%DIM1)
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.XNEBOR') THEN
          INST%MESH%XNEBOR%R(1:INST%MESH%XNEBOR%DIM1) =
     &    VALEUR(1:INST%MESH%XNEBOR%DIM1)
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.YNEBOR') THEN
          INST%MESH%YNEBOR%R(1:INST%MESH%YNEBOR%DIM1) =
     &    VALEUR(1:INST%MESH%YNEBOR%DIM1)
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.TOB') THEN
          INST%TOB%R(1:INST%TOB%DIM1) = VALEUR(1:INST%TOB%DIM1)
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.CHESTR') THEN
          INST%CHESTR%R(1:INST%CHESTR%DIM1) = VALEUR(1:INST%CHESTR%DIM1)
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.CONCENTRATION') THEN
          IF(PRESENT(BLOCK_INDEX))THEN
            INST%CS%ADR(BLOCK_INDEX)%P%R(1:INST%CS%ADR
     &           (BLOCK_INDEX)%P%DIM1) =VALEUR(1:INST%CS%ADR
     &           (BLOCK_INDEX)%P%DIM1)
          ELSE
            IERR = INDEX_BLOCK_MISSING
            ERR_MESS = 'THE BOCK NUMBER IS MISSING FOR'//TRIM(VARNAME)
          END IF
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.QBOR') THEN
          IF(PRESENT(BLOCK_INDEX))THEN
            INST%QBOR%ADR(BLOCK_INDEX)%P%R(1:INST%QBOR%ADR
     &           (BLOCK_INDEX)%P%DIM1) =VALEUR(1:INST%QBOR%ADR
     &           (BLOCK_INDEX)%P%DIM1)
          ELSE
            IERR = INDEX_BLOCK_MISSING
            ERR_MESS = 'THE BOCK NUMBER IS MISSING FOR'//TRIM(VARNAME)
          END IF
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.CBOR') THEN
          IF(PRESENT(BLOCK_INDEX))THEN
            INST%CBOR%ADR(BLOCK_INDEX)%P%R(1:INST%CBOR%ADR
     &           (BLOCK_INDEX)%P%DIM1) = VALEUR(1:INST%CBOR%ADR
     &           (BLOCK_INDEX)%P%DIM1)
          ELSE
            IERR = INDEX_BLOCK_MISSING
            ERR_MESS = 'THE BOCK NUMBER IS MISSING FOR'//TRIM(VARNAME)
          END IF
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.EBOR') THEN
          IF(PRESENT(BLOCK_INDEX))THEN
            INST%EBOR%ADR(BLOCK_INDEX)%P%R(1:INST%EBOR%ADR
     &           (BLOCK_INDEX)%P%DIM1) =VALEUR(1:INST%EBOR%ADR
     &           (BLOCK_INDEX)%P%DIM1)
          ELSE
            IERR = INDEX_BLOCK_MISSING
            ERR_MESS = 'THE BOCK NUMBER IS MISSING FOR'//TRIM(VARNAME)
          END IF
        ! <set_double_array>
        ELSE
          IERR = UNKNOWN_VAR_ERROR
          ERR_MESS = 'UNKNOWN VARIABLE NAME : '//TRIM(VARNAME)
        ENDIF
!
      END SUBROUTINE SET_DOUBLE_ARRAY_SIS_D
!
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !>@brief Get an integer variable from sisyphe
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      SUBROUTINE GET_INTEGER_ARRAY_SIS_D
     &     (INST, VARNAME, VALEUR, DIM1, IERR)
!
        TYPE(INSTANCE_SIS),    INTENT(IN) :: INST
        CHARACTER(LEN=SIS_VAR_LEN), INTENT(IN)  :: VARNAME
        INTEGER,               INTENT(IN) :: DIM1
        INTEGER,               INTENT(OUT) :: VALEUR(DIM1)
        INTEGER,               INTENT(OUT) :: IERR
!
        IERR = 0
        VALEUR = -1
        IF(TRIM(VARNAME).EQ.'MODEL.LIHBOR') THEN
          VALEUR(1:INST%LIHBOR%DIM1) = INST%LIHBOR%I(1:INST%LIHBOR%DIM1)
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.CLU') THEN
          VALEUR(1:INST%CLU%DIM1) = INST%CLU%I(1:INST%CLU%DIM1)
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.CLV') THEN
          VALEUR(1:INST%CLV%DIM1) =  INST%CLV%I(1:INST%CLV%DIM1)
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.LIQBOR') THEN
          VALEUR(1:INST%LIQBOR%DIM1) =
     &          INST%LIQBOR%I(1:INST%LIQBOR%DIM1)
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.LICBOR') THEN
          VALEUR(1:INST%LICBOR%DIM1) =
     &    INST%LICBOR%I(1:INST%LICBOR%DIM1)
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.LIEBOR') THEN
          VALEUR(1:INST%LIEBOR%DIM1) =
     &    INST%LIEBOR%I(1:INST%LIEBOR%DIM1)
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.NUMLIQ') THEN
          VALEUR(1:INST%NUMLIQ%DIM1) =
     &    INST%NUMLIQ%I(1:INST%NUMLIQ%DIM1)
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.IKLE') THEN
          VALEUR(1:SIZE(INST%MESH%IKLE%I)) =
     &          INST%MESH%IKLE%I(1:SIZE(INST%MESH%IKLE%I))
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.NACHB') THEN
          VALEUR(1:SIZE(INST%MESH%NACHB%I)) =
     &         INST%MESH%NACHB%I(1:SIZE(INST%MESH%NACHB%I))
        ! <get_integer_array>
        ELSE
          IERR = UNKNOWN_VAR_ERROR
          ERR_MESS = 'UNKNOWN VARIABLE NAME : '//TRIM(VARNAME)
        ENDIF
!
      END SUBROUTINE GET_INTEGER_ARRAY_SIS_D

!
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !>@brief Defines the value of an integer variable of sisyphe
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      SUBROUTINE SET_INTEGER_ARRAY_SIS_D
     &     (INST, VARNAME, VALEUR, DIM1, IERR)
!
        TYPE(INSTANCE_SIS),    INTENT(INOUT) :: INST
        CHARACTER(LEN=SIS_VAR_LEN), INTENT(IN)  :: VARNAME
        INTEGER,               INTENT(IN) :: DIM1
        INTEGER,               INTENT(IN) :: VALEUR(DIM1)
        INTEGER,               INTENT(OUT) :: IERR
!
        IERR = 0
        IF(TRIM(VARNAME).EQ.'MODEL.LIHBOR') THEN
          INST%LIHBOR%I(1:INST%LIHBOR%DIM1) = VALEUR(1:INST%LIHBOR%DIM1)
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.CLU') THEN
          INST%CLU%I(1:INST%CLU%DIM1) = VALEUR(1:INST%CLU%DIM1)
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.CLV') THEN
          INST%CLV%I(1:INST%CLV%DIM1) = VALEUR(1:INST%CLV%DIM1)
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.LIQBOR') THEN
          INST%LIQBOR%I(1:INST%LIQBOR%DIM1) = VALEUR(1:INST%LIQBOR%DIM1)
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.LICBOR') THEN
          INST%LICBOR%I(1:INST%LICBOR%DIM1) = VALEUR(1:INST%LICBOR%DIM1)
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.LIEBOR') THEN
          INST%LIEBOR%I(1:INST%LIEBOR%DIM1) = VALEUR(1:INST%LIEBOR%DIM1)
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.IKLE') THEN
          INST%MESH%IKLE%I(1:SIZE(INST%MESH%IKLE%I)) =
     &    VALEUR(1:SIZE(INST%MESH%IKLE%I))
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.NACHB') THEN
          INST%MESH%NACHB%I(1:SIZE(INST%MESH%NACHB%I)) =
     &    VALEUR(1:SIZE(INST%MESH%NACHB%I))
        ! <set_integer_array>
        ELSE
          IERR = UNKNOWN_VAR_ERROR
          ERR_MESS = 'UNKNOWN VARIABLE NAME : '//TRIM(VARNAME)
        ENDIF
!
      END SUBROUTINE SET_INTEGER_ARRAY_SIS_D
!
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !>@brief Get a double variable from sisyphe
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !>@param[in,out] INST The instance
      !>@param[in] VARNAME Name of the variable to read
      !>@param[out] VALEUR Contains the read value
      !>@param[in] INDEX1 Index on the first dimension
      !>@param[in] INDEX2 Index on the second dimension
      !>@param[in] INDEX3 Index on the third dimension
      !>@param[out] IERR 0 if subroutine successfull,
      !!                        error id otherwise
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      SUBROUTINE GET_DOUBLE_SIS_D
     &     (INST, VARNAME, VALEUR, INDEX1, INDEX2, INDEX3, IERR)
!
        TYPE(INSTANCE_SIS),         INTENT(IN) :: INST
        CHARACTER(LEN=SIS_VAR_LEN), INTENT(IN) :: VARNAME
        DOUBLE PRECISION,           INTENT(OUT):: VALEUR
        INTEGER,                    INTENT(IN) :: INDEX1
        INTEGER,                    INTENT(IN) :: INDEX2
        INTEGER,                    INTENT(IN) :: INDEX3
        INTEGER,                    INTENT(OUT):: IERR
!
        IERR = 0
        VALEUR = 0.0
!
        IF(TRIM(VARNAME).EQ.'MODEL.FLOWRATEQ') THEN
          VALEUR=INST%Q%R(INDEX1)
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.EVOLUTION') THEN
          VALEUR = INST%E%R(INDEX1)
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.Z') THEN
          VALEUR = INST%Z%R(INDEX1)
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.BOTTOMELEVATION') THEN
          VALEUR = INST%ZF%R(INDEX1)
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.ZF_C') THEN
          VALEUR = INST%ZF_C%R(INDEX1)
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.FLBOR') THEN
          VALEUR = INST%FLBOR%R(INDEX1)
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.FLBOR_SIS') THEN
          VALEUR = INST%FLBOR_SIS%R(INDEX1)
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.X') THEN
          VALEUR = INST%MESH%X%R(INDEX1)
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.Y') THEN
          VALEUR = INST%MESH%Y%R(INDEX1)
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.XNEBOR') THEN
          VALEUR = INST%MESH%XNEBOR%R(INDEX1)
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.YNEBOR') THEN
          VALEUR = INST%MESH%YNEBOR%R(INDEX1)
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.TIMESTEP') THEN
          VALEUR = INST%DT
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.TOB') THEN
          VALEUR = INST%TOB%R(INDEX1)
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.CHESTR') THEN
          VALEUR = INST%CHESTR%R(INDEX1)
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.D50') THEN
          VALEUR = INST%D50(INDEX1)
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.CBOR_CLASSE') THEN
          VALEUR = INST%CBOR_CLASSE(INDEX1)
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.MPM') THEN
          VALEUR = INST%MPM
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.PARTHENIADES') THEN
          VALEUR = INST%PARTHENIADES
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.SHIELDS') THEN
          VALEUR = INST%AC(INDEX1)
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.XWC') THEN
          VALEUR = INST%XWC(INDEX1)
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.POROSITY') THEN
          VALEUR = INST%XKV
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.KSPRATIO') THEN
          VALEUR = INST%KSPRATIO
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.PHISED') THEN
          VALEUR = INST%PHISED
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.BETA') THEN
          VALEUR = INST%BETA2
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.ALPHA') THEN
          VALEUR = INST%ALPHA
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.CONCENTRATION') THEN
          VALEUR = INST%CS%ADR(INDEX1)%P%R(INDEX2)
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.QBOR') THEN
          VALEUR = INST%QBOR%ADR(INDEX1)%P%R(INDEX2)
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.EBOR') THEN
          VALEUR = INST%EBOR%ADR(INDEX1)%P%R(INDEX2)
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.CBOR') THEN
          VALEUR = INST%CBOR%ADR(INDEX1)%P%R(INDEX2)
        ! <get_double>
        ELSE
          IERR = UNKNOWN_VAR_ERROR
          ERR_MESS = 'UNKNOWN VARIABLE NAME : '//TRIM(VARNAME)
        ENDIF
!
      END SUBROUTINE GET_DOUBLE_SIS_D

      !
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !>@brief Defines the value of a double variable of sisyphe
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !>@param[in,out] INST The instance
      !>@param[in] VARNAME Name of the variable to write
      !>@param[in] VALEUR The value to write in the variable
      !>@param[in] INDEX1 Index on the first dimension
      !>@param[in] INDEX2 Index on the second dimension
      !>@param[in] INDEX3 Index on the third dimension
      !>@param[out] IERR 0 if subroutine successfull,
      !!                        error id otherwise
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      SUBROUTINE SET_DOUBLE_SIS_D
     &     (INST, VARNAME, VALEUR, INDEX1, INDEX2, INDEX3, IERR)
!
        TYPE(INSTANCE_SIS),    INTENT(INOUT) :: INST
        CHARACTER(LEN=SIS_VAR_LEN), INTENT(IN)  :: VARNAME
        DOUBLE PRECISION,      INTENT(IN) :: VALEUR
        INTEGER,               INTENT(IN) :: INDEX1
        INTEGER,               INTENT(IN) :: INDEX2
        INTEGER,               INTENT(IN) :: INDEX3
        INTEGER,               INTENT(OUT) :: IERR
!
        IERR = 0

!
        IF(TRIM(VARNAME).EQ.'MODEL.FLOWRATEQ') THEN
          INST%Q%R(INDEX1) = VALEUR
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.EVOLUTION') THEN
          INST%E%R(INDEX1) = VALEUR
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.Z') THEN
          INST%Z%R(INDEX1) = VALEUR
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.BOTTOMELEVATION') THEN
          INST%ZF%R(INDEX1) = VALEUR
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.ZF_C') THEN
          INST%ZF_C%R(INDEX1) = VALEUR
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.FLBOR') THEN
          INST%FLBOR%R(INDEX1) = VALEUR
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.FLBOR_SIS') THEN
          INST%FLBOR_SIS%R(INDEX1) = VALEUR
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.X') THEN
          INST%MESH%X%R(INDEX1) = VALEUR
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.Y') THEN
          INST%MESH%Y%R(INDEX1) = VALEUR
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.XNEBOR') THEN
          INST%MESH%XNEBOR%R(INDEX1) = VALEUR
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.YNEBOR') THEN
          INST%MESH%YNEBOR%R(INDEX1) = VALEUR
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.TIMESTEP') THEN
          INST%DT = VALEUR
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.TOB') THEN
          INST%TOB%R(INDEX1) = VALEUR
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.CHESTR') THEN
          INST%CHESTR%R(INDEX1) = VALEUR
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.D50') THEN
          INST%D50(INDEX1) = VALEUR
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.CBOR_CLASSE') THEN
          INST%CBOR_CLASSE(INDEX1) = VALEUR
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.MPM') THEN
          INST%MPM = VALEUR
          INST%MPM_ARAY%R(:) = VALEUR
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.PARTHENIADES') THEN
          INST%PARTHENIADES = VALEUR
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.SHIELDS') THEN
          INST%AC(INDEX1) = VALEUR
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.XWC') THEN
          INST%XWC(INDEX1) = VALEUR
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.POROSITY') THEN
          INST%XKV = VALEUR
          INST%CSF_SABLE = 1 - VALEUR
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.KSPRATIO') THEN
          INST%KSPRATIO = VALEUR
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.PHISED') THEN
          INST%PHISED = VALEUR
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.BETA') THEN
          INST%BETA2 = VALEUR
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.ALPHA') THEN
          INST%ALPHA = VALEUR
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.CONCENTRATION') THEN
          INST%CS%ADR(INDEX1)%P%R(INDEX2) = VALEUR
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.QBOR') THEN
          INST%QBOR%ADR(INDEX1)%P%R(INDEX2) = VALEUR
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.CBOR') THEN
          INST%CBOR%ADR(INDEX1)%P%R(INDEX2) = VALEUR
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.EBOR') THEN
          INST%EBOR%ADR(INDEX1)%P%R(INDEX2) = VALEUR
        ! <set_double>
        ELSE
          IERR = UNKNOWN_VAR_ERROR
          ERR_MESS = 'UNKNOWN VARIABLE NAME : '//TRIM(VARNAME)
        ENDIF
!
      END SUBROUTINE SET_DOUBLE_SIS_D
!
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !>@brief Get an integer variable from sisyphe
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !>@param[in,out] INST The instance
      !>@param[in] VARNAME Name of the variable to read
      !>@param[out] VALEUR Containis the read value
      !>@param[in] INDEX1 Index on the first dimension
      !>@param[in] INDEX2 Index on the second dimension
      !>@param[in] INDEX3 Index on the third dimension
      !>@param[out] IERR 0 if subroutine successfull,
      !!                        error id otherwise
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      SUBROUTINE GET_INTEGER_SIS_D
     &     (INST, VARNAME, VALEUR, INDEX1, INDEX2, INDEX3, IERR)
!
        TYPE(INSTANCE_SIS),    INTENT(IN) :: INST
        CHARACTER(LEN=SIS_VAR_LEN), INTENT(IN)  :: VARNAME
        INTEGER,               INTENT(OUT) :: VALEUR
        INTEGER,               INTENT(IN) :: INDEX1
        INTEGER,               INTENT(IN) :: INDEX2
        INTEGER,               INTENT(IN) :: INDEX3
        INTEGER,               INTENT(OUT) :: IERR
!
        IERR = 0
        VALEUR = -1
        IF(TRIM(VARNAME).EQ.'MODEL.LIHBOR') THEN
          VALEUR = INST%LIHBOR%I(INDEX1)
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.CLU') THEN
          VALEUR = INST%CLU%I(INDEX1)
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.CLV') THEN
          VALEUR =  INST%CLV%I(INDEX1)
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.LIQBOR') THEN
          VALEUR =  INST%LIQBOR%I(INDEX1)
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.LICBOR') THEN
          VALEUR =  INST%LICBOR%I(INDEX1)
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.LIEBOR') THEN
          VALEUR =  INST%LIEBOR%I(INDEX1)
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.NUMLIQ') THEN
          VALEUR =  INST%NUMLIQ%I(INDEX1)
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.NPOIN') THEN
          VALEUR = INST%MESH%NPOIN
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.NELEM') THEN
          VALEUR = INST%MESH%NELEM
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.NPTFR') THEN
          VALEUR = INST%MESH%NPTFR
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.NELMAX') THEN
          VALEUR = INST%MESH%NELMAX
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.IKLE') THEN
          VALEUR = INST%MESH%IKLE%I((INDEX2-1)*INST%MESH%IKLE%DIM1
     &                               + INDEX1)
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.NACHB') THEN
          VALEUR = INST%MESH%NACHB%I((INDEX2-1)*INST%NBMAXNSHARE
     &          + INDEX1)
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.NTIMESTEPS') THEN
          VALEUR = INST%NIT
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.CURRENTSTEP') THEN
          VALEUR = INST%LT
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.CPL_PERIOD') THEN
          VALEUR = INST%TEL%PERICOU
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.NSICLA') THEN
          VALEUR = INST%NSICLA
        ! <get_integer>
        ELSE
          IERR = UNKNOWN_VAR_ERROR
          ERR_MESS = 'UNKNOWN VARIABLE NAME : '//TRIM(VARNAME)
        ENDIF
!
      END SUBROUTINE GET_INTEGER_SIS_D

!
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !>@brief Defines the value of an integer variable of sisyphe
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !>@param[in,out] INST The instance
      !>@param[in] VARNAME Name of the variable to write
      !>@param[in] VALEUR The value to write in the variable
      !>@param[in] INDEX1 Index on the first dimension
      !>@param[in] INDEX2 Index on the second dimension
      !>@param[in] INDEX3 Index on the third dimension
      !>@param[out] IERR 0 if subroutine successfull,
      !!                        error id otherwise
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      SUBROUTINE SET_INTEGER_SIS_D
     &     (INST, VARNAME, VALEUR, INDEX1, INDEX2, INDEX3, IERR)
!
        TYPE(INSTANCE_SIS),    INTENT(INOUT) :: INST
        CHARACTER(LEN=SIS_VAR_LEN), INTENT(IN)  :: VARNAME
        INTEGER,               INTENT(IN) :: VALEUR
        INTEGER,               INTENT(IN) :: INDEX1
        INTEGER,               INTENT(IN) :: INDEX2
        INTEGER,               INTENT(IN) :: INDEX3
        INTEGER,               INTENT(OUT) :: IERR
!
        IERR = 0
        IF(TRIM(VARNAME).EQ.'MODEL.LIHBOR') THEN
          INST%LIHBOR%I(INDEX1) = VALEUR
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.CLU') THEN
          INST%CLU%I(INDEX1) = VALEUR
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.CLV') THEN
          INST%CLV%I(INDEX1) = VALEUR
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.LIQBOR') THEN
          INST%LIQBOR%I(INDEX1) = VALEUR
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.LICBOR') THEN
          INST%LICBOR%I(INDEX1) = VALEUR
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.LIEBOR') THEN
          INST%LIEBOR%I(INDEX1) = VALEUR
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.NTIMESTEPS') THEN
          INST%NIT = VALEUR
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.NSICLA') THEN
          INST%NSICLA = VALEUR
        ! <set_integer>
        ELSE
          IERR = UNKNOWN_VAR_ERROR
          ERR_MESS = 'UNKNOWN VARIABLE NAME : '//TRIM(VARNAME)
        ENDIF
!
      END SUBROUTINE SET_INTEGER_SIS_D
!
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !>@brief Get a string variable from sisyphe
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !>@param[in,out] INST The instance
      !>@param[in] VARNAME Name of the variable to read
      !>@param[out] VALEUR Containis the read value
      !>@param[in] VALUELEN Length of VALEUR
      !>@param[in] INDEX1 Index on the first dimension
      !>@param[in] INDEX2 Index on the second dimension
      !>@param[out] IERR 0 if subroutine successfull,
      !!                        error id otherwise
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      SUBROUTINE GET_STRING_SIS_D
     &     (INST, VARNAME, VALEUR, VALUELEN, INDEX1, INDEX2, IERR)
!
        TYPE(INSTANCE_SIS),    INTENT(IN) :: INST
        CHARACTER(LEN=SIS_VAR_LEN), INTENT(IN)  :: VARNAME
        INTEGER,               INTENT(IN) :: VALUELEN
        INTEGER,               INTENT(IN) :: INDEX1
        INTEGER,               INTENT(IN) :: INDEX2
        CHARACTER,             INTENT(OUT) :: VALEUR(VALUELEN)
        INTEGER,               INTENT(OUT) :: IERR
!
        INTEGER I,J
!
        IERR = 0
        VALEUR = ""
        IF(TRIM(VARNAME).EQ.'MODEL.RESULTFILE') THEN
          I = INST%SISRES
          DO J = 1,250
            VALEUR(J:J) = INST%SIS_FILES(I)%NAME(J:J)
          ENDDO
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.BCFILE') THEN
          I = INST%SISCLI
          DO J = 1,250
            VALEUR(J:J) = INST%SIS_FILES(I)%NAME(J:J)
          ENDDO
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.GEOMETRYFILE') THEN
          I = INST%SISGEO
          DO J = 1,250
            VALEUR(J:J) = INST%SIS_FILES(I)%NAME(J:J)
          ENDDO
        ! <get_string>
        ELSE
          IERR = UNKNOWN_VAR_ERROR
          ERR_MESS = 'UNKNOWN VARIABLE NAME : '//TRIM(VARNAME)
        ENDIF
!
      END SUBROUTINE GET_STRING_SIS_D

      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !>@brief Defines the value of a string variable of sisyphe
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !>@param[in,out] INST The instance
      !>@param[in] VARNAME Name of the variable to write
      !>@param[in] VALEUR The value to write in the variable
      !>@param[in] VALUELEN Length of VALEUR
      !>@param[in] INDEX1 Index on the first dimension
      !>@param[in] INDEX2 Index on the second dimension
      !>@param[out] IERR 0 if subroutine successfull,
      !!                        error id otherwise
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      SUBROUTINE SET_STRING_SIS_D
     &     (INST, VARNAME, VALEUR, VALUELEN, INDEX1, INDEX2, IERR)
!
        TYPE(INSTANCE_SIS),    INTENT(INOUT) :: INST
        CHARACTER(LEN=SIS_VAR_LEN), INTENT(IN)  :: VARNAME
        INTEGER,               INTENT(IN) :: VALUELEN
        INTEGER,               INTENT(IN) :: INDEX1
        INTEGER,               INTENT(IN) :: INDEX2
        CHARACTER,             INTENT(IN) :: VALEUR(VALUELEN)
        INTEGER,               INTENT(OUT) :: IERR
!
        INTEGER I,J
!
        IERR = 0
        IF(TRIM(VARNAME).EQ.'MODEL.RESULTFILE') THEN
          I = INST%SISRES
          DO J=1,VALUELEN
            INST%SIS_FILES(I)%NAME(J:J) = VALEUR(J)
          ENDDO
        ! <set_string>
        ELSE
          IERR = UNKNOWN_VAR_ERROR
          ERR_MESS = 'UNKNOWN VARIABLE NAME : '//TRIM(VARNAME)
        ENDIF
!
      END SUBROUTINE SET_STRING_SIS_D

      !
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !>@brief Get a boolean variable from sisyphe
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !>@param[in,out] INST The instance
      !>@param[in] VARNAME Name of the variable to read
      !>@param[out] VALEUR Containis the read value
      !>@param[in] INDEX1 Index on the first dimension
      !>@param[in] INDEX2 Index on the second dimension
      !>@param[in] INDEX3 Index on the third dimension
      !>@param[out] IERR 0 if subroutine successfull,
      !!                        error id otherwise
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      SUBROUTINE GET_BOOLEAN_SIS_D
     &     (INST, VARNAME, VALEUR, INDEX1, INDEX2, INDEX3, IERR)
!
        TYPE(INSTANCE_SIS),    INTENT(IN) :: INST
        CHARACTER(LEN=SIS_VAR_LEN), INTENT(IN)  :: VARNAME
        INTEGER,               INTENT(OUT) :: VALEUR
        INTEGER,               INTENT(IN) :: INDEX1
        INTEGER,               INTENT(IN) :: INDEX2
        INTEGER,               INTENT(IN) :: INDEX3
        INTEGER,               INTENT(OUT) :: IERR
!
        IERR = 0
        VALEUR = 0
        IF(TRIM(VARNAME).EQ.'XXXXXX') THEN
          VALEUR = 0
        ! <get_boolean>
        ELSE
          IERR = UNKNOWN_VAR_ERROR
          ERR_MESS = 'UNKNOWN VARIABLE NAME : '//TRIM(VARNAME)
        ENDIF
!
      END SUBROUTINE GET_BOOLEAN_SIS_D
!
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !>@brief Defines the value of a boolean variable of sisyphe
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !>@param[in,out] INST The instance
      !>@param[in] VARNAME Name of the variable to write
      !>@param[in] VALEUR The value to write in the variable
      !>@param[in] INDEX1 Index on the first dimension
      !>@param[in] INDEX2 Index on the second dimension
      !>@param[in] INDEX3 Index on the third dimension
      !>@param[out] IERR 0 if subroutine successfull,
      !!                        error id otherwise
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      SUBROUTINE SET_BOOLEAN_SIS_D
     &     (INST, VARNAME, VALEUR, INDEX1, INDEX2, INDEX3, IERR)
!
        TYPE(INSTANCE_SIS),    INTENT(INOUT) :: INST
        CHARACTER(LEN=SIS_VAR_LEN), INTENT(IN)  :: VARNAME
        INTEGER,               INTENT(IN) :: VALEUR
        INTEGER,               INTENT(IN) :: INDEX1
        INTEGER,               INTENT(IN) :: INDEX2
        INTEGER,               INTENT(IN) :: INDEX3
        INTEGER,               INTENT(OUT) :: IERR
!
        INTEGER DEBUG
!
        IERR = 0
        IF(TRIM(VARNAME).EQ.'XXXXXX') THEN
          DEBUG = 1
        ! <set_boolean>
        ELSE
          IERR = UNKNOWN_VAR_ERROR
          ERR_MESS = 'UNKNOWN VARIABLE NAME : '//TRIM(VARNAME)
        ENDIF
!
      END SUBROUTINE SET_BOOLEAN_SIS_D

      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !>@brief Get size informations on a variable of sisyphe
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !>@param[in] INST Instance
      !>@param[in] VARNAME Name of the variable
      !>@param[out] DIM1 Size of the first dimension
      !>@param[out] DIM2 Size of the second dimension
      !>@param[out] DIM3 Size of the third dimension
      !>@param[out] IERR 0 if subroutine successfull,
      !!                        error id otherwise
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      SUBROUTINE GET_VAR_SIZE_SIS_D
     &         (INST, VARNAME, DIM1, DIM2, DIM3, IERR)
        TYPE(INSTANCE_SIS),    INTENT(IN) :: INST
        CHARACTER(LEN=SIS_VAR_LEN), INTENT(IN)  :: VARNAME
        INTEGER,               INTENT(OUT) :: DIM1
        INTEGER,               INTENT(OUT) :: DIM2
        INTEGER,               INTENT(OUT) :: DIM3
        INTEGER,               INTENT(OUT) :: IERR
!
        IERR = 0
        DIM1 = 0
        DIM2 = 0
        DIM3 = 0
        IF(TRIM(VARNAME).EQ.'MODEL.FLOWRATEQ') THEN
          DIM1 = INST%Q%DIM1
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.D50') THEN
          DIM1 = SIZE(INST%D50)
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.CBOR_CLASSE') THEN
          DIM1 = SIZE(INST%CBOR_CLASSE)
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.SHIELDS') THEN
          DIM1 = SIZE(INST%AC)
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.XWC') THEN
          DIM1 = SIZE(INST%XWC)
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.EVOLUTION') THEN
          DIM1 = INST%E%DIM1
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.Z') THEN
          DIM1 = INST%Z%DIM1
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.BOTTOMELEVATION') THEN
          DIM1 = INST%ZF%DIM1
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.ZF_C') THEN
          DIM1 = INST%ZF_C%DIM1
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.FLBOR') THEN
          DIM1 = INST%FLBOR%DIM1
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.FLBOR_SIS') THEN
          DIM1 = INST%FLBOR_SIS%DIM1
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.X') THEN
          DIM1 = INST%MESH%X%DIM1
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.Y') THEN
          DIM1 = INST%MESH%Y%DIM1
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.XNEBOR') THEN
          DIM1 = INST%MESH%XNEBOR%DIM1
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.YNEBOR') THEN
          DIM1 = INST%MESH%YNEBOR%DIM1
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.TOB') THEN
          DIM1 = INST%TOB%DIM1
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.CHESTR') THEN
          DIM1 = INST%CHESTR%DIM1
        ELSEIF(TRIM(VARNAME).EQ.'MODEL.LIHBOR') THEN
          DIM1 = INST%LIHBOR%DIM1
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.CLU') THEN
          DIM1 = INST%CLU%DIM1
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.CLV') THEN
          DIM1 = INST%CLV%DIM1
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.LIQBOR') THEN
          DIM1 = INST%LIQBOR%DIM1
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.LICBOR') THEN
          DIM1 = INST%LICBOR%DIM1
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.LIEBOR') THEN
          DIM1 = INST%LIEBOR%DIM1
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.RESULTFILE') THEN
          DIM1 = 250
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.NTIMESTEPS') THEN
          DIM1 = 0
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.PARTHENIADES') THEN
          DIM1 = 0
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.IKLE') THEN
          DIM1 = INST%MESH%IKLE%DIM2
          DIM2 = INST%MESH%IKLE%DIM1
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.NACHB')THEN
          DIM1 = INST%NPTIR
          DIM2 = INST%NBMAXNSHARE
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.CONCENTRATION') THEN
          DIM1 = INST%CS%N
          DIM2 = INST%CS%ADR(1)%P%DIM1
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.QBOR') THEN
          DIM1 = INST%QBOR%N
          DIM2 = INST%QBOR%ADR(1)%P%DIM1
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.CBOR') THEN
          DIM1 = INST%CBOR%N
          DIM2 = INST%CBOR%ADR(1)%P%DIM1
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.EBOR') THEN
          DIM1 = INST%EBOR%N
          DIM2 = INST%EBOR%ADR(1)%P%DIM1
        ! <get_var_size>
        ELSE
          IERR = UNKNOWN_VAR_ERROR
          ERR_MESS = 'UNKNOWN VARIABLE NAME : '//TRIM(VARNAME)
        ENDIF
!
      END SUBROUTINE GET_VAR_SIZE_SIS_D
!
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !>@brief Get the information on the type of variable
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !>@param[in] VARNAME Name of the varaible
      !>@param[out] VARTYPE Type of the varaible (INTEGER, DOUBLE,
      !                     BOOLEAN, STRING)
      !>@param[out] READONLY True if the varaible cannot be modified
      !>@param[out] NDIM Name of the varaible
      !>@param[out] IENT 1 if the numbering is on point
      !>@param[out] JENT 1 if the numbering is on point
      !>@param[out] KENT 1 if the numbering is on point
      !>@param[out] GETPOS Postion after which the get is posible
      !!                        on the variable
      !>@param[out] SETPOS Postion after which the set is posible
      !!                        on the variable
      !>@param[out] IERR 0 if subroutine successfull,
      !!                        error id otherwise
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      SUBROUTINE GET_VAR_TYPE_SIS_D
     &        (VARNAME, VARTYPE, READONLY, NDIM, IENT, JENT, KENT,
     &         GETPOS, SETPOS, IERR)
!
        CHARACTER(LEN=SIS_VAR_LEN),  INTENT(IN)  :: VARNAME
        CHARACTER(LEN=SIS_TYPE_LEN), INTENT(OUT) :: VARTYPE
        LOGICAL,                     INTENT(OUT) :: READONLY
        INTEGER,                     INTENT(OUT) :: NDIM
        INTEGER,                     INTENT(OUT) :: IERR
        INTEGER,                     INTENT(OUT) :: IENT
        INTEGER,                     INTENT(OUT) :: JENT
        INTEGER,                     INTENT(OUT) :: KENT
        INTEGER,                     INTENT(OUT) :: GETPOS
        INTEGER,                     INTENT(OUT) :: SETPOS
!
        IERR = 0
        VARTYPE = ''
        READONLY = .TRUE.
        NDIM = 0
        IENT = 0
        JENT = 0
        KENT = 0
        GETPOS = -1
        SETPOS = -1
!
        IF(TRIM(VARNAME).EQ.'MODEL.FLOWRATEQ') THEN
          VARTYPE = 'DOUBLE'
          READONLY = .FALSE.
          NDIM = 1
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.D50') THEN
          VARTYPE = 'DOUBLE'
          READONLY = .FALSE.
          NDIM = 1
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.CBOR_CLASSE') THEN
          VARTYPE = 'DOUBLE'
          READONLY = .FALSE.
          NDIM = 1
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.SHIELDS') THEN
          VARTYPE = 'DOUBLE'
          READONLY = .FALSE.
          NDIM = 1
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.PARTHENIADES') THEN
          VARTYPE = 'DOUBLE'
          READONLY = .FALSE.
          NDIM = 0
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.XWC') THEN
          VARTYPE = 'DOUBLE'
          READONLY = .FALSE.
          NDIM = 1
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.NTIMESTEPS') THEN
          VARTYPE = 'INTEGER'
          READONLY = .FALSE.
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.EVOLUTION') THEN
          VARTYPE = 'DOUBLE'
          READONLY = .FALSE.
          NDIM = 1
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.Z') THEN
          IENT = 1
          VARTYPE = 'DOUBLE'
          READONLY = .FALSE.
          NDIM = 1
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.BOTTOMELEVATION') THEN
          IENT = 1
          VARTYPE = 'DOUBLE'
          READONLY = .FALSE.
          NDIM = 1
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.ZF_C') THEN
          IENT = 1
          VARTYPE = 'DOUBLE'
          READONLY = .FALSE.
          NDIM = 1
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.QBOR') THEN
          VARTYPE = 'DOUBLE'
          READONLY = .FALSE.
          NDIM = 1
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.EBOR') THEN
          VARTYPE = 'DOUBLE'
          READONLY = .FALSE.
          NDIM = 1
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.CBOR') THEN
          VARTYPE = 'DOUBLE'
          READONLY = .FALSE.
          NDIM = 1
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.FLBOR') THEN
          VARTYPE = 'DOUBLE'
          READONLY = .FALSE.
          NDIM = 1
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.FLBOR_SIS') THEN
          VARTYPE = 'DOUBLE'
          READONLY = .FALSE.
          NDIM = 1
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.X') THEN
          IENT = 1
          VARTYPE = 'DOUBLE'
          READONLY = .FALSE.
          NDIM = 1
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.Y') THEN
          IENT = 1
          VARTYPE = 'DOUBLE'
          READONLY = .FALSE.
          NDIM = 1
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.XNEBOR') THEN
          VARTYPE = 'DOUBLE'
          READONLY = .FALSE.
          NDIM = 1
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.YNEBOR') THEN
          VARTYPE = 'DOUBLE'
          READONLY = .FALSE.
          NDIM = 1
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.TIMESTEP') THEN
          VARTYPE = 'DOUBLE'
          READONLY = .FALSE.
          NDIM = 1
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.TOB') THEN
          VARTYPE = 'DOUBLE'
          READONLY = .FALSE.
          NDIM = 1
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.CHESTR') THEN
          IENT = 1
          VARTYPE = 'DOUBLE'
          READONLY = .FALSE.
          NDIM = 1
        ELSEIF(TRIM(VARNAME).EQ.'MODEL.LIHBOR') THEN
          VARTYPE = 'INTEGER'
          READONLY = .FALSE.
          NDIM = 1
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.CLU') THEN
          VARTYPE = 'INTEGER'
          READONLY = .FALSE.
          NDIM = 1
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.CLV') THEN
          VARTYPE = 'INTEGER'
          READONLY = .FALSE.
          NDIM = 1
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.LIQBOR') THEN
          VARTYPE = 'INTEGER'
          READONLY = .FALSE.
          NDIM = 1
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.LICBOR') THEN
          VARTYPE = 'INTEGER'
          READONLY = .FALSE.
          NDIM = 1
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.LIEBOR') THEN
          VARTYPE = 'INTEGER'
          READONLY = .FALSE.
          NDIM = 1
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.RESULTFILE') THEN
          VARTYPE = 'STRING'
          READONLY = .FALSE.
          NDIM = 1
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.IKLE') THEN
          VARTYPE = 'INTEGER'
          READONLY = .FALSE.
          NDIM = 2
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.NACHB') THEN
          VARTYPE = 'INTEGER'
          READONLY = .TRUE.
          NDIM = 2
          GETPOS = RUN_ALLOCATION_POS
          SETPOS = RUN_ALLOCATION_POS
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.CONCENTRATION') THEN
          VARTYPE = 'DOUBLE_BLOCK'
          READONLY = .FALSE.
          NDIM = 2
          GETPOS = NO_POSITION
          SETPOS = NO_POSITION
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.NSICLA') THEN
          VARTYPE = 'INTEGER'
          READONLY = .FALSE.
          NDIM = 0
          GETPOS = NO_POSITION
          SETPOS = NO_POSITION
        ! <get_var_type>
        ELSE
          IERR = UNKNOWN_VAR_ERROR
          ERR_MESS = 'UNKNOWN VARIABLE NAME : '//TRIM(VARNAME)
        ENDIF
!
      END SUBROUTINE GET_VAR_TYPE_SIS_D
!
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !>@brief Get the description of the ith variable
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !>@param[in] I Number of the variable
      !>@param[in] VAR_LEN Size of varname
      !>@param[in] INFO_LEN Size of varinfo
      !>@param[out] VARNAME Name of the variable
      !>@param[out] VARINFO Description of the variable
      !>@param[out] IERR 0 if subroutine successfull,
      !!                        error id otherwise
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      SUBROUTINE GET_VAR_INFO_SIS_D(I, VAR_LEN, INFO_LEN,
     &                              VARNAME, VARINFO, IERR)
!
        INTEGER, INTENT(IN) :: I
        INTEGER, INTENT(IN) :: VAR_LEN
        INTEGER, INTENT(IN) :: INFO_LEN
        CHARACTER, INTENT(OUT) :: VARNAME(VAR_LEN)
        CHARACTER, INTENT(OUT) :: VARINFO(INFO_LEN)
        INTEGER, INTENT(OUT) :: IERR
!
        INTEGER :: J
!
        IERR = 0

        DO J=1,SIS_VAR_LEN
          VARNAME(J:J) = VNAME_SIS(I)(J:J)
        ENDDO
        DO J=1,SIS_INFO_LEN
          VARINFO(J:J) = VINFO_SIS(I)(J:J)
        ENDDO

        RETURN
      END SUBROUTINE GET_VAR_INFO_SIS_D
!
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !>@brief Build a description of each variable
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !>@param[out] IERR 0 if subroutine successfull,
      !!                        error id otherwise
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      SUBROUTINE SET_VAR_LIST_SIS_D(IERR)
!
        INTEGER, INTENT(OUT) :: IERR
!
        INTEGER :: I
!
        I=0
        IERR = 0
!
        IF(.NOT.ALLOCATED(VNAME_SIS)) THEN
          ALLOCATE(VNAME_SIS(NB_VAR_SIS),STAT=IERR)
          IF(IERR.NE.0) RETURN
          ALLOCATE(VINFO_SIS(NB_VAR_SIS),STAT=IERR)
          IF(IERR.NE.0) RETURN
          I = I + 1
          VNAME_SIS(I) = 'MODEL.FLOWRATEQ'
          VINFO_SIS(I) = 'SOLID TRANSPORT FLOWRATE'
          I = I + 1
          VNAME_SIS(I) = 'MODEL.D50'
          VINFO_SIS(I) = 'MEDIAN GRAIN SIZE'
          I = I + 1
          VNAME_SIS(I) = 'MODEL.CBOR_CLASSE'
          VINFO_SIS(I) = 'IMPOSED CONCENTRATION IN CASE FILE'
          I = I + 1
          VNAME_SIS(I) = 'MODEL.SHIELDS'
          VINFO_SIS(I) = 'SHIELDS PARAMETER'
          I = I + 1
          VNAME_SIS(I) = 'MODEL.XWC'
          VINFO_SIS(I) = 'SETTLING VELOCITY'
          I = I + 1
          VNAME_SIS(I) = 'MODEL.EVOLUTION'
          VINFO_SIS(I) = 'EVOLUTION OF BED'
          I = I + 1
          VNAME_SIS(I) = 'MODEL.PARTHENIADES'
          VINFO_SIS(I) = 'PARTHENIADES CONSTANT'
          I = I + 1
          VNAME_SIS(I) = 'MODEL.Z'
          VINFO_SIS(I) = 'FREE SURFACE ELEVATION'
          I = I + 1
          VNAME_SIS(I) = 'MODEL.BOTTOMELEVATION'
          VINFO_SIS(I) = 'LEVEL OF THE BOTTOM'
          I = I + 1
          VNAME_SIS(I) = 'MODEL.ZF_C'
          VINFO_SIS(I) = 'EVOLUTION DUE TO BEDLOAD'
          I = I + 1
          VNAME_SIS(I) = 'MODEL.QBOR'
          VINFO_SIS(I) = 'BOUNDARY VALUE ON Q FOR EACH BOUNDARY POINT'
          I = I + 1
          VNAME_SIS(I) = 'MODEL.EBOR'
          VINFO_SIS(I) = 'BOUNDARY VALUE ON E FOR EACH BOUNDARY POINT'
          I = I + 1
          VNAME_SIS(I) = 'MODEL.CBOR'
          VINFO_SIS(I) = 'BOUNDARY VALUE ON C FOR EACH BOUNDARY POINT'
          I = I + 1
          VNAME_SIS(I) = 'MODEL.FLBOR'
          VINFO_SIS(I) = 'BOUNDARY VALUE ON ZF FOR EACH BOUNDARY POINT'
          I = I + 1
          VNAME_SIS(I) = 'MODEL.FLBOR_SIS'
          VINFO_SIS(I) = ''
          I = I + 1
          VNAME_SIS(I) = 'MODEL.X'
          VINFO_SIS(I) = 'X COORDINATES FOR EACH POINT OF THE MESH'
          I = I + 1
          VNAME_SIS(I) = 'MODEL.Y'
          VINFO_SIS(I) = 'Y COORDINATES FOR EACH POINT OF THE MESH'
          I = I + 1
          VNAME_SIS(I) = 'MODEL.XNEBOR'
          VINFO_SIS(I) = ''
          I = I + 1
          VNAME_SIS(I) = 'MODEL.YNEBOR'
          VINFO_SIS(I) = ''
          I = I + 1
          VNAME_SIS(I) = 'MODEL.TIMESTEP'
          VINFO_SIS(I) = 'TIME STEP'
          I = I + 1
          VNAME_SIS(I) = 'MODEL.TOB'
          VINFO_SIS(I) = 'SHEAR STRESS'
          I = I + 1
          VNAME_SIS(I) = 'MODEL.CHESTR'
          VINFO_SIS(I) = 'STRIKLER ON POINT'
          I = I + 1
          VNAME_SIS(I) = 'MODEL.LIHBOR'
          VINFO_SIS(I) = 'BOUNDARY TYPE ON H FOR EACH BOUNDARY POINT'
          I = I + 1
          VNAME_SIS(I) = 'MODEL.CLU'
          VINFO_SIS(I) = 'BOUNDARY TYPE ON U FOR EACH BOUNDARY POINT'
          I = I + 1
          VNAME_SIS(I) = 'MODEL.CLV'
          VINFO_SIS(I) = 'BOUNDARY TYPE ON V FOR EACH BOUNDARY POINT'
          I = I + 1
          VNAME_SIS(I) = 'MODEL.LIQBOR'
          VINFO_SIS(I) = 'BOUNDARY TYPE ON Q FOR EACH BOUNDARY POINT'
          I = I + 1
          VNAME_SIS(I) = 'MODEL.LICBOR'
          VINFO_SIS(I) = 'BOUNDARY TYPE ON C FOR EACH BOUNDARY POINT'
          I = I + 1
          VNAME_SIS(I) = 'MODEL.LIEBOR'
          VINFO_SIS(I) = 'BOUNDARY TYPE ON E FOR EACH BOUNDARY POINT'
          I = I + 1
          VNAME_SIS(I) = 'MODEL.NUMLIQ'
          VINFO_SIS(I) = 'LIQUID BOUNDARY NUMBERING'
          I = I + 1
          VNAME_SIS(I) = 'MODEL.NPOIN'
          VINFO_SIS(I) = 'NUMBER OF POINT IN THE MESH'
          I = I + 1
          VNAME_SIS(I) = 'MODEL.NELEM'
          VINFO_SIS(I) = 'NUMBER OF ELEMENT IN THE MESH'
          I = I + 1
          VNAME_SIS(I) = 'MODEL.NPFTR'
          VINFO_SIS(I) = 'NUMBER OF BOUNDARY POINTS'
          I = I + 1
          VNAME_SIS(I) = 'MODEL.NELMAX'
          VINFO_SIS(I) = 'MAXIMUM NUMBER OF ELEMENTS IN THE MESH'
          I = I + 1
          VNAME_SIS(I) = 'MODEL.IKLE'
          VINFO_SIS(I) = 'CONNECTIVITY TABLE OF ELEMENTS AND NODES'
          I = I + 1
          VNAME_SIS(I) = 'MODEL.NACHB'
          VINFO_SIS(I) = 'NUMBERS OF PROC CONTAINING A GIVEN POINT'
          I = I + 1
          VNAME_SIS(I) = 'MODEL.NTIMESTEPS'
          VINFO_SIS(I) = 'NUMBER OF TIME STEPS'
          I = I + 1
          VNAME_SIS(I) = 'MODEL.CURRENTSTEP'
          VINFO_SIS(I) = 'CURRENT TIME STEP'
          I = I + 1
          VNAME_SIS(I) = 'MODEL.RESULTFILE'
          VINFO_SIS(I) = 'RESULTS FILE OF THE CASE'
          I = I + 1
          VNAME_SIS(I) = 'MODEL.BCFILE'
          VINFO_SIS(I) = 'BOUNDARY CONDITIONS FILE OF THE CASE'
          I = I + 1
          VNAME_SIS(I) = 'MODEL.GEOMETRYFILE'
          VINFO_SIS(I) = 'GEOMETRY FILE OF THE CASE'
          I = I + 1
          VNAME_SIS(I) = 'MODEL.CONCENTRATION'
          VINFO_SIS(I) = 'CONCENTRATION AT TIME N'
          I = I + 1
          VNAME_SIS(I) = 'MODEL.NSICLA'
          VINFO_SIS(I) = 'NUMBER OF SIZE-CLASSES OF BED MATERIAL'
          ! <set_var_list>
          IF(I.NE.NB_VAR_SIS) THEN
            IERR = INCREASE_NB_VAR_SIS_ERROR
            RETURN
          ENDIF
        ENDIF
!
      END SUBROUTINE SET_VAR_LIST_SIS_D
!
      END MODULE API_HANDLE_VAR_SIS
