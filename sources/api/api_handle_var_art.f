!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!>@brief Getter/setter of artemis variables
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      MODULE API_HANDLE_VAR_ART

        USE API_HANDLE_ERROR
        USE API_INSTANCE_ART
        IMPLICIT NONE
        !> Size of the string containing the name of a variable
        INTEGER, PARAMETER :: ART_VAR_LEN=40
        !> Size of the string containing the type of a variable
        INTEGER, PARAMETER :: ART_TYPE_LEN=12
        !> Size of the string containing the information about a variable
        INTEGER, PARAMETER :: ART_INFO_LEN=200
        !> The maximum number of variable
        INTEGER, PARAMETER :: NB_VAR_ART=20
        !> List of variable names
        CHARACTER(LEN=ART_VAR_LEN),ALLOCATABLE :: VNAME_ART(:)
        !> List of variable info
        CHARACTER(LEN=ART_INFO_LEN),ALLOCATABLE :: VINFO_ART(:)
!
      CONTAINS
!
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !>@brief Get a double array from artemis
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      SUBROUTINE GET_DOUBLE_ARRAY_ART_D
     &     (INST, VARNAME, VALEUR, DIM1, IERR, BLOCK_INDEX)
!
        TYPE(INSTANCE_ART),         INTENT(IN) :: INST
        CHARACTER(LEN=ART_VAR_LEN), INTENT(IN) :: VARNAME
        INTEGER,                    INTENT(IN) :: DIM1
        DOUBLE PRECISION,           INTENT(OUT):: VALEUR(DIM1)
        INTEGER,                    INTENT(OUT):: IERR
        INTEGER, OPTIONAL,          INTENT(IN) :: BLOCK_INDEX
!
        IERR = 0
        VALEUR = 0.0
!
        IF(TRIM(VARNAME).EQ.'MODEL.X') THEN
          VALEUR(1:INST%MESH%X%DIM1) = INST%MESH%X%R(1:INST%MESH%X%DIM1)
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.Y') THEN
          VALEUR(1:INST%MESH%Y%DIM1) = INST%MESH%Y%R(1:INST%MESH%Y%DIM1)
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.WAVEPHASE') THEN
          VALEUR(1:SIZE(INST%PHAS%R)) =
     &     INST%PHAS%R(1:SIZE(INST%PHAS%R))
        ! <get_double_array>
        ELSE
          IERR = UNKNOWN_VAR_ERROR
          ERR_MESS = 'UNKNOWN VARIABLE NAME : '//TRIM(VARNAME)
        ENDIF
!
      END SUBROUTINE GET_DOUBLE_ARRAY_ART_D
!
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !>@brief Defines the value of a double array of artemis
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      SUBROUTINE SET_DOUBLE_ARRAY_ART_D
     &     (INST, VARNAME, VALEUR, DIM1, IERR, BLOCK_INDEX)
!
        TYPE(INSTANCE_ART),    INTENT(INOUT) :: INST
        CHARACTER(LEN=ART_VAR_LEN), INTENT(IN)  :: VARNAME
        INTEGER,               INTENT(IN) :: DIM1
        DOUBLE PRECISION,      INTENT(IN) :: VALEUR(DIM1)
        INTEGER,               INTENT(OUT) :: IERR
        INTEGER, OPTIONAL,          INTENT(IN) :: BLOCK_INDEX
!
        IERR = 0
        IF(TRIM(VARNAME).EQ.'MODEL.X') THEN
          INST%MESH%X%R(1:INST%MESH%X%DIM1) = VALEUR(1:INST%MESH%X%DIM1)
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.Y') THEN
          INST%MESH%X%R(1:INST%MESH%Y%DIM1) = VALEUR(1:INST%MESH%Y%DIM1)
        ! <set_double_array>
        ELSE
          IERR = UNKNOWN_VAR_ERROR
          ERR_MESS = 'UNKNOWN VARIABLE NAME : '//TRIM(VARNAME)
        ENDIF
!
      END SUBROUTINE SET_DOUBLE_ARRAY_ART_D
!
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !>@brief Get an integer array
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      SUBROUTINE GET_INTEGER_ARRAY_ART_D
     &     (INST, VARNAME, VALEUR, DIM1, IERR)
!
        TYPE(INSTANCE_ART),    INTENT(IN) :: INST
        CHARACTER(LEN=ART_VAR_LEN), INTENT(IN)  :: VARNAME
        INTEGER,               INTENT(IN) :: DIM1
        INTEGER,               INTENT(OUT) :: VALEUR(DIM1)
        INTEGER,               INTENT(OUT) :: IERR
!
        IERR = 0
        VALEUR = -1
        IF(TRIM(VARNAME).EQ.'MODEL.LIHBOR') THEN
          VALEUR(1:INST%LIHBOR%DIM1) = INST%LIHBOR%I(1:INST%LIHBOR%DIM1)
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.LIUBOR') THEN
          VALEUR(1:INST%LIUBOR%DIM1) = INST%LIUBOR%I(1:INST%LIUBOR%DIM1)
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.LIVBOR') THEN
          VALEUR(1:INST%LIVBOR%DIM1) = INST%LIVBOR%I(1:INST%LIVBOR%DIM1)
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.KP1BOR') THEN
          VALEUR(1:INST%MESH%KP1BOR%DIM1) =
     &    INST%MESH%KP1BOR%I(1:INST%MESH%KP1BOR%DIM1)
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.IKLE') THEN
          VALEUR(1:SIZE(INST%MESH%IKLE%I)) =
     &    INST%MESH%IKLE%I(1:SIZE(INST%MESH%IKLE%I))
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.NACHB') THEN
          VALEUR(1:SIZE(INST%MESH%NACHB%I)) =
     &         INST%MESH%NACHB%I(1:SIZE(INST%MESH%NACHB%I))
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.KNOLG') THEN
          VALEUR(1:INST%MESH%KNOLG%DIM1) =
     &         INST%MESH%KNOLG%I(1:INST%MESH%KNOLG%DIM1)
        ! <get_integer_array>
        ELSE
          IERR = UNKNOWN_VAR_ERROR
          ERR_MESS = 'UNKNOWN VARIABLE NAME : '//TRIM(VARNAME)
        ENDIF
!
      END SUBROUTINE GET_INTEGER_ARRAY_ART_D
!
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !>@brief Defines the value of an integer variable of telemac2d
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      SUBROUTINE SET_INTEGER_ARRAY_ART_D
     &     (INST, VARNAME, VALEUR, DIM1, IERR)
!
        TYPE(INSTANCE_ART),    INTENT(INOUT) :: INST
        CHARACTER(LEN=ART_VAR_LEN), INTENT(IN)  :: VARNAME
        INTEGER,               INTENT(IN) :: DIM1
        INTEGER,               INTENT(IN) :: VALEUR(DIM1)
        INTEGER,               INTENT(OUT) :: IERR
!
        IERR = 0
        IF(TRIM(VARNAME).EQ.'MODEL.LIHBOR') THEN
          INST%LIHBOR%I(1:INST%LIHBOR%DIM1) = VALEUR(1:INST%LIUBOR%DIM1)
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.LIUBOR') THEN
          INST%LIUBOR%I(1:INST%LIUBOR%DIM1) = VALEUR(1:INST%LIUBOR%DIM1)
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.LIVBOR') THEN
          INST%LIVBOR%I(1:INST%LIVBOR%DIM1) = VALEUR(1:INST%LIVBOR%DIM1)
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.IKLE') THEN
          INST%MESH%IKLE%I(1:SIZE(INST%MESH%IKLE%I)) =
     &    VALEUR(1:SIZE(INST%MESH%IKLE%I))
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.NACHB') THEN
          INST%MESH%NACHB%I(1:SIZE(INST%MESH%NACHB%I)) =
     &    VALEUR(1:SIZE(INST%MESH%NACHB%I))
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.KNOLG') THEN
          INST%MESH%KNOLG%I(1:INST%MESH%KNOLG%DIM1) =
     &    VALEUR(1:INST%MESH%KNOLG%DIM1)
        ! <set_integer_array>
        ELSE
          IERR = UNKNOWN_VAR_ERROR
          ERR_MESS = 'UNKNOWN VARIABLE NAME : '//TRIM(VARNAME)
        ENDIF
!
      END SUBROUTINE SET_INTEGER_ARRAY_ART_D
!
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !>@brief Get a double variable from telemac2d
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !>@param[in,out] INST the instance
      !>@param[in] VARNAME name of the variable to read
      !>@param[out] VALEUR Contains the read value
      !>@param[in] INDEX1 index on the first dimension
      !>@param[in] INDEX2 index on the second dimension
      !>@param[in] INDEX3 index on the third dimension
      !>@param[out] IERR 0 if subroutine successfull,
      !!                        error id otherwise
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      SUBROUTINE GET_DOUBLE_ART_D
     &     (INST, VARNAME, VALEUR, INDEX1, INDEX2, INDEX3, IERR)
!
        TYPE(INSTANCE_ART),         INTENT(IN) :: INST
        CHARACTER(LEN=ART_VAR_LEN), INTENT(IN) :: VARNAME
        DOUBLE PRECISION,           INTENT(OUT):: VALEUR
        INTEGER,                    INTENT(IN) :: INDEX1
        INTEGER,                    INTENT(IN) :: INDEX2
        INTEGER,                    INTENT(IN) :: INDEX3
        INTEGER,                    INTENT(OUT):: IERR
!
        IERR = 0
        VALEUR = 0.0
!
        IF(TRIM(VARNAME).EQ.'MODEL.X') THEN
          VALEUR = INST%MESH%X%R(INDEX1)
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.Y') THEN
          VALEUR = INST%MESH%Y%R(INDEX1)
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.WAVEPHASE') THEN
          VALEUR = INST%PHAS%R(INDEX1)
        ! <get_double>
        ELSE
          IERR = UNKNOWN_VAR_ERROR
          ERR_MESS = 'UNKNOWN VARIABLE NAME : '//TRIM(VARNAME)
        ENDIF
!
      END SUBROUTINE GET_DOUBLE_ART_D
!
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !>@brief Defines the value of a double variable of telemac2d
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
      SUBROUTINE SET_DOUBLE_ART_D
     &     (INST, VARNAME, VALEUR, INDEX1, INDEX2, INDEX3, IERR)
!
        TYPE(INSTANCE_ART),    INTENT(INOUT) :: INST
        CHARACTER(LEN=ART_VAR_LEN), INTENT(IN)  :: VARNAME
        DOUBLE PRECISION,      INTENT(IN) :: VALEUR
        INTEGER,               INTENT(IN) :: INDEX1
        INTEGER,               INTENT(IN) :: INDEX2
        INTEGER,               INTENT(IN) :: INDEX3
        INTEGER,               INTENT(OUT) :: IERR
!
        IERR = 0
        IF(TRIM(VARNAME).EQ.'xxx') THEN
          CONTINUE
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.WAVEPHASE') THEN
          INST%PHAS%R(INDEX1) = VALEUR
        ! <set_double>
        ELSE
          IERR = UNKNOWN_VAR_ERROR
          ERR_MESS = 'UNKNOWN VARIABLE NAME : '//TRIM(VARNAME)
        ENDIF
!
      END SUBROUTINE SET_DOUBLE_ART_D
!
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !>@brief Get an integer variable from telemac2d
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
      SUBROUTINE GET_INTEGER_ART_D
     &     (INST, VARNAME, VALEUR, INDEX1, INDEX2, INDEX3, IERR)
!
        TYPE(INSTANCE_ART),    INTENT(IN) :: INST
        CHARACTER(LEN=ART_VAR_LEN), INTENT(IN)  :: VARNAME
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
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.LIUBOR') THEN
          VALEUR = INST%LIUBOR%I(INDEX1)
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.LIVBOR') THEN
          VALEUR = INST%LIVBOR%I(INDEX1)
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.KP1BOR') THEN
          VALEUR = INST%MESH%KP1BOR%I(INDEX1)
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.NPOIN') THEN
          VALEUR = INST%MESH%NPOIN
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.NELEM') THEN
          VALEUR = INST%MESH%NELEM
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.NPTFR') THEN
          VALEUR = INST%MESH%NPTFR
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.NTIMESTEPS') THEN
          VALEUR = INST%NIT
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.IKLE') THEN
          VALEUR = INST%MESH%IKLE%I((INDEX2-1)*INST%MESH%IKLE%DIM1
     &                               + INDEX1)
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.NACHB') THEN
          VALEUR = INST%MESH%NACHB%I((INDEX2-1)*INST%NBMAXNSHARE
     &          + INDEX1)
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.KNOLG') THEN
          VALEUR = INST%MESH%KNOLG%I(INDEX1)
        ! <get_integer>
        ELSE
          IERR = UNKNOWN_VAR_ERROR
          ERR_MESS = 'UNKNOWN VARIABLE NAME : '//TRIM(VARNAME)
        ENDIF
!
      END SUBROUTINE GET_INTEGER_ART_D
!
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !>@brief Defines the value of an integer variable of telemac2d
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
      SUBROUTINE SET_INTEGER_ART_D
     &     (INST, VARNAME, VALEUR, INDEX1, INDEX2, INDEX3, IERR)
!
        TYPE(INSTANCE_ART),    INTENT(INOUT) :: INST
        CHARACTER(LEN=ART_VAR_LEN), INTENT(IN)  :: VARNAME
        INTEGER,               INTENT(IN) :: VALEUR
        INTEGER,               INTENT(IN) :: INDEX1
        INTEGER,               INTENT(IN) :: INDEX2
        INTEGER,               INTENT(IN) :: INDEX3
        INTEGER,               INTENT(OUT) :: IERR
!
        IERR = 0
        IF(TRIM(VARNAME).EQ.'MODEL.LIHBOR') THEN
          INST%LIHBOR%I(INDEX1) = VALEUR
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.LIUBOR') THEN
          INST%LIUBOR%I(INDEX1) = VALEUR
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.LIVBOR') THEN
          INST%LIVBOR%I(INDEX1) = VALEUR
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.NTIMESTEPS') THEN
          INST%NIT = VALEUR
        ! <set_integer>
        ELSE
          IERR = UNKNOWN_VAR_ERROR
          ERR_MESS = 'UNKNOWN VARIABLE NAME : '//TRIM(VARNAME)
        ENDIF
!
      END SUBROUTINE SET_INTEGER_ART_D
!
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !>@brief Get a string variable from telemac2d
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !>@param[in,out] INST The instance
      !>@param[in] VARNAME Name of the variable to read
      !>@param[out] VALEUR Containis the read value
      !>@param[in] VALUELEN Length of the string
      !>@param[in] INDEX1 Index on the first dimension
      !>@param[in] INDEX2 Index on the second dimension
      !>@param[out] IERR 0 if subroutine successfull,
      !!                        error id otherwise
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      SUBROUTINE GET_STRING_ART_D
     &     (INST, VARNAME, VALEUR, VALUELEN, INDEX1, INDEX2, IERR)
!
        TYPE(INSTANCE_ART),    INTENT(IN) :: INST
        CHARACTER(LEN=ART_VAR_LEN), INTENT(IN)  :: VARNAME
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
          I = INST%ARTRES
          DO J = 1,VALUELEN
            VALEUR(J:J) = INST%ART_FILES(I)%NAME(J:J)
          ENDDO
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.BCFILE') THEN
          I = INST%ARTCLI
          DO J = 1,VALUELEN
            VALEUR(J:J) = INST%ART_FILES(I)%NAME(J:J)
          ENDDO
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.GEOMETRYFILE') THEN
          I = INST%ARTGEO
          DO J = 1,VALUELEN
            VALEUR(J:J) = INST%ART_FILES(I)%NAME(J:J)
          ENDDO
        ! <get_string>
        ELSE
          IERR = UNKNOWN_VAR_ERROR
          ERR_MESS = 'UNKNOWN VARIABLE NAME : '//TRIM(VARNAME)
        ENDIF
!
      END SUBROUTINE GET_STRING_ART_D
!
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !>@brief Defines the value of a string variable of telemac2d
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !>@param[in,out] INST The instance
      !>@param[in] VARNAME Name of the variable to write
      !>@param[in] VALEUR The value to write in the variable
      !>@param[in] VALUELEN Length of the string
      !>@param[in] INDEX1 Index on the first dimension
      !>@param[in] INDEX2 Index on the second dimension
      !>@param[out] IERR 0 if subroutine successfull,
      !!                        error id otherwise
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      SUBROUTINE SET_STRING_ART_D
     &     (INST, VARNAME, VALEUR, VALUELEN, INDEX1, INDEX2, IERR)
!
        TYPE(INSTANCE_ART),    INTENT(INOUT) :: INST
        CHARACTER(LEN=ART_VAR_LEN), INTENT(IN)  :: VARNAME
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
          I = INST%ARTRES
          DO J=1,VALUELEN
            INST%ART_FILES(I)%NAME(J:J) = VALEUR(J)
          ENDDO
        ! <set_string>
        ELSE
          IERR = UNKNOWN_VAR_ERROR
          ERR_MESS = 'UNKNOWN VARIABLE NAME : '//TRIM(VARNAME)
        ENDIF
!
      END SUBROUTINE SET_STRING_ART_D
!
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !>@brief Get a boolean variable from telemac2d
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
      SUBROUTINE GET_BOOLEAN_ART_D
     &     (INST, VARNAME, VALEUR, INDEX1, INDEX2, INDEX3, IERR)
!
        TYPE(INSTANCE_ART),    INTENT(IN) :: INST
        CHARACTER(LEN=ART_VAR_LEN), INTENT(IN)  :: VARNAME
        INTEGER,               INTENT(OUT) :: VALEUR
        INTEGER,               INTENT(IN) :: INDEX1
        INTEGER,               INTENT(IN) :: INDEX2
        INTEGER,               INTENT(IN) :: INDEX3
        INTEGER,               INTENT(OUT) :: IERR
!
        IERR = 0
        VALEUR = 0
        IF(TRIM(VARNAME).EQ.'MODEL.DEBUG') THEN
          VALEUR = INST%DEBUG
        ! <get_boolean>
        ELSE
          IERR = UNKNOWN_VAR_ERROR
          ERR_MESS = 'UNKNOWN VARIABLE NAME : '//TRIM(VARNAME)
        ENDIF
!
      END SUBROUTINE GET_BOOLEAN_ART_D
!
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !>@brief Defines the value of a boolean variable of telemac2d
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
      SUBROUTINE SET_BOOLEAN_ART_D
     &     (INST, VARNAME, VALEUR, INDEX1, INDEX2, INDEX3, IERR)
!
        TYPE(INSTANCE_ART),    INTENT(INOUT) :: INST
        CHARACTER(LEN=ART_VAR_LEN), INTENT(IN)  :: VARNAME
        INTEGER,               INTENT(IN) :: VALEUR
        INTEGER,               INTENT(IN) :: INDEX1
        INTEGER,               INTENT(IN) :: INDEX2
        INTEGER,               INTENT(IN) :: INDEX3
        INTEGER,               INTENT(OUT) :: IERR
!
        IERR = 0
        IF(TRIM(VARNAME).EQ.'MODEL.DEBUG') THEN
          INST%DEBUG = VALEUR
        ! <set_boolean>
        ELSE
          IERR = UNKNOWN_VAR_ERROR
          ERR_MESS = 'UNKNOWN VARIABLE NAME : '//TRIM(VARNAME)
        ENDIF
!
      END SUBROUTINE SET_BOOLEAN_ART_D
!
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !>@brief Get size informations on a variable of telemac2d
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !>@param[in] INST Instance
      !>@param[in] VARNAME Name of the variable
      !>@param[out] DIM1 Size of the first dimension 0 if ndim=0
      !>@param[out] DIM2 Size of the second dimension 0 if ndim<1
      !>@param[out] DIM3 Size of the third dimension 0 if ndim<2
      !>@param[out] IERR 0 if subroutine successfull,
      !!                 error id otherwise
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      SUBROUTINE GET_VAR_SIZE_ART_D
     &         (INST, VARNAME, DIM1, DIM2, DIM3, IERR)
!
        TYPE(INSTANCE_ART),    INTENT(IN) :: INST
        CHARACTER(LEN=ART_VAR_LEN), INTENT(IN)  :: VARNAME
        INTEGER,               INTENT(OUT) :: DIM1
        INTEGER,               INTENT(OUT) :: DIM2
        INTEGER,               INTENT(OUT) :: DIM3
        INTEGER,               INTENT(OUT) :: IERR
!
        IERR = 0
        DIM1 = 0
        DIM2 = 0
        DIM3 = 0
!
        IF(TRIM(VARNAME).EQ.'MODEL.X') THEN
          DIM1 = INST%MESH%X%DIM1
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.Y') THEN
          DIM1 = INST%MESH%Y%DIM1
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.LIHBOR') THEN
          DIM1 = INST%LIHBOR%DIM1
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.LIUBOR') THEN
          DIM1 = INST%LIUBOR%DIM1
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.LIVBOR') THEN
          DIM1 = INST%LIVBOR%DIM1
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.RESULTFILE') THEN
          DIM1 = 250
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.EQUATION') THEN
          DIM1 = 20
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.GEOMETRYFILE') THEN
          DIM1 = 250
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.IKLE')THEN
          DIM1 = INST%MESH%IKLE%DIM2
          DIM2 = INST%MESH%IKLE%DIM1
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.NACHB')THEN
          DIM1 = INST%NPTIR
          DIM2 = INST%NBMAXNSHARE
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.KNOLG') THEN
          DIM1 = INST%MESH%KNOLG%DIM1
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.WAVEPHASE') THEN
          DIM1 = INST%PHAS%DIM1
        ! <get_var_size>
        ENDIF
!
      END SUBROUTINE GET_VAR_SIZE_ART_D
!
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !>@brief Get type information of a varaible
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !>@param[in] VARNAME Name of the variable
      !>@param[out] VARTYPE Type of the variable (INTEGER, STRING,
      !!                    DOUBLE, BOOLEAN)
      !>@param[out] NDIM Number of dimension
      !>@param[out] READONLY True if the variable cannot be modified
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
      SUBROUTINE GET_VAR_TYPE_ART_D
     &        (VARNAME, VARTYPE, READONLY, NDIM,IENT,JENT,KENT,
     &         GETPOS,SETPOS,IERR)
!
        CHARACTER(LEN=ART_VAR_LEN),  INTENT(IN)  :: VARNAME
        CHARACTER(LEN=ART_TYPE_LEN), INTENT(OUT) :: VARTYPE
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
        GETPOS = NO_POSITION
        SETPOS = NO_POSITION
!
        IF(TRIM(VARNAME).EQ.'MODEL.BCFILE') THEN
          VARTYPE = 'STRING'
          READONLY = .FALSE.
          NDIM = 0
          GETPOS = RUN_READ_CASE_POS
          SETPOS = RUN_READ_CASE_POS
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.DEBUG') THEN
          VARTYPE = 'INTEGER'
          READONLY = .FALSE.
          NDIM = 0
          GETPOS = RUN_SET_CONFIG_POS
          SETPOS = RUN_SET_CONFIG_POS
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.LIHBOR') THEN
          VARTYPE = 'INTEGER'
          READONLY = .FALSE.
          NDIM = 1
          GETPOS = RUN_ALLOCATION_POS
          SETPOS = RUN_ALLOCATION_POS
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.LIUBOR') THEN
          VARTYPE = 'INTEGER'
          READONLY = .FALSE.
          NDIM = 1
          GETPOS = RUN_ALLOCATION_POS
          SETPOS = RUN_ALLOCATION_POS
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.LIVBOR') THEN
          VARTYPE = 'INTEGER'
          READONLY = .FALSE.
          NDIM = 1
          GETPOS = RUN_ALLOCATION_POS
          SETPOS = RUN_ALLOCATION_POS
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.IKLE') THEN
          VARTYPE = 'INTEGER'
          READONLY = .TRUE.
          NDIM = 2
          GETPOS = RUN_ALLOCATION_POS
          SETPOS = RUN_ALLOCATION_POS
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.NACHB') THEN
          VARTYPE = 'INTEGER'
          READONLY = .TRUE.
          NDIM = 2
          GETPOS = RUN_ALLOCATION_POS
          SETPOS = RUN_ALLOCATION_POS
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.KNOLG') THEN
          VARTYPE = 'INTEGER'
          READONLY = .TRUE.
          NDIM = 1
          GETPOS = RUN_ALLOCATION_POS
          SETPOS = RUN_ALLOCATION_POS
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.X') THEN
          VARTYPE = 'DOUBLE'
          READONLY = .FALSE.
          NDIM = 1
          IENT = 1
          GETPOS = RUN_ALLOCATION_POS
          SETPOS = RUN_ALLOCATION_POS
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.Y') THEN
          VARTYPE = 'DOUBLE'
          READONLY = .FALSE.
          NDIM = 1
          IENT = 1
          GETPOS = RUN_ALLOCATION_POS
          SETPOS = RUN_ALLOCATION_POS
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.RESULTFILE') THEN
          VARTYPE = 'STRING'
          READONLY = .FALSE.
          NDIM = 1
          GETPOS = RUN_ALLOCATION_POS
          SETPOS = RUN_ALLOCATION_POS
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.GEOMETRYFILE') THEN
          VARTYPE = 'STRING'
          READONLY = .FALSE.
          NDIM = 1
          GETPOS = RUN_ALLOCATION_POS
          SETPOS = RUN_ALLOCATION_POS
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.NPOIN') THEN
          VARTYPE = 'INTEGER'
          READONLY = .FALSE.
          GETPOS = RUN_ALLOCATION_POS
          SETPOS = RUN_ALLOCATION_POS
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.NELEM') THEN
          VARTYPE = 'INTEGER'
          READONLY = .FALSE.
          GETPOS = RUN_ALLOCATION_POS
          SETPOS = RUN_ALLOCATION_POS
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.NELMAX') THEN
          VARTYPE = 'INTEGER'
          READONLY = .FALSE.
          GETPOS = RUN_ALLOCATION_POS
          SETPOS = RUN_ALLOCATION_POS
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.NPTFR') THEN
          VARTYPE = 'INTEGER'
          READONLY = .FALSE.
          GETPOS = RUN_ALLOCATION_POS
          SETPOS = RUN_ALLOCATION_POS
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.NTIMESTEPS') THEN
          VARTYPE = 'INTEGER'
          READONLY = .FALSE.
          NDIM = 0
          GETPOS = RUN_READ_CASE_POS
          SETPOS = RUN_READ_CASE_POS
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.WAVEPHASE') THEN
          VARTYPE = 'DOUBLE'
          READONLY = .FALSE.
          NDIM = 1
          GETPOS = NO_POSITION
          SETPOS = NO_POSITION
        ! <get_var_type>
        ELSE
          IERR = UNKNOWN_VAR_ERROR
          ERR_MESS = 'UNKNOWN VARIABLE NAME : '//TRIM(VARNAME)
        ENDIF
!
      END SUBROUTINE GET_VAR_TYPE_ART_D
!
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !>@brief Get the name and description of the ith variable
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !>@param[in] I Number of the variable
      !>@param[in] VAR_LEN Size of varname
      !>@param[in] INFO_LEN Size of varinfo
      !>@param[out] VARNAME Name of the variable
      !>@param[out] VARINFO Description of the variable
      !>@param[out] IERR 0 if subroutine successfull,
      !!                        error id otherwise
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      SUBROUTINE GET_VAR_INFO_ART_D(I, VAR_LEN, INFO_LEN,
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

        DO J=1,ART_VAR_LEN
          VARNAME(J:J) = VNAME_ART(I)(J:J)
        ENDDO
        DO J=1,ART_INFO_LEN
          VARINFO(J:J) = VINFO_ART(I)(J:J)
        ENDDO

        RETURN
      END SUBROUTINE GET_VAR_INFO_ART_D
!
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !>@brief Get a description of each variable
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !>@param[out] ierr 0 if subroutine successfull,
      !!                        error id otherwise
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      SUBROUTINE SET_VAR_LIST_ART_D(IERR)
!
        INTEGER, INTENT(OUT) :: IERR
!
        INTEGER :: I
!
        I=0
        IERR = 0
        IF(.NOT.ALLOCATED(VNAME_ART)) THEN
          ALLOCATE(VNAME_ART(NB_VAR_ART),STAT=IERR)
          IF(IERR.NE.0) RETURN
          ALLOCATE(VINFO_ART(NB_VAR_ART),STAT=IERR)
          IF(IERR.NE.0) RETURN
!
          I = I + 1
          VNAME_ART(I) = 'MODEL.BCFILE'
          VINFO_ART(I) = 'BOUNDARY CONDITION FILE NAME'
          I = I + 1
          VNAME_ART(I) = 'MODEL.DEBUG'
          VINFO_ART(I) = 'ACTIVATING DEBUG MODE'
          I = I + 1
          VNAME_ART(I) = 'MODEL.GEOMETRYFILE'
          VINFO_ART(I) = 'NAME OF THE GEOMERY FILE'
          I = I + 1
          VNAME_ART(I) = 'MODEL.IKLE'
          VINFO_ART(I) = 'CONNECTIVITY TABLE BETWEEN ELEMENT AND NODES'
          I = I + 1
          VNAME_ART(I) = 'MODEL.NACHB'
          VINFO_ART(I) = 'NUMBERS OF PROC CONTAINING A GIVEN POINT'
          I = I + 1
          VNAME_ART(I) = 'MODEL.KNOLG'
          VINFO_ART(I) =
     &         'GIVES THE INITIAL GLOBAL NUMBER OF A LOCAL POINT'
          I = I + 1
          VNAME_ART(I) = 'MODEL.LIHBOR'
          VINFO_ART(I) = 'BOUNDARY TYPE ON H FOR EACH BOUNDARY POINT'
          I = I + 1
          VNAME_ART(I) = 'MODEL.LIUBOR'
          VINFO_ART(I) = 'BOUNDARY TYPE ON U FOR EACH BOUNDARY POINT'
          I = I + 1
          VNAME_ART(I) = 'MODEL.LIVBOR'
          VINFO_ART(I) = 'BOUNDARY TYPE ON V FOR EACH BOUNDARY POINT'
          I = I + 1
          VNAME_ART(I) = 'MODEL.NELEM'
          VINFO_ART(I) = 'NUMBER OF ELEMENT IN THE MESH'
          I = I + 1
          VNAME_ART(I) = 'MODEL.NELMAX'
          VINFO_ART(I) = 'MAXIMUM NUMBER OF ELEMENTS ENVISAGED'
          I = I + 1
          VNAME_ART(I) = 'MODEL.NPOIN'
          VINFO_ART(I) = 'NUMBER OF POINT IN THE MESH'
          I = I + 1
          VNAME_ART(I) = 'MODEL.NPTFR'
          VINFO_ART(I) = 'NUMBER OF BOUNDARY POINTS'
          I = I + 1
          VNAME_ART(I) = 'MODEL.NTIMESTEPS'
          VINFO_ART(I) = 'NUMBER OF TIME STEPS'
          I = I + 1
          VNAME_ART(I) = 'MODEL.RESULTFILE'
          VINFO_ART(I) = 'NAME OF THE RESULT FILE'
          I = I + 1
          VNAME_ART(I) = 'MODEL.X'
          VINFO_ART(I) = 'X COORDINATES FOR EACH POINT OF THE MESH'
          I = I + 1
          VNAME_ART(I) = 'MODEL.Y'
          VINFO_ART(I) = 'Y COORDINATES FOR EACH POINT OF THE MESH'
          I = I + 1
          VNAME_ART(I) = 'MODEL.EQUATION'
          VINFO_ART(I) = 'NAME OF THE EQUATION USED'
          I = I + 1
          VNAME_ART(I) = 'MODEL.KP1BOR'
          VINFO_ART(I) = 'ARRAY OF NEIGHBOORS FOR EACH ELEMENT'
          I = I + 1
          VNAME_ART(I) = 'MODEL.WAVEPHASE'
          VINFO_ART(I) = 'PHASE OF THE WAVE'
          ! <set_var_list>
          IF(I.NE.NB_VAR_ART) THEN
            IERR = INCREASE_NB_VAR_ART_ERROR
            RETURN
          ENDIF
        ENDIF
!
      END SUBROUTINE SET_VAR_LIST_ART_D
!
      END MODULE API_HANDLE_VAR_ART
