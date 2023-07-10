!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!>@brief Getter/setter of tomawac variables
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      MODULE API_HANDLE_VAR_WAC

        USE API_HANDLE_ERROR
        USE API_INSTANCE_WAC
        IMPLICIT NONE
        !> Size of the string containing the name of a variable
        INTEGER, PARAMETER :: WAC_VAR_LEN=40
        !> Size of the string containing the type of a variable
        INTEGER, PARAMETER :: WAC_TYPE_LEN=12
        !> Size of the string containing the information about a variable
        INTEGER, PARAMETER :: WAC_INFO_LEN=200
        !> The maximum number of variable
        INTEGER, PARAMETER :: NB_VAR_WAC=29
        !> List of variable names
        CHARACTER(LEN=WAC_VAR_LEN),ALLOCATABLE :: VNAME_WAC(:)
        !> List of variable info
        CHARACTER(LEN=WAC_INFO_LEN),ALLOCATABLE :: VINFO_WAC(:)
!
      CONTAINS
!
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !>@brief Get a double array
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      SUBROUTINE GET_DOUBLE_ARRAY_WAC_D
     &     (INST, VARNAME, VALEUR, DIM1, IERR, BLOCK_INDEX)
!
        TYPE(INSTANCE_WAC),         INTENT(IN) :: INST
        CHARACTER(LEN=WAC_VAR_LEN), INTENT(IN) :: VARNAME
        INTEGER,                    INTENT(IN) :: DIM1
        DOUBLE PRECISION,           INTENT(OUT):: VALEUR(DIM1)
        INTEGER,                    INTENT(OUT):: IERR
        INTEGER, OPTIONAL,          INTENT(IN) :: BLOCK_INDEX
!
        IERR = 0
!
        IF(TRIM(VARNAME).EQ.'MODEL.X') THEN
          VALEUR(1:INST%MESH%X%DIM1) = INST%MESH%X%R(1:INST%MESH%X%DIM1)
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.Y') THEN
          VALEUR(1:INST%MESH%Y%DIM1) = INST%MESH%Y%R(1:INST%MESH%Y%DIM1)
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.BOTTOM') THEN
          VALEUR(1:SIZE(INST%ZF)) =
     &     INST%ZF(1:SIZE(INST%ZF))
!IMDC: NEW VARIABLES ADDED
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.WINDX') THEN
          VALEUR(1:SIZE(INST%WINDX)) =
     &     INST%WINDX(1:SIZE(INST%WINDX))
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.WINDY') THEN
          VALEUR(1:SIZE(INST%WINDY)) =
     &     INST%WINDY(1:SIZE(INST%WINDY))
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.VARIAN') THEN
          VALEUR(1:SIZE(INST%VARIAN)) =
     &     INST%VARIAN(1:SIZE(INST%VARIAN))
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.PFREAD5') THEN
          VALEUR(1:SIZE(INST%PFREAD5)) =
     &     INST%PFREAD5(1:SIZE(INST%PFREAD5))
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.PFREAD8') THEN
          VALEUR(1:SIZE(INST%PFREAD8)) =
     &     INST%PFREAD8(1:SIZE(INST%PFREAD8))
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.TM01') THEN
          VALEUR(1:SIZE(INST%TM01)) =
     &     INST%TM01(1:SIZE(INST%TM01))
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.TM02') THEN
          VALEUR(1:SIZE(INST%TM02)) =
     &     INST%TM02(1:SIZE(INST%TM02))
        ! <get_double_array>
        ELSE
          IERR = UNKNOWN_VAR_ERROR
          ERR_MESS = 'UNKNOWN VARIABLE NAME : '//TRIM(VARNAME)
        ENDIF
!
      END SUBROUTINE GET_DOUBLE_ARRAY_WAC_D
!
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !>@brief Defines the value of a double variable of tomawac
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      SUBROUTINE SET_DOUBLE_ARRAY_WAC_D
     &     (INST, VARNAME, VALEUR, DIM1, IERR, BLOCK_INDEX)
!
        TYPE(INSTANCE_WAC),    INTENT(INOUT) :: INST
        CHARACTER(LEN=WAC_VAR_LEN), INTENT(IN)  :: VARNAME
        INTEGER,               INTENT(IN) :: DIM1
        DOUBLE PRECISION,      INTENT(IN) :: VALEUR(DIM1)
        INTEGER,               INTENT(OUT) :: IERR
        INTEGER, OPTIONAL,     INTENT(IN) :: BLOCK_INDEX
!
        IERR = 0
        IF(TRIM(VARNAME).EQ.'MODEL.X') THEN
          INST%MESH%X%R(1:INST%MESH%X%DIM1) = VALEUR(1:INST%MESH%X%DIM1)
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.Y') THEN
          INST%MESH%Y%R(1:INST%MESH%Y%DIM1) = VALEUR(1:INST%MESH%Y%DIM1)
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.BOTTOM') THEN
          INST%ZF(1:SIZE(INST%ZF)) = VALEUR(1:SIZE(INST%ZF))
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.WINDX') THEN
          INST%WINDX(1:SIZE(INST%WINDX)) = VALEUR(1:SIZE(INST%WINDX))
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.WINDY') THEN
          INST%WINDY(1:SIZE(INST%WINDY)) = VALEUR(1:SIZE(INST%WINDY))
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.VARIAN') THEN
          INST%VARIAN(1:SIZE(INST%VARIAN)) = VALEUR(1:SIZE(INST%VARIAN))
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.TM01') THEN
          INST%TM01(1:SIZE(INST%TM01)) = VALEUR(1:SIZE(INST%TM01))
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.TM02') THEN
          INST%TM02(1:SIZE(INST%TM02)) = VALEUR(1:SIZE(INST%TM02))
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.PFREAD5') THEN
          INST%PFREAD5(1:SIZE(INST%PFREAD5)) =
     &       VALEUR(1:SIZE(INST%PFREAD5))
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.PFREAD8') THEN
          INST%PFREAD8(1:SIZE(INST%PFREAD8)) =
     &       VALEUR(1:SIZE(INST%PFREAD8))

        ! <set_double_array>
        ELSE
          IERR = UNKNOWN_VAR_ERROR
          ERR_MESS = 'UNKNOWN VARIABLE NAME : '//TRIM(VARNAME)
        ENDIF
!
      END SUBROUTINE SET_DOUBLE_ARRAY_WAC_D
!
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !>@brief Get an integer variable from tomawac
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      SUBROUTINE GET_INTEGER_ARRAY_WAC_D
     &     (INST, VARNAME, VALEUR, DIM1, IERR)
!
        TYPE(INSTANCE_WAC),    INTENT(IN) :: INST
        CHARACTER(LEN=WAC_VAR_LEN), INTENT(IN)  :: VARNAME
        INTEGER,               INTENT(IN) :: DIM1
        INTEGER,               INTENT(OUT) :: VALEUR(DIM1)
        INTEGER,               INTENT(OUT) :: IERR
!
        IERR = 0
        IF(TRIM(VARNAME).EQ.'MODEL.IKLE') THEN
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
      END SUBROUTINE GET_INTEGER_ARRAY_WAC_D
!
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !>@brief Defines the value of an integer variable of tomawac
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      SUBROUTINE SET_INTEGER_ARRAY_WAC_D
     &     (INST, VARNAME, VALEUR, DIM1, IERR)
!
        TYPE(INSTANCE_WAC),    INTENT(INOUT) :: INST
        CHARACTER(LEN=WAC_VAR_LEN), INTENT(IN)  :: VARNAME
        INTEGER,               INTENT(IN) :: DIM1
        INTEGER,               INTENT(IN) :: VALEUR(DIM1)
        INTEGER,               INTENT(OUT) :: IERR
!
        IERR = 0
        IF(TRIM(VARNAME).EQ.'MODEL.IKLE') THEN
          INST%MESH%IKLE%I(1:SIZE(INST%MESH%IKLE%I))
     &    = VALEUR(1:SIZE(INST%MESH%IKLE%I))
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
      END SUBROUTINE SET_INTEGER_ARRAY_WAC_D
!
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !>@brief Get a double variable from tomawac
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
      SUBROUTINE GET_DOUBLE_WAC_D
     &     (INST, VARNAME, VALEUR, INDEX1, INDEX2, INDEX3, IERR)
!
        TYPE(INSTANCE_WAC),         INTENT(IN) :: INST
        CHARACTER(LEN=WAC_VAR_LEN), INTENT(IN) :: VARNAME
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
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.AT') THEN
          VALEUR = INST%AT
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.DT') THEN
          VALEUR = INST%DT
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.BETAM') THEN
          VALEUR = INST%BETAM
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.VX_CTE') THEN
          VALEUR = INST%VX_CTE
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.VY_CTE') THEN
          VALEUR = INST%VY_CTE      
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.BOTTOM') THEN
          VALEUR = INST%ZF(INDEX1)
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.WINDX') THEN
          VALEUR = INST%WINDX(INDEX1)
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.WINDY') THEN
          VALEUR = INST%WINDY(INDEX1)
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.VARIAN') THEN
          VALEUR = INST%VARIAN(INDEX1)
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.PFREAD5') THEN
          VALEUR = INST%PFREAD5(INDEX1)
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.PFREAD8') THEN
          VALEUR = INST%PFREAD8(INDEX1)
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.TM01') THEN
          VALEUR = INST%TM01(INDEX1)
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.TM02') THEN
          VALEUR = INST%TM02(INDEX1)
        ! <get_double_array>

        ! <get_double>
        ELSE
          IERR = UNKNOWN_VAR_ERROR
          ERR_MESS = 'UNKNOWN VARIABLE NAME : '//TRIM(VARNAME)
        ENDIF
!
      END SUBROUTINE GET_DOUBLE_WAC_D
!
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !>@brief Defines the value of a double variable of tomawac
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
      SUBROUTINE SET_DOUBLE_WAC_D
     &     (INST, VARNAME, VALEUR, INDEX1, INDEX2, INDEX3, IERR)
!
        TYPE(INSTANCE_WAC),    INTENT(INOUT) :: INST
        CHARACTER(LEN=WAC_VAR_LEN), INTENT(IN)  :: VARNAME
        DOUBLE PRECISION,      INTENT(IN) :: VALEUR
        INTEGER,               INTENT(IN) :: INDEX1
        INTEGER,               INTENT(IN) :: INDEX2
        INTEGER,               INTENT(IN) :: INDEX3
        INTEGER,               INTENT(OUT) :: IERR
!
        IERR = 0
        IF(TRIM(VARNAME).EQ.'MODEL.X') THEN
          INST%MESH%X%R(INDEX1) = VALEUR
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.Y') THEN
          INST%MESH%Y%R(INDEX1) = VALEUR
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.AT') THEN
          INST%AT = VALEUR
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.DT') THEN
          INST%DT = VALEUR
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.BETAM') THEN
          INST%BETAM = VALEUR
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.VX_CTE') THEN
          INST%VX_CTE = VALEUR
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.VY_CTE') THEN
          INST%VY_CTE = VALEUR           
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.BOTTOM') THEN
          INST%ZF(INDEX1) = VALEUR
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.WINDX') THEN
          INST%WINDX(INDEX1) = VALEUR
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.WINDY') THEN
          INST%WINDY(INDEX1) = VALEUR
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.VARIAN') THEN
          INST%VARIAN(INDEX1) = VALEUR
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.PFREAD5') THEN
          INST%PFREAD5(INDEX1) = VALEUR
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.PFREAD8') THEN
          INST%PFREAD8(INDEX1) = VALEUR
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.TM01') THEN
          INST%TM01(INDEX1) = VALEUR
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.TM02') THEN
          INST%TM02(INDEX1) = VALEUR

        ! <set_double>
        ELSE
          IERR = UNKNOWN_VAR_ERROR
          ERR_MESS = 'UNKNOWN VARIABLE NAME : '//TRIM(VARNAME)
        ENDIF
!
      END SUBROUTINE SET_DOUBLE_WAC_D
!
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !>@brief Get an integer variable from tomawac
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
      SUBROUTINE GET_INTEGER_WAC_D
     &     (INST, VARNAME, VALEUR, INDEX1, INDEX2, INDEX3, IERR)
!
        TYPE(INSTANCE_WAC),    INTENT(IN) :: INST
        CHARACTER(LEN=WAC_VAR_LEN), INTENT(IN)  :: VARNAME
        INTEGER,               INTENT(OUT) :: VALEUR
        INTEGER,               INTENT(IN) :: INDEX1
        INTEGER,               INTENT(IN) :: INDEX2
        INTEGER,               INTENT(IN) :: INDEX3
        INTEGER,               INTENT(OUT) :: IERR
!
        IERR = 0
        VALEUR = -1
        IF(TRIM(VARNAME).EQ.'MODEL.NPOIN') THEN
          VALEUR = INST%MESH%NPOIN
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.NELEM') THEN
          VALEUR = INST%MESH%NELEM
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.NPTFR') THEN
          VALEUR = INST%MESH%NPTFR
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.NTIMESTEPS') THEN
          VALEUR = INST%NIT
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.LT') THEN
          VALEUR = INST%LT
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
      END SUBROUTINE GET_INTEGER_WAC_D
!
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !>@brief Defines the value of an integer variable of tomawac
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
      SUBROUTINE SET_INTEGER_WAC_D
     &     (INST, VARNAME, VALEUR, INDEX1, INDEX2, INDEX3, IERR)
!
        TYPE(INSTANCE_WAC),    INTENT(INOUT) :: INST
        CHARACTER(LEN=WAC_VAR_LEN), INTENT(IN)  :: VARNAME
        INTEGER,               INTENT(IN) :: VALEUR
        INTEGER,               INTENT(IN) :: INDEX1
        INTEGER,               INTENT(IN) :: INDEX2
        INTEGER,               INTENT(IN) :: INDEX3
        INTEGER,               INTENT(OUT) :: IERR
!
        IERR = 0
        IF(TRIM(VARNAME).EQ.'MODEL.NTIMESTEPS') THEN
          INST%NIT = VALEUR
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.LT') THEN
          INST%LT = VALEUR
        ! <set_integer>
        ELSE
          IERR = UNKNOWN_VAR_ERROR
          ERR_MESS = 'UNKNOWN VARIABLE NAME : '//TRIM(VARNAME)
        ENDIF
!
      END SUBROUTINE SET_INTEGER_WAC_D
!
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !>@brief Get a string variable from tomawac
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
      SUBROUTINE GET_STRING_WAC_D
     &     (INST, VARNAME, VALEUR, VALUELEN, INDEX1, INDEX2, IERR)
!
        TYPE(INSTANCE_WAC),    INTENT(IN) :: INST
        CHARACTER(LEN=WAC_VAR_LEN), INTENT(IN)  :: VARNAME
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
          I = INST%WACRES
          DO J = 1,VALUELEN
            VALEUR(J:J) = INST%WAC_FILES(I)%NAME(J:J)
          ENDDO
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.BCFILE') THEN
          I = INST%WACCLI
          DO J = 1,VALUELEN
            VALEUR(J:J) = INST%WAC_FILES(I)%NAME(J:J)
          ENDDO
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.GEOMETRYFILE') THEN
          I = INST%WACGEO
          DO J = 1,VALUELEN
            VALEUR(J:J) = INST%WAC_FILES(I)%NAME(J:J)
          ENDDO
        ! <get_string>
        ELSE
          IERR = UNKNOWN_VAR_ERROR
          ERR_MESS = 'UNKNOWN VARIABLE NAME : '//TRIM(VARNAME)
        ENDIF
!
      END SUBROUTINE GET_STRING_WAC_D
!
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !>@brief Defines the value of a string variable of tomawac
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
      SUBROUTINE SET_STRING_WAC_D
     &     (INST, VARNAME, VALEUR, VALUELEN, INDEX1, INDEX2, IERR)
!
        TYPE(INSTANCE_WAC),    INTENT(INOUT) :: INST
        CHARACTER(LEN=WAC_VAR_LEN), INTENT(IN)  :: VARNAME
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
          I = INST%WACRES
          DO J=1,VALUELEN
            INST%WAC_FILES(I)%NAME(J:J) = VALEUR(J)
          ENDDO
        ! <set_string>
        ELSE
          IERR = UNKNOWN_VAR_ERROR
          ERR_MESS = 'UNKNOWN VARIABLE NAME : '//TRIM(VARNAME)
        ENDIF
!
      END SUBROUTINE SET_STRING_WAC_D
!
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !>@brief Get a boolean variable from tomawac
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
      SUBROUTINE GET_BOOLEAN_WAC_D
     &     (INST, VARNAME, VALEUR, INDEX1, INDEX2, INDEX3, IERR)
!
        TYPE(INSTANCE_WAC),    INTENT(IN) :: INST
        CHARACTER(LEN=WAC_VAR_LEN), INTENT(IN)  :: VARNAME
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
      END SUBROUTINE GET_BOOLEAN_WAC_D
!
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !>@brief Defines the value of a boolean variable of tomawac
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
      SUBROUTINE SET_BOOLEAN_WAC_D
     &     (INST, VARNAME, VALEUR, INDEX1, INDEX2, INDEX3, IERR)
!
        TYPE(INSTANCE_WAC),    INTENT(INOUT) :: INST
        CHARACTER(LEN=WAC_VAR_LEN), INTENT(IN)  :: VARNAME
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
      END SUBROUTINE SET_BOOLEAN_WAC_D
!
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !>@brief Get size informations on a variable of tomawac
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !>@param[in] INST Instance
      !>@param[in] VARNAME Name of the variable
      !>@param[out] DIM1 Size of the first dimension
      !>@param[out] DIM2 Size of the second dimension
      !>@param[out] DIM3 Size of the third dimension
      !>@param[out] IERR 0 if subroutine successfull,
      !!                        error id otherwise
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      SUBROUTINE GET_VAR_SIZE_WAC_D
     &         (INST, VARNAME, DIM1, DIM2, DIM3, IERR)
!
        TYPE(INSTANCE_WAC),    INTENT(IN) :: INST
        CHARACTER(LEN=WAC_VAR_LEN), INTENT(IN)  :: VARNAME
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
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.BOTTOM') THEN
          DIM1 = SIZE(INST%ZF)
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.WINDX') THEN
          DIM1 = SIZE(INST%WINDX)
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.WINDY') THEN
          DIM1 = SIZE(INST%WINDY)
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.VARIAN') THEN
          DIM1 = SIZE(INST%VARIAN)
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.TM01') THEN
          DIM1 = SIZE(INST%TM01)
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.TM02') THEN
          DIM1 = SIZE(INST%TM02)
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.PFREAD5') THEN
          DIM1 = SIZE(INST%PFREAD5)
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.PFREAD8') THEN
          DIM1 = SIZE(INST%PFREAD8)



        ! <get_var_size>
        ENDIF
!
      END SUBROUTINE GET_VAR_SIZE_WAC_D
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
      SUBROUTINE GET_VAR_TYPE_WAC_D
     &        (VARNAME, VARTYPE, READONLY, NDIM,IENT,JENT,KENT,
     &         GETPOS,SETPOS,IERR)
!
        CHARACTER(LEN=WAC_VAR_LEN),  INTENT(IN)  :: VARNAME
        CHARACTER(LEN=WAC_TYPE_LEN), INTENT(OUT) :: VARTYPE
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
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.AT') THEN
          VARTYPE = 'DOUBLE'
          READONLY = .FALSE.
          NDIM = 0
          GETPOS = RUN_ALLOCATION_POS
          SETPOS = RUN_ALLOCATION_POS
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.DT') THEN
          VARTYPE = 'DOUBLE'
          READONLY = .FALSE.
          NDIM = 0
          GETPOS = RUN_ALLOCATION_POS
          SETPOS = RUN_ALLOCATION_POS
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.LT') THEN
          VARTYPE = 'INTEGER'
          READONLY = .FALSE.
          NDIM = 0
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
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.BOTTOM') THEN
          VARTYPE = 'DOUBLE'
          READONLY = .FALSE.
          NDIM = 1
          GETPOS = NO_POSITION
          SETPOS = NO_POSITION
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.WINDX') THEN
          VARTYPE = 'DOUBLE'
          READONLY = .FALSE.
          NDIM = 1
          GETPOS = NO_POSITION
          SETPOS = NO_POSITION
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.WINDY') THEN
          VARTYPE = 'DOUBLE'
          READONLY = .FALSE.
          NDIM = 1
          GETPOS = NO_POSITION
          SETPOS = NO_POSITION
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.VARIAN') THEN
          VARTYPE = 'DOUBLE'
          READONLY = .FALSE.
          NDIM = 1
          GETPOS = NO_POSITION
          SETPOS = NO_POSITION
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.TM01') THEN
          VARTYPE = 'DOUBLE'
          READONLY = .FALSE.
          NDIM = 1
          GETPOS = NO_POSITION
          SETPOS = NO_POSITION
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.TM02') THEN
          VARTYPE = 'DOUBLE'
          READONLY = .FALSE.
          NDIM = 1
          GETPOS = NO_POSITION
          SETPOS = NO_POSITION
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.PFREAD5') THEN
          VARTYPE = 'DOUBLE'
          READONLY = .FALSE.
          NDIM = 1
          GETPOS = NO_POSITION
          SETPOS = NO_POSITION
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.PFREAD8') THEN
          VARTYPE = 'DOUBLE'
          READONLY = .FALSE.
          NDIM = 1
          GETPOS = NO_POSITION
          SETPOS = NO_POSITION
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.BETAM') THEN
          VARTYPE = 'DOUBLE'
          READONLY = .FALSE.
          NDIM = 0
          GETPOS = NO_POSITION
          SETPOS = NO_POSITION
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.VX_CTE') THEN
          VARTYPE = 'DOUBLE'
          READONLY = .FALSE.
          NDIM = 0
          GETPOS = NO_POSITION
          SETPOS = NO_POSITION
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.VY_CTE') THEN
          VARTYPE = 'DOUBLE'
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
      END SUBROUTINE GET_VAR_TYPE_WAC_D
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
      SUBROUTINE GET_VAR_INFO_WAC_D(I, VAR_LEN, INFO_LEN,
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

        DO J=1,WAC_VAR_LEN
          VARNAME(J:J) = VNAME_WAC(I)(J:J)
        ENDDO
        DO J=1,WAC_INFO_LEN
          VARINFO(J:J) = VINFO_WAC(I)(J:J)
        ENDDO

        RETURN
      END SUBROUTINE GET_VAR_INFO_WAC_D
!
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !>@brief Get a description of each variable
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !>@param[out] IERR 0 if subroutine successfull,
      !!                        error id otherwise
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      SUBROUTINE SET_VAR_LIST_WAC_D(IERR)
!
        INTEGER, INTENT(OUT) :: IERR
!
        INTEGER :: I
!
        I=0
        IERR = 0
        IF(.NOT.ALLOCATED(VNAME_WAC)) THEN
          ALLOCATE(VNAME_WAC(NB_VAR_WAC),STAT=IERR)
          IF(IERR.NE.0) RETURN
          ALLOCATE(VINFO_WAC(NB_VAR_WAC),STAT=IERR)
          IF(IERR.NE.0) RETURN
!
          I = I + 1
          VNAME_WAC(I) = 'MODEL.AT'
          VINFO_WAC(I) = 'CURRENT TIME'
          I = I + 1
          VNAME_WAC(I) = 'MODEL.DT'
          VINFO_WAC(I) = 'TIME STEP'         
          I = I + 1
          VNAME_WAC(I) = 'MODEL.LT'
          VINFO_WAC(I) = 'CURRENT TIME STEP'
          I = I + 1
          VNAME_WAC(I) = 'MODEL.BCFILE'
          VINFO_WAC(I) = 'BOUNDARY CONDITION FILE NAME'
          I = I + 1
          VNAME_WAC(I) = 'MODEL.DEBUG'
          VINFO_WAC(I) = 'ACTIVATING DEBUG MODE'
          I = I + 1
          VNAME_WAC(I) = 'MODEL.GEOMETRYFILE'
          VINFO_WAC(I) = 'NAME OF THE GEOMERY FILE'
          I = I + 1
          VNAME_WAC(I) = 'MODEL.IKLE'
          VINFO_WAC(I) = 'CONNECTIVITY TABLE BETWEEN ELEMENT AND NODES'
          I = I + 1
          VNAME_WAC(I) = 'MODEL.NACHB'
          VINFO_WAC(I) = 'NUMBERS OF PROC CONTAINING A GIVEN POINT'
          I = I + 1
          VNAME_WAC(I) = 'MODEL.KNOLG'
          VINFO_WAC(I) =
     &         'GIVES THE INITIAL GLOBAL NUMBER OF A LOCAL POINT'
          I = I + 1
          VNAME_WAC(I) = 'MODEL.NELEM'
          VINFO_WAC(I) = 'NUMBER OF ELEMENT IN THE MESH'
          I = I + 1
          VNAME_WAC(I) = 'MODEL.NELMAX'
          VINFO_WAC(I) = 'MAXIMUM NUMBER OF ELEMENTS ENVISAGED'
          I = I + 1
          VNAME_WAC(I) = 'MODEL.NPOIN'
          VINFO_WAC(I) = 'NUMBER OF POINT IN THE MESH'
          I = I + 1
          VNAME_WAC(I) = 'MODEL.NPTFR'
          VINFO_WAC(I) = 'NUMBER OF BOUNDARY POINTS'
          I = I + 1
          VNAME_WAC(I) = 'MODEL.NTIMESTEPS'
          VINFO_WAC(I) = 'NUMBER OF TIME STEPS'
          I = I + 1
          VNAME_WAC(I) = 'MODEL.RESULTFILE'
          VINFO_WAC(I) = 'NAME OF THE RESULT FILE'
          I = I + 1
          VNAME_WAC(I) = 'MODEL.X'
          VINFO_WAC(I) = 'X COORDINATES FOR EACH POINT OF THE MESH'
          I = I + 1
          VNAME_WAC(I) = 'MODEL.Y'
          VINFO_WAC(I) = 'Y COORDINATES FOR EACH POINT OF THE MESH'
          I = I + 1
          VNAME_WAC(I) = 'MODEL.BOTTOM'
          VINFO_WAC(I) = 'BOTTOM'
          I = I + 1
          VNAME_WAC(I) = 'MODEL.EQUATION'
          VINFO_WAC(I) = 'NAME OF THE EQUATION USED IN THE CODE'
          I = I + 1
          VNAME_WAC(I) = 'MODEL.WINDX'
          VINFO_WAC(I) = 'WINDX'
          I = I + 1
          VNAME_WAC(I) = 'MODEL.WINDY'
          VINFO_WAC(I) = 'WINDY'
          I = I + 1
          VNAME_WAC(I) = 'MODEL.VARIAN'
          VINFO_WAC(I) = 'VARIAN'
          I = I + 1
          VNAME_WAC(I) = 'MODEL.TM01'
          VINFO_WAC(I) = 'TM01'
          I = I + 1
          VNAME_WAC(I) = 'MODEL.TM02'
          VINFO_WAC(I) = 'TM02'
          I = I + 1
          VNAME_WAC(I) = 'MODEL.PFREAD5'
          VINFO_WAC(I) = 'PFREAD5'
          I = I + 1
          VNAME_WAC(I) = 'MODEL.PFREAD8'
          VINFO_WAC(I) = 'PFREAD8'
          I = I + 1
          VNAME_WAC(I) = 'MODEL.BETAM'
          VINFO_WAC(I) = 'BETAM'
          I = I + 1
          VNAME_WAC(I) = 'MODEL.VX_CTE'
          VINFO_WAC(I) = 'VX_CTE'
          I = I + 1
          VNAME_WAC(I) = 'MODEL.VY_CTE'
          VINFO_WAC(I) = 'VY_CTE'
          ! <set_var_list>
          IF(I.NE.NB_VAR_WAC) THEN
            IERR = INCREASE_NB_VAR_WAC_ERROR
            RETURN
          ENDIF
        ENDIF
!
      END SUBROUTINE SET_VAR_LIST_WAC_D
!
      END MODULE API_HANDLE_VAR_WAC
