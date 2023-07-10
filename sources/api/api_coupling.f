!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!>@brief Module handling the exchange between telemac2d et sisyphe api
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
!
      MODULE API_COUPLING
!
      USE API_INSTANCE_SIS
      USE API_INSTANCE_T2D
      USE API_HANDLE_VAR_T2D
      USE API_HANDLE_VAR_SIS

      IMPLICIT NONE
      PRIVATE

      PUBLIC :: SAVE_CHARR_SUSP_CPL
      PUBLIC :: CHARR_OR_SUSP_CPL
      PUBLIC :: SET_VAR_SIS_CPL
      PUBLIC :: SET_VAR_T2D_CPL

      CONTAINS

!
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !>@brief Saves original charr and susp values after first sisyphe call
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !>@param[in] INST_SIS Sisyphe instance
      !>@param[in,out] INST_T2D Telemac instance
      !>@param[out] IERR 0 if subroutine successfull,
      !!                             error id otherwise
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      SUBROUTINE SAVE_CHARR_SUSP_CPL(INST_SIS, INST_T2D, IERR)
        TYPE(INSTANCE_SIS),  INTENT(IN) :: INST_SIS
        TYPE(INSTANCE_T2D),  INTENT(INOUT) :: INST_T2D
        INTEGER,             INTENT(OUT) :: IERR
!
        IERR = 0

        INST_T2D%CHARR_SIS = INST_SIS%TEL%SIS_CPL%CHARR
        INST_T2D%SUSP_SIS = INST_SIS%TEL%SIS_CPL%SUSP

      END SUBROUTINE SAVE_CHARR_SUSP_CPL


      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !>@brief Deals with cases : bedload of suspension
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !>@param[in] INST_SIS Sisyphe instance
      !>@param[in,out] INST_T2D Telemac instance
      !>@param[out] CHARR_SUSP Defines which sisyphe call
      !!                             = 1 Means Bedload
      !!                             = 2 Means Suspension
      !!                             = 3 Means Both
      !>@param[out] IERR 0 if subroutine successfull,
      !!                             error id otherwise
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      SUBROUTINE CHARR_OR_SUSP_CPL(INST_SIS, INST_T2D, CHARR_SUSP, IERR)
        TYPE(INSTANCE_SIS),  INTENT(IN) :: INST_SIS
        TYPE(INSTANCE_T2D),  INTENT(INOUT) :: INST_T2D
        INTEGER,             INTENT(OUT) :: CHARR_SUSP
        INTEGER,             INTENT(OUT) :: IERR
        LOGICAL                          :: YES_CHARR
!
        IERR = 0
        CHARR_SUSP = 0

        IF(INST_T2D%SUSP_SIS.AND.INST_T2D%CHARR_SIS
     &                      .AND.INST_T2D%SIS%PERCOU.NE.1) THEN
          INST_T2D%LEOPRD_CHARR=INST_T2D%SIS%NIT+INST_T2D%SIS%PERCOU
        ELSE
          INST_T2D%LEOPRD_CHARR=INST_T2D%SIS%LEOPRD
        ENDIF

        INST_T2D%SUSP1=INST_T2D%SUSP_SIS
     &                      .AND.INST_T2D%SIS%PERCOU.EQ.1
        IF(INST_T2D%SUSP1.OR.(INST_T2D%CHARR_SIS
     &                   .AND.(INST_T2D%SIS%PERCOU*(INST_T2D%SIS%LT
     &                         /INST_T2D%SIS%PERCOU).EQ.INST_T2D%SIS%LT
     &                        ))) THEN
          CHARR_SUSP = 1
          YES_CHARR = .TRUE.
        END IF

        IF(INST_T2D%SUSP_SIS.AND.INST_T2D%SIS%PERCOU.NE.1) THEN
          IF (YES_CHARR) THEN
            CHARR_SUSP = 3
          ELSE
            CHARR_SUSP = 2
          END IF
        END IF
      END SUBROUTINE CHARR_OR_SUSP_CPL


      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !>@brief Sets loop variables for sisyphe in case of coupling
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !>@param[in,out] INST_SIS Telemac2d instance
      !>@param[in] INST_T2D Sisyphe instance
      !>@param[in] CALL_TYPE Defines which sisyphe call
      !!                             = 0 Means Initializing
      !!                             = 1 Means Bedload CALL
      !!                             = 2 Means Suspension CALL
      !>@param[out] IERR 0 if subroutine successfull,
      !!                             error id otherwise
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      SUBROUTINE SET_VAR_SIS_CPL(INST_T2D, CALL_TYPE, INST_SIS,IERR)
        TYPE(INSTANCE_T2D),  INTENT(IN) :: INST_T2D
        INTEGER,             INTENT(IN) :: CALL_TYPE
        TYPE(INSTANCE_SIS),  INTENT(INOUT) :: INST_SIS
        INTEGER,             INTENT(OUT) :: IERR
!
        IERR = 0

        IF (CALL_TYPE.EQ.0) THEN
          INST_SIS%CPL_T2D_SIS = .TRUE.
          !INS
          INST_SIS%TEL%LOOPCOUNT =>  INST_T2D%SIS%LT
          INST_SIS%TEL%GRAPHCOUNT => INST_T2D%SIS%LEOPRD
          INST_SIS%TEL%COMPLEO => INST_T2D%COMPLEO
          INST_SIS%TEL%YAGOUT = INST_T2D%LEO
          INST_SIS%TEL%LISTCOUNT => INST_T2D%SIS%LISPRD
          INST_SIS%TEL%NIT => INST_T2D%SIS%NIT
          INST_SIS%TEL%U => INST_T2D%SIS%U
          INST_SIS%TEL%V => INST_T2D%SIS%V
          INST_SIS%TEL%H => INST_T2D%SIS%H
          INST_SIS%TEL%HN => INST_T2D%SIS%H
          INST_SIS%TEL%HPROP => INST_T2D%SIS%H

          !OUTS
          INST_SIS%TEL%ZF => INST_T2D%SIS%ZF
          INST_SIS%TEL%UETCAR => INST_T2D%SIS%CF
          INST_SIS%TEL%CF => INST_T2D%SIS%CF
          INST_SIS%TEL%KS => INST_T2D%SIS%CHESTR

          INST_SIS%TEL%SIS_CPL%NSIS_CFD = 1
          INST_SIS%TEL%SIS_CPL%SISYPHE_CFD = .FALSE.
          INST_SIS%TEL%SIS_CPL%CONSTFLOW = .FALSE.
          INST_SIS%TEL%SIS_CPL%CHARR = .FALSE.
          INST_SIS%TEL%SIS_CPL%SUSP = .FALSE.

          !INS
          INST_SIS%TEL%CODE = 'TELEMAC2D               '
          INST_SIS%TEL%PERICOU => INST_T2D%SIS%PERCOU
          INST_SIS%TEL%U3D => INST_T2D%SIS%U
          INST_SIS%TEL%V3D => INST_T2D%SIS%V
          INST_SIS%TEL%T = INST_T2D%SIS%AT
          INST_SIS%TEL%VISC => INST_T2D%SIS%VISC
          INST_SIS%TEL%DT = INST_T2D%SIS%DT

          INST_SIS%TEL%FLBOR => INST_T2D%SIS%FLBOR
          INST_SIS%TEL%DM1 => INST_T2D%SIS%DM1
          INST_SIS%TEL%UCONV => INST_T2D%SIS%USIS
          INST_SIS%TEL%VCONV => INST_T2D%SIS%VSIS
          INST_SIS%TEL%ZCONV => INST_T2D%SIS%ZCONV
          INST_SIS%TEL%THETAW => INST_T2D%SIS%DIRMOY
          INST_SIS%TEL%HW => INST_T2D%SIS%HM0
          INST_SIS%TEL%TW => INST_T2D%SIS%TPR5
          INST_SIS%TEL%UW => INST_T2D%SIS%ORBVEL

        ELSE IF (CALL_TYPE.EQ.1) THEN
          INST_SIS%TEL%HN => INST_T2D%SIS%HN
          INST_SIS%TEL%HPROP => INST_T2D%SIS%HPROP

          INST_SIS%TEL%LOOPCOUNT =>  INST_T2D%SIS%LT
          INST_SIS%TEL%GRAPHCOUNT = INST_T2D%LEOPRD_CHARR

          INST_SIS%TEL%T = INST_T2D%SIS%AT
          INST_SIS%TEL%PERICOU = INST_T2D%SIS%PERCOU
          INST_SIS%TEL%DT = INST_T2D%SIS%DT*INST_T2D%SIS%PERCOU

          INST_SIS%TEL%SIS_CPL%CHARR = INST_T2D%CHARR_SIS
          INST_SIS%TEL%SIS_CPL%SUSP = INST_T2D%SUSP1

          INST_SIS%TEL%SOLSYS = INST_T2D%SIS%SOLSYS
        ELSE IF (CALL_TYPE.EQ.2) THEN

          INST_SIS%TEL%HN => INST_T2D%SIS%HN
          INST_SIS%TEL%HPROP => INST_T2D%SIS%HPROP

          INST_SIS%TEL%LOOPCOUNT =>  INST_T2D%SIS%LT
          INST_SIS%TEL%GRAPHCOUNT = INST_T2D%SIS%LEOPRD

          INST_SIS%TEL%T = INST_T2D%SIS%AT
          INST_SIS%TEL%PERICOU = 1
          INST_SIS%TEL%DT = INST_T2D%SIS%DT

          INST_SIS%TEL%SIS_CPL%CHARR = INST_T2D%CHARR_TEL
          INST_SIS%TEL%SIS_CPL%SUSP = INST_T2D%SUSP_SIS

          INST_SIS%TEL%SOLSYS = INST_T2D%SIS%SOLSYS

        END IF

      END SUBROUTINE SET_VAR_SIS_CPL


      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !>@brief Sends variables to telemac2d after sisyphe call
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !>@param[in,out] INST_T2D Telemac2d instance
      !>@param[in] INST_SIS Sisyphe instance
      !>@param[out] IERR 0 if subroutine successfull,
      !!                             error id otherwise
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      SUBROUTINE SET_VAR_T2D_CPL(INST_SIS, INST_T2D,IERR)
        TYPE(INSTANCE_SIS),  INTENT(IN) :: INST_SIS
        TYPE(INSTANCE_T2D),  INTENT(INOUT) :: INST_T2D
        INTEGER,             INTENT(OUT) :: IERR
!
        IERR = 0
        INST_T2D%ZF     = INST_SIS%TEL%ZF
        INST_T2D%CHESTR = INST_SIS%TEL%KS
        INST_T2D%SIS%CF = INST_SIS%TEL%CF

        INST_T2D%SIS%SIS_CPL%NSIS_CFD =INST_SIS%TEL%SIS_CPL%NSIS_CFD
        INST_T2D%SIS%SIS_CPL%SISYPHE_CFD
     &                           = INST_SIS%TEL%SIS_CPL%SISYPHE_CFD
        INST_T2D%SIS%SIS_CPL%CONSTFLOW = INST_SIS%TEL%SIS_CPL%CONSTFLOW

        INST_T2D%SIS%SIS_CPL%CHARR = INST_T2D%CHARR_SIS
        INST_T2D%SIS%SIS_CPL%SUSP = INST_T2D%SUSP_SIS
      END SUBROUTINE SET_VAR_T2D_CPL
      END MODULE API_COUPLING
