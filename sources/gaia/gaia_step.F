!                   ********************
                    SUBROUTINE GAIA_STEP
!                   ********************
!
     &(LOOPCOUNT,GRAFCOUNT,LISTCOUNT,TELNIT,
     & U_TEL,V_TEL,H_TEL,ZF_TEL,UETCAR,DELTAR,
     & CF_TEL,KS_TEL,
     & CODE,U3D,V3D,T_TEL,VISC_TEL,DT_TEL,CHARR_TEL,SUSP_TEL,
     & XMVE_TEL,GRAV_TEL,
     & THETAW_TEL,HW_TEL,TW_TEL,UW_TEL,YAGOUT,API_ITER,GRCOMP)
!
!***********************************************************************
! GAIA
!***********************************************************************
!
!>@brief  The step part of Gaia, called within the time loop of the
!!        hydrodynamics modules
!
!>@history GAIA has been created as a copy of SISYPHE, then
!!        modified so as to clarify the role of GAIA as opposed to
!!        TELEMAC2D and TELEMAC3D (that take care of the hydrodynamics
!!        and of the suspension). It also proposes a unified bed
!!        evolution framework for the 2D and 3D simulations, making it
!!        possible to handle multiple layers with multiple sediment types
!!        in both cases.
!
!> When in GAIA_STEP, there are three possibilites:
!! - bedload only (no suspension): GAIA only
!!   computes the bedload, bed evolution and mass balance calculation.
!!   It is called only once by TELEMAC2D or TELEMAC3D. SECOND_SUSP_STEP is
!!   always false.
!! - suspension only (no bedload):
!!   SECOND_SUSP_STEP is false at the start, then SUSPENSION_MAIN_GAIA prepares
!!   the suspension terms and sets SECOND_SUSP_STEP to true.
!!   A RETURN call is done to exit GAIA and go back to the main
!!   TELEMAC2D or TELEMAC3D time loop. In that loop, GAIA is called a
!!   second time with SECOND_SUSP_STEP = true, and performs the bed evolution
!!   and mass balance calculations. SECOND_SUSP_STEP is set to false for the
!!   next time iteration.
!! - suspension and beload:
!!   SECOND_SUSP_STEP is false at the start, the bedload is calculated, then
!!   SUSPENSION_MAIN_GAIA prepares the suspension terms and sets
!!   SECOND_SUSP_STEP to true. A RETURN call is done to exit GAIA_STEP and go back
!!   to the main TELEMAC2D or TELEMAC3D time loop. In that loop, GAIA_STEP
!!   is called a second time with SECOND_SUSP_STEP = true, and performs the
!!   bed evolution and mass balance calculations. SECOND_SUSP_STEP is set to
!!   false for the next time iteration.
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!>@param  [in,out]  CF_TEL     QUADRATIC FRICTION COEFFICIENT FROM TELEMAC
!>@param  [in,out]  CHARR_TEL  LOGICAL, BED LOAD OR NOT: Sent to TELEMAC-2D
!>@param  [in]      CODE       NAME OF CALLING PROGRAMME (TELEMAC2D OR 3D)
!>@param  [in]      DT_TEL     TIME STEP FROM TELEMAC
!>@param  [in]      GRAFCOUNT  PERIOD OF GRAPHICAL OUTPUTS
!>@param  [in]      GRCOMP     COUNTER FOR GRAPHICAL OUTPUTS
!>@param  [in]      H_TEL      WATER DEPTH FROM TEL H (N+1)
!>@param  [in]      HW_TEL     SIGNIFICANT WAVE HEIGHT
!>@param  [in,out]  KS_TEL     BED ROUGHNESS SENT TO TELEMAC
!>@param  [in]      LISTCOUNT  PERIODE DE SORTIE LISTING
!>@param  [in]      LOOPCOUNT  NUMERO DE L'ITERATION
!>@param  [in,out]  SUSP_TEL   LOGICAL, SUSPENDED LOAD OR NOT: Sent to TELEMAC
!>@param  [in]      TELNIT     NUMBER OF TELEMAC ITERATIONS
!>@param  [in]      T_TEL      CURRENT TIME IN CALLING PROGRAMME
!>@param  [in]      THETAW_TEL WAVE DURECTION (DEG WRT OX AXIS)
!>@param  [in]      TW_TEL     MEAN WAVE PERIOD
!>@param  [in]      U3D, V3D   3D VELOCITY SENT BY TELEMAC 3D
!>@param  [in,out]  UETCAR     SQUARE OF THE FRICTION VELOCITY
!!                             (COUPLING WITH TEL 3D)
!>@param  [in]      U_TEL      U VELOCITY FROM TELEMAC
!>@param  [in]      UW_TEL     ORBITAL WAVE VELOCITY
!>@param  [in]      VISC_TEL   VELOCITY DIFFUSIVITY (TELEMAC-2D)
!>@param  [in]      V_TEL      V VELOCITY FROM TELEMAC
!!                             IS DM1*GRAD(ZCONV), SEE SOLSYS.
!>@param  [in,out]  ZF_TEL     BOTTOM ELEVATION OF THE CALLING TELEMAC
!>@param  [in,out]  API_ITER   OPTIONAL, CURRENT ITERATION NUMBER IN THE
!!                             API_GAIA
!>@param  [in]      YAGOUT     LOGICAL: IF YES GRAPHIC OUTPUT (STEERED BY T2D)
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE INTERFACE_GAIA, EX_GAIA_STEP => GAIA_STEP
      USE BIEF
      USE DECLARATIONS_TELEMAC
      USE DECLARATIONS_GAIA
      USE INTERFACE_HERMES
      USE DECLARATIONS_SPECIAL
!
      IMPLICIT NONE
!
!!-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER,           INTENT(IN)    :: LOOPCOUNT,GRAFCOUNT
      INTEGER,           INTENT(IN)    :: LISTCOUNT,TELNIT
      CHARACTER(LEN=24), INTENT(IN)    :: CODE
      TYPE(BIEF_OBJ),    INTENT(IN)    :: U_TEL,V_TEL,H_TEL
      TYPE(BIEF_OBJ),    INTENT(INOUT) :: ZF_TEL,UETCAR,KS_TEL
      TYPE(BIEF_OBJ),    INTENT(IN)    :: DELTAR
      TYPE(BIEF_OBJ),    INTENT(IN)    :: U3D,V3D,VISC_TEL
      TYPE(BIEF_OBJ),    INTENT(INOUT) :: CF_TEL
      DOUBLE PRECISION,  INTENT(IN)    :: T_TEL,XMVE_TEL,GRAV_TEL
      LOGICAL,           INTENT(INOUT) :: CHARR_TEL,SUSP_TEL
      DOUBLE PRECISION,  INTENT(IN)    :: DT_TEL
      TYPE(BIEF_OBJ),    INTENT(IN)    :: THETAW_TEL,HW_TEL,TW_TEL
      TYPE(BIEF_OBJ),    INTENT(IN)    :: UW_TEL
      LOGICAL,           INTENT(IN)    :: YAGOUT
      INTEGER, OPTIONAL, INTENT(IN)    :: API_ITER,GRCOMP
!
!!-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      LOGICAL :: COMP
      INTEGER :: GGRCOMP
!=======================================================================
!------------------------------------------------------------------
!     CASE 1: SECOND_SUSP_STEP = FALSE: NO SUSPENSION IN THE CALCULATION OR
!             FIRST CALL OF GAIA IN THE TIME LOOP. COMPUTE THE
!             BEDLOAD AND/OR PREPARE THE SUSPENSION IF REQUIRED
!------------------------------------------------------------------
      IF(.NOT.SECOND_SUSP_STEP) THEN
        CALL GAIA_PREPARE_STEP
     &  (H_TEL, U_TEL, V_TEL, CF_TEL, CHARR_TEL, CODE, DELTAR, DT_TEL,
     &   HW_TEL, KS_TEL, LISTCOUNT, LOOPCOUNT, TELNIT, SUSP_TEL,
     &   T_TEL, THETAW_TEL, TW_TEL, UETCAR, UW_TEL, ZF_TEL)
!
!       BEDLOAD COMPUTATION
!
        IF(CHARR) THEN
!
          IF(DEBUG.GT.0) WRITE(LU,*) 'BEDLOAD_MAIN_GAIA'
          CALL BEDLOAD_MAIN_GAIA
     &        (ACLADM,KSP,KSR,VOLU2D,UNSV2D,
     &         CF,EBOR,FW,HN,LIQBOR,MASK,MASKEL,
     &         MASKPT,QBOR,U2D,V2D,S,UNLADM,UW,THETAW,
     &         MU,TOB,TOBW,TW,ZF,DEBUG,HIDFAC,ICF,
     &         IELMT,KDDL,KDIR,KENT,KLOG,KNEU,KSORT,
     &         NPOIN,NPTFR,NSICLA,
     &         OPTBAN,BETA,DCLA,GRAV,HIDI,HMIN,
     &         VCE,XMVE,XMVS0,XWC,PI,KARMAN,ZERO,
     &         KARIM_HOLLY_YANG,MSK,SUSP,VF,
     &         MESH,LIEBOR,LIMTEC,MASKTR,
     &         IT1,T1,T2,T3,T4,T5,T6,T7,T8,T9,
     &         T10,T11,T12,T13,UNORM,AC,DT,
     &         BREACH,CALFA_CL,COEFPN,COEFCR,HIDING,
     &         QSCL_C,QSCL_S,QS_C,QSCLXC,QSXC,QSCLYC,
     &         QSYC,SALFA_CL,ENTET,
     &         SECCURRENT,SLOPEFF,PHISED,DEVIA,BETA2,BIJK,
     &         SEDCO,HOULE,U3D,V3D,CODE,FLBCLA,MAXADV,RATIO_SAND,
     &         H_TEL, HW, THETAC, TOBCW_MEAN, TOBCW_MAX, CSTAEQ)
!
          IF(DEBUG.GT.0) WRITE(LU,*) 'END_BEDLOAD_MAIN'
!
!
        ENDIF !ENDIF(CHARR)
!
!       SUSPENSION COMPUTATION
!
        IF(SUSP) THEN
!
          IF(BED_MODEL.EQ.1.OR.BED_MODEL.EQ.2)THEN
!           CALL TO BED1_UPDATE TO UPDATE MASS_MIX_TOT,
!           NECESSARY TO SUSPENSION_ERODE
            CALL BED1_UPDATE(ZR,ZF,VOLU2D)
          ENDIF
!
          IF(DEBUG.GT.0) WRITE(LU,*) 'SUSPENSION_COMPUTATION'
          CALL GAIA_SUSPENSION_ERODE
          SECOND_SUSP_STEP = .TRUE.
          IF(DEBUG.GT.0) WRITE(LU,*) 'END_SUSPENSION_COMPUTATION'
!
!         THIS RETURN CALL ENDS THE GAIA CALL TO GO BACK TO
!         T2D OR T3D AND SOLVE ADVECTION-DIFFUSION EQ.
!         GAIA WILL BE CALLED AGAIN FURTHER ON THE COMPLETE THE
!         BED EVOLUTION AND MASS BALANCE
          RETURN
!
        ENDIF
!
      ENDIF !.NOT.SECOND_SUSP_STEP
!
!---------------------------------------------------------------------
!     CASE 2: SECOND_SUSP_STEP AND SUSP = TRUE OR SUSP = FALSE:
!             SECOND CALL TO GAIA IN THE TIME LOOP IF SUSP = TRUE
!             OR CONTINUATION OF THE ONLY CALL TO GAIA IF SUSP = FALSE
!             COMPUTE THE BED EVOLUTION AND THE MASS BALANCE
!---------------------------------------------------------------------
      IF(SECOND_SUSP_STEP.AND.SUSP) THEN
!
        CALL GAIA_SUSPENSION_DEPOSIT(CODE)
!
        CALL BED1_UPDATE(ZR,ZF,VOLU2D)
!
      ENDIF

      CALL GAIA_BEDLOAD_UPDATE()
!
!=======================================================================
!
!     MAXIMUM BOTTOM SLOPE : EVOL IN T1
!
      IF(SLIDE) THEN
!
        CALL GAIA_SLIDE()
!
        CALL BED1_UPDATE(ZR,ZF,VOLU2D)
!
      ENDIF !ENDIF (SLIDE)
!
!========================================================================
!
!     CONSOLIDATION
!
      CALL GAIA_CONSOLIDATION()
!
!     BED EVOLUTION
!
      CALL GAIA_EVOLUTIONS()
!=====================================================================
!          COMPUTES THE EVOLUTIONS FOR THIS CYCLE OF TIMESTEP
!          AND UPDATES AFTER THIS COMPUTATION
!=====================================================================

      CALL GAIA_BALANCE(ZF_TEL,TELNIT)
!
!=====================================================================
!            PRINTS OUT RESULTS AT THIS TIMESTEP
!            AND COMPARES AGAINST A REFERENCE FILE
!=====================================================================
!
      IF(PRESENT(GRCOMP)) THEN
        GGRCOMP = GRCOMP
        COMP = .TRUE.
      ELSE
        GGRCOMP = 0
        COMP = .FALSE.
      ENDIF

      CALL GAIA_WRITE_RESULTS
     &  (CODE,GRAFCOUNT,GGRCOMP,COMP,LISTCOUNT,YAGOUT,T_TEL)
!
!     SENDS THE NEW ZF TO TELEMAC-2D OR 3D
!
      CALL OV ('X=Y     ', X=ZF_TEL%R, Y=ZF%R, DIM1=NPOIN)
!
!------------------------------------------------------------------
!
#if defined COMPAD
      CALL AD_GAIA_LOOPRECORDS_END
#endif
!
!------------------------------------------------------------------
!
      IF(VALID) THEN
        IF(DEBUG.GT.0) WRITE(LU,*) 'CALL TO BIEF_VALIDA'
!
!       THE SUBROUTINE VALIDA FROM THE LIBRARY IS STANDARD
!       IT CAN BE MODIFIED FOR EACH PARTICULAR CASE
!       BUT ITS CALL MUST BE LEFT IN THE LOOP ON TIME
!
        CALL BIEF_VALIDA(TB,TEXTPR,GAI_FILES(GAIREF)%LU,
     &                   GAI_FILES(GAIREF)%FMT,
     &                   VARSOR,TEXTE,GAI_FILES(GAIRES)%LU,
     &                   GAI_FILES(GAIRES)%FMT,
     &                   MAXVAR,NPOIN,LT,VALNIT,ALIRV)
        IF(DEBUG.GT.0) WRITE(LU,*) 'RETURN FROM BIEF_VALIDA'
      ENDIF
!
!------------------------------------------------------------------
!
#if defined COMPAD
      CALL AD_GAIA_TIMESTEP_END
#endif
!
!=====================================================================
!     RESET SECOND_SUSP_STEP TO FALSE FOR THE NEXT TIME STEP
!=====================================================================
      SECOND_SUSP_STEP=.FALSE.
!
      IF(DEBUG.GT.0) WRITE(LU,*) 'END OF GAIA STEP'
!=======================================================================
!
!                     /* END LOOP ON TIME */
!
!=======================================================================
!
#if defined COMPAD
      CALL AD_GAIA_END
#endif
!
!-----------------------------------------------------------------------
!
      RETURN
      END
