!                   ****************************
                    SUBROUTINE GAIA_PREPARE_STEP
!                   ****************************
     &(H_TEL, U_TEL, V_TEL, CF_TEL, CHARR_TEL, CODE, DELTAR, DT_TEL,
     & HW_TEL, KS_TEL, LISTCOUNT, LOOPCOUNT, TELNIT, SUSP_TEL,
     & T_TEL, THETAW_TEL, TW_TEL, UETCAR, UW_TEL, ZF_TEL)
!
!***********************************************************************
! GAIA
!***********************************************************************
!>@brief Initial step of the time loop updating data received from
!!Telemac, Handle waves, tomawac coupling information, tidal flats,
!!masking
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!>@param  [in]      H_TEL      WATER DEPTH FROM TEL H (N+1)
!>@param  [in]      U_TEL      U VELOCITY FROM TELEMAC
!>@param  [in,out]  CF_TEL     QUADRATIC FRICTION COEFFICIENT FROM TELEMAC
!>@param  [in,out]  CHARR_TEL  LOGICAL, BED LOAD OR NOT: Sent to TELEMAC-2D
!>@param  [in]      CODE       NAME OF CALLING PROGRAMME (TELEMAC2D OR 3D)
!>@param  [in]      DT_TEL     TIME STEP FROM TELEMAC
!>@param  [in]      HW_TEL     SIGNIFICANT WAVE HEIGHT
!>@param  [in,out]  KS_TEL     BED ROUGHNESS SENT TO TELEMAC
!>@param  [in]      LISTCOUNT  PERIODE DE SORTIE LISTING
!>@param  [in]      LOOPCOUNT  NUMERO DE L'ITERATION
!>@param  [in]      TELNIT     NUMBER OF TELEMAC ITERATIONS
!>@param  [in]      STAGE      INTEGER, TO STATE WHICH PART OF THE SUBROUTINE
!!                             IS PASSED
!>@param  [in,out]  SUSP_TEL   LOGICAL, SUSPENDED LOAD OR NOT: Sent to TELEMAC
!>@param  [in]      T_TEL      CURRENT TIME IN CALLING PROGRAMME
!>@param  [in]      THETAW_TEL WAVE DURECTION (DEG WRT OX AXIS IF TRIGO in TOMAWAC)
!>@param  [in]      TW_TEL     MEAN WAVE PERIOD
!>@param  [in,out]  UETCAR     SQUARE OF THE FRICTION VELOCITY
!!                             (COUPLING WITH TEL 3D)
!>@param  [in]      UW_TEL     ORBITAL WAVE VELOCITY
!>@param  [in]      V_TEL      V VELOCITY FROM TELEMAC
!!                             IS DM1*GRAD(ZCONV), SEE SOLSYS.
!>@param  [in,out]  ZF_TEL     BOTTOM ELEVATION OF THE CALLING TELEMAC
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE INTERFACE_GAIA, EX_GAIA_PREPARE_STEP =>
     &                    GAIA_PREPARE_STEP
      USE BIEF
      USE DECLARATIONS_TELEMAC
      USE DECLARATIONS_TOMAWAC, ONLY :TRIGO
      USE DECLARATIONS_GAIA
      USE INTERFACE_HERMES
      USE DECLARATIONS_SPECIAL
!
      IMPLICIT NONE
!
!!-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER,           INTENT(IN)    :: LOOPCOUNT
      INTEGER,           INTENT(IN)    :: LISTCOUNT,TELNIT
      CHARACTER(LEN=24), INTENT(IN)    :: CODE
      TYPE(BIEF_OBJ),    INTENT(IN)    :: U_TEL,V_TEL,H_TEL
      TYPE(BIEF_OBJ),    INTENT(INOUT) :: ZF_TEL,UETCAR,KS_TEL
      TYPE(BIEF_OBJ),    INTENT(IN)    :: DELTAR
      TYPE(BIEF_OBJ),    INTENT(INOUT) :: CF_TEL
      DOUBLE PRECISION,  INTENT(IN)    :: T_TEL
      LOGICAL,           INTENT(INOUT) :: CHARR_TEL,SUSP_TEL
      DOUBLE PRECISION,  INTENT(IN)    :: DT_TEL
      TYPE(BIEF_OBJ),    INTENT(IN)    :: THETAW_TEL,HW_TEL,TW_TEL
      TYPE(BIEF_OBJ),    INTENT(IN)    :: UW_TEL
!
!!-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER I
!
!---------------------------------------------------------------------
!
      CHARR = CHARR_TEL
      SUSP= SUSP_TEL
      AT0=T_TEL
!
      IF(DEBUG.GT.0) WRITE(LU,*) 'GAIA STAGE 1, COMPUTE_SUSP FALSE'
!
!=======================================================================
!
!                        /* LOOP ON TIME */
!
!=======================================================================
!---------------------------------------------------------------------
!       STARTS THE COMPUTATIONS
!---------------------------------------------------------------------
!
#if defined COMPAD
      CALL AD_GAIA_TIMESTEP_BEGIN
#endif

!
!------------------------------------------------------------------
!
!         ALGORITHMIC DIFFERENTIATION
#if defined COMPAD
      CALL AD_GAIA_LOOPRECORDS_BEGIN
#endif
!
!------------------------------------------------------------------
!
!     DETERMINES THE TIME STEP AND THE TIMESTEP NUMBER
!
      DT=DT_TEL*MOFAC
      LT    = LOOPCOUNT
!
!     PRINTOUTS TO LISTING :
!
      IF(LISTCOUNT*(LT/LISTCOUNT).EQ.LT.OR.LT.EQ.TELNIT) THEN
        ENTET = .TRUE.
      ELSE
        ENTET = .FALSE.
      ENDIF
!
!     FORCE CUSTOM WAVE CONDITIONS
!
      IF(DEBUG.GT.0) WRITE(LU,*) 'USER_FORCING_GAIA'
      CALL USER_FORCING_GAIA
      IF(DEBUG.GT.0) WRITE(LU,*) 'END_USER_FORCING_GAIA'
!
!     OV INSTEAD OF OS IN ORDER TO AVOID PROBLEMS WITH QUASI-BUBBLE ELEMENTS
!     OPERATES ONLY ON THE (1:NPOIN) RANGE OF THE TELEMAC FIELDS
!     IT IS A HIDDEN DISCRETISATION CHANGE
!
      CALL OV('X=Y     ', X=U2D%R, Y=U_TEL%R, DIM1=NPOIN)
      CALL OV('X=Y     ', X=V2D%R, Y=V_TEL%R, DIM1=NPOIN)
      CALL OV('X=Y     ', X=HN%R, Y=H_TEL%R, DIM1=NPOIN)
      CALL OS('X=Y     ', X=ZF, Y=ZF_TEL)
!     CLIPS NEGATIVE DEPTHS
      IF(OPTBAN.GT.0) THEN
!     OPTBAN=1 : DEFAULT OPTION! TO CHANGE?
        DO I = 1,HN%DIM1
          IF(HN%R(I).LT.HMIN) THEN
            U2D%R(I)=0.D0
            V2D%R(I)=0.D0
            HN%R(I)=HMIN
          ENDIF
        ENDDO
      ENDIF
!     FREE SURFACE
      CALL OS('X=Y+Z   ', X=Z, Y=ZF, Z=HN)
!
!     COPY OF TOMAWAC VARIABLES
!
      IF(INCLUS(COUPLING,'TOMAWAC')) THEN
!       INCIDENT WAVE DIRECTION
        CALL OS( 'X=Y     ',THETAW,THETAW_TEL)
!       THETAW_TEL is in ° but not necessary in trigo
!       Transformation in trigo for GAIA
        IF(.NOT.TRIGO)THEN
          DO I=1,NPOIN
            THETAW%R(I)=90.D0-THETAW%R(I)
            THETAW%R(I)=MODULO(THETAW%R(I),360.D0)
          ENDDO
        ENDIF
!       Wave period
        CALL OS( 'X=Y     ', TW, TW_TEL)
!       SIGNIFICANT HEIGHT
        CALL OS( 'X=Y     ', HW , HW_TEL)
!       SIGNIFICANT HEIGHT
        CALL OS( 'X=Y     ', UW , UW_TEL)
        HW%TYPR='Q'
        TW%TYPR='Q'
        THETAW%TYPR='Q'
        UW%TYPR='Q'
      ENDIF
!
!     =========================================================================
!     TREATMENT OF TIDAL FLATS, DEFINITION OF THE MASKS
!     =====================================================================!
!
      IF(OPTBAN.EQ.2) THEN
!
!       BUILDS MASKING BY ELEMENTS
!
        CALL OS ('X=Y     ', X=MSKTMP, Y=MASKEL)
        CALL OS ('X=C     ', X=MASKEL, C=1.D0)
!         MASKS ARE DERIVED FROM THE NON-CLIPPED VALUES OF H
!         PROVIDED BY TELEMAC
          CALL MASKTF(MASKEL%R,H_TEL%R,HMIN,MESH%IKLE%I,
     &                NELEM,NPOIN)
!
!      JMH 17/12/2009
!
!      ELSEIF(OPTBAN.EQ.1) THEN
!
!        CANCELS Q QU AND QV IF HN.LE.0.D0
!        CALL MASKAB_GAIA(HN%R,Q%R,QU%R,QV%R,NPOIN)
!
      ENDIF
!
!     BUILDS THE MASK OF THE POINTS FROM THE MASK OF THE ELEMENTS
!     AND CHANGES IFAMAS (IFABOR WITH MASKING)
!
      IF(MSK) CALL MASKTO(MASKEL%R,MASKPT,IFAMAS%I,
     &              MESH%IKLE%I,
     &              MESH%IFABOR%I,MESH%ELTSEG%I,MESH%NSEG,
     &              NELEM,IELMT,MESH)
!
!------------------------------------------------------------------
!
#if defined(COMPAD)
      CALL AD_GAIA_SUBITERATION_BEGIN
#endif
!
!------------------------------------------------------------------
!
      IF(ENTET) CALL ENTETE_GAIA(2,AT0,LT)
!
!---------------------------------------------------------------------
!     FRICTION COEFFICIENT VARIABLE IN TIME
!---------------------------------------------------------------------
!
      CALL CORSTR_GAIA
!
!     TREATS THE BOUNDARY CONDITIONS
!
      IF(DEBUG.GT.0) WRITE(LU,*) 'CONLIT_GAIA'
      CALL CONLIT_GAIA
      IF(DEBUG.GT.0) WRITE(LU,*) 'END CONLIT_GAIA'
!
! =======================================================================
!
!     MEAN DIAMETER FOR THE ACTIVE-LAYER AND UNDER-LAYER
!
      IF(NMUD.EQ.0.AND.NSICLA.GT.1.AND.NSAND.GT.0) THEN
        CALL MEAN_GRAIN_SIZE_GAIA
      ENDIF
!
!     NORM OF THE MEAN VELOCITY: UNORM
!
      CALL OS('X=N(Y,Z)',X=UNORM,Y=U2D,Z=V2D)
!
!     DIRECTION OF CURRENT THETAC (° and trigo )
!                                 (same convention as THETAW in GAIA)
      CALL BEDLOAD_DIRECTION_GAIA(U2D,V2D,NPOIN,PI,THETAC)
!     radians to  degres
      DO I=1,NPOIN
        THETAC%R(I)=THETAC%R(I)*180/PI
        THETAC%R(I)=MODULO(THETAC%R(I),360.D0)
      ENDDO
!
!     WAVE ORBITAL VELOCITY: UW
!
      IF(HOULE) THEN
!       Forcing done inside gaia, set through the steering file
        IF(UW%TYPR.NE.'Q') THEN
        CALL CALCUW_GAIA(UW%R,HN%R,HW%R,TW%R,GRAV,NPOIN,TYPE_HOULE)
        ELSE
!         Monochromatic wave forcing: UW has just been calculated
!         in CALCUW_GAIA
          IF(TYPE_HOULE.EQ.1)THEN
            CONTINUE
!           Coupling with TOMAWAC (irregular waves)
!           UW calculated from sprectum is an Urms!
!           transformation for a Jonswap spectrum (Soulsby 1993)
          ELSEIF(TYPE_HOULE.EQ.2)THEN
            DO I=1,NPOIN
              UW%R(I)=SQRT(2.D0)*UW%R(I)
            ENDDO
          ELSE
            WRITE(LU,*)'VALUE OF TYPE OF WAVES IS NOT OK'
            CALL PLANTE(1)
            STOP
          ENDIF
        ENDIF
      ENDIF
!
!     BED SHEAR STRESS COMPUTATION
!
      CALL TOB_GAIA
     &(TOB,TOBW, TOBCW_MEAN, TOBCW_MAX, THETAC, THETAW,MU,
     & KS, KSP,KSR,CF, FW,UETCAR, CF_TEL,KS_TEL, CODE ,
     & ICR, KSPRATIO,HOULE,
     & GRAV,XMVE,  XMVS0(1), VCE, KARMAN,ZERO,
     & HMIN,HN, ACLADM, UNORM,UW, TW, NPOIN,KSCALC,IKS,
     & DELTAR, H_TEL)
!
!     REFERENCE ELEVATION COMPUTATION
!
      IF(SUSP) CALL ZREF_GAIA
!
!-----------------------------------------------------------------------
!
      RETURN
      END
