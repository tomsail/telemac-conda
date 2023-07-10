!                   ******************
                    SUBROUTINE KS_GAIA
!                   ******************
!
     &(IKS,KS,KSP,KSR,KSPRATIO,HOULE,GRAV,XMVE,XMVS,VCE,
     & HN,ACLADM,UNORM,UW,TW,NPOIN)
!
!***********************************************************************
! GAIA
!***********************************************************************
!
!>@brief Bed roughness predictor
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!>@param[in]     IKS      Flag, choice of bed roughness predictor
!>@param[in,out] KS       Total bed roughness
!>@param[in,out] KSP      Bed skin roughness
!>@param[in,out] KSR      Ripple bed roughness
!>@param[in]     KSPRATIO Ratio between skin bed roughness and grain diameter
!>@param[in]     HOULE    Logical, wave effect or not
!>@param[in]     GRAV     Acceleration of gravity
!>@param[in]     XMVE     Fluid density
!>@param[in]     XMVS     Sediment density
!>@param[in]     VCE      Water viscosity
!>@param[in]     HN       Water depth
!>@param[in]     ACLADM   Mean diametr of active layer
!>@param[in]     UNORM    Norm of the mean flow velocity
!>@param[in]     UW       Orbital wave velocity
!>@param[in]     TW       Wave period
!>@param[in]     NPOIN    Number of points
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!
!!-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER,            INTENT(IN)  :: NPOIN,IKS
      LOGICAL,            INTENT(IN)  :: HOULE
      DOUBLE PRECISION,   INTENT(IN)  :: XMVE,XMVS, VCE,GRAV
      DOUBLE PRECISION,   INTENT(IN)  :: KSPRATIO
      TYPE(BIEF_OBJ), INTENT(IN)      :: HN,UNORM
      TYPE(BIEF_OBJ), INTENT(IN)      :: TW,UW
      TYPE(BIEF_OBJ), INTENT(INOUT)   :: KS,KSP,KSR
      TYPE(BIEF_OBJ), INTENT(IN)      :: ACLADM
!
!!-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
!
! +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!  BED ROUGHNESS PREDICTOR
!  SKIN   : KSP
!  TOTAL  : KS
!  RIPPLES : KSR
!
!  NOTE: IT IS RECOMMENDED TO USE FRICTION LAW NO 3 WHEN COUPLING TO
!        AVOID UNNECESSARY COMPUTATION
! +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!
! SKIN BED ROUGHNESS --> KSP
!
! RIPPLED BED ROUGHNESS --> KSR =KSP
!
!
! TOTAL BED ROUGHNESS --> KS
!       IKS= 1: FLAT BED     KS=KSP
!       IKS = 2: RIPPLED BED KS= KSP + KSR
!       IKS= 3:  DUNED BED   KS= KSP +KSR +KSMR +KSD
!
      IF(IKS.EQ.1) THEN
!
        CALL OS('X=Y     ', X=KS, Y=KSP)
!
      ELSEIF(IKS.EQ.2) THEN
!
        IF(HOULE) THEN
!         WIBERG AND HARRIS: KSR (RIPPLES)
!                    KS (RIPPLES + SKIN)
          CALL RIDE_GAIA(KSR%R, TW%R, UW%R, UNORM%R, GRAV, XMVE,
     &              XMVS, VCE, NPOIN, KSPRATIO, ACLADM%R)
          CALL OS('X=Y+Z   ', X=KS, Y=KSP, Z=KSR)
        ELSE
!         VR PREDICTOR : KSR (RIPPLES)
          CALL RIDE_VR_GAIA(KSR%R,KS%R,UNORM%R,HN%R,GRAV,XMVE,
     &                 XMVS,NPOIN,ACLADM%R)
          CALL OS('X=Y+Z   ', X=KS, Y=KSP, Z=KSR)
        ENDIF
!
      ELSEIF(IKS.EQ.3) THEN
!
!       VR PREDICTOR : KSR (RIPPLES)
!
        CALL RIDE_VR_GAIA(KSR%R,KS%R,UNORM%R,HN%R,GRAV,XMVE,
     &               XMVS,NPOIN,ACLADM%R)
        CALL OS('X=X+Y   ', X=KS, Y=KSP)
!
      ELSE
!
        WRITE(LU,201) IKS
201     FORMAT(1X,'KS_GAIA:',/,1X,
     &            'BED ROUGHNESS PREDICTOR OPTION',/,1X,
     &            'UNEXPECTED VALUE:',1I6)
        CALL PLANTE(1)
        STOP
!
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
