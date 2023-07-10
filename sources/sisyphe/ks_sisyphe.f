!                   *********************
                    SUBROUTINE KS_SISYPHE
!                   *********************
!
     &(IKS,KS,KSP,KSR,KSPRATIO,HOULE,GRAV,XMVE,XMVS,VCE,
     & HN,ACLADM,UNORM,UW,TW,NPOIN)
!
!***********************************************************************
! SISYPHE   V6P1                                   21/07/2011
!***********************************************************************
!
!brief    BED ROUGHNESS PREDICTOR
!
!history  C. VILLARET & N. HUYBRECHTS
!+        21/08/2010
!+
!+
!history  C. VILLARET & P. TASSI
!+        24/07/2011
!+        name of variables
!+

!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| IKS            |-->| FLAG, CHOICE OF BED ROUGHNESS PREDICTOR
!| KS             |<->| TOTAL BED ROUGHNESS
!| KSP            |<->| SKIN  BED ROUGHNESS
!| KSR            |<->| RIPPLE BED ROUGHNESS
!| KSPRATIO       |-->| RATIO BETWEEN SKIN BED ROUGHNESS AND GRAIN DIAMETER
!| HOULE          |-->| LOGICAL, WAVE EFFECT OR NOT
!| GRAV           |-->| ACCELERATION OF GRAVITY
!| XMVE           |-->| FLUID DENSITY
!| XMVS           |-->| SEDIMENT DENSITY
!| VCE            |-->| WATER VISCOSITY
!| HMIN           |-->| MINIMUM WATER DEPTH
!| HN             |-->| WATER DEPTH
!| ACLADM         |-->| MEAN GRAIN SIZE
!| UNORM          |-->| NORM OF THE MEAN FLOW VELOCITY
!| UW             |-->| ORBITAL WAVE VELOCITY
!| TW             |-->| WAVE PERIOD
!| NPOIN          |-->| NUMBER OF POINTS
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
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
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
!
! +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!  BED ROUGHNESS PREDICTOR
!  SKIN   : KSP
!  TOTAL  : KS
!  RIPPLES : KSR
!  KS PUT IN CHESTR IF NO COUPLING, RE-COMPUTED OTHERWISE
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
!       IKS= 0: FLAT BED     KS=KSP
!       IKS = 1: RIPPLED BED KS= KSP + KSR
!       IKS=3 :  DUNED BED   KS= KSP +KSR +KSMR +KSD
!
      IF(IKS.EQ.0) THEN
!
        CALL OS('X=Y     ', X=KS, Y=KSP)
!
      ELSEIF(IKS.EQ.1) THEN
!
        IF(HOULE) THEN
!         WIBERG AND HARRIS: KSR (RIPPLES)
!                    KS (RIPPLES + SKIN)
          CALL RIDE(KSR%R, TW%R, UW%R, UNORM%R, GRAV, XMVE,
     &              XMVS, VCE, NPOIN, KSPRATIO, ACLADM%R)
          CALL OS('X=Y+Z   ', X=KS, Y=KSP, Z=KSR)
        ELSE
!         VR PREDICTOR : KSR (RIPPLES)
          CALL RIDE_VR(KSR%R,KS%R,UNORM%R,HN%R,GRAV,XMVE,
     &                 XMVS,NPOIN,ACLADM%R)
          CALL OS('X=Y+Z   ', X=KS, Y=KSP, Z=KSR)
        ENDIF
!
      ELSEIF(IKS.EQ.3) THEN
!
!       VR PREDICTOR : KSR (RIPPLES)
!
        CALL RIDE_VR(KSR%R,KS%R,UNORM%R,HN%R,GRAV,XMVE,
     &               XMVS,NPOIN,ACLADM%R)
        CALL OS('X=X+Y   ', X=KS, Y=KSP)
!
      ELSE
!
        WRITE(LU,201) IKS
201     FORMAT(1X,'KS_SISYPHE:',/,1X,
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
