!                     **********************
                      SUBROUTINE TASSEMENT_2
!                     **********************
!
     &(NPOIN,DTS,ELAY,DZF_TASS,T2,LT,XMVS,XMVE,NOMBLAY,
     & ES,CONC_VASE,MS_VASE,XWC,COEF_N,CONC_GEL,CONC_MAX)
!
!***********************************************************************
! SISYPHE   V6P2                                   13/01/2012
!***********************************************************************
!
!BRIEF    COMPUTES THE CONSOLIDATION BASED ON GIBSON THEORY
!+
!
!history LAN ANH VAN (LHSV)
!+        10/01/2011
!+        V6P2
!+   FIRST VERSION IN TEST (NOT YET CALLED IN CURRENT VERSION 6.2)
!+
!history  PABLO SANTORO (IMFIA) AND PABLO TASSI (EDF R&D - LHSV)
!+        01/08/2015
!+        V7P1
!+   INFLUENCE OF THE DIFFUSION EFFECT IN COMPUTATION OF
!+   THE EFFECTIVE STRESS, SEE VAN (2012)
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| COEF_N         |-->| PERMEABILITY COEFFICIENT
!| CONC_GEL       |-->| GEL CONCENTRATION
!| CONC_MAX       |-->| MAXIMUM CONCENTRATION
!| CONC_VASE      |<->| MUD CONCENTRATION FOR EACH LAYER
!| DTS            |-->| TIME STEP FOR SUSPENSION
!| DZF_TASS       |-->| BED EVOLUTION DUE TO CONSOLIDATION
!| ELAY           |<->| THICKNESS OF EACH LAYER
!| ES             |<->| LAYER THICKNESSES AS DOUBLE PRECISION
!| GRAV           |-->| GRAVITY ACCELERATION
!| LT             |-->| ITERATION
!| MS_VASE        |<->| MASS OF MUD PER LAYER (KG/M2)
!| NOMBLAY        |-->| NUMBER OF LAYERS FOR CONSOLIDATION
!| NPOIN          |-->| NUMBER OF POINTS
!| T2             |<->| WORK BIEF_OBJ STRUCTURE
!| XMVE           |-->| WATER DENSITY
!| XMVS           |-->| SEDIMENT DENSITY
!| XWC            |-->| SETTLING VELOCITY
!| ZF             |-->| ELEVATION OF BOTTOM
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_SISYPHE, ONLY : NLAYMAX
      USE INTERFACE_SISYPHE, EX_TASSEMENT_2 => TASSEMENT_2
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER,INTENT(IN)              :: NPOIN
      INTEGER, INTENT(IN)             :: LT,NOMBLAY
      DOUBLE PRECISION, INTENT(IN)    :: DTS
      DOUBLE PRECISION, INTENT(IN)    :: XMVS,XMVE
      TYPE (BIEF_OBJ),  INTENT(INOUT) :: DZF_TASS,ELAY,T2
      DOUBLE PRECISION, INTENT(INOUT) :: MS_VASE(NPOIN,NOMBLAY)
      DOUBLE PRECISION, INTENT(INOUT) :: ES(NPOIN,NOMBLAY)
      DOUBLE PRECISION, INTENT(IN)    :: CONC_VASE(NOMBLAY)
      DOUBLE PRECISION, INTENT(IN)    :: XWC
      DOUBLE PRECISION, INTENT(IN)    :: COEF_N,CONC_GEL,CONC_MAX
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER I,J
!     FALLING VELOCITY OF EACH LAYER
      DOUBLE PRECISION V_S(NLAYMAX)
!     EFFECTIVE STRESS OF EACH LAYER
      DOUBLE PRECISION SIG_EFF(NLAYMAX)
!     PERMEABILITY
      DOUBLE PRECISION KSED(NLAYMAX),KCONSO(NLAYMAX)
!     SEDIMENT FLUX BETWEEN TWO CONSECUTIVE LAYERS
      DOUBLE PRECISION FLUX(NLAYMAX)
!     DIFFUSION TERM
      DOUBLE PRECISION DIFFU(NLAYMAX)
!
!     ******************************************************************
!     * PROGRAM SIMULATING THE SEDIMENTATION-CONSOLIDATION             *
!     ******************************************************************
!
      DO I =1,NPOIN
        T2%R(I)=0.D0
        DO J=1,NOMBLAY
          T2%R(I)=T2%R(I)+ES(I,J)
        ENDDO

!       FROM VAN (2012)
!       DEPENDING ON THE CLOSURE EQUATIONS,
!       THE INPUT CAN BE EFFECTIVE STRESS OF DIFFUSION TERM (=
!       KC*DSIGMA/DC/(G*RHO_F))
!
!        EFFECTIVE STRESS: DIFFUSION TERM
!       ----------------------------------
        DO J = 1,NOMBLAY
          DIFFU(J)=11.55D0*(CONC_VASE(J)/(XMVS*0.0296D0))**12.D0*
     & (LT*DTS)**(-3.4D0)
        ENDDO

!       EFFECTIVE STRESS
!       ------------------------
        DO J = 1,NOMBLAY
          SIG_EFF(J)=119033.D0*(CONC_VASE(J)/XMVS)**14.D0
        ENDDO

!       PERMEABILITY
!       --------------
        DO J=1,NOMBLAY-1
!       SEDIMENTATION
          KSED(J)=XWC*(1.D0-CONC_VASE(J)/XMVS)*
     &            (1.D0-(CONC_VASE(J)/CONC_GEL))**COEF_N/
     &            ((XMVS-XMVE)*(CONC_VASE(J)/XMVS)/XMVE)
!       CONSOLIDATION
          KCONSO(J)=XWC*(1.D0-CONC_VASE(J)/XMVS)*
     &            (1.D0-(CONC_VASE(J)/CONC_MAX))**COEF_N/
     &            ((XMVS-XMVE)*(CONC_VASE(J)/XMVS)/XMVE)
!
        IF (CONC_VASE(J).GT.CONC_GEL) THEN
!
!     SEDIMENTATION AND CONSOLIDATION :
!     --------------------------------
        IF ((ES(I,J+1) + ES(I,J)).GT.1.D-8) THEN
            V_S(J) =
     & KCONSO(J) * CONC_VASE(J) * (1.D0/XMVS - 1.D0/XMVE)
     & + DIFFU(J)/CONC_VASE(J)*
     & (CONC_VASE(J+1)-CONC_VASE(J))/
     & (0.5D0 * (ES(I,J+1) + ES(I,J)))
! CALCULATE FROM SIG_EFF
!            V_S(J) =
!     &          KCONSO(J) * CONC_VASE(J) * (1.D0/XMVS - 1.D0/XMVE)
!     &          + ( KCONSO(J) / (XMVE * GRAV)) *
!     &          (SIG_EFF(J+1) - SIG_EFF(J)) /
!     &          (0.5D0 * (ES(I,J+1) + ES(I,J)))
          ELSE
            V_S(J) = 1.D8
          ENDIF
        ELSE
!      PURE SEDIMENTATION :
!     ---------------
          V_S(J) = KSED(J)*CONC_VASE(J)*(1.D0/XMVS-1.D0/XMVE)
        ENDIF
      ENDDO
!
        DO J=1,NOMBLAY
          IF (V_S(J).GT.0.D0) V_S(J) = 0.D0
        ENDDO
!
!      FALLING VELOCITY AT THE LEVEL OF ZR (AT THE BED)
          V_S(NOMBLAY) = 0.D0
!      SEDIMENT FLUX :
!     --------------
        DO J=NOMBLAY-1,1,-1
          FLUX(J) =
     &    (V_S(J)-V_S(J+1))*CONC_VASE(J+1)*CONC_VASE(J)/
     &          (CONC_VASE(J+1)-CONC_VASE(J))
          IF (FLUX(J).GT.0.D0) FLUX(J) = 0.D0
        ENDDO
!      SEDIMENT FLUX AT THE RIGID BED
        FLUX(NOMBLAY) = 0.D0
!
!      REDISTRIBUTE THE MASS :
!      ----------------------------------
!      RECALCULATE THE FLUX FROM LAYER 1 TO NCOUCH_TASS
        IF ((MS_VASE(I,1)+DTS*FLUX(1)).LT.0.D0) THEN
          FLUX(1) = -MS_VASE(I,1)/DTS
        ENDIF
        DO J=2,NOMBLAY
          IF ((MS_VASE(I,J)-DTS*(FLUX(J-1)-FLUX(J))).LT.0.D0) THEN
            FLUX(J) = -MS_VASE(I,J)/DTS + FLUX(J-1)
          ENDIF
        ENDDO
!      MASS OF FIRST LAYER
        MS_VASE(I,1)=MS_VASE(I,1)+DTS*FLUX(1)
!      MASS OF LAYER 2 TO NCOUCH_TASS
        DO J=2,NOMBLAY
          MS_VASE(I,J) = MS_VASE(I,J) - DTS * (FLUX(J-1)-FLUX(J))
        ENDDO
!
!      THICKNESSES
        ELAY%R(I)=0.D0
!
        DO J=1,NOMBLAY
          ES(I,J) = MS_VASE(I,J) / CONC_VASE(J)
          ELAY%R(I)=ELAY%R(I) + ES(I,J)
        ENDDO
!      BED EVOLUTION DUE TO CONSOLIDATION
        DZF_TASS%R(I)=ELAY%R(I)-T2%R(I)
      ENDDO
!  END SUBROUTINE TASSEMENT_2
      RETURN
      END
