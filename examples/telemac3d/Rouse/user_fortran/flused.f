!                   *****************
                    SUBROUTINE FLUSED
!                   *****************
!
     &(ATABOF , BTABOF ,
     & LITABF , WC      ,
     & HN      ,
     & TOB    , S3D_FLUDPT, S3D_FLUER, S3D_TOCD,
     & NPOIN3 , NPOIN2 , NPLAN  , KLOG    ,
     & HMIN   , S3D_SEDCO, S3D_SETDEP, S3D_SEDNCO,
     & S3D_WCS, S3D_MIXTE, S3D_FLUDPTC, S3D_FLUDPTNC)
!
!***********************************************************************
! TELEMAC3D   V7P0                                   03/06/2014
!***********************************************************************
!
!brief    WRITES THE FLUXES AT THE BOTTOM AND FREE SURFACE
!+                FOR THE SEDIMENT.
!
!note     CHECKS MASS BALANCE AT THE BOTTOM AND FREE SURFACE.
!+         RESULTS IN A BOUNDARY CONDITION ON SEDIMENT FLUXES.
!
!history  C LE NORMANT (LNH)
!+        13/05/92
!+        V5P5
!+
!
!history  JACEK A. JANKOWSKI PINXIT
!+        **/03/99
!+
!+   FORTRAN95 VERSION
!
!history  N.DURAND (HRW), S.E.BOURBAN (HRW)
!+        13/07/2010
!+        V6P0
!+   Translation of French comments within the FORTRAN sources into
!+   English comments
!
!history  N.DURAND (HRW), S.E.BOURBAN (HRW)
!+        21/08/2010
!+        V6P0
!+   Creation of DOXYGEN tags for automated documentation and
!+   cross-referencing of the FORTRAN sources
!
!history  C. VILLARET (HR-WALLINGFORD) & J-M HERVOUET (EDF LAB, LNHE)
!+        20/01/2014
!+        V7P0
!+   Erosion and deposition fluxes cancelled on tidal flats.
!
!history  J-M HERVOUET (EDF LAB, LNHE)
!+        03/06/2014
!+        V7P0
!+   Crushed planes treated with IPBOT.
!
!history  G. ANTOINE & M. JODEAU & J.M. HERVOUET (EDF - LNHE)
!+        13/10/2014
!+        V7P0
!+   New developments in sediment for mixed sediment transport
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| ATABOF         |<->| FOR BOUNDARY CONDITION (BOTTOM)
!| BTABOF         |<->| FOR BOUNDARY CONDITION (BOTTOM)
!| HMIN           |-->| MINIMUM WATER DEPTH TO PREVENT EROSION ON TIDAL FLATS
!| HN             |-->| WATER DEPTH AT TIME N
!| KLOG           |-->| CONVENTION FOR SOLID BOUNDARY
!| LITABF         |-->| FOR BOUNDARY CONDITION BOTTOM
!| NPLAN          |-->| NUMBER OF PLANES IN THE 3D MESH OF PRISMS
!| NPOIN2         |-->| NUMBER OF 2D POINTS
!| NPOIN3         |-->| NUMBER OF 3D POINTS
!| S3D_FLUDPT     |<->| IMPLICIT DEPOSITION FLUX
!| S3D_FLUDPTC    |<->| IMPLICIT DEPOSITION FLUX FOR COHESIVE SEDIMENT
!| S3D_FLUDPTNC   |<->| IMPLICIT DEPOSITION FLUX FOR NON-COHESIVE SEDIMENT
!| S3D_FLUER      |<->| EROSION  FLUX FOR EACH 2D POINT
!| S3D_MIXTE      |-->| LOGICAL, MIXED SEDIMENTS OR NOT
!| S3D_SEDCO      |-->| LOGICAL FOR COHESIVE SEDIMENT
!| S3D_SEDNCO     |-->| LOGICAL, SEDIMENT NON-COHESIVE OR NOT
!| S3D_SETDEP     |-->| CHOICE OF CONVECTION SCHEME FOR VERTICAL SETTLING
!| S3D_TOCD       |-->| CRITICAL SHEAR STRESS FOR SEDIMENT DEPOSITION
!| S3D_WCS        |-->| SETTLING VELOCITY OF SAND
!| TOB            |<->| BOTTOM FRICTION
!| WC             |-->| SETTLING VELOCITY OF MUD
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_TELEMAC3D, ONLY: IPBOT,SIGMAG,OPTBAN
      USE INTERFACE_TELEMAC3D, EX_FLUSED => FLUSED
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN) :: NPOIN3,NPOIN2,NPLAN,KLOG,S3D_SETDEP
      LOGICAL, INTENT(IN) :: S3D_SEDCO, S3D_SEDNCO, S3D_MIXTE
!
!     BOTTOM
!     ****
!
!     BY POINTS
!     ----------
!
      INTEGER, INTENT(IN) :: LITABF(NPOIN2)
!
!     BY FACES
!     ---------
      DOUBLE PRECISION, INTENT(INOUT) :: ATABOF(NPOIN2), BTABOF(NPOIN2)
!
!     OTHER ARRAYS
!
      DOUBLE PRECISION, INTENT(IN) :: WC(NPOIN3)
      DOUBLE PRECISION, INTENT(IN) :: HN(NPOIN2)
      DOUBLE PRECISION, INTENT(INOUT) :: TOB(NPOIN2)
      DOUBLE PRECISION, INTENT(INOUT) :: S3D_FLUDPT(NPOIN2)
      DOUBLE PRECISION, INTENT(INOUT) :: S3D_FLUER(NPOIN2)
      DOUBLE PRECISION, INTENT(INOUT) :: S3D_FLUDPTC(*)
      DOUBLE PRECISION, INTENT(INOUT) :: S3D_FLUDPTNC(*)
!
      DOUBLE PRECISION, INTENT(IN) :: S3D_TOCD
      DOUBLE PRECISION, INTENT(IN) :: S3D_WCS(*)
      DOUBLE PRECISION, INTENT(IN) :: HMIN
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER I,I3D
!
      INTRINSIC MAX
!
!-----------------------------------------------------------------------
!
!     COMPUTES THE DEPOSITION PROBABILITY
!
      IF(S3D_SEDCO) THEN
!
!       COHESIVE SEDIMENT (Here S3D_FLUDPT>0)
!
        IF(SIGMAG.OR.OPTBAN.EQ.1) THEN
          DO I=1,NPOIN2
            IF(IPBOT%I(I).NE.NPLAN-1) THEN
!             DEPOSITION ON THE FIRST FREE PLANE WITH LOCAL VELOCITY
              I3D=I+IPBOT%I(I)*NPOIN2
              S3D_FLUDPT(I) = WC(I3D)*MAX(1.D0-TOB(I)/
     &                        MAX(S3D_TOCD,1.D-6),0.D0)
            ELSE
!             TIDAL FLAT
              S3D_FLUDPT(I) = 0.D0
            ENDIF
          ENDDO
        ELSE
          DO I=1,NPOIN2
            S3D_FLUDPT(I) = WC(I)*MAX(1.D0-(TOB(I)/MAX(S3D_TOCD,1.D-6)),
     &                                      0.D0)
          ENDDO
        ENDIF
!
      ENDIF

      IF(S3D_SEDNCO) THEN
!
!       NON COHESIVE SEDIMENT
!
        IF(SIGMAG.OR.OPTBAN.EQ.1) THEN
          DO I=1,NPOIN2
            IF(IPBOT%I(I).NE.NPLAN-1) THEN
!             DEPOSITION ON THE FIRST FREE PLANE WITH LOCAL VELOCITY
!             BEGINNING OF SPECIFIC TO THIS CASE
!             S3D_FLUDPT(I) = WC(I)
              S3D_FLUDPT(I) = 0.D0
!             END OF SPECIFIC TO THIS CASE
            ELSE
!             TIDAL FLAT
              S3D_FLUDPT(I) = 0.D0
            ENDIF
          ENDDO
        ELSE
          DO I=1,NPOIN2
!           BEGINNING OF SPECIFIC TO THIS CASE
!           S3D_FLUDPT(I) = WC(I)
            S3D_FLUDPT(I) = 0.D0
!           END OF SPECIFIC TO THIS CASE
          ENDDO
        ENDIF
!
      ENDIF

      IF(S3D_MIXTE) THEN

        IF(SIGMAG.OR.OPTBAN.EQ.1) THEN
          DO I=1,NPOIN2
            IF(IPBOT%I(I).NE.NPLAN-1) THEN
!             DEPOSITION ON THE FIRST FREE PLANE WITH LOCAL VELOCITY
              I3D = I+IPBOT%I(I)*NPOIN2
              S3D_FLUDPTC(I) = WC(I3D)*MAX(1.D0-TOB(I)/
     &                                     MAX(S3D_TOCD,1.D-6),0.D0)
              S3D_FLUDPTNC(I)= S3D_WCS(I)
              S3D_FLUDPT(I)  = S3D_FLUDPTC(I)+S3D_FLUDPTNC(I)
            ELSE
!             TIDAL FLAT
              S3D_FLUDPT(I)   = 0.D0
              S3D_FLUDPTC(I)  = 0.D0
              S3D_FLUDPTNC(I) = 0.D0
            ENDIF
          ENDDO
        ELSE
          DO I=1,NPOIN2
            S3D_FLUDPTC(I)  = WC(I)*MAX(1.D0-(TOB(I)/
     &                                  MAX(S3D_TOCD,1.D-6)),0.D0)
            S3D_FLUDPTNC(I) = S3D_WCS(I)
            S3D_FLUDPT(I)   = S3D_FLUDPTC(I)+S3D_FLUDPTNC(I)
          ENDDO
        ENDIF
!
      ENDIF
!

!-----------------------------------------------------------------------
!
!     COMPUTATION OF THE TRACER FLUX ON THE BOTTOM
!
      IF(S3D_SETDEP.EQ.1) THEN
!
!       USING HMIN TO CLIP EROSION (DIFFERENT FROM USING IPBOT)
        DO I=1,NPOIN2
          IF(HN(I).LE.HMIN) THEN
            S3D_FLUER(I) = 0.D0
          ENDIF
        ENDDO
!
        DO I=1,NPOIN2
          IF(LITABF(I).EQ.KLOG) THEN
!           TOM : erosion and deposition are treated with advection
            ATABOF(I) = 0.D0
            BTABOF(I) = 0.D0
          ENDIF
        ENDDO
!
      ELSEIF(SIGMAG.OR.OPTBAN.EQ.1) THEN
!
        DO I=1,NPOIN2
          ATABOF(I) = 0.D0
          BTABOF(I) = 0.D0
          IF(LITABF(I).EQ.KLOG) THEN
!           NO EROSION AND DEPOSITION ON TIDAL FLATS
            IF(IPBOT%I(I).NE.NPLAN-1) THEN
              ATABOF(I) = -S3D_FLUDPT(I)
              BTABOF(I) =  S3D_FLUER(I)
            ENDIF
          ENDIF
        ENDDO
!
      ELSE
!
        DO I=1,NPOIN2
          IF(LITABF(I).EQ.KLOG) THEN
!           NZ = 1.D0+GRADZFX(I)**2+GRADZFY(I)**2
!           NZ = -1.D0/SQRT(NZ)
!           WC
!           ATABOF(I) = - WC(I) * PDEPOT(I) * NZ
!           BTABOF(I) = - S3D_FLUER(I) * NZ
!           JMH: BEWARE, IN DIFF3D NZ IS CONSIDERED AS -1.
!                HENCE WRONG FORMULA BELOW IS ACTUALLY CORRECT
            ATABOF(I) = -S3D_FLUDPT(I)
            BTABOF(I) =  S3D_FLUER(I)
          ENDIF
        ENDDO
!
      ENDIF
!
!-----------------------------------------------------------------------
!
!     BOUNDARY CONDITION AT THE FREE SURFACE
!
!     FLUX  = 0 (SETTLING VELOCITY FLUX + DIFFUSIVE FLUX)
!
!     ALREADY DONE IN LIMI3D !!
!
!     DO I=1,NPOIN2
!       ATABOS(I)=0.D0
!       BTABOS(I)=0.D0
!     ENDDO
!
!-----------------------------------------------------------------------
!
      RETURN
      END

