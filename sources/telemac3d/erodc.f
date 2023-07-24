!                   ****************
                    SUBROUTINE ERODC
!                   ****************
!
     &( S3D_CONC, S3D_EPAI, S3D_FLUER, TOB    ,
     &  S3D_MPART, DT     , NPOIN2 , S3D_NCOUCH,S3D_TOCE, HN, HMIN,
     &  S3D_MIXTE, S3D_EPAICO)
!
!***********************************************************************
! TELEMAC3D   V7P2
!***********************************************************************
!
!brief    MODELS EROSION
!+               (WITHIN MULTI-LAYER CONSOLIDATION MODEL).
!+
!+            THE USER PROVIDES THE LAW DEFINING THE CRITICAL
!+                EROSION VELOCITY AS A FUNCTION OF THE CONCENTRATION.
!+
!+            THE EROSION LAW CAN BE CHANGED BY THE USER
!+               (PARTHENIADES FORMULATION BY DEFAULT).
!
!history  C LE NORMANT (LNH)
!+        04/05/93
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
!history  C. VILLARET & T. BENSON & D. KELLY (HR-WALLINGFORD)
!+        27/02/2014
!+        V7P0
!+   New developments in sediment merged on 25/02/2014.
!
!history  G. ANTOINE & M. JODEAU & J.M. HERVOUET (EDF - LNHE)
!+        13/10/2014
!+        V7P0
!+   New developments in sediment for mixed sediment transport
!
!history  J.M. HERVOUET (EDF-LNHE)
!+        07/09/2015
!+        V7P1
!+   Optimisation and divisions by 0 more secured.
!
!history  J.M. HERVOUET (EDF-LNHE)
!+        30/05/2016
!+        V7P2
!+   Formulas TOB/MAX(S3D_TOCE,1.D-10)-1.D0 secured, they could be negative
!+   They are replaced by (TOB_TOCE)/MAX(S3D_TOCE,1.D_20)
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| DT             |-->| TIME STEP
!| HN             |-->| DEPTH AT TIME N
!| HMIN           |-->| MINIMAL VALUE FOR DEPTH
!| NPOIN2         |-->| NUMBER OF POINTS IN 2D
!| S3D_CONC       |-->| CONCENTRATION OF BED LAYER
!| S3D_EPAI       |<->| THICKNESS OF SOLID BED LAYER
!|                |   | (S3D_EPAI=DZ/(1+S3D_IVIDE), DZ total bed thickness)
!| S3D_EPAICO     |-->| THICKNESS OF COHESIVE SUB-LAYER
!| S3D_FLUER      |<->| EROSION  FLUX
!| S3D_MIXTE      |-->| LOGICAL, MIXED SEDIMENTS OR NOT
!| S3D_MPART      |-->| EMPIRICAL COEFFICIENT (PARTHENIADES)
!| S3D_NCOUCH     |-->| NUMBER OF LAYERS WITHIN THE BED
!|                |   | (S3D_GIBSONMODEL)
!| S3D_TOCE       |-->| CRITICAL EROSION SHEAR STRESS
!| TOB            |-->| BOTTOM FRICTION
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE INTERFACE_TELEMAC3D, EX_ERODC => ERODC
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER,          INTENT(IN)    :: NPOIN2,S3D_NCOUCH
      DOUBLE PRECISION, INTENT(IN)    :: S3D_CONC(NPOIN2,S3D_NCOUCH)
      DOUBLE PRECISION, INTENT(IN)    :: HN(NPOIN2)
      DOUBLE PRECISION, INTENT(IN)    :: S3D_TOCE(NPOIN2,S3D_NCOUCH)
      DOUBLE PRECISION, INTENT(INOUT) :: S3D_EPAI(NPOIN2,S3D_NCOUCH)
      DOUBLE PRECISION, INTENT(IN)    :: S3D_EPAICO(*)
      DOUBLE PRECISION, INTENT(INOUT) :: S3D_FLUER(NPOIN2)
      DOUBLE PRECISION, INTENT(IN)    :: TOB(NPOIN2)
      LOGICAL,          INTENT(IN)    :: S3D_MIXTE
      DOUBLE PRECISION, INTENT(IN)    :: S3D_MPART,DT,HMIN
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER IC,IPOIN
      DOUBLE PRECISION QS,TEMPS,QERODE,FLUER_LOC
      INTRINSIC MIN,MAX
!
!-----------------------------------------------------------------------
!
      IF(S3D_MIXTE) THEN
!
        DO IPOIN=1,NPOIN2
          IF(HN(IPOIN).LT.HMIN) THEN
            QERODE=0.D0
          ELSEIF(TOB(IPOIN).GT.S3D_TOCE(IPOIN,1)) THEN
!           EROSION OF TOP LAYER IF TOB > CRITICAL SHEAR STRESS
            FLUER_LOC=S3D_MPART*(TOB(IPOIN)-S3D_TOCE(IPOIN,1))/
     &                              MAX(S3D_TOCE(IPOIN,1),1.D-20)
!           LIMITING LARGE VALUES IN VIEW OF AVAILABLE SEDIMENT
            QERODE=MIN(FLUER_LOC*DT,S3D_CONC(IPOIN,1)*S3D_EPAICO(IPOIN))
          ELSE
            QERODE=0.D0
          ENDIF
          S3D_FLUER(IPOIN)=QERODE/DT
        ENDDO
!
      ELSE
!
        DO IPOIN=1,NPOIN2
!
          S3D_FLUER(IPOIN) = 0.D0
          IF(HN(IPOIN).LT.HMIN) CYCLE
!
!         TEMPS: TIME COUNTER FOR EROSION
          TEMPS=DT
          QERODE=0.D0
!
          DO IC=1, S3D_NCOUCH
            IF(TEMPS.LE.1.D-12) EXIT
!           EROSION OF TOP LAYER IF TOB > CRITICAL SHEAR STRESS
            IF(TOB(IPOIN).GT.S3D_TOCE(IPOIN,IC)) THEN
              FLUER_LOC=S3D_MPART*(TOB(IPOIN)-S3D_TOCE(IPOIN,IC))/
     &                                MAX(S3D_TOCE(IPOIN,IC),1.D-20)
!             LAYER THICKNESS AFTER EROSION ----
              QS=MIN(FLUER_LOC*TEMPS,S3D_CONC(IPOIN,IC)*
     &               S3D_EPAI(IPOIN,IC))
              QERODE=QERODE+QS
!
              IF(QS.GT.1.D-20) THEN
                TEMPS=TEMPS*(1.D0-QS/(FLUER_LOC*TEMPS))
              ELSE
                EXIT
              ENDIF
            ENDIF
          ENDDO
!
!         END OF THE EROSION STEP
!
          S3D_FLUER(IPOIN)=QERODE/DT
!
        ENDDO
!
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END

