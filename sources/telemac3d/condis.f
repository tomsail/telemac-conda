!                   *****************
                    SUBROUTINE CONDIS
!                   *****************
!
     &(S3D_IVIDE, S3D_EPAI, S3D_TREST, S3D_CONC, S3D_TEMP  , S3D_HDEP  ,
     & ZR      , ZF    , X     , Y     , NPOIN2    ,
     & NPF     , S3D_NCOUCH, S3D_TASSE, S3D_ITASS, S3D_RHOS  , S3D_XKV ,
     & S3D_CFDEP, S3D_ESOMT, S3D_TOCE, S3D_SEDCO, S3D_CONC_LAYER,
     & S3D_TOCE_LAYER, S3D_ES_LAYER, S3D_SEDNCO, S3D_MIXTE, S3D_EPAICO,
     & S3D_EPAINCO, S3D_PVSCO, S3D_PVSNCO, S3D_PVSNCO0)
!
!***********************************************************************
! TELEMAC3D   V7P1
!***********************************************************************
!
!brief    INITIALISES THE SEDIMENT VARIABLES.
!
!history  JACEK A. JANKOWSKI PINXIT
!+        **/03/99
!+
!+   FORTRAN95 VERSION
!
!history  NOEMIE DURAND (CHC-NRC); C LE NORMANT (LNH)
!+        18/07/06
!+        V5P7
!+
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
!history  J-M HERVOUET (EDF LAB, LNHE)
!+        02/05/2014
!+        V7P0
!+   Call to NOEROD of Sisyphe for non erodable bed in the case of non
!+   cohesive sediments.
!
!history  G. ANTOINE & M. JODEAU & J.M. HERVOUET (EDF - LNHE)
!+        13/10/2014
!+        V7P0
!+   New developments in sediment for mixed sediment transport
!
!history  C. VILLARET (EDF LAB, LNHE)
!+        02/09/2015
!+        V7P1
!+   Noerod from Sisyphe called to get the same non erodable bed.
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| NPF            |<--| NUMBER OF POINTS WITHIN THE BED ALONG THE VERTICAL
!| NPOIN2         |-->| NUMBER OF POINTS IN 2D
!| S3D_CONC       |<--| CONCENTRATION OF MUD BED LAYER
!| S3D_CONC_LAYER |-->| INPUT CONCENTRATION OF MUD BED LAYER
!|                |   | (MULTILAYER MODEL)
!| S3D_CFDEP      |<->| CONCENTRATION OF SAND LAYER
!| S3D_EPAI       |<--| THICKNESS OF SOLID FRACTION oF THE BED LAYER
!|                |   | (S3D_EPAI=DZ/(1+S3D_IVIDE), DZ BED LAYER THICKNESS)
!| S3D_EPAICO     |<->| THICKNESS OF COHESIVE SUB-LAYER
!| S3D_EPAINCO    |<->| THICKNESS OF NON-COHESIVE SUB-LAYER
!| S3D_ES_LAYER   |-->| INPUT BED LAYER THICKNESS
!| S3D_ESOMT      |<--| CUMULATED BED EVOLUTION
!| S3D_HDEP       |<--| THICKNESS OF FRESH DEPOSIT(FLUID MUD LAYER)
!| S3D_ITASS      |-->| INDEX OF MODEL CHOICE
!| S3D_IVIDE      |<--| VOID RATIO
!|                |   | (S3D_GIBSONMODEL ONLY)
!| S3D_MIXTE      |-->| LOGICAL, MIXED SEDIMENTS OR NOT
!| S3D_NCOUCH     |-->| NUMBER OF LAYERS WITHIN THE BED
!|                |   | (S3D_GIBSONMODEL)
!| S3D_PVSCO      |<->| VOLUMIC PERCENTAGE OF COHESIVE SEDIMENT
!| S3D_PVSNCO     |<->| VOLUMIC PERCENTAGE OF NON-COHESIVE SEDIMENT
!| S3D_PVSNCO0    |-->| INITIAL VOLUMIC PERCENTAGE OF NON-COHESIVE SEDIMENT
!| S3D_RHOS       |-->| DENSITY OF SEDIMENT
!| S3D_SEDCO      |-->| COHESIVE SEDIMENT (LOGICAL)
!| S3D_SEDNCO     |-->| LOGICAL, SEDIMENT NON-COHESIVE OR NOT
!| S3D_TASSE      |-->| MULTILAYER SETTLING MODEL
!| S3D_TEMP       |<--| TIME COUNTER FOR CONSOLIDATION MODEL
!|                |   | (MULTILAYER MODEL)
!| S3D_TOCE       |<--| BED SHEAR STRESS OF MUD BED LAYER
!| S3D_TOCE_LAYER |-->| INPUT BED SHEAR STRESS
!| S3D_TREST      |<->| CONSOLIDATION TIME SCALE
!|                |   | (ONLY FOR MULTILAYER MODEL)
!| S3D_XKV        |-->| NON COHESIVE BED POROSITY
!| X              |-->| FIRST COORDINATE OF 2D MESH
!| Y              |-->| SECOND COORDINATE OF 2D MESH
!| ZF             |-->| BOTTOM ELEVATION
!| ZR             |<--| ELEVATION OF RIDIG BED
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_TELEMAC3D, ONLY : H,Z,NPLAN
      USE INTERFACE_TELEMAC3D, EX_CONDIS => CONDIS
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)             :: NPOIN2,S3D_NCOUCH
      DOUBLE PRECISION, INTENT(OUT)   :: S3D_IVIDE(*), S3D_CFDEP
!
      DOUBLE PRECISION, INTENT(INOUT) :: S3D_EPAI(NPOIN2,S3D_NCOUCH)
      DOUBLE PRECISION, INTENT(INOUT) :: S3D_EPAICO(*), S3D_EPAINCO(*)
      DOUBLE PRECISION, INTENT(INOUT) :: S3D_PVSCO(*), S3D_PVSNCO(*)
      DOUBLE PRECISION, INTENT(INOUT) :: S3D_CONC(NPOIN2,S3D_NCOUCH)
      DOUBLE PRECISION, INTENT(INOUT) :: S3D_TEMP(*)
      DOUBLE PRECISION, INTENT(INOUT) :: S3D_HDEP(NPOIN2)
      DOUBLE PRECISION, INTENT(INOUT) :: ZR(NPOIN2)
      DOUBLE PRECISION, INTENT(IN)    :: ZF(NPOIN2)
      DOUBLE PRECISION, INTENT(IN)    :: X(NPOIN2),Y(NPOIN2)
      DOUBLE PRECISION, INTENT(INOUT) :: S3D_TREST(S3D_NCOUCH)
      DOUBLE PRECISION, INTENT(INOUT) :: S3D_TOCE(NPOIN2,S3D_NCOUCH)
      DOUBLE PRECISION, INTENT(IN)    :: S3D_CONC_LAYER(S3D_NCOUCH)
      DOUBLE PRECISION, INTENT(IN)    :: S3D_ES_LAYER(S3D_NCOUCH)
      DOUBLE PRECISION, INTENT(IN)    :: S3D_TOCE_LAYER(S3D_NCOUCH)
      INTEGER, INTENT(INOUT)          :: NPF(NPOIN2)
      TYPE(BIEF_OBJ), INTENT (INOUT)  :: S3D_ESOMT
      LOGICAL, INTENT(IN)             :: S3D_TASSE
      LOGICAL, INTENT(IN)             :: S3D_SEDCO, S3D_SEDNCO
      LOGICAL, INTENT(IN)             :: S3D_MIXTE
      INTEGER, INTENT(IN)             :: S3D_ITASS
      DOUBLE PRECISION, INTENT(IN)    :: S3D_RHOS,S3D_XKV, S3D_PVSNCO0
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      DOUBLE PRECISION ECOUCH,PVSCO0,HAUTSED,ERROR
      INTEGER IPOIN,IC,IPF,CHOIX,NLISS,NLAYER,K,J
      DOUBLE PRECISION, POINTER :: ZS(:)
!
!     A POINTER TO THE FREE SURFACE IN Z
!
      ZS => Z(1+(NPLAN-1)*NPOIN2:NPLAN*NPOIN2)

!     INITIALISES BED EVOLUTION S3D_ESOMT-----
!
      CALL OS('X=0     ',X=S3D_ESOMT)
!
!     --------------------------------------------------------
!     INITIAL CONDITIONS FOR THE MULTILAYER COHESIVE BED MODEL
!     --------------------------------------------------------
!
!     CALLING NOEROD OF LIBRARY SISYPHE, TO GET ZR
!     NOTE: CHOIX AND NLISS NOT DEFINED, NOT USED
!
      CALL NOEROD(H%R,ZF,ZR,ZS,X,Y,NPOIN2,CHOIX,NLISS)
!
      IF(S3D_SEDCO) THEN
!
!       COHESIVE SEDIMENT OR NON COHESIVE
!
        DO IPOIN=1,NPOIN2
          S3D_HDEP(IPOIN) = 0.D0
          DO IC=1, S3D_NCOUCH
            S3D_CONC(IPOIN,IC) = S3D_CONC_LAYER(IC)
            S3D_TOCE(IPOIN,IC) = S3D_TOCE_LAYER(IC)
            S3D_EPAI(IPOIN,IC) = S3D_ES_LAYER(IC)
            S3D_HDEP(IPOIN)    = S3D_HDEP(IPOIN) + S3D_EPAI(IPOIN,IC)
          ENDDO
        ENDDO
!
      ENDIF
!
      IF(S3D_SEDNCO) THEN
!       LAST METHOD TO DEFINE HARD BOTTOM
!       NON ERODABLE BED: CALLING NOEROD OF SISYPHE
!       ZS, CHOIX AND NLISS ARE NOT USED IN DEFAULT NOEROD
!       NOEROD IS IN LIBRARY SISYPHE
!
!       ONLY ONE LAYER
        S3D_CFDEP= (1.D0-S3D_XKV)*S3D_RHOS
        DO IPOIN = 1,NPOIN2
          S3D_HDEP(IPOIN) = 0.D0
!         CV adding layers for non cohesive
          DO IC=1, S3D_NCOUCH
            S3D_CONC(IPOIN,IC) = S3D_CFDEP
            S3D_EPAI(IPOIN,IC) = S3D_ES_LAYER(IC)
            S3D_HDEP(IPOIN)   = S3D_HDEP(IPOIN)+S3D_EPAI(IPOIN,IC)
          ENDDO
        ENDDO
      ENDIF
!
      IF(S3D_MIXTE) THEN
        S3D_CFDEP= (1.D0-S3D_XKV)*S3D_RHOS
        PVSCO0 = 1.D0-S3D_PVSNCO0
        DO IPOIN=1,NPOIN2
          S3D_CONC(IPOIN,1)  = S3D_CONC_LAYER(1)
          S3D_TOCE(IPOIN,1)  = S3D_TOCE_LAYER(1)
          S3D_PVSCO(IPOIN)   = PVSCO0
          S3D_PVSNCO(IPOIN)  = S3D_PVSNCO0
          S3D_EPAICO(IPOIN)  = PVSCO0 * S3D_ES_LAYER(1)
          S3D_EPAINCO(IPOIN) = S3D_PVSNCO0* S3D_ES_LAYER(1)
          S3D_EPAI(IPOIN,1)  = S3D_ES_LAYER(1)
          S3D_HDEP(IPOIN)    = S3D_EPAI(IPOIN,1)
        ENDDO
      ENDIF
!
!     Debut modif CV
!     CORRECTION OF LAYERS (see in INIT_AVAI)
!     THE HEIGHT OF SEDIMENT (SUM OF ES) MUST NOT BE MORE THAN ZF-ZR
!     IF SO, THE HEIGHT OF THE LAST LAYER IS REDUCED
!     IF THERE ARE LAYERS UNDER ZR, THEY ARE NOT TAKEN INTO ACCOUNT
!
      DO J=1,NPOIN2
        HAUTSED = 0.D0
        NLAYER  = S3D_NCOUCH
        DO K=1,S3D_NCOUCH
          IF(HAUTSED + S3D_EPAI(J,K) .GE. ZF(J) - ZR(J)) THEN
            S3D_EPAI(J,K) = ZF(J) - ZR(J) -  HAUTSED
            NLAYER = K
            HAUTSED = HAUTSED + S3D_EPAI(J,K)
            EXIT
          ENDIF
          HAUTSED = HAUTSED + S3D_EPAI(J,K)
        ENDDO
!       OTHER LAYERS SET TO 0.D0
        IF(NLAYER.LT.S3D_NCOUCH) THEN
          DO K=NLAYER+1,S3D_NCOUCH
            S3D_EPAI(J,K) = 0.D0
          ENDDO
        ENDIF
!       THE THICKNESS OF THE LAST LAYER IS ENLARGED SO THAT
!       THE HEIGHT OF SEDIMENT (SUM OF ES) IS EQUAL TO ZF-ZR
        IF(HAUTSED.LT.ZF(J)-ZR(J)) THEN
          S3D_EPAI(J,S3D_NCOUCH)=S3D_EPAI(J,S3D_NCOUCH)+
     &                           ZF(J)-ZR(J)-HAUTSED
        ENDIF
      ENDDO
!
      DO J=1, NPOIN2
        S3D_HDEP(J) =0.D0
        DO K = 1, S3D_NCOUCH
          S3D_HDEP(J) = S3D_HDEP(J)+ S3D_EPAI(J,K)
        ENDDO
        ERROR= S3D_HDEP(J)-(ZF(J)-ZR(J))
        IF(ABS(ERROR).GT.1.D-6) THEN
          WRITE(LU,*) 'CONDIS: INITIALISATION ERROR ',S3D_ITASS
          CALL PLANTE(1)
          STOP
        ENDIF
      ENDDO
!
! END modif CV
!
!     ------------------------------------------
!     INITIAL CONDITIONS FOR CONSOLIDATION MODEL
!     ------------------------------------------
!
      IF(S3D_TASSE) THEN
!
        IF(S3D_ITASS.EQ.1) THEN
!
!         SIMPLE MULTI-LAYER MODEL
!
!         CHANGES HOURS INTO SECONDS  -----
          CALL OV('X=CX    ', X=S3D_TREST, C=3600.D0, DIM1=S3D_NCOUCH)
!         INITIALISES S3D_TEMP
          CALL OV('X=C     ', X=S3D_TEMP, C=0.D0,DIM1=NPOIN2*S3D_NCOUCH)
!
        ELSEIF(S3D_ITASS.EQ.2) THEN
!
!         S3D_GIBSONMODEL
!
          DO IPOIN=1,NPOIN2
            NPF(IPOIN) =S3D_NCOUCH
            DO IPF= 1, S3D_NCOUCH
              ECOUCH=(S3D_RHOS-S3D_CONC(IPOIN,IPF))/S3D_CONC(IPOIN,IPF)
              IF(IPF.EQ.1) THEN
                S3D_IVIDE(IPOIN+(IPF-1)*NPOIN2)=ECOUCH
              ELSE
                S3D_IVIDE(IPOIN+(IPF-1)*NPOIN2)= 2.D0*ECOUCH
     &                           -S3D_IVIDE(IPOIN+(IPF-2)*NPOIN2)
              ENDIF
            ENDDO
            S3D_IVIDE(IPOIN+S3D_NCOUCH*NPOIN2)= 2.D0*ECOUCH
     &                           -S3D_IVIDE(IPOIN+(S3D_NCOUCH-1)*NPOIN2)
          ENDDO
!
        ELSE
!
          WRITE(LU,*) 'CONDIS: UNKNOWN SETTLING MODEL: ',S3D_ITASS
          CALL PLANTE(1)
          STOP
!
        ENDIF
!
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
