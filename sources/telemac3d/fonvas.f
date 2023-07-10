!                   *****************
                    SUBROUTINE FONVAS
!                   *****************
!
     &(S3D_EPAI, S3D_CONC, S3D_HDEP,
     & S3D_FLUDP, S3D_FLUDPT, S3D_FLUER, ZF    , TA     ,
     & NPOIN2, NPOIN3 ,
     & S3D_NCOUCH, DT    ,
     & S3D_ZF_S, S3D_ESOMT,
     & VOLU2D  , S3D_MASDEP, S3D_SETDEP, ZR    , TS     , S3D_FLUDPTC,
     & S3D_FLUDPTNC, S3D_FLUERC, S3D_FLUERNC, S3D_MIXTE, S3D_FLUDPC,
     & S3D_FLUDPNC, S3D_PVSCO, S3D_PVSNCO, S3D_CFDEP, S3D_EPAICO,
     & S3D_EPAINCO)
!
!***********************************************************************
! TELEMAC3D   V7P2                                   21/08/2010
!***********************************************************************
!
!brief    MODELS THE MUD BED EVOLUTION.
!
!history  C LE NORMANT (LNH)
!+        13/05/92
!+        V5P1
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
!+   ZR not used anymore, S3D_ZF_Sand S3D_ESOMTadded.
!+   (note JMH: not a good idea, see next but one history)
!+   S3D_MASDEPcomputed: total deposited mass.
!
!history  J-M HERVOUET (EDF LAB, LNHE)
!+        30/04/2014
!+        V7P0
!+   IF(S3D_SETDEP.EQ.0) changed into IF(S3D_SETDEP.NE.1) to allow options
!+   other than 0 and 1.
!
!history  J-M HERVOUET (EDF LAB, LNHE)
!+        02/05/2014
!+        V7P0
!+   Argument ZR added again. It is important to write:
!+   ZF=ZR+S3D_HDEP, not ZF=ZF+evolution, otherwise we find cases where
!+   ZF<ZR, due to truncation errors.
!
!history  G. ANTOINE & M. JODEAU & J.M. HERVOUET (EDF - LNHE)
!+        13/10/2014
!+        V7P0
!+   New developments in sediment for mixed sediment transport
!
!history  J-M HERVOUET (EDF LAB, LNHE)
!+        30/05/2016
!+        V7P2
!+   Removing FLUXC and FLUXNC, variables not initialised, computed, and
!+   not used. DELTAFC and DELTAFNC, TOTMASSC and TOTMASSNC removed.
!+   Clipping on concentrations removed (they are data and are not 0).
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| DT             |-->| HYDRAULIC TIME STEP
!| NPOIN2         |-->| NUMBER OF POINTS  (2D MESH)
!| NPOIN3         |-->| NUMBER OF POINTS  (3D MESH)
!| S3D_CFDEP      |-->| CONCENTRATION OF MUD DEPOSIT (G/L)
!| S3D_CONC       |-->| MUD BED LAYER CONCENTRATION
!|                |   | (MULTILAYER MODEL)
!| S3D_EPAI       |<->| THICKNESS OF SOLID FRACTION OF THE BED LAYER
!|                |   | (S3D_EPAI=DZ/(1+S3D_IVIDE), DZ BED LAYER THICKNESS
!| S3D_EPAICO     |<->| THICKNESS OF COHESIVE SUB-LAYER
!| S3D_EPAINCO    |<->| THICKNESS OF NON-COHESIVE SUB-LAYER
!| S3D_ESOMT      |<->| CUMULATED BED EVOLUTION
!| S3D_FLUDP      |<->| DEPOSITION FLUX
!| S3D_FLUDPC     |<->| DEPOSITION FLUX FOR COHESIVE SEDIMENT IN 2D
!| S3D_FLUDPNC    |<->| DEPOSITION FLUX FOR NON-COHESIVE SEDIMENT IN 2D
!| S3D_FLUDPT     |<--| IMPLICIT DEPOSITION FLUX
!| S3D_FLUDPTC    |<--| IMPLICIT DEPOSITION FLUX FOR COHESIVE SEDIMENT
!| S3D_FLUDPTNC   |<--| IMPLICIT DEPOSITION FLUX FOR NON-COHESIVE SEDIMENT
!| S3D_FLUER      |<--| EROSION FLUX FOR POINTS IN 2D
!| S3D_FLUERC     |<--| EROSION FLUX FOR COHESIVE SEDIMENT IN 2D
!| S3D_FLUERNC    |<--| EROSION FLUX FOR NON-COHESIVE SEDIMENT IN 2D
!| S3D_HDEP       |<->| THICKNESS OF FRESH DEPOSIT (FLUID MUD LAYER)
!| S3D_MASDEP     |<->| DEPOSITED MASS
!| S3D_MIXTE      |-->| LOGICAL, MIXED SEDIMENTS OR NOT
!| S3D_NCOUCH     |-->| NUMBER OF LAYERS WITHIN THE BED
!|                |   | (S3D_GIBSONMULTILAYER SETTLING MODEL)
!| S3D_PVSCO      |<->| PERCENTAGE OF MUD
!| S3D_PVSNCO     |<->| PERCENTAGE OF SAND
!| S3D_SETDEP     |-->| CHOICE OF ADVECTION SCHEME FOR VERTICAL SETTLING
!| S3D_ZF_S       |<->| BED EVOLUTION
!| TA             |-->| ACTIVE TRACOR
!| TS             |-->| SAND CONCENTRATION
!| VOLU2D         |-->|  INTEGRAL OF TEST FUNCTIONS IN 2D (SURFACE OF ELEMENTS)
!| ZF             |<->| BOTTOM ELEVATION
!| ZR             |-->| RIGID BED LEVEL
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE INTERFACE_PARALLEL
      USE INTERFACE_TELEMAC3D, EX_FONVAS => FONVAS
      USE DECLARATIONS_TELEMAC3D, ONLY : IPBOT,OPTBAN,NPLAN
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN) ::  NPOIN2,NPOIN3
      INTEGER, INTENT(IN) ::  S3D_NCOUCH
!
      DOUBLE PRECISION, INTENT(INOUT) :: S3D_HDEP(NPOIN2)
!
      DOUBLE PRECISION, INTENT(INOUT) :: S3D_EPAI(NPOIN2,S3D_NCOUCH)
      DOUBLE PRECISION, INTENT(INOUT) :: S3D_EPAICO(*), S3D_EPAINCO(*)
!
      DOUBLE PRECISION, INTENT(IN)    :: S3D_CONC(NPOIN2,S3D_NCOUCH)
      DOUBLE PRECISION, INTENT(IN)    :: ZR(NPOIN2)
      DOUBLE PRECISION, INTENT(INOUT) :: ZF(NPOIN2)
      DOUBLE PRECISION, INTENT(IN)    :: TA(NPOIN3)
      DOUBLE PRECISION, INTENT(IN)    :: TS(NPOIN3)
      DOUBLE PRECISION, INTENT(IN)    :: S3D_FLUDPT(NPOIN2)
      DOUBLE PRECISION, INTENT(IN)    :: S3D_FLUER(NPOIN2)
      DOUBLE PRECISION, INTENT(IN)    :: S3D_FLUDPTC(*),S3D_FLUERC(*)
      DOUBLE PRECISION, INTENT(IN)    :: S3D_FLUDPTNC(*)
      DOUBLE PRECISION, INTENT(IN)    :: S3D_FLUERNC(*)
      DOUBLE PRECISION, INTENT(INOUT) :: S3D_FLUDP(NPOIN2)
      DOUBLE PRECISION, INTENT(INOUT) :: S3D_ZF_S(NPOIN2)
      DOUBLE PRECISION, INTENT(INOUT) :: S3D_FLUDPC(*), S3D_FLUDPNC(*)
      DOUBLE PRECISION, INTENT(INOUT) :: S3D_PVSCO(*), S3D_PVSNCO(*)
      DOUBLE PRECISION, INTENT(INOUT) :: S3D_ESOMT(NPOIN2)
!
      DOUBLE PRECISION, INTENT(IN)    :: DT
      DOUBLE PRECISION, INTENT(IN)    :: S3D_CFDEP
      DOUBLE PRECISION, INTENT(INOUT) :: S3D_MASDEP
!
!
      LOGICAL, INTENT(IN) :: S3D_MIXTE
      INTEGER, INTENT(IN) :: S3D_SETDEP
!
      TYPE(BIEF_OBJ), INTENT(IN) :: VOLU2D
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      DOUBLE PRECISION TOTMASS,QERODE,QS,DELTAF,FLUX,SEDBED,MASTMP
      DOUBLE PRECISION QERODEC,QSC,QERODENC,QSNC
      INTEGER IPOIN,IC
!
!=======================================================================
! FIRST STEP
!     COMPUTES THE DEPOSITED QUANTITY (IN MATERIAL COORDINATES)
!     S3D_CFDEP=S3D_CONC(S3D_NCOUCH)
!     COMPUTES THE ERODED QUANTITY
!=======================================================================
!
! Multi layer model
! +++++++++++++++++++
! deposition in the first top layer
! calculate the layers thicknesses and deposited thicknes:
!  S3D_HDEP = sum(S3D_EPAI)
!
      FLUX=0.D0
!
      IF(S3D_MIXTE) THEN
!
        IF(S3D_SETDEP.NE.1) THEN
          IF(OPTBAN.EQ.1) THEN
            DO IPOIN=1,NPOIN2
              IF(IPBOT%I(IPOIN).NE.NPLAN-1) THEN
!         COMPUTES FIRST THE DEPOSIT FLUX OF COHESIVE SEDIMENTS
                S3D_FLUDPC(IPOIN)=S3D_FLUDPTC(IPOIN)*
     &          TA(IPBOT%I(IPOIN)*NPOIN2+IPOIN)
                S3D_FLUDPC(IPOIN)=MAX(S3D_FLUDPC(IPOIN),0.D0)
!         THEN COMPUTES THE DEPOSIT FLUX OF NON COHESIVE SEDIMENTS
                S3D_FLUDPNC(IPOIN)=S3D_FLUDPTNC(IPOIN)*
     &          TS(IPBOT%I(IPOIN)*NPOIN2+IPOIN)
                S3D_FLUDPNC(IPOIN)=MAX(S3D_FLUDPNC(IPOIN),0.D0)
!         THE GLOBAL DEPOSITION FLUX IS THE SUMM OF BOTH C & NC
                S3D_FLUDP(IPOIN)=S3D_FLUDPC(IPOIN)+S3D_FLUDPNC(IPOIN)
                S3D_FLUDP(IPOIN)=MAX(S3D_FLUDP(IPOIN),0.D0)
              ELSE
                S3D_FLUDPC(IPOIN) = 0.D0
                S3D_FLUDPNC(IPOIN)= 0.D0
                S3D_FLUDP(IPOIN)  = 0.D0
              ENDIF
            ENDDO
          ELSE
            DO IPOIN=1,NPOIN2
!             COMPUTES FIRST THE DEPOSIT FLUX OF COHESIVE SEDIMENTS
              S3D_FLUDPC(IPOIN)=S3D_FLUDPTC(IPOIN)*TA(IPOIN)
              S3D_FLUDPC(IPOIN)=MAX(S3D_FLUDPC(IPOIN),0.D0)
!             THEN COMPUTES THE DEPOSIT FLUX OF NON COHESIVE SEDIMENTS
              S3D_FLUDPNC(IPOIN)=S3D_FLUDPTNC(IPOIN)*TS(IPOIN)
              S3D_FLUDPNC(IPOIN)=MAX(S3D_FLUDPNC(IPOIN),0.D0)
!             THE GLOBAL DEPOSITION FLUX IS THE SUM OF BOTH C & NC
              S3D_FLUDP(IPOIN)=S3D_FLUDPC(IPOIN)+S3D_FLUDPNC(IPOIN)
              S3D_FLUDP(IPOIN)=MAX(S3D_FLUDP(IPOIN),0.D0)
            ENDDO
          ENDIF
        ENDIF
!
!       BED EVOLUTION
!
        DO IPOIN=1,NPOIN2
!
!         COMPUTES QERODE FOR COHESIVE SEDIMENTS
!
          QERODEC  = S3D_FLUERC(IPOIN)*DT
!
!         COMPUTES QERODE FOR NON-COHESIVE SEDIMENT
!
          QERODENC  = S3D_FLUERNC(IPOIN)*DT
!
!         COMPUTES QERODE FOR ALL THE SEDIMENTS
!
          DELTAF  = S3D_FLUDP(IPOIN)-S3D_FLUER(IPOIN)
          FLUX    = FLUX+DELTAF*VOLU2D%R(IPOIN)
          QERODE  = S3D_FLUER(IPOIN)*DT
!
          QSC  = S3D_CONC(IPOIN,1)*S3D_EPAICO(IPOIN)
          QSNC = S3D_CFDEP*S3D_EPAINCO(IPOIN)
          QS   = QSC + QSNC
!
!         check if we have eroded enough entire layers
!
          IF(QSC.LT.QERODEC) THEN
            S3D_EPAICO(IPOIN) = 0.D0
          ELSE
            QSC           = QSC - QERODEC
            S3D_EPAICO(IPOIN) = QSC/S3D_CONC(IPOIN,1)
          ENDIF
!
          IF(QSNC.LT.QERODENC) THEN
            S3D_EPAINCO(IPOIN) = 0.D0
          ELSE
            QSNC           = QSNC - QERODENC
            S3D_EPAINCO(IPOIN) = QSNC/S3D_CFDEP
          ENDIF
!
!         DEPOSITION IN THE TOP LAYER
!
          S3D_EPAICO(IPOIN)  = S3D_EPAICO(IPOIN)+
     &                         S3D_FLUDPC(IPOIN)*DT/S3D_CONC(IPOIN,1)
          S3D_EPAINCO(IPOIN) = S3D_EPAINCO(IPOIN)+
     &                         S3D_FLUDPNC(IPOIN)*DT/S3D_CFDEP
          S3D_EPAI(IPOIN,1)  = S3D_EPAICO(IPOIN) + S3D_EPAINCO(IPOIN)
!
!         UPDATES PERCENTAGES OF EACH CLASSE
!
          IF(S3D_EPAI(IPOIN, 1).GT.0.D0) THEN
            S3D_PVSCO(IPOIN)  = S3D_EPAICO(IPOIN)/S3D_EPAI(IPOIN,1)
            S3D_PVSNCO(IPOIN) = 1.D0-S3D_PVSCO(IPOIN)
          ELSE
            S3D_PVSCO(IPOIN)  = 0.D0
            S3D_PVSNCO(IPOIN) = 0.D0
          ENDIF
!
!         EVOLUTION OBTAINED FROM OLD AND NEW SEDIMENT HEIGHT
!
          S3D_ZF_S(IPOIN) = S3D_EPAI(IPOIN,1)-S3D_HDEP(IPOIN)
!
!         SEDIMENT HEIGHT UPDATED
!
          S3D_HDEP(IPOIN) = S3D_EPAI(IPOIN,1)
!
        ENDDO
!
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++
!
!     UPDATE THE CUMULATED BED EVOLUTION : S3D_ESOMT
!     BOTTOM ELEVATION : ZF
!
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++
!
        CALL OV('X=Y+Z   ', X=S3D_ESOMT, Y=S3D_ESOMT, Z=S3D_ZF_S,
     &          DIM1=NPOIN2)
        CALL OV('X=Y+Z   ', X=ZF, Y=ZR, Z=S3D_HDEP, DIM1=NPOIN2)
!
!***********************************************************
! PREVIOUS CASE NON MIXED
!************************************************************
!
      ELSE

!       EXPLICIT SCHEME (S3D_SETDEP=1) S3D_FLUDPCOMPUTED IN SET_DIF
!
!       OTHER SCHEMES : S3D_FLUDPBUILT HERE
!
        IF(S3D_SETDEP.NE.1) THEN
          IF(OPTBAN.EQ.1) THEN
            DO IPOIN=1,NPOIN2
!             correction for tidal flats: take the first point above crushed planes
!             IPBOT =0  :  no tidal flats  IPBOT = NPLAN-1 : dry element
              IF(IPBOT%I(IPOIN).NE.NPLAN-1) THEN
                S3D_FLUDP(IPOIN)=S3D_FLUDPT(IPOIN)*
     &          TA(IPBOT%I(IPOIN)*NPOIN2+IPOIN)
                S3D_FLUDP(IPOIN)=MAX(S3D_FLUDP(IPOIN),0.D0)
              ELSE
                S3D_FLUDP(IPOIN)=0.D0
              ENDIF
            ENDDO
          ELSE
            DO IPOIN=1,NPOIN2
              S3D_FLUDP(IPOIN)=S3D_FLUDPT(IPOIN)*TA(IPOIN)
!             S3D_FLUDPMUST BE POSITIVE, EVEN IF TA<0 DUE TO TRUNCATION ERRORS
!             PROBLEM SEEN WITH TA=-1.D-87 !!!!!
              S3D_FLUDP(IPOIN)=MAX(S3D_FLUDP(IPOIN),0.D0)
            ENDDO
          ENDIF
        ENDIF
!
!       BED EVOLUTION
!
        DO IPOIN=1,NPOIN2
!
          DELTAF=S3D_FLUDP(IPOIN)-S3D_FLUER(IPOIN)
          FLUX=FLUX+DELTAF*VOLU2D%R(IPOIN)
          TOTMASS=0.D0
          QERODE = S3D_FLUER(IPOIN)*DT
!
          DO IC=1,S3D_NCOUCH
!
            QS = S3D_CONC(IPOIN,IC)*S3D_EPAI(IPOIN,IC)
!
            TOTMASS = TOTMASS + QS
!           check if we have eroded enough entire layers
            IF(TOTMASS.LT.QERODE) THEN
              S3D_EPAI(IPOIN,IC) = 0.D0
            ELSE
!             we have got to the correct layer.
!             How much of it do we need to erode?
              QS = TOTMASS - QERODE
!             calculate new thickness
              S3D_EPAI(IPOIN,IC) = QS/S3D_CONC(IPOIN,IC)
!             jump out of layer loop
              EXIT
            ENDIF
!
          ENDDO
!
!         Then Deposition in Top layer
!
          S3D_EPAI(IPOIN,1)=S3D_EPAI(IPOIN,1)+
     &                      S3D_FLUDP(IPOIN)*DT/S3D_CONC(IPOIN,1)
!
!         COMPUTING THE NEW SEDIMENT BED THICKNESS
!
          SEDBED= 0.D0
          DO IC=1,S3D_NCOUCH
            SEDBED=SEDBED+S3D_EPAI(IPOIN,IC)
          ENDDO
!
!         EVOLUTION OBTAINED FROM OLD AND NEW SEDIMENT HEIGHT
!
          S3D_ZF_S(IPOIN)=SEDBED-S3D_HDEP(IPOIN)
!
!         SEDIMENT HEIGHT UPDATED
!
          S3D_HDEP(IPOIN) = SEDBED
!
        ENDDO
!
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++
!
!     UPDATE THE CUMULATED BED EVOLUTION : S3D_ESOMT
!     BOTTOM ELEVATION : ZF
!
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++
!
        CALL OV('X=Y+Z   ', X=S3D_ESOMT, Y=S3D_ESOMT, Z=S3D_ZF_S,
     &          DIM1=NPOIN2)
!
!       THIS WAY OF WRITING THE NEW ZF ENSURES
!       THAT ZF ABOVE ZR EVEN WITH TRUNCATION
!       ERRORS, IF S3D_HDEP>0, THIS IS IMPORTANT
!
        CALL OV('X=Y+Z   ', X=ZF, Y=ZR, Z=S3D_HDEP, DIM1=NPOIN2)
!
      ENDIF
!
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++
! TASSEMENT HERE
! ---> add the other models here
!
!!+++++++++++++++++++++++++++++++++++++++++++++++++++++++
! COMPUTES HERE DEPOSITED MASS
!
!     TOTAL DEPOSITED MASS --> S3D_MASDEP
!
!       fixed bug...add up the deposition flux for all partitions (S3D_MASDEP)
!       S3D_MASDEP= S3D_MASDEP+ FLUX*DT
        MASTMP = FLUX*DT
        IF(NCSIZE.GT.1) MASTMP=P_SUM(MASTMP)
        S3D_MASDEP= S3D_MASDEP+ MASTMP
!
!=======================================================================
!
      RETURN
      END
