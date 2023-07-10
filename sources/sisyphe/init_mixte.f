!                   *********************
                    SUBROUTINE INIT_MIXTE
!                   *********************
!
     &(XMVS,NPOIN,AVAIL,NSICLA,ES,ES_SABLE, ES_VASE,ELAY,NOMBLAY,
     & CONC_VASE,MS_SABLE,MS_VASE,ZF,ZR,AVA0,CONC,DEBU,MIXTE)
!
!***********************************************************************
! SISYPHE   V6P2                                   21/07/2011
!***********************************************************************
!
!brief
!
!history
!+
!+        V6P0
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
!history  C.VILLARET (EDF-LNHE), P.TASSI (EDF-LNHE)
!+        19/07/2011
!+        V6P1
!+  Name of variables
!+
!history  C.VILLARET (EDF-LNHE)
!+        22/08/2012
!+        V6P2
!+  Changing the calling to init_compo_coh: the number of layers is fixed
!+  Testing SUM(layers) = ZF-ZR
!+  Compute the initial mass balance
!+
!history  PABLO SANTORO (IMFIA) AND PABLO TASSI (EDF R&D - LHSV)
!+        01/08/2015
!+        V7P1
!+  small correction for continuous computation
!
!history  J,RIEHME (ADJOINTWARE)
!+        November 2016
!+        V7P2
!+   Replaced EXTERNAL statements to parallel functions / subroutines
!+   by the INTERFACE_PARALLEL
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| AVA0           |-->| VOLUME PERCENT
!| AVAIL          |<->| VOLUME PERCENT OF EACH CLASS
!| CONC           |<->| CONC OF EACH BED LAYER (KG/M3)
!| CONC_VASE      |<->| MUD CONCENTRATION FOR EACH LAYER
!| DEBU           |-->| FLAG, FOR PREVIOUS SEDIMENTOLOGICAL FILE
!| ELAY           |<->| THICKNESS OF TOTAL LAYER
!| ES             |<->| LAYER THICKNESSES AS DOUBLE PRECISION
!| ES_SABLE       |<->| THICKNESS OF SAND LAYER (M)
!| ES_VASE        |<->| THICKNESS OF MUD LAYER (M)
!| MIXTE          |-->| SEDIMENT MIXTE (SABLE + VASE)
!| MS_SABLE       |<->| MASS OF SAND PER LAYER (KG/M2)
!| MS_VASE        |<->| MASS OF MUD PER LAYER (KG/M2)
!| NOMBLAY        |-->| NUMBER OF LAYERS FOR CONSOLIDATION
!| NPOIN          |-->| NUMBER OF POINTS
!| NSICLA         |-->| NUMBER OF SIZE CLASSES FOR BED MATERIALS
!| XMVS           |-->| WATER DENSITY
!| ZF             |-->| ELEVATION OF BOTTOM
!| ZR             |-->| NON ERODABLE BED
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE INTERFACE_SISYPHE, EX_INIT_MIXTE=> INIT_MIXTE
      USE DECLARATIONS_SISYPHE, ONLY :MASVT,MASV0,T1,BILMA,VOLU2D
      USE DECLARATIONS_SISYPHE, ONLY :MASST,MASS0,T2
!
      USE DECLARATIONS_SPECIAL
      USE INTERFACE_PARALLEL, ONLY : P_DSUM
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)              :: NPOIN,NSICLA,NOMBLAY
      DOUBLE PRECISION, INTENT(IN)     :: XMVS
      DOUBLE PRECISION, INTENT(INOUT)  :: AVAIL(NPOIN,NOMBLAY,NSICLA)
      DOUBLE PRECISION, INTENT(INOUT)  :: ES(NPOIN,NOMBLAY)
      DOUBLE PRECISION, INTENT(INOUT)  :: ELAY(NPOIN)
      DOUBLE PRECISION, INTENT(IN)     :: ZR(NPOIN),ZF(NPOIN)
      DOUBLE PRECISION,  INTENT(INOUT) :: MS_SABLE(NPOIN,NOMBLAY)
      DOUBLE PRECISION,  INTENT(INOUT) :: MS_VASE(NPOIN,NOMBLAY)
!
      DOUBLE PRECISION,  INTENT(INOUT) :: ES_SABLE(NPOIN,NOMBLAY)
      DOUBLE PRECISION,  INTENT(INOUT) :: ES_VASE(NPOIN,NOMBLAY)
!
      DOUBLE PRECISION, INTENT(IN)     :: CONC_VASE(NOMBLAY)
      DOUBLE PRECISION,  INTENT(INOUT) :: CONC(NPOIN,NOMBLAY)
      DOUBLE PRECISION, INTENT(IN)     :: AVA0(NSICLA)
      LOGICAL, INTENT (IN)             :: DEBU, MIXTE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
!     LOCAL VARIABLES
!
      INTEGER I,J,K,NK
      DOUBLE PRECISION HAUTSED
      DOUBLE PRECISION DIFF,EST
!
!-----------------------------------------------------------------------
!
!*******INITIAL SEDIMENT COMPOSITION IS IDENTICAL AT EACH NODE
! DEFAULT INITIALISATION: ALL LAYERS ARE EMPTY EXCEPT BED LAYER
! OTHERWISE SET THICKNESS OF THE MUD LAYERS IN EPAI_VASE(I= 1, NCOUCH_TASS-1)
!

!  INITIALISATION OF ES : THICKNESS OF EACH LAYERS
!  INIT_COMPO_COH : composition of the sediment bed : thickness of layers
!                  and concentrations The number of sediment bed layers is fixed

      IF(.NOT.DEBU) THEN
!
        CALL INIT_COMPO_COH(ES,CONC_VASE,CONC,NPOIN,
     &     NOMBLAY,NSICLA,AVAIL,AVA0)
!
!       Recalcul des epaisseurs pour satisfaire : Sum (ES)=ZF-ZR
!
        DO I=1,NPOIN
!
          ELAY(I)=ZF(I)-ZR(I)
!
!
!         THE HEIGHT OF SEDIMENT (SUM OF ES) MUST BE EQUAL TO ZF-ZR
!         IF SO, THE HEIGHT OF THE LAST LAYER IS REDUCED
!         IF THERE ARE LAYERS UNDER ZR, THEY ARE NOT TAKEN INTO ACCOUNT
!
          HAUTSED = 0.D0
!
          NK=NOMBLAY
          DO K=1,NOMBLAY
!
            IF(HAUTSED + ES(I,K) .GE. ELAY(I)) THEN
              ES(I,K) = ELAY(I) -  HAUTSED
              NK=K
              HAUTSED = HAUTSED + ES(I,K)
              GOTO 144
            ENDIF
            HAUTSED = HAUTSED + ES(I,K)
!
          ENDDO
!
144       CONTINUE
!
!         FOR CLEAN OUTPUTS
!
          IF(NK.LT.NOMBLAY) THEN
            DO K=NK+1,NOMBLAY
              ES(I,K) = 0.D0
            ENDDO
          ENDIF
!
!         THE THICKNESS OF THE LAST LAYER IS ENLARGED SO THAT
!         THE HEIGHT OF SEDIMENT (SUM OF ES) IS EQUAL TO ZF-ZR
!
          IF(HAUTSED.LT.ELAY(I)) THEN
            ES(I,NOMBLAY)=ES(I,NOMBLAY)+ELAY(I)-HAUTSED
          ENDIF
!
        ENDDO
!
      ELSE
!
!      En cas de suite de calcul
!      Check that sum of layers (simple precision) is equal to ZF-ZR
!
        DO I=1,NPOIN
!
          ELAY(I)=ZF(I)-ZR(I)
!
          EST=0.D0
!
!         IF(NOMBLAY.GT.1) THEN
!
          DO J=1,NOMBLAY
            EST=EST+ES(I,J)
            CONC(I,J) = CONC_VASE(J)
          ENDDO
!         ELSE
!           EST=ES(I,1)
!         ENDIF
!
          DIFF= ELAY(I) - EST
!
          IF(ABS(DIFF).GE.1.D-4) THEN
            WRITE(LU,*) 'ERROR IN INIT-MIXTE:'
            WRITE(LU,*) 'THE SUM OF THICKNESS OF BED LAYERS
     &     IS DIFFERENT FROM ERODIBLE BED THICKNESS'
            CALL PLANTE(1)
            STOP
          ELSE
            ES(I,NOMBLAY) = MAX(ES(I,NOMBLAY)+ DIFF,0.D0)
          ENDIF
!
        ENDDO
!
      ENDIF
!
! END LOOP  (initialization of layers)
!
! Check sum ELAY = ZF-ZR
!                = SUM (ES)
      DO I = 1, NPOIN
        EST=0.D0
        DO J= 1, NOMBLAY
          EST=EST+ES(I,J)
        ENDDO
        DIFF=ABS(EST-ELAY(I))
        IF(DIFF.GT.1.D-08) THEN
          WRITE(LU,*) 'ERREUR POINT I'
     &     , I, 'ELAY=',ELAY(I), 'EST=', EST
          CALL PLANTE(1)
          STOP
        ENDIF
      ENDDO
!

!  COMPUTING THE INITIAL MASSES OF MUD AND SAND
!
      DO I=1,NPOIN
        T1%R(I)=0.D0
        T2%R(I)=0.D0
        DO J=1,NOMBLAY
          IF(NSICLA.EQ.1) THEN
            ES_VASE(I,J) = ES(I,J)
            MS_VASE(I,J) = ES(I,J)*CONC(I,J)
          ELSE
! FOR MIXTE SEDIMENTS : (MUD, second class )
!....         FILLING VOIDS BETWEEN SAND GRAINS ....(XKV=1)
!
            ES_SABLE(I,J)=ES(I,J)*AVAIL(I,J,1)
            ES_VASE(I,J)= ES(I,J)*AVAIL(I,J,2)
!
            MS_VASE(I,J) = ES_VASE(I,J)*CONC(I,J)
            MS_SABLE(I,J)= ES_SABLE(I,J)*XMVS
          ENDIF
          T1%R(I)= T1%R(I)+MS_VASE(I,J)
          IF(MIXTE) T2%R(I)=T2%R(I) + MS_SABLE(I,J)
        ENDDO
      ENDDO
!
!
! FOR MASS BALANCE
!
      IF(BILMA) THEN
        MASV0=DOTS(T1,VOLU2D)
        IF(MIXTE) MASS0= DOTS(T2,VOLU2D)
        IF(NCSIZE.GT.1) THEN
          MASV0=P_DSUM(MASV0)
          IF(MIXTE) MASS0=P_DSUM(MASS0)
        ENDIF
!
        MASVT=MASV0
        IF(MIXTE) MASST=MASS0
        IF (.NOT.MIXTE) THEN
          WRITE(LU,2) MASV0
        ELSE
          WRITE(LU,20) MASV0, MASS0
        ENDIF
      ENDIF
!
002   FORMAT(1X,'INITIAL MASS OF THE MUD BED: ', G20.11, ' KG')
020   FORMAT(1X,'INITIAL MASS OF THE MUD BED: ', G20.11, ' KG',
     &     /,1X,'INITIAL MASS OF THE SAND BED: ', G20.11, ' KG')

!
!-----------------------------------------------------------------------
!
      RETURN
      END
