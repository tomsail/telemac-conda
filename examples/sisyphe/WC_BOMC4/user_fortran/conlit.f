!                   *****************
                    SUBROUTINE CONLIT
!                   *****************
!
     &(NBOR,AT)
!
!***********************************************************************
! SISYPHE   V7P3                                   17/03/2017
!***********************************************************************
!
!brief    ALLOWS TO IMPOSE TIME VARYING BOUNDARY CONDITIONS
!+               (CONSTANT VALUES CAN BE DIRECTLY IMPOSED IN CONDIM
!+                INPUT FILE).
!+
!+
!+            ALLOWS TO IMPOSE A SAND TRANSPORT RATE AT SOME
!+                BOUNDARY NODES (QBOR AND LIQBOR). IT IS THEN NECESSARY
!+                TO ALSO IMPOSE LIEBOR = KSORT AT THESE NODES !
!
!history  E. PELTIER; C. LENORMANT; J.-M. HERVOUET
!+        11/09/1995
!+
!+
!
!history  CV
!+        19/06/2008
!+        V5P9
!+   TAKES INTO ACCOUNT CBOR_VASE AND CBOR_SABLE
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
!history  P. TASSI & J-M HERVOUET (LNHE)
!+        24/07/2012
!+        V6P2
!+   Dealing with new key-word "prescribed solid discharges"
!
!history  P. TASSI
!+        09/07/2013
!+        V6P3
!+   Correction for multiples boundaries. Thanks to Dougal Clunie
!+   for pointing out this error.
!
!history  R. KOPMANN (BAW)
!+        13/07/2016
!+        V7P2
!+        Integrating liquid boundary file for QS
!
!history  M. SECHER AND P. TASSI (EDF)
!+        17/03/2017
!+        V7P3
!+        Add conditional for liquid boundary file for QS
!
!history  J,RIEHME (ADJOINTWARE)
!+        November 2016
!+        V7P2
!+   Replaced EXTERNAL statements to parallel functions / subroutines
!+   by the INTERFACE_PARALLEL
!
!history  F.CORDIER & P.TASSI (EDF-LNHE)
!+        17/09/2018
!+        V8P0
!+        Recirculation of sediment from downstream at t to upstream a t+1
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| NBOR           |-->| GLOBAL NUMBER OF BOUNDARY POINT
!| AT             |-->| TEMPS (s)
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_SISYPHE
      USE DECLARATIONS_TELEMAC
!
      USE DECLARATIONS_SPECIAL
      USE INTERFACE_PARALLEL, ONLY : P_MAX, P_SUM
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)          :: NBOR(NPTFR)
      DOUBLE PRECISION, INTENT(IN) :: AT
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER I,K,IFRLIQ,IRANK
      INTEGER YADEB(MAXFRO)
      INTEGER :: NODES_INFLOW
!
      DOUBLE PRECISION, EXTERNAL :: CGL
!

      DOUBLE PRECISION QOUT(NSICLA)
      INTEGER RECIRCULATE, J
!-----------------------------------------------------------------------
! *** CHANNEL RECIRCULATION ***
! INITIALISATION OF QOUT AND RECIRCULATE (1 = YES, 0 = NO)
      RECIRCULATE = 1
! GIVES HERE THE NUMBER OF NODES AT THE CHANNEL INFLOW
      NODES_INFLOW = 19
! *****************************
!
!     INITIALISATION OF YADEB
!
      IF(NFRLIQ.GE.1) THEN
        DO I=1,NFRLIQ
          YADEB(I)=0
        ENDDO
      ENDIF
!
!     WE COMPUTE QSOUT FOR EACH CLASS OF SEDIMENT
      IF(NSICLA.GT.1) THEN
!     TO BE MODIFIED IF MORE OR LESS THAN 5 CLASSES
          CALL OS('X=Y     ',X=T2,Y=FLBCLA%ADR(1)%P)
          CALL OS('X=Y     ',X=T3,Y=FLBCLA%ADR(2)%P)
          CALL OS('X=Y     ',X=T4,Y=FLBCLA%ADR(3)%P)
          CALL OS('X=Y     ',X=T5,Y=FLBCLA%ADR(4)%P)
          CALL OS('X=Y     ',X=T6,Y=FLBCLA%ADR(5)%P)
      ENDIF
!
      DO I=1, NSICLA
        QOUT(I)=0.D0
      ENDDO
!
!-----------------------------------------------------------------------
!
!     DISTRIBUTING THE (TOTAL) PRESCRIBED EVOLUTION TAKEN IN THE
!     BOUNDARY CONDITIONS FILE WITH RESPECT TO FRACTIONS IN THE
!     FIRST LAYER
!
      IF(NSICLA.GT.1) THEN
        DO I=NSICLA,1,-1
          DO K=1,NPTFR
            EBOR%ADR(I)%P%R(K)=AVAIL(NBOR(K),1,I)*EBOR%ADR(1)%P%R(K)
          ENDDO
        ENDDO
      ENDIF
!
!-----------------------------------------------------------------------
!
      DO  K=1,NPTFR
!
        I = NBOR(K)
!
!       HERE KADH (WALL WITH NO SLIP CONDITION) IS CHANGED INTO KLOG (WALL)
!
        IF(LIEBOR%I(K).EQ.KADH) THEN
          LIEBOR%I(K)= KLOG
        ENDIF
! COMPUTE THE SUM OF THE EXITING VOLUMES OF SEDIMENT AT EACH NODE FOR EACH SIZE FRACTION
        IF(LIQBOR%I(K).EQ.KSORT) THEN
          IF(AT.LE.2.0E-2) THEN ! ONLY FOR INITIALIZATION PURPOSES
            DO I=1, NSICLA
              QOUT(I)=0.D0
            ENDDO
          ELSE
! TO BE MODIFIED IF MORE OR LESS THAN 5 CLASSES
            QOUT(1)=QOUT(1)+T2%R(K)
            QOUT(2)=QOUT(2)+T3%R(K)
            QOUT(3)=QOUT(3)+T4%R(K)
            QOUT(4)=QOUT(4)+T5%R(K)
            QOUT(5)=QOUT(5)+T6%R(K)
          ENDIF
        ENDIF
!       CASES WHERE WE HAVE A PRESCRIBED DISCHARGE
!
        IF(LIQBOR%I(K).EQ.KENT) YADEB(NUMLIQ%I(K))=1
!
!       DIRICHLET CONDITIONS
!       EITHER ON EVOLUTION OR ON SOLID DISCHARGE
!
!       EXAMPLE 1: IMPOSED SOLID DISCHARGE - FREE BED EVOLUTION
!
!       QBOR%ADR(J)%P%R(K) IS THE SOLID DISCHARGE IMPOSED AT THE BOUNDARY
!       NODE K , CLASS OF SEDIMENT J, EXCLUDING VOIDS
!
!       LIEBOR%I(K)=KSORT
!       LIQBOR%I(K)=KENT
!
!       QBOR%ADR(1)%P%R(K)=1.D-4
!       QBOR%ADR(2)%P%R(K)=1.D-4 .....
!
!       EXAMPLE 2: IMPOSED BED EVOLUTON
!
!       LIEBOR%I(K)=KENT
!       (LIQBOR%I(K)=KSORT IS DONE IN SISYPHE.F)
!       IF(LIEBOR%I(K).EQ.KENT) THEN
!         EBOR%ADR(1)%P%R(K)=1.D-4
!         EBOR%ADR(2)%P%R(K)=1.D-4.....
!       ENDIF
!
      ENDDO
!
!     DEALING WITH PRESCRIBED SOLID DISCHARGES
!     COMMUNICATE J AND QOUT FROM ALL PROCESSORS
!
      DO I=1, NSICLA
        IF(NCSIZE.GT.1) THEN
          QOUT(I)=P_SUM(QOUT(I))
        ENDIF
      ENDDO
      IF(NCSIZE.GT.1) J=P_SUM(J)
!
      IF(NFRLIQ.GT.0) THEN
        DO IFRLIQ=1,NFRLIQ
          IF(NCSIZE.GT.1) YADEB(IFRLIQ)=P_MAX(YADEB(IFRLIQ))
          IF(YADEB(IFRLIQ).EQ.1) THEN
!
            IF(NSICLA.GT.1) THEN
              DO I=1,NSICLA
                DO K=1,NPTFR
                  IF(NUMLIQ%I(K).EQ.IFRLIQ.AND.
     &               LIQBOR%I(K).EQ.KENT) THEN
!
                    IF (RECIRCULATE.EQ.1.AND.LT.NE.1) THEN
                        QBOR%ADR(I)%P%R(K)=QOUT(I)/DBLE(NODES_INFLOW)
                    ELSE
                        QBOR%ADR(I)%P%R(K)=0.0D0
                    ENDIF
                    IF (QBOR%ADR(I)%P%R(K).LT.1.0D-12) THEN
                      QBOR%ADR(I)%P%R(K)=0.0D0
                    ENDIF
                  ENDIF
                ENDDO
              ENDDO
            ENDIF
          ENDIF
        ENDDO
      ENDIF
!
!-----------------------------------------------------------------------
!     LICBOR : BOUNDARY CONDITION FOR SEDIMENT CONCENTRATION
!-----------------------------------------------------------------------
!
      IF(SUSP) THEN
!
        DO K=1,NPTFR
!
!         SO FAR LICBOR=LIEBOR (WITH KADH CHANGED INTO KLOG, SEE ABOVE,
!                               BUT CAN BE CHANGED)
!
          LICBOR%I(K) = LIEBOR%I(K)
!
!         ENTRANCE : IMPOSED CONCENTRATION
!         -------------------------------
!
!         NOTE JMH: KSORT MUST BE TREATED ALSO BECAUSE SUBROUTINE DIFFIN
!                   MAY CHANGE A KSORT INTO KENT, DEPENDING OF FLOW
!
          IFRLIQ=NUMLIQ%I(K)
          IF(LIEBOR%I(K).EQ.KENT.OR.LIEBOR%I(K).EQ.KSORT) THEN
            DO I=1,NSICLA
              IRANK=I+(IFRLIQ-1)*NSICLA
              CBOR%ADR(I)%P%R(K) = CBOR_CLASSE(IRANK)
            ENDDO
          ENDIF
!
!         READING BOUNDARY CONDITION FILE
!
          IF(LICBOR%I(K).EQ.KENT.AND.
     &                         SIS_FILES(SISLIQ)%NAME(1:1).NE.' ') THEN
            IF(IFRLIQ.GT.0) THEN
              DO I=1,NSICLA
                CBOR%ADR(I)%P%R(K)=CGL(IFRLIQ,AT)/XMVS
              ENDDO
            ENDIF
          ENDIF
!
        ENDDO
!
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
