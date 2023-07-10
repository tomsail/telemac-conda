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
      USE INTERFACE_PARALLEL, ONLY : P_MAX
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
!
      DOUBLE PRECISION, EXTERNAL :: CGL
      DOUBLE PRECISION, EXTERNAL :: QGL
!
!-----------------------------------------------------------------------
!
!     INITIALISATION OF YADEB
!
      IF(NFRLIQ.GE.1) THEN
        DO I=1,NFRLIQ
          YADEB(I)=0
        ENDDO
      ENDIF
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
      ! Presribed a input on boundary
      IF(LT.EQ.1) THEN
        DO K=1,NPTFR
          I=NBOR(K)
          IF(NCSIZE.GT.1) I = MESH%KNOLG%I(I)
          IF(I.EQ.1) Q2BOR%R(K) = 0.001
          IF(I.EQ.2) Q2BOR%R(K) = 0.001
          IF(I.EQ.3) Q2BOR%R(K) = 0.001
          IF(I.EQ.4) Q2BOR%R(K) = 0.001
          IF(I.EQ.5) Q2BOR%R(K) = 0.001
          IF(I.EQ.6) Q2BOR%R(K) = 0.001
          IF(I.EQ.7) Q2BOR%R(K) = 0.001
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
        ELSEIF(LIEBOR%I(K).EQ.KENT) THEN
!         WARNING !!!!!!!!!!!!!!    DYNAMITE !!!!!!!!!
!         FOR COMPATIBILITY OF OLD STUDIES, LIEBOR=KENT SUPERSEDES LIQBOR=KENT
!         IF BOTH ARE GIVEN AS KENT
          LIQBOR%I(K)= KSORT
        ENDIF
!
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
!
      IF(NFRLIQ.GT.0) THEN
        DO IFRLIQ=1,NFRLIQ
          IF(NCSIZE.GT.1) YADEB(IFRLIQ)=P_MAX(YADEB(IFRLIQ))
          IF(YADEB(IFRLIQ).EQ.1) THEN
!
!         READING BOUNDARY CONDITION FILE WITH SOLID DISCHARGE
!
            IF(CHARR) THEN
!           AVOID OVERRIDING WITH SUSPENDED SEDIMENT TRANSPORT
              IF(SIS_FILES(SISLIQ)%NAME(1:1).NE.' ') THEN
                SOLDIS(IFRLIQ)=QGL(IFRLIQ,AT)
              ENDIF
            ENDIF

            CALL DISIMP(SOLDIS(IFRLIQ),Q2BOR,NUMLIQ%I,IFRLIQ,NSOLDIS,
     &                  T5,T1,
!                             MASK OF LIQUID BOUNDARIES DONE IN SISYPHE
     &                  NPTFR,MASK%R,MESH)
!
            IF(NSICLA.GT.1) THEN
              DO I=1,NSICLA
                DO K=1,NPTFR
                  IF(NUMLIQ%I(K).EQ.IFRLIQ.AND.
     &               LIQBOR%I(K).EQ.KENT) THEN
                    QBOR%ADR(I)%P%R(K)=AVAIL(NBOR(K),1,I)*T1%R(K)
                  ENDIF
                ENDDO
              ENDDO
            ELSE
              DO K=1,NPTFR
                IF(NUMLIQ%I(K).EQ.IFRLIQ.AND.
     &             LIQBOR%I(K).EQ.KENT) THEN
                  QBOR%ADR(1)%P%R(K)=T1%R(K)
                ENDIF
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
