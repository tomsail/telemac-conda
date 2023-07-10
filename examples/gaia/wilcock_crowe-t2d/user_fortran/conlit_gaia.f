!                   **********************
                    SUBROUTINE CONLIT_GAIA
!                   **********************
!
!***********************************************************************
! GAIA
!***********************************************************************
!
!>@brief Allows to impose time varying boundary conditions
!!
!!       Allows to impose a sand transport rate at some
!!       boundary nodes (qbor and liqbor). It is then necessary
!!       to also impose liebor = ksort at these nodes !
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      USE BIEF
      USE DECLARATIONS_GAIA
      USE DECLARATIONS_TELEMAC
!
      USE DECLARATIONS_SPECIAL
      USE INTERFACE_PARALLEL, ONLY : P_MAX, P_SUM, P_SUM
      IMPLICIT NONE
!
      INTEGER I,K,IFRLIQ,ISAND
      INTEGER YADEB(MAXFRO)
      INTEGER :: NODES_INFLOW
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
!     FIRST LAYER IF THERE ARE SANDS
!     IF THERE ARE MUDS, BEDLOAD IS NOT ALLOWED
!
      DO ISAND=1, NSAND
        DO K=1,NPTFR
          EBOR%ADR(NUM_ISAND_ICLA(ISAND))%P%R(K)=
     &      RATIO_SAND(ISAND,1,MESH%NBOR%I(K)) *
     &      EBOR%ADR(NUM_ISAND_ICLA(1))%P%R(K)
        ENDDO
      ENDDO
!
!-----------------------------------------------------------------------
!
      DO  K=1,NPTFR
!
        I = MESH%NBOR%I(K)
!
!       HERE KADH (WALL WITH NO SLIP CONDITION) IS CHANGED INTO KLOG (WALL)
!
        IF(LIEBOR%I(K).EQ.KADH) THEN
          LIEBOR%I(K)= KLOG
        ENDIF
! COMPUTE THE SUM OF THE EXITING VOLUMES OF SEDIMENT AT EACH NODE FOR EACH SIZE FRACTION
        IF(LIQBOR%I(K).EQ.KSORT) THEN
          IF(AT0.LE.2.0E-2) THEN ! ONLY FOR INITIALIZATION PURPOSES
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
!       (LIQBOR%I(K)=KSORT IS DONE IN GAIA.F)
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
!
      RETURN
      END
