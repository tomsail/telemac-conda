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
      USE INTERFACE_PARALLEL, ONLY : P_MAX
      IMPLICIT NONE
!
      INTEGER I,K,IFRLIQ,ISAND
      INTEGER YADEB(MAXFRO)
!
      DOUBLE PRECISION, EXTERNAL :: QGL_GAIA
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
!     FIRST LAYER IF THERE ARE SANDS
!     IF THERE ARE MUDS, BEDLOAD IS NOT ALLOWED
!
      DO ISAND=1, NSAND
        DO K=1,NPTFR
          EBOR%ADR(NUM_ISAND_ICLA(ISAND))%P%R(K)=
     &      RATIO_SAND(ISAND,1,MESH%NBOR%I(K)) *
     &      EBOR%ADR(1)%P%R(K)
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
!       (LIQBOR%I(K)=KSORT IS DONE IN GAIA.F)
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
!     AVOID OVERRIDING WITH SUSPENDED SEDIMENT TRANSPORT
          IF(GAI_FILES(GAILIQ)%NAME(1:1).NE.' ') THEN
                SOLDIS(IFRLIQ)=QGL_GAIA(IFRLIQ,AT0)
          ENDIF
      ENDIF

            CALL DISIMP_GAIA(SOLDIS(IFRLIQ),Q2BOR,NUMLIQ%I,IFRLIQ,
     &                  NSOLDIS,T5,T1,
!                             MASK OF LIQUID BOUNDARIES DONE IN GAIA
     &                  NPTFR,MASK%R,MESH)
!
            IF(NSICLA.GT.1) THEN
              DO I=1,NSICLA
                ISAND=NUM_ICLA_ISAND(I)
                IF(.NOT.SEDCO(I)) THEN
                  DO K=1,NPTFR
                    IF(NUMLIQ%I(K).EQ.IFRLIQ.AND.
     &                 LIQBOR%I(K).EQ.KENT) THEN
! --TEMPORARY!!-- RATIO_SAND NEED TO BE REPLACED BY A GENERAL VARIABLE
! (NOT RELATED TO ONLY SANDS)
                      IF(NPROP.EQ.0) THEN
                  QBOR%ADR(I)%P%R(K)= RATIO_SAND(ISAND,1,MESH%NBOR%I(K))
     &                                * T1%R(K)
                      ELSE
                  QBOR%ADR(I)%P%R(K)= RATIO_DEBIMP(ISAND)
     &                                * T1%R(K)
                      ENDIF
                    ENDIF
                  ENDDO
                ENDIF
              ENDDO
            ELSE
              DO K=1,NPTFR
                IF(NUMLIQ%I(K).EQ.IFRLIQ.AND.
     &             LIQBOR%I(K).EQ.KENT) THEN
                  IF(.NOT.SEDCO(1))
     &               QBOR%ADR(1)%P%R(K)=T1%R(K)
                ENDIF
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
