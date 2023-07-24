!                     ************************
                      SUBROUTINE PRERES_KHIONE
!                     ************************
!
     &(NPOIN,LT,TELSOR,TN,NPOIN3,NPLAN)
!
!***********************************************************************
! KHIONE   V7P3
!***********************************************************************
!
!brief    PREPARES THE VARIABLES WHICH WILL BE WRITTEN TO
!+         THE RESULTS FILE OR TO THE LISTING.
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| LT             |-->| CURRENT NUMBER OF OF TIME STEP
!| L3D            |-->| TRUE IF CALLED BY T3D
!| NPOIN          |-->| NUMBER OF NODES
!| TELSOR         |-->| OUTPUT OF TELEMAC2D
!| TN             |-->| TELEMAC2D TRACER VALUES
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_KHIONE
!
      USE DECLARATIONS_SPECIAL
      USE INTERFACE_KHIONE, EX_PRERES_KHIONE => PRERES_KHIONE
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER,               INTENT(IN) :: LT,NPOIN
      TYPE(BIEF_OBJ),        INTENT(IN) :: TELSOR,TN
      INTEGER, OPTIONAL,     INTENT(IN) :: NPOIN3,NPLAN
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER I,J,II,IPLAN
      LOGICAL IMP,LEO
!
!-----------------------------------------------------------------------
!
      IMP = .FALSE.
      LEO = .FALSE.
      IF( LT.EQ.(LT/LISPRD)*LISPRD ) IMP = .TRUE.
      IF( LT.EQ.(LT/LEOPRD)*LEOPRD ) LEO = .TRUE.
!
!     NO PRINTOUT REQUIRED (LISTING OR RESULT FILE): EXITS
      IF( .NOT.(LEO.OR.IMP) ) RETURN
!
!-----------------------------------------------------------------------
!
!     FOR EVERY TIME STEP
!
!=======================================================================
!     COMPUTING TOTAL ICE THICKNESS
!=======================================================================
!
      IF(DYN_ICOVER) THEN
!       TOTAL ti = STATIC ICE ti + DYN. ICE ti
        CALL OS( 'X=Y+Z   ', X=THIFEM,Y=THIFEMS,Z=TN%ADR(IND_DTI)%P)
      ELSE
        CALL OS( 'X=Y     ', X=THIFEM,Y=THIFEMS)
      ENDIF
!
!=======================================================================
!     COMPUTES THE EQUIVALENT SURFACE ELEVATION
!=======================================================================
!
      IF( (LEO.AND.SORLEO(16)).OR.(IMP.AND.SORIMP(16)) ) THEN
!       TELSOR%ADR(4)%P: H
!       TELSOR%ADR(6)%P: ZF
        IF (IPRES.EQ.1.OR.IPRES.EQ.2) THEN
          DO I = 1,NPOIN
            T3%R(I) = TELSOR%ADR(4)%P%R(I) + TELSOR%ADR(6)%P%R(I) +
     &        RHO_ICE/RO0 * THIFEM%R(I)
          ENDDO
        ELSE IF (IPRES.EQ.0) THEN
          DO I = 1,NPOIN
            T3%R(I) = TELSOR%ADR(4)%P%R(I) + TELSOR%ADR(6)%P%R(I)
          ENDDO
        ENDIF
      ENDIF
!
!=======================================================================
!     COMPUTES THE TOP OF THE ICE COVER
!=======================================================================
!
      IF( (LEO.AND.SORLEO(17)).OR.(IMP.AND.SORIMP(17)) ) THEN
!       TELSOR%ADR(4)%P: H
!       TELSOR%ADR(6)%P: ZF
        IF (IPRES.EQ.1.OR.IPRES.EQ.2) THEN
          DO I = 1,NPOIN
            T4%R(I) = TELSOR%ADR(4)%P%R(I) + TELSOR%ADR(6)%P%R(I) +
     &        THIFEM%R(I)
          ENDDO
        ELSE IF (IPRES.EQ.0) THEN
          DO I = 1,NPOIN
            T4%R(I) = TELSOR%ADR(4)%P%R(I) + TELSOR%ADR(6)%P%R(I) +
     &        (1 - RHO_ICE/RO0)*THIFEM%R(I)
          ENDDO
        ENDIF
      ENDIF
!
!=======================================================================
!     COMPUTES THE BOTTOM OF THE ICE COVER, ALSO THE SURFACE ELEVATION
!=======================================================================
!
      IF( (LEO.AND.SORLEO(18)).OR.(IMP.AND.SORIMP(18)) ) THEN
!       TELSOR%ADR(4)%P: H
!       TELSOR%ADR(6)%P: ZF
        IF (IPRES.EQ.1.OR.IPRES.EQ.2) THEN
          CALL OS( 'X=Y+Z   ', X=T5,Y=TELSOR%ADR(4)%P,Z=TELSOR%ADR(6)%P)
        ELSE IF (IPRES.EQ.0) THEN
          DO I = 1,NPOIN
            T5%R(I) = TELSOR%ADR(4)%P%R(I) + TELSOR%ADR(6)%P%R(I) -
     &        RHO_ICE/RO0 * THIFEM%R(I)
          ENDDO
        ENDIF
      ENDIF
!
!=======================================================================
!     CONVERTER ICE CHARACTERISTICS (PRIME NUMBER) INTO ITS REAL PART
!=======================================================================
!
      IF( (LEO.AND.SORLEO(20)).OR.(IMP.AND.SORIMP(20)) ) THEN
        DO I = 1,NPOIN
          ICETYPE%R(I) = 1.D0 * ICETYPE%I(I)
        ENDDO
      ENDIF
!
!=======================================================================
!     VARIABLES OF THERMAL BUDGET FROM TRACER
!=======================================================================
!
      IF(PRESENT(NPOIN3)) THEN
!
!       SURFACE
!
        IF( (LEO.AND.SORLEO(24)).OR.(IMP.AND.SORIMP(24)) ) THEN
          DO I = 1,NPOIN
            II = NPOIN3 - NPOIN + I
            CTOTS%R(I) = TN%ADR(IND_FRA)%P%R(II)
          ENDDO
          IF(NC_FRA.GT.1) THEN
            DO I = 2,NC_FRA
              DO J = 1,NPOIN
                II = NPOIN3 - NPOIN + J
                CTOTS%R(J) = CTOTS%R(J) + TN%ADR(IND_FRA+I-1)%P%R(II)
              ENDDO
            ENDDO
          ENDIF
        ENDIF
!
        IF( (LEO.AND.SORLEO(23)).OR.(IMP.AND.SORIMP(23)) ) THEN
          DO I = 1,NPOIN
            II = NPOIN3 - NPOIN + I
            NTOT%R(I) = TN%ADR(IND_FRA)%P%R(II) / VK_FRZL(1)
          ENDDO
          IF(NC_FRA.GT.1) THEN
            DO I = 2,NC_FRA
              DO J = 1,NPOIN
                II = NPOIN3 - NPOIN + J
                NTOTS%R(J) = NTOTS%R(J) + TN%ADR(IND_FRA+I-1)%P%R(II) /
     &                      VK_FRZL(I)
              ENDDO
            ENDDO
          ENDIF
        ENDIF
!
        DO I=1,NC_FRA
          IF( (LEO.AND.SORLEO(24+I+2*NC_FRA)).OR.
     &        (IMP.AND.SORIMP(24+I+2*NC_FRA)) ) THEN
            DO J = 1, NPOIN
              II = NPOIN3 - NPOIN + J
              FRZLS%ADR(I)%P%R(J) = TN%ADR(IND_FRA+I-1)%P%R(II)
            ENDDO
          ENDIF
        ENDDO
!
        DO I=1,NC_FRA
          IF( (LEO.AND.SORLEO(24+I+3*NC_FRA))
     &         .OR.(IMP.AND.SORIMP(24+I+3*NC_FRA)) ) THEN
            DO J = 1,NPOIN
              II = NPOIN3 - NPOIN + J
              NBPS%ADR(I)%P%R(J) = TN%ADR(IND_FRA+I-1)%P%R(II) /
     &                            VK_FRZL(I)
            ENDDO
          ENDIF
        ENDDO
!
        IF( (LEO.AND.SORLEO(24+4*NC_FRA+2))
     &       .OR.(IMP.AND.SORIMP(24+4*NC_FRA+2)) ) THEN
          DO I = 1,NPOIN
            II = NPOIN3 - NPOIN + I
            TEMPS%R(I) = TN%ADR(IND_T)%P%R(II)
          ENDDO
        ENDIF
!
        IF(SALINITY) THEN
          IF( (LEO.AND.SORLEO(24+4*NC_FRA+4))
     &         .OR.(IMP.AND.SORIMP(24+2*NC_FRA+4)) ) THEN
            DO I = 1,NPOIN
              II = NPOIN3 - NPOIN + I
              SALS%R(I) = TN%ADR(IND_S)%P%R(II)
            ENDDO
          ENDIF
        ENDIF
!
      ELSE !T2D
!
        IF( (LEO.AND.SORLEO(22)).OR.(IMP.AND.SORIMP(22)) ) THEN
          CALL OS('X=Y     ', X=CTOT, Y=TN%ADR(IND_FRA)%P)
          IF(NC_FRA.GT.1) THEN
            DO I = 2,NC_FRA
              CALL OS('X=X+Y   ', X=CTOT, Y=TN%ADR(IND_FRA+I-1)%P)
            ENDDO
          ENDIF
        ENDIF
!
        IF( (LEO.AND.SORLEO(21)).OR.(IMP.AND.SORIMP(21)) ) THEN
          CALL OS('X=CY    ', X=NTOT, Y=TN%ADR(IND_FRA)%P,
     &                        C=1.D0/VK_FRZL(1))
          IF(NC_FRA.GT.1) THEN
            DO I = 2,NC_FRA
              CALL OS('X=X+CY  ', X=NTOT, Y=TN%ADR(IND_FRA+I-1)%P,
     &                            C=1.D0/VK_FRZL(I))
            ENDDO
          ENDIF
        ENDIF
!
        IF( (LEO.AND.SORLEO(24)).OR.(IMP.AND.SORIMP(24)) ) THEN
          CALL OS('X=Y     ', X=CTOTS, Y=CTOT)
        ENDIF
!
        IF( (LEO.AND.SORLEO(23)).OR.(IMP.AND.SORIMP(23)) ) THEN
          CALL OS('X=Y     ', X=NTOTS, Y=NTOT)
        ENDIF
!
        DO I=1,NC_FRA
          IF( (LEO.AND.SORLEO(24+I)).OR.(IMP.AND.SORIMP(24+I)) ) THEN
            CALL OS('X=Y     ', X=FRZL%ADR(I)%P,
     &                          Y=TN%ADR(IND_FRA+I-1)%P)
          ENDIF
          IF( (LEO.AND.SORLEO(24+I+2*NC_FRA)).OR.
     &        (IMP.AND.SORIMP(24+I+2*NC_FRA)) ) THEN
            CALL OS('X=Y     ', X=FRZLS%ADR(I)%P, Y=FRZL%ADR(I)%P)
          ENDIF
        ENDDO
!
        DO I=1,NC_FRA
          IF( (LEO.AND.SORLEO(24+I+NC_FRA))
     &         .OR.(IMP.AND.SORIMP(24+I+NC_FRA)) ) THEN
            CALL OS('X=CY    ', X=NBP%ADR(I)%P, Y=TN%ADR(IND_FRA+I-1)%P,
     &                          C=1.D0/VK_FRZL(I))
          ENDIF
          IF( (LEO.AND.SORLEO(24+I+3*NC_FRA))
     &         .OR.(IMP.AND.SORIMP(24+I+3*NC_FRA)) ) THEN
            CALL OS('X=Y     ', X=NBPS%ADR(I)%P, Y=NBP%ADR(I)%P)
          ENDIF
        ENDDO
!
        IF( (LEO.AND.SORLEO(24+4*NC_FRA+1))
     &       .OR.(IMP.AND.SORIMP(24+4*NC_FRA+1)) ) THEN
          CALL OS('X=Y     ', X=TEMP, Y=TN%ADR(IND_T)%P)
        ENDIF
!
        IF( (LEO.AND.SORLEO(24+4*NC_FRA+2))
     &       .OR.(IMP.AND.SORIMP(24+4*NC_FRA+2)) ) THEN
          CALL OS('X=Y     ', X=TEMPS, Y=TEMP)
        ENDIF
!
        IF(SALINITY) THEN
          IF( (LEO.AND.SORLEO(24+4*NC_FRA+3))
     &         .OR.(IMP.AND.SORIMP(24+4*NC_FRA+3)) ) THEN
            CALL OS('X=Y     ', X=SAL, Y=TN%ADR(IND_S)%P)
          ENDIF
          IF( (LEO.AND.SORLEO(24+4*NC_FRA+4))
     &         .OR.(IMP.AND.SORIMP(24+4*NC_FRA+4)) ) THEN
            CALL OS('X=Y     ', X=SALS, Y=SAL)
          ENDIF
        ENDIF
!
      ENDIF
!
      IF(SALINITY) THEN
        I=2
      ELSE
        I=0
      ENDIF

      IF(DYN_ICOVER) THEN
        IF( (LEO.AND.SORLEO(24+4*NC_FRA+3+I))
     &       .OR.(IMP.AND.SORIMP(24+4*NC_FRA+3+I)) ) THEN
          CALL OS('X=Y     ', X=DYNCOVC, Y=TN%ADR(IND_DCI)%P)
        ENDIF
!
        IF( (LEO.AND.SORLEO(24+4*NC_FRA+4+I))
     &       .OR.(IMP.AND.SORIMP(24+4*NC_FRA+4+I)) ) THEN
          CALL OS('X=Y     ', X=DYNCOVT, Y=TN%ADR(IND_DTI)%P)
        ENDIF
!
      ENDIF !WHICH CALLING TELEMAC
!
!=======================================================================
!
!-----------------------------------------------------------------------
!
      RETURN
      END
