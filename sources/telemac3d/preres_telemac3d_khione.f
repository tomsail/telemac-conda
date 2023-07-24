!                   **********************************
                    SUBROUTINE PRERES_TELEMAC3D_KHIONE
!                   **********************************
!
!***********************************************************************
! TELEMAC3D   V8P3
!***********************************************************************
!
!brief    PREPARES THE VARIABLES WHICH WILL BE WRITTEN TO
!+         THE RESULTS FILE OR TO THE LISTING.
!
!history  F TACCONE (LNHE) Creation
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_KHIONE
      USE DECLARATIONS_TELEMAC3D, ONLY:TA,Z,OPTBAN,T3_01,T3_02,T3_03,
     &                                 NPOIN2,NPLAN,LT
      IMPLICIT NONE
      LOGICAL IMP,LEO
      INTEGER I,J
      DOUBLE PRECISION CTOT_TMP(NPOIN2)

      IMP = .FALSE.
      LEO = .FALSE.
      IF( LT.EQ.(LT/LISPRD)*LISPRD ) IMP = .TRUE.
      IF( LT.EQ.(LT/LEOPRD)*LEOPRD ) LEO = .TRUE.
!
!     NO PRINTOUT REQUIRED (LISTING OR RESULT FILE): EXITS
      IF( .NOT.(LEO.OR.IMP) ) RETURN
!
      IF( (LEO.AND.SORLEO(22)).OR.(IMP.AND.SORIMP(22)) ) THEN
        CALL OS('X=0     ', X=CTOT)
        DO J=1,NC_FRA
          CALL VERMOY(CTOT_TMP,CTOT_TMP,
     &                TA%ADR(IND_FRA+J-1)%P%R,
     &                TA%ADR(IND_FRA+J-1)%P%R,1,Z,
     &                T3_01%R,T3_02%R,T3_03%R,1,NPLAN,NPOIN2,
     &                NPLAN,OPTBAN)
          DO I=1,NPOIN2
            CTOT%R(I) = CTOT%R(I) + CTOT_TMP(I)
          ENDDO
        ENDDO
      ENDIF
!
      IF( (LEO.AND.SORLEO(21)).OR.(IMP.AND.SORIMP(21)) ) THEN
        CALL OS('X=0     ', X=NTOT)
        DO J=1,NC_FRA
          CALL VERMOY(CTOT_TMP,CTOT_TMP,
     &                TA%ADR(IND_FRA+J-1)%P%R,
     &                TA%ADR(IND_FRA+J-1)%P%R,1,Z,
     &                T3_01%R,T3_02%R,T3_03%R,1,NPLAN,NPOIN2,
     &                NPLAN,OPTBAN)
          DO I=1,NPOIN2
            NTOT%R(I) = NTOT%R(I) + CTOT_TMP(I) / VK_FRZL(J)
          ENDDO
        ENDDO
      ENDIF
!
      DO I=1,NC_FRA
        IF( (LEO.AND.SORLEO(24+I)).OR.(IMP.AND.SORIMP(24+I)) ) THEN
          CALL VERMOY(FRZL%ADR(I)%P%R,FRZL%ADR(I)%P%R,
     &                TA%ADR(IND_FRA+I-1)%P%R,
     &                TA%ADR(IND_FRA+I-1)%P%R,1,Z,
     &                T3_01%R,T3_02%R,T3_03%R,1,NPLAN,NPOIN2,
     &                NPLAN,OPTBAN)
        ENDIF
      ENDDO
!
      DO I=1,NC_FRA
        IF( (LEO.AND.SORLEO(24+I+NC_FRA))
     &       .OR.(IMP.AND.SORIMP(24+I+NC_FRA)) ) THEN
          CALL VERMOY(FRZL%ADR(I)%P%R,FRZL%ADR(I)%P%R,
     &                TA%ADR(IND_FRA+I-1)%P%R,
     &                TA%ADR(IND_FRA+I-1)%P%R,1,Z,
     &                T3_01%R,T3_02%R,T3_03%R,1,NPLAN,NPOIN2,
     &                NPLAN,OPTBAN)
          DO J = 1,NPOIN2
            NBP%ADR(I)%P%R(J) = FRZL%ADR(I)%P%R(J) / VK_FRZL(I)
          ENDDO
        ENDIF
      ENDDO
!
      IF( (LEO.AND.SORLEO(24+4*NC_FRA+1))
     &     .OR.(IMP.AND.SORIMP(24+4*NC_FRA+1)) ) THEN
          CALL VERMOY(TEMP%R,TEMP%R,
     &                TA%ADR(IND_T)%P%R,
     &                TA%ADR(IND_T)%P%R,1,Z,
     &                T3_01%R,T3_02%R,T3_03%R,1,NPLAN,NPOIN2,
     &                NPLAN,OPTBAN)
      ENDIF
!
      IF(SALINITY) THEN
        IF( (LEO.AND.SORLEO(24+4*NC_FRA+3))
     &       .OR.(IMP.AND.SORIMP(24+4*NC_FRA+3)) ) THEN
          CALL VERMOY(SAL%R,SAL%R,
     &                TA%ADR(IND_S)%P%R,
     &                TA%ADR(IND_S)%P%R,1,Z,
     &                T3_01%R,T3_02%R,T3_03%R,1,NPLAN,NPOIN2,
     &                NPLAN,OPTBAN)
        ENDIF
      ENDIF

      RETURN
      END
