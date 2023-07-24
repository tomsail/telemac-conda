!                   **************************
                    SUBROUTINE COMPUTE_BC_SEDI
!                   **************************
!
!***********************************************************************
! TELEMAC3D   V8P0                                  02/08/2018
!***********************************************************************
!
!brief    COMPUTES THE BOUNDARY CONDITIONS FOR SUSPENDED SEDIMENTS
!
!history  R.WALTHER & S.PAVAN
!+        02/08/2018
!+        V8P0
!+   Creation of the subroutine
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
!
      USE DECLARATIONS_SPECIAL
      USE DECLARATIONS_TELEMAC3D
!     WARNING: OPTBAN EXIST FOR T3D AND GAIA
      USE DECLARATIONS_GAIA, ONLY:FLUDPT,NSUSP_TEL,NUM_ISUSP_ICLA,
     &            FLUER,SETDEP
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER ITRAC,IPOIN,I3D
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
!     INITIALISATION
!
      CALL OS('X=0     ',X=FLUDPT)
!
      DO ITRAC = IND_SED,IND_SED+NSUSP_TEL-1
        ISUSP=ITRAC-IND_SED+1
          IF(SIGMAG.OR.OPTBAN.EQ.1) THEN
            DO IPOIN=1,NPOIN2
              IF(IPBOT%I(IPOIN).NE.NPLAN-1) THEN
!               DEPOSITION ON THE FIRST FREE PLANE WITH LOCAL VELOCITY
                I3D=IPOIN+IPBOT%I(IPOIN)*NPOIN2
                FLUDPT%ADR(NUM_ISUSP_ICLA(ISUSP))%P%R(IPOIN) =
     &             WCHU%ADR(ITRAC)%P%R(I3D)
!
!             FLUER RECEIVED FROM GAIA
              ELSE
                FLUDPT%ADR(NUM_ISUSP_ICLA(ISUSP))%P%R(IPOIN) = 0.D0
                FLUER%ADR(NUM_ISUSP_ICLA(ISUSP))%P%R(IPOIN) = 0.D0
              ENDIF
            ENDDO
          ELSE
            DO IPOIN=1,NPOIN2
              FLUDPT%ADR(NUM_ISUSP_ICLA(ISUSP))%P%R(IPOIN) =
     &           WCHU%ADR(ITRAC)%P%R(IPOIN)
            ENDDO
          ENDIF
!         ONLY CALCULATE ATABOF AND BTABOF IF NOT USING SETDEP=1
          IF(SETDEP.NE.1) THEN
            DO IPOIN=1,NPOIN2
              ATABOF%ADR(ITRAC)%P%R(IPOIN) =
     &             -FLUDPT%ADR(NUM_ISUSP_ICLA(ISUSP))%P%R(IPOIN)
              BTABOF%ADR(ITRAC)%P%R(IPOIN) =
     &             FLUER%ADR(NUM_ISUSP_ICLA(ISUSP))%P%R(IPOIN)
            ENDDO
            ATABOF%ADR(ITRAC)%P%TYPR='Q'
            BTABOF%ADR(ITRAC)%P%TYPR='Q'
          ENDIF
      ENDDO
!
!-----------------------------------------------------------------------
!
      RETURN
      END
