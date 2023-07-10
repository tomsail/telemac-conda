!                   ********************************
                    SUBROUTINE CVSP_CHECK_MASS_BILAN
!                   ********************************
!
!
!***********************************************************************
! SISYPHE   V7P2                                   25/02/2019
!***********************************************************************
!
!brief  CHECKING MASS PER VOLUME AND CORRECTING
!
!
!history R.KOPMANN (BAW)
!+        2019
!+        V7P2
!+
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| VOLTOT             |--| CURRENT TOTAL VOLUME
!| VCUMUCL_ACT        |--| VOLUME PER CLASS OVER OPEN BOUNDARY
!| FLUXTCLA           |--| FREE FLUXES BY CLASS
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

      USE DECLARATIONS_SISYPHE
!
      USE DECLARATIONS_SPECIAL
      USE INTERFACE_PARALLEL, ONLY : P_DSUM

      IMPLICIT NONE

      INTEGER I,K,J,IFRLIQ,IPTFR
      DOUBLE PRECISION KORR, VCUMUCL_ACT,FLUXTCLA
      LOGICAL RET, CVSP_CHECK_F
!
! CALCULATING TOTAL VOLUME PER CLASS
      DO I=1,NSICLA
        VOLTOT(I) = 0.D0
        DO J=1,NPOIN
          DO K=1,PRO_MAX(J)-1
            VOLTOT(I) = VOLTOT(I) + (PRO_F(J,K,I)+PRO_F(J,K+1,I))/2.D0
     & *(PRO_D(J,K+1,I)-PRO_D(J,K,I))*VOLU2D%R(J)
          ENDDO
        END DO
      END DO
      IF(NCSIZE.GT.1) THEN
        DO I=1,NSICLA
          VOLTOT(I) = P_DSUM(VOLTOT(I))
        ENDDO
      ENDIF
!
      DO I = 1,NSICLA
!
!       COMPUTES THE FREE FLUXES BY CLASS
!
        FLUXTCLA=0.D0
        IF(NFRLIQ.GT.0.AND.CHARR) THEN
          IF(NPTFR.GT.0) THEN
            DO IPTFR=1,NPTFR
              IFRLIQ=NUMLIQ%I(IPTFR)
              IF(IFRLIQ.GT.0) THEN
                FLUXTCLA=FLUXTCLA+FLBCLA%ADR(I)%P%R(IPTFR)
              ENDIF
            ENDDO
          ENDIF
          IF(NCSIZE.GT.1) FLUXTCLA=P_DSUM(FLUXTCLA)
        ENDIF
!
! CALCULATING VOLUME OVER OPEN BOUNDARY
        VCUMUCL_ACT = - FLUXTCLA*DT/CSF_SABLE
!
! CORRECTING
        KORR = 0.D0
        IF(ABS(VOLINI(I)+VCUMUCL(I)+VCUMUCL_ACT+VOLNESTORCL(I)-
     &     VOLTOT(I)).GT.1.D-13) THEN
          KORR = VOLTOT(I) / (VOLINI(I)+VCUMUCL(I)+VCUMUCL_ACT
     &       +VOLNESTORCL(I))
        ENDIF
        IF(KORR.GT.0.D0) THEN
          DO J=1,NPOIN
            DO K = 1, PRO_MAX(J)
              PRO_F(J,K,I) = PRO_F(J,K,I)/KORR
            ENDDO
          ENDDO
        ENDIF
      END DO ! I=1,NSICLA
!
!     ADDITIONAL CHECK FOR PRO_F
      DO J=1,NPOIN
        DO K = 1, PRO_MAX(J)
          RET = CVSP_CHECK_F(J,K,'check_mass:   ')
        ENDDO
      END DO
!
      RETURN
      END SUBROUTINE
