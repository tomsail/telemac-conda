!                   *************************
                    SUBROUTINE CVSP_INTERFACE
!                   *************************
!
!***********************************************************************
! GAIA
!***********************************************************************
!
!>@brief interface to cvsm model
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


      USE INTERFACE_GAIA_BEDLOAD
      USE DECLARATIONS_GAIA
      USE INTERFACE_PARALLEL, ONLY:  P_DSUM

      IMPLICIT NONE
      INTEGER I,ICLA,ILAYER

! CVSM NEEDS ZFCL, AVAIL, ZF, ZR
! CONVERSION EVCL_MB TO ZFCL, MASS_SAND TO AVAIL, SUM UP ZFCL TO ZF
          DO ICLA = 1,NSICLA  
            DO I = 1,NPOIN
              ZFCL_C%ADR(ICLA)%P%R(I)  
     &           = EVCL_MB%ADR(ICLA)%P%R(I) * MPA2T(ICLA) 
              IF(ES(I,1).GT.0.D0) THEN
                AVAIL(I,1,ICLA)                             
     &           = MASS_SAND(ICLA,1,I) * MPA2T(ICLA) / ES(I,1) 
              ELSE
                AVAIL(I,1,ICLA) = 0.D0
              ENDIF
              ZF%R(I) = ZF%R(I)+ZFCL_C%ADR(ICLA)%P%R(I)  ! new ZF
            ENDDO
          ENDDO
!
! CALL MAIN ROUTINE CVSM
          IF(DEBUG.GT.0) WRITE(LU,*)'CVSP_MAIN_GAIA'
          CALL CVSP_MAIN_GAIA(ZFCL_C,ZF,NSICLA,NPOIN)
          IF(DEBUG.GT.0) WRITE(LU,*)'CVSP_MAIN_GAIA'
!
! CVSM CHANGES ES, AVAIL
! CONVERSION AVAIL TO MASS_SAND AND SUM UP TO TOTAL MASS SAND
        MASS_SAND_TOT(:,:) = 0.D0

        DO ILAYER=1,NOMBLAY
          DO ICLA=1,NSICLA
            DO I=1,NPOIN
              MASS_SAND(ICLA,ILAYER,I) = AVAIL(I,ILAYER,ICLA) / 
     &            MPA2T(ICLA)*ES(I,ILAYER)
              MASS_SAND_TOT(ILAYER,I) = MASS_SAND_TOT(ILAYER,I)
     &        + MASS_SAND(ICLA,ILAYER,I)
            END DO
          END DO
        END DO
! COMPUTING RATIO_SAND
        DO I= 1,NPOIN
          DO ILAYER = 1,NOMBLAY
            DO ICLA = 1,NSICLA
              RATIO_SAND(ICLA,ILAYER,I) = 
     &           MIN(1.D0,MASS_SAND(ICLA,ILAYER,I) 
     &            / MASS_SAND_TOT(ILAYER,I))
            ENDDO
          ENDDO
        ENDDO

!     COMPUTE ZF FROM ES
      DO I = 1,NPOIN
        ZF%R(I) = ZR%R(I)
        DO ILAYER = 1,NOMBLAY
          ZF%R(I) = ZF%R(I) + ES(I,ILAYER)
        ENDDO
      ENDDO
!     COMPUTES MASSTOT AT EVERY TIME STEP, FOR EVERY CLASS
!     NECESSARY FOR BALANCE

!     MAYBE TO DO ONLY AT THE LAST TIME STEP?
!
      DO ICLA=1,NSICLA
        MASSTOT(ICLA)=0.D0
      ENDDO
!
      DO ICLA=1,NSICLA
        DO I=1,NPOIN
          DO ILAYER=1,NOMBLAY
            MASSTOT(ICLA)= MASSTOT(ICLA) +
     &                   MASS_SAND(ICLA,ILAYER,I)*VOLU2D%R(I)
          ENDDO
        ENDDO
      ENDDO
!
      IF(NCSIZE.GT.1) THEN
        DO I=1,NSICLA
          MASSTOT(I)=P_DSUM(MASSTOT(I))
        ENDDO
      ENDIF

      RETURN
      END SUBROUTINE
