!                   *******************************
                    SUBROUTINE MEAN_GRAIN_SIZE_GAIA
!                   *******************************
!
!
!***********************************************************************
! GAIA
!***********************************************************************
!
!>@brief Geometric mean grain sizes of active-layer and under-layer.
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_TELEMAC
      USE DECLARATIONS_GAIA, ONLY: ACLADM,UNLADM,DCLA,RATIO_SAND,NPOIN,
     &    NSAND,NOMBLAY,NUM_ISAND_ICLA
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!-----------------------------------------------------------------------
!
      INTEGER I,J
!
!-----------------------------------------------------------------------
!
!     UNLADM IS NEEDED FOR HUNZIKER
!
!     NOTE THAT MASS FRACTION AND VOLUME FRACTION ARE THE SAME WHEN
!     THERE ARE ONLY SANDS
      DO J=1,NPOIN
        ACLADM%R(J) = 0.D0
        UNLADM%R(J) = 0.D0
        DO I=1,NSAND
          IF(RATIO_SAND(I,1,J).GT.0.D0) THEN
            ACLADM%R(J) = ACLADM%R(J) + DCLA(NUM_ISAND_ICLA(I))
     &                    *RATIO_SAND(I,1,J)
          ENDIF
        ACLADM%R(J)=MAX(ACLADM%R(J),0.D0)
        ENDDO
        IF(ACLADM%R(J).LE.0.D0) ACLADM%R(J) = DCLA(NUM_ISAND_ICLA(1))
      ENDDO
      IF(NOMBLAY.GT.1) THEN
        DO J=1,NPOIN
          DO I=1,NSAND
            IF(RATIO_SAND(I,2,J).GT.0.D0) THEN
              UNLADM%R(J) = UNLADM%R(J) + DCLA(NUM_ISAND_ICLA(I))
     &                     *RATIO_SAND(I,2,J)
            ENDIF
            UNLADM%R(J)=MAX(UNLADM%R(J),0.D0)
          ENDDO
          IF(UNLADM%R(J).LE.0.D0) UNLADM%R(J) = ACLADM%R(J)
        ENDDO
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
