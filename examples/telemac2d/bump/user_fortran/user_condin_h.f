!                   ************************
                    SUBROUTINE USER_CONDIN_H
!                   ************************
!
!
!***********************************************************************
! TELEMAC2D   V7P3
!***********************************************************************
!
!brief    USER INITIALISES THE PHYSICAL PARAMETERS U, V
!
!history  J-M HERVOUET (LNHE)
!+        30/08/2007
!+        V6P0
!+
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_TELEMAC
      USE DECLARATIONS_TELEMAC2D
      USE TPXO
      USE OKADA
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER I, J
      INTEGER NA
      INTEGER IERR
!
      DOUBLE PRECISION TEMP, WEIGHT1, WEIGHT2
      DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE :: SUBX
      DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE :: EXACTH, EXACTU
      LOGICAL YESITIS
      INTEGER FID
!
!-----------------------------------------------------------------------
!
!     TESTING IF ANALYTICAL SOLUTION FILE EXISTS
      IF(T2D_FILES(T2DFO1)%NAME(1:1).NE.' ') THEN
        WRITE(LU,*) 'READING ANALYTIC SOLUTION FROM FILE'
        FID = T2D_FILES(T2DFO1)%LU
!
!       COMPUTING NUMBER OF LINES IN ANALYTIC SOLUTION FILE
        NA = 0
        IERR = 0
        DO WHILE (IERR==0)
          READ(FID, '(A)', IOSTAT=IERR)
          IF (IERR==0) THEN
            NA = NA + 1
          ENDIF
        ENDDO
!
        ALLOCATE(SUBX(NA))
        ALLOCATE(EXACTH(NA))
        ALLOCATE(EXACTU(NA))
!
!       READING ANALYTIC SOLUTION
        REWIND(FID)
        DO I=1,NA
          READ(FID, *) SUBX(I), EXACTH(I), EXACTU(I)
        ENDDO
!
!       INITIALISATION OF H, U, V
        DO I=1,NA-1
          DO J=1,NPOIN
            IF ((SUBX(I).LE.X(J)).AND.(X(J).LE.SUBX(I+1))) THEN
              TEMP = SUBX(I+1) - SUBX(I)
              WEIGHT1 = (SUBX(I+1) - X(J))/TEMP
              WEIGHT2 = (X(J) - SUBX(I))/TEMP
              H%R(J) = WEIGHT1*EXACTH(I) + WEIGHT2*EXACTH(I+1)
              U%R(J) = WEIGHT1*EXACTU(I) + WEIGHT2*EXACTU(I+1)
              V%R(J) = 0.0D0
            ENDIF
          ENDDO
        ENDDO
!
!----------------------------------------------------------------------
!
        DEALLOCATE(SUBX)
        DEALLOCATE(EXACTH)
        DEALLOCATE(EXACTU)
      ELSE
        WRITE(LU,*) 'NO ANALYTIC SOLUTION DEFAULT INIT'
        DO J=1,NPOIN
          H%R(J) = 1.D0-ZF%R(J)
        END DO
      END IF
!
      RETURN
      END
