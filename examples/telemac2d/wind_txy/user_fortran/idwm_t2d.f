!                   *******************
                    SUBROUTINE IDWM_T2D
!                   *******************
     &(ELEV,POINTS,IDWM,NPOIN,NUMSTA)
!
!***********************************************************************
! TELEMAC2D   V7P1                                   29/07/2016
!***********************************************************************
!
!brief    USES INVERSE DISTANCE WEIGHTING METHOD TO COMPUTE A WIND FIELD
!+               THAT VARIES IN TIME AND SPACE
!
!history  P. PRODANOVIC (RIGGS ENGINEERING LTD)
!+        23/04/2014
!+        V6P3
!+   Initial version.
!
!history  P. PRODANOVIC (RIGGS ENGINEERING LTD)
!+        29/05/2016
!+        V7P0
!+   Improvement to manage divisions by 0 when station and mesh nodes
!+   are on the same point. Also, now allows proper interpolation for
!+   directions between 359 and 1 degrees azimuth.
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_SPECIAL
!
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
!     VARIABLES THAT ARE USED BY THE SUBROUTINE TO PASS DATA IN AND OUT
      INTEGER,INTENT(IN) :: NPOIN, NUMSTA
      DOUBLE PRECISION, INTENT(IN) :: POINTS(NPOIN,2)
      DOUBLE PRECISION, INTENT(IN) :: ELEV(NUMSTA,3)
      DOUBLE PRECISION, INTENT(INOUT) :: IDWM(NPOIN)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
!     VARIABLES THAT ARE INTERNAL TO THE SOUBROUTINE
      INTEGER N, M, I, J
!
!     WEIGHTS, DENOMINATOR
      DOUBLE PRECISION :: W1, W2, W3, W4, DEN
!
!     MAX AND MIN EXTENTS OF THE ELEV INPUT
      DOUBLE PRECISION :: ELEV_MIN, ELEV_MAX, TMP1, TMP2, TMP3, TMP4
!
      DOUBLE PRECISION, ALLOCATABLE, DIMENSION(:) :: DIST, E
!
!     DISTANCES IN EACH QUADRANT
      DOUBLE PRECISION, ALLOCATABLE, DIMENSION(:) :: DISTQ1, DISTQ2
      DOUBLE PRECISION, ALLOCATABLE, DIMENSION(:) :: DISTQ3, DISTQ4
!
!     MINIMUM (IN QUADRANTS 1 TO 4) OF THE PREVIOUS POINT
      DOUBLE PRECISION :: MIN1PRE, MIN2PRE, MIN3PRE, MIN4PRE
!
!     CURRENT MINS
      DOUBLE PRECISION :: MIN1CUR, MIN2CUR, MIN3CUR, MIN4CUR
!
!     LOCATIONS OF THE MININIMS (USED FOR ARRAY REFERENCING)
      INTEGER :: MIN1LOC, MIN2LOC, MIN3LOC, MIN4LOC
!
!-----------------------------------------------------------------------
!
!     DEFINE N AND M; THIS IS DONE TO HAMONIZE PASTING OF CODE
      N = NUMSTA
      M = NPOIN
!
!     WE KNOW EVERYTHING GETS PASSED INTO THE FUNCTION PROPERLY
      ALLOCATE(DIST(N), E(N), DISTQ1(N), DISTQ2(N))
      ALLOCATE(DISTQ3(N), DISTQ4(N))
!
!     SET MIN_PREV VARS TO LARGE NUMBERS
      MIN1CUR = 1.D30
      MIN2CUR = 1.D30
      MIN3CUR = 1.D30
      MIN4CUR = 1.D30
!
      MIN1PRE = 1.0D30
      MIN2PRE = 1.0D30
      MIN3PRE = 1.0D30
      MIN4PRE = 1.0D30
!
!     INITIALIZE MIN_LOC
      MIN1LOC = -1
      MIN2LOC = -1
      MIN3LOC = -1
      MIN4LOC = -1
!
      ELEV_MIN = MINVAL(E)
      ELEV_MAX = MAXVAL(E)
!
!     MAIN LOOP TO DO THE INTERPOLATION
      DO J = 1, M
        DO I = 1, N
          ! THIS IS FORTRAN 77 WAY OF HAVING LONG LINES;
          ! CHARACTER * OR & HAS TO BE AT COL 6 OF THE FILE
          DIST(I)=SQRT((ELEV(I,1)-POINTS(J,1))**2 +
     &      (ELEV(I,2)-POINTS(J,2))**2)

          ! FIND MIN DIST IN EACH OF THE FOUR QUADRANTS
          ! 2 | 1
          ! -----
          ! 3 | 4

          ! QUADRANT 1
          IF (ELEV(I,1) >= POINTS(J,1) .AND. ELEV(I,2) >=
     &      POINTS(J,2) ) THEN

            DISTQ1(I) = DIST(I)

            IF (DISTQ1(I) > 0) THEN
              IF (DISTQ1(I) < MIN1PRE) THEN
                MIN1CUR = DISTQ1(I)
                MIN1LOC = I
              END IF
            END IF
          END IF

          ! QUADRANT 2
          IF (ELEV(I,1) < POINTS(J,1) .AND. ELEV(I,2) >=
     &      POINTS(J,2) ) THEN

            DISTQ2(I) = DIST(I)

            IF (DISTQ2(I) > 0) THEN
              IF (DISTQ2(I) < MIN2PRE) THEN
                MIN2CUR = DISTQ2(I)
                MIN2LOC = I
              END IF
            END IF
          END IF

          ! QUADRANT 3
          IF (ELEV(I,1) < POINTS(J,1) .AND. ELEV(I,2) <
     &      POINTS(J,2) ) THEN

            DISTQ3(I) = DIST(I)

            IF (DISTQ3(I) > 0) THEN
              IF (DISTQ3(I) < MIN3PRE) THEN
                MIN3CUR = DISTQ3(I)
                MIN3LOC = I
              END IF
            END IF
          END IF

          ! QUADRANT 4
          IF (ELEV(I,1) > POINTS(J,1) .AND. ELEV(I,2) <
     &      POINTS(J,2) ) THEN

            DISTQ4(I) = DIST(I)

            IF (DISTQ4(I) > 0) THEN
              IF (DISTQ4(I) < MIN4PRE) THEN
                  MIN4CUR = DISTQ4(I)
                  MIN4LOC = I
                END IF
              END IF
            END IF

          MIN1PRE = MIN1CUR
          MIN2PRE = MIN2CUR
          MIN3PRE = MIN3CUR
          MIN4PRE = MIN4CUR
        END DO ! N

        ! TO FIX THE DIVIDE BY ZERO ERROR
      IF (MIN1CUR < 1.0E-6) THEN
        MIN1CUR = 1.0E-16
      ELSEIF (MIN2CUR < 1.0E-6) THEN
        MIN2CUR = 1.0E-16
      ELSEIF (MIN3CUR < 1.0E-6) THEN
        MIN3CUR = 1.0E-16
      ELSEIF (MIN4CUR < 1.0E-6) THEN
        MIN4CUR = 1.0E-16
      END IF

        ! CALCULATE WEIGHTS
        DEN = (1/(MIN1CUR**2)) +(1/(MIN2CUR**2)) +(1/(MIN3CUR**2)) +
     &    (1/(MIN4CUR**2))
        W1 = (1/(MIN1CUR**2))/DEN
        W2 = (1/(MIN2CUR**2))/DEN
        W3 = (1/(MIN3CUR**2))/DEN
        W4 = (1/(MIN4CUR**2))/DEN

        ! FOR SOME REASON, IF MIN1LOC (OR MIN2LOC, ETC) ARE NEGATIVE VALUES,
        ! FORTRAN COMPILER CAN RETURN LARGE VALUES WHEN REFERENCING THE -VE
        ! VALUE OF AN ARRAY; THE FOLLOWING IF STATEMENTS ADDRESS THIS.

        IF (MIN1LOC < 1) THEN
          TMP1 = 0
        ELSE
          TMP1 = ELEV(MIN1LOC,3)
        END IF

        IF (MIN2LOC < 1) THEN
          TMP2 = 0
        ELSE
          TMP2 = ELEV(MIN2LOC,3)
        END IF

        IF (MIN3LOC < 1) THEN
          TMP3 = 0
        ELSE
          TMP3 = ELEV(MIN3LOC,3)
        END IF

        IF (MIN4LOC < 1) THEN
          TMP4 = 0
        ELSE
          TMP4 = ELEV(MIN4LOC,3)
        END IF

        ! IN CASE WHEN ALL INPUT DATA IS MISSING, ALSO OUTPUT MISSING
        IF (MIN1LOC == -1 .AND. MIN2LOC == -1 .AND. MIN3LOC == -1
     &    .AND. MIN4LOC == -1) THEN
          WRITE(LU,*) 'ALL INPUT DATA IS MISSING. EXITING!'
          CALL PLANTE(1)
        ELSE
          IDWM(J) = W1*TMP1+W2*TMP2+ W3*TMP3+W4*TMP4
        END IF

        ! CONSTRAIN INTERPOLATED POINTS TO MIN AND MAX OF INPUT DATA
        IF (IDWM(J) > ELEV_MAX) THEN
          !IDWM(J) = ELEV_MAX
        END IF

        IF (IDWM(J) < ELEV_MIN) THEN
          !IDWM(J) = ELEV_MIN
        END IF

        ! RESET THE ARRAYS
        DO I = 1, N
          DISTQ1(I) = -999.0
          DISTQ2(I) = -999.0
          DISTQ2(I) = -999.0
          DISTQ2(I) = -999.0
        END DO

        MIN1CUR = 1.0D30
        MIN2CUR = 1.0D30
        MIN3CUR = 1.0D30
        MIN4CUR = 1.0D30

        MIN1PRE = 1.0D30
        MIN2PRE = 1.0D30
        MIN3PRE = 1.0D30
        MIN4PRE = 1.0D30

        MIN1LOC = -1
        MIN2LOC = -1
        MIN3LOC = -1
        MIN4LOC = -1

      END DO ! M
!
!-----------------------------------------------------------------------
!
      RETURN
      END
