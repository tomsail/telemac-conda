!                       ********************************
                        SUBROUTINE CHECK_COORD_PRECISION
!                       ********************************
     &(X,Y,IKLE,NELEM,NDP,NPOIN,IS_DOUBLE)
!
!***********************************************************************
! STBTEL
!***********************************************************************
!
!brief    CHECK IF THE FILE SHOULD BE WRITTEN IS A DOUBLE PRECISION
!+        FORMAT ACCORDING TO ITS COORDINATES VALUES
!
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| X        |-->| X Coordinates
!| Y        |-->| Y coordinates
!| IKLE     |-->| Connectivity table (ndp, nelem)
!| NELEM    |-->| Number of elements
!| NDP      |-->| Number of points per elements
!| NPOIN    |-->| Number of points
!| ISDOUBLE |<--| True if the coordinates require a double
!|          |   | precision
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      ! NAME IN THE TEMPORARY FOLDER OF THE FILES :
      ! EQUAL ' ' IF FILE NOT AVAILABLE
      INTEGER, INTENT(IN) :: NPOIN, NELEM, NDP
      DOUBLE PRECISION, INTENT(IN) :: X(NPOIN)
      DOUBLE PRECISION, INTENT(IN) :: Y(NPOIN)
      INTEGER, INTENT(IN) :: IKLE(NDP, NELEM)
      LOGICAL, INTENT(OUT) :: IS_DOUBLE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      DOUBLE PRECISION :: EPS, X1, X2, X3, Y1, Y2, Y3
      INTEGER I
      REAL TMP

      ! precision(tmp) should be 6
      EPS = 10.D0**(-PRECISION(TMP))

      IS_DOUBLE = .FALSE.
      DO I=1,NELEM
        X1 = X(IKLE(1,I))
        Y1 = Y(IKLE(1,I))
        X2 = X(IKLE(2,I))
        Y2 = Y(IKLE(2,I))
        X3 = X(IKLE(3,I))
        Y3 = Y(IKLE(3,I))
        IF (( SQRT((X1-X2)**2+(Y1-Y2)**2) .LE. EPS).OR.
     &     ( SQRT((X2-X3)**2+(Y2-Y3)**2) .LE. EPS).OR.
     &     ( SQRT((X3-X1)**2+(Y3-Y1)**2) .LE. EPS)) THEN
          IS_DOUBLE = .TRUE.
          RETURN
        ENDIF
      ENDDO
!
      END SUBROUTINE
