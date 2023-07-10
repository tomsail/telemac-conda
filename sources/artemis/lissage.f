!                       ******************
                        SUBROUTINE LISSAGE
!                       ******************
!
     &(NVAL,ENTREE,SORTIE)
!
!***********************************************************************
! ARTEMIS   V7P0                                   July 2014
!***********************************************************************
!
!brief    SMOOTHES A FUNCTION USING LEAST SQUARE METHOD
!         BASED ON A PARABOLA FIT TO 5 EQUALLY SPACED POINTS.
!
!history  S.BOURBAN (HRW)
!+        1997
!+
!+
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| NVAL            |   | DIMENSION OF THE FUNCTION
!| ENTREE         |   | INPUT VALUES
!| SORTIE         |   | OUTPUT VALUES
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN) :: NVAL
      DOUBLE PRECISION, INTENT(INOUT) :: ENTREE(NVAL)
      DOUBLE PRECISION, INTENT(INOUT) :: SORTIE(NVAL)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER I
!
      INTRINSIC DABS
!
!-----------------------------------------------------------------------
!     INITIALISES ENTREE
      DO I = 1, NVAL
        ENTREE(I) = DABS(ENTREE(I))
      ENDDO
      WRITE(LU,*)
!
!-----------------------------------------------------------------------
!
      DO I = 1, NVAL
!
!-----------------------------------------------------------------------
        IF(I.EQ.1) THEN
          SORTIE(I)=31.D0*ENTREE(I)+9.D0*ENTREE(I+1)
     &-3.D0*ENTREE(I+2)-5.D0*ENTREE(I+3)+3.D0*ENTREE(I+4)
          SORTIE(I)=SORTIE(I)/35.D0
!
!-----------------------------------------------------------------------
        ELSEIF(I.EQ.2) THEN
          SORTIE(I)=9.D0*ENTREE(I)+13.D0*ENTREE(I+1)
     &+12.D0*ENTREE(I+2)+6.D0*ENTREE(I+3)-5.D0*ENTREE(I+4)
          SORTIE(I)=SORTIE(I)/35.D0
!
!-----------------------------------------------------------------------
        ELSEIF(I.EQ.NVAL-1) THEN
          SORTIE(I)=-5.D0*ENTREE(NVAL-4)+6.D0*ENTREE(NVAL-3)
     &+12.D0*ENTREE(NVAL-2)+13.D0*ENTREE(NVAL-1)+9.D0*ENTREE(NVAL)
          SORTIE(I)=SORTIE(I)/35.D0
!
!-----------------------------------------------------------------------
        ELSEIF(I.EQ.NVAL) THEN
          SORTIE(I)=3.D0*ENTREE(NVAL-4)-5.D0*ENTREE(NVAL-3)
     &-3.D0*ENTREE(NVAL-2)+9.D0*ENTREE(NVAL-1)+31.D0*ENTREE(NVAL)
          SORTIE(I)=SORTIE(I)/35.D0
!
!-----------------------------------------------------------------------
        ELSE
          SORTIE(I)=-3.D0*ENTREE(I-2)+12.D0*ENTREE(I-1)
     &+17.D0*ENTREE(I)+12.D0*ENTREE(I+1)-3.D0*ENTREE(I+2)
          SORTIE(I)=SORTIE(I)/35.D0
        ENDIF
      ENDDO
!
!-----------------------------------------------------------------------
      RETURN
      END
