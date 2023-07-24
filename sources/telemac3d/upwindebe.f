!                   ********************
                    SUBROUTINE UPWINDEBE
!                   ********************
!
     &(D,X,IKLE,NELMAX,NELEM2,SURFAC,NPLAN,WCC,EXT,DELTA)
!
!***********************************************************************
! TELEMAC3D   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    UPWINDS THE ADVECTION TERM OF VERTICAL VELOCITY.
!code
!+        A DIFFUSION TERM WITH DIFFUSION COEFFICIENT ABS(WCC)*DZ/2
!+        IS ADDED TO THE MATRIX. FORMULA IS OBTAINED BY SIMPLIFYING
!+        THE Z PART OF DIFFUSION MATRIX BUILT IN SUBROUTINE MT02PP
!+        DZ THEN VANISHES.
!+
!+        THIS IS USED IN DIFF3D FOR SEDIMENT SETTLING VELOCITY
!
!history  J.M. HERVOUET  (LNHE)
!+        12/12/05
!+        V5P6
!+
!
!history  N.DURAND (HRW), S.E.BOURBAN (HRW)
!+        13/07/2010
!+        V6P0
!+   Translation of French comments within the FORTRAN sources into
!+   English comments
!
!history  N.DURAND (HRW), S.E.BOURBAN (HRW)
!+        21/08/2010
!+        V6P0
!+   Creation of DOXYGEN tags for automated documentation and
!+   cross-referencing of the FORTRAN sources
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| D              |<->| MATRIX DIAGONAL
!| DELTA          |-->| UPWIND COEFFICIENT (BETWEEN 0 AND 1)
!| EXT            |-->| TYPE OF OFF-DIAGONAL TERMS
!|                |   | TYPEXT = 'Q' : ANY VALUE
!|                |   | TYPEXT = 'S' : SYMMETRIC
!|                |   | TYPEXT = '0' : ZERO
!| IKLE           |-->| CONNECTIVITY TABLE
!| NELEM2         |-->| NUMBER OF ELEMENTS IN 2D MESH
!| NELMAX         |-->| MAXIMUM NUMBER OF ELEMENTS
!| NPLAN          |-->| NUMBER OF PLANES ON THE VERTICAL
!| SURFAC         |-->| AREA OF TRIANGLES
!| WCC            |-->| VELOCITY (NEGATIVE IF SETTLING VELOCITY)
!|                |   | CAN BE ALSO WSCONV IN THE TRANSFORMED MESH
!| X              |<->| MATRIX OFF-DIAGONAL TERMS
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)             :: NELMAX,NELEM2,NPLAN
      INTEGER, INTENT(IN)             :: IKLE(NELMAX,6)
      DOUBLE PRECISION, INTENT(IN   ) :: SURFAC(NELMAX),WCC(*),DELTA
      DOUBLE PRECISION, INTENT(INOUT) :: D(*),X(NELMAX,30)
      CHARACTER(LEN=1), INTENT(IN)    :: EXT
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER I,I1,I2,I3,I4,I5,I6,IELEM2,IELEM3
      DOUBLE PRECISION UP1,UP2,UP3,DS6
!
      INTRINSIC ABS
!
!=======================================================================
!
!=======================================================================
!
      DS6=DELTA/6.D0
!
      IF(EXT.EQ.'S') THEN
!
      DO I=1,NPLAN-1
        DO IELEM2=1,NELEM2
          IELEM3=IELEM2+(I-1)*NELEM2
          I1=IKLE(IELEM3,1)
          I2=IKLE(IELEM3,2)
          I3=IKLE(IELEM3,3)
          I4=IKLE(IELEM3,4)
          I5=IKLE(IELEM3,5)
          I6=IKLE(IELEM3,6)
!         AVERAGE VELOCITY TAKEN AT THE BOTTOM OF THE PRISM
!         IF IT IS WSCONV (CALL BY PRECON, IT IS CONSTANT ON THE VERTICAL)
!         AND WCC(I4,I5 OR I6) NOT DEFINED FOR LAST LAYER OF ELEMENTS
!         IF IT IS THE SETTLING VELOCITY, IT IS CONSTANT EVERYWHERE
          UP1=SURFAC(IELEM2)*DS6*ABS(WCC(I1))
          UP2=SURFAC(IELEM2)*DS6*ABS(WCC(I2))
          UP3=SURFAC(IELEM2)*DS6*ABS(WCC(I3))
          X(IELEM3,03)=X(IELEM3,03)-UP1
          X(IELEM3,08)=X(IELEM3,08)-UP2
          X(IELEM3,12)=X(IELEM3,12)-UP3
          D(I1)=D(I1)+UP1
          D(I2)=D(I2)+UP2
          D(I3)=D(I3)+UP3
          D(I4)=D(I4)+UP1
          D(I5)=D(I5)+UP2
          D(I6)=D(I6)+UP3
        ENDDO
      ENDDO
!
      ELSEIF(EXT.EQ.'Q') THEN
!
      DO I=1,NPLAN-1
        DO IELEM2=1,NELEM2
          IELEM3=IELEM2+(I-1)*NELEM2
          I1=IKLE(IELEM3,1)
          I2=IKLE(IELEM3,2)
          I3=IKLE(IELEM3,3)
          I4=IKLE(IELEM3,4)
          I5=IKLE(IELEM3,5)
          I6=IKLE(IELEM3,6)
!         AVERAGE VELOCITY TAKEN AT THE BOTTOM OF THE PRISM
!         IF IT IS WSCONV (CALL BY PRECON, IT IS CONSTANT ON THE VERTICAL)
!         AND WCC(I4,I5 OR I6) NOT DEFINED FOR LAST LAYER OF ELEMENTS
!         IF IT IS THE SETTLING VELOCITY, IT IS CONSTANT EVERYWHERE
          UP1=SURFAC(IELEM2)*DS6*ABS(WCC(I1))
          UP2=SURFAC(IELEM2)*DS6*ABS(WCC(I2))
          UP3=SURFAC(IELEM2)*DS6*ABS(WCC(I3))
          X(IELEM3,03)=X(IELEM3,03)-UP1
          X(IELEM3,08)=X(IELEM3,08)-UP2
          X(IELEM3,12)=X(IELEM3,12)-UP3
          X(IELEM3,18)=X(IELEM3,18)-UP1
          X(IELEM3,23)=X(IELEM3,23)-UP2
          X(IELEM3,27)=X(IELEM3,27)-UP3
          D(I1)=D(I1)+UP1
          D(I2)=D(I2)+UP2
          D(I3)=D(I3)+UP3
          D(I4)=D(I4)+UP1
          D(I5)=D(I5)+UP2
          D(I6)=D(I6)+UP3
        ENDDO
      ENDDO
!
      ELSE
        WRITE(LU,1001) EXT
1001    FORMAT(1X,'UPWINDEBE: UNEXPECTED VALUE OF EXT: ',A1)
        CALL PLANTE(1)
        STOP
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
