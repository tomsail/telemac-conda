!                   ********************
                    SUBROUTINE DIAG_MURD
!                   ********************
!
     &(DIAG,XM,NELEM,NELMAX,NPOIN3,IKLE,IELM3,DIM1X)
!
!***********************************************************************
! TELEMAC3D   V6P2                                   21/08/2010
!***********************************************************************
!
!brief    BUILDS THE DIAGONAL OF THE MURD MATRIX.
!
!warning  THE JACOBIAN MUST BE POSITIVE
!
!history  J-M HERVOUET (LNHE)
!+        23/04/2010
!+        V6P0
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
!history  J-M HERVOUET (LNHE)
!+        24/10/2011
!+        V6P2
!+   Arguments IELM3 and DIM1X added. Tetrahedra treated.
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| DIAG           |<->| DIAGONAL OF THE MURD MATRIX
!| DIM1X          |-->| FIRST DIMENSION OF XM
!| IELM3          |-->| TYPE OF ELEMENT
!| IKLE           |-->| CONNECTIVITY TABLE
!| NELEM          |-->| NUMBER OF ELEMENTS
!| NELMAX         |-->| MAXIMUM NUMBER OF ELEMENTS
!| NPOIN3         |-->| NUMBER OF 3D POINTS
!| XM             |-->| OFF-DIAGONAL TERMS
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN) :: NELEM,NELMAX,NPOIN3,IELM3,DIM1X
!                                        6 OR 4
      INTEGER, INTENT(IN) :: IKLE(NELMAX,*)
!
      DOUBLE PRECISION, INTENT(INOUT) :: DIAG(NPOIN3)
      DOUBLE PRECISION, INTENT(IN)    :: XM(DIM1X,NELMAX)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER IELEM,I1,I2,I3,I4,I5,I6
!
!-----------------------------------------------------------------------
!
!     INITIALISES DIAG
!
      DO I1=1,NPOIN3
        DIAG(I1)=0.D0
      ENDDO
!
!     COMPUTES THE SUM OF EACH LINE (WITH A - SIGN)
!     THE DIAGONAL TERMS ARE 0
!
      IF(IELM3.EQ.41) THEN
!
        DO IELEM = 1,NELEM
          I1=IKLE(IELEM,1)
          I2=IKLE(IELEM,2)
          I3=IKLE(IELEM,3)
          I4=IKLE(IELEM,4)
          I5=IKLE(IELEM,5)
          I6=IKLE(IELEM,6)
          DIAG(I1)=DIAG(I1)-XM(01,IELEM)-XM(02,IELEM)
     &                     -XM(03,IELEM)-XM(04,IELEM)-XM(05,IELEM)
          DIAG(I2)=DIAG(I2)-XM(16,IELEM)-XM(06,IELEM)
     &                     -XM(07,IELEM)-XM(08,IELEM)-XM(09,IELEM)
          DIAG(I3)=DIAG(I3)-XM(17,IELEM)-XM(21,IELEM)
     &                     -XM(10,IELEM)-XM(11,IELEM)-XM(12,IELEM)
          DIAG(I4)=DIAG(I4)-XM(18,IELEM)-XM(22,IELEM)
     &                     -XM(25,IELEM)-XM(13,IELEM)-XM(14,IELEM)
          DIAG(I5)=DIAG(I5)-XM(19,IELEM)-XM(23,IELEM)
     &                     -XM(26,IELEM)-XM(28,IELEM)-XM(15,IELEM)
          DIAG(I6)=DIAG(I6)-XM(20,IELEM)-XM(24,IELEM)
     &                     -XM(27,IELEM)-XM(29,IELEM)-XM(30,IELEM)
        ENDDO
!
      ELSEIF(IELM3.EQ.51.OR.IELM3.EQ.31) THEN
!
        DO IELEM = 1,NELEM
          I1=IKLE(IELEM,1)
          I2=IKLE(IELEM,2)
          I3=IKLE(IELEM,3)
          I4=IKLE(IELEM,4)
          DIAG(I1)=DIAG(I1)-XM(01,IELEM)-XM(02,IELEM)-XM(03,IELEM)
          DIAG(I2)=DIAG(I2)-XM(04,IELEM)-XM(05,IELEM)-XM(07,IELEM)
          DIAG(I3)=DIAG(I3)-XM(06,IELEM)-XM(08,IELEM)-XM(10,IELEM)
          DIAG(I4)=DIAG(I4)-XM(09,IELEM)-XM(11,IELEM)-XM(12,IELEM)
        ENDDO
!
      ELSE
        WRITE(LU,*) 'DIAG_MURD ELEMENT ',IELM3,' NOT IMPLEMENTED'
        CALL PLANTE(1)
        STOP
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
