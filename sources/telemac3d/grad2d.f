!                   *****************
                    SUBROUTINE GRAD2D
!                   *****************
!
     &(DFDX,DFDY,FU,NPLAN,S,UNSV2D,FU2,FU3,FU4,IELM2,MESH2D,MSK,MASKEL)
!
!***********************************************************************
! TELEMAC3D   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    COMPUTES THE 2D GRADIENT OF FUNCTION F.
!
!history  F LEPEINTRE (LNH)    ; J-M JANIN (LNH)
!+        25/11/97
!+        V5P7
!+
!
!history  JACEK A. JANKOWSKI PINXIT
!+        **/03/99
!+
!+   FORTRAN95 VERSION
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
!| DFDX           |<->| DF/DX
!| DFDY           |<->| DF/DY
!| FU             |-->| FUNCTION TO DIFFERENTIATE
!| FU2            |<->| POINTER ON FU
!| FU3            |<->| POINTER ON DF/DX
!| FU4            |<->| POINTER ON DF/DY
!| IELM2          |-->| TYPE OF 2D DISCRETISATION
!| MASKEL         |-->| MASKING OF ELEMENTS
!|                |   | =1. : NORMAL   =0. : MASKED ELEMENT
!| MESH2D         |<->| 2D MESH
!| MSK            |-->| IF YES, THERE IS MASKED ELEMENTS.
!| NPLAN          |-->| NUMBER OF PLANES IN THE 3D MESH OF PRISMS
!| S              |<->| VOID STRUCTURE
!| UNSV2D         |<->| INVERSE OF INTEGRALS OF TEST FUNCTIONS
!|                |   | (VOLUME OF BASES)IN 2D
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_TELEMAC
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      TYPE(BIEF_OBJ), INTENT(INOUT) :: DFDX, DFDY
      TYPE(BIEF_OBJ), INTENT(IN)    :: FU
      TYPE(BIEF_OBJ), INTENT(INOUT) :: UNSV2D,S
      TYPE(BIEF_OBJ), INTENT(IN)    :: MASKEL
      TYPE(BIEF_MESH), INTENT(INOUT):: MESH2D
      INTEGER, INTENT(IN)           :: NPLAN, IELM2
      LOGICAL, INTENT(IN)           :: MSK
!     FU2,3,4 MUST BE 2D WORK FIELD - NO CHECKING
      TYPE(BIEF_OBJ), INTENT(INOUT) :: FU2,FU3,FU4
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER NPOIN,IPLAN
      DOUBLE PRECISION, POINTER :: SAVEFU2(:),SAVEFU3(:),SAVEFU4(:)
!
!-----------------------------------------------------------------------
!
      NPOIN = MESH2D%NPOIN
!
      SAVEFU2=> FU2%R
      SAVEFU3=> FU3%R
      SAVEFU4=> FU4%R
!
      DO IPLAN=1,NPLAN
!
!     WORKING ON POINTERS RATHER THAN COPYING
      FU2%R=>  FU%R((IPLAN-1)*NPOIN+1:IPLAN*NPOIN)
      FU3%R=>DFDX%R((IPLAN-1)*NPOIN+1:IPLAN*NPOIN)
      FU4%R=>DFDY%R((IPLAN-1)*NPOIN+1:IPLAN*NPOIN)
!
!     CALCUL DE LA DERIVEE DF/DX
!
      CALL VECTOR(FU3,'=','GRADF          X',IELM2,1.D0,FU2,S,S,
     &            S,S,S,MESH2D,MSK,MASKEL)
      IF (NCSIZE.GT.1) CALL PARCOM (FU3, 2, MESH2D)
      CALL OS('X=XY    ' ,X=FU3,Y=UNSV2D)
!
!     CALCUL DE LA DERIVEE DF/DY
!
      CALL VECTOR(FU4,'=','GRADF          Y',IELM2,1.D0,FU2,S,S,
     &            S,S,S,MESH2D,MSK,MASKEL)
      IF (NCSIZE.GT.1) CALL PARCOM (FU4, 2, MESH2D)
      CALL OS('X=XY    ' ,X=FU4,Y=UNSV2D)
!
      ENDDO
!
      FU2%R=>SAVEFU2
      FU3%R=>SAVEFU3
      FU4%R=>SAVEFU4
!
!-----------------------------------------------------------------------
!
      RETURN
      END
