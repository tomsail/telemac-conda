!                   *****************
                    SUBROUTINE FWSPEC
!                   *****************
!
     &(FW,FWCOEF,X,Y,NPOIN,PRIVE,ZF)
!
!***********************************************************************
! ARTEMIS   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    SPECIFIES THE BOTTOM FRICTION COEFFICIENT
!+                IF IT IS VARIABLE IN SPACE.
!
!history  J-M HERVOUET (LNH)
!+
!+
!+   LINKED TO BIEF 5.0
!
!history  D. AELBRECHT (LNH)
!+        02/06/1999
!+        V5P1
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
!| FW             |<--| BOTTOM FRICTION FACTOR
!| FWCOEF         |-->| CONSTANT FRICTION FACTOR IMPOSED
!| NPOIN          |-->| NUMBER OF POINTS
!| PRIVE          |-->| PRIVATE TABLE DEFINED IN PRINCI
!| X,Y            |-->| MESH COORDINATES
!| ZF             |-->| BOTTOM
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE INTERFACE_ARTEMIS, EX_FWSPEC => FWSPEC
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN) :: NPOIN
      DOUBLE PRECISION, INTENT(INOUT) :: FW(NPOIN)
      DOUBLE PRECISION, INTENT(IN) :: X(NPOIN),Y(NPOIN)
      DOUBLE PRECISION, INTENT(IN) :: ZF(NPOIN),FWCOEF
      TYPE(BIEF_OBJ), INTENT(IN) :: PRIVE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
!
!-----------------------------------------------------------------------
!
!     HERE THE FRICTION COEFFICIENT IS CONSTANT
!
      CALL OV('X=C     ', X=FW, C=FWCOEF, DIM1=NPOIN)
!
      CALL USER_FWSPEC(FW,FWCOEF,X,Y,NPOIN,PRIVE,ZF)
!
!-----------------------------------------------------------------------
!
      RETURN
      END
