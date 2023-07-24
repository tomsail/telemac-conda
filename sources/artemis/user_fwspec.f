!                   **********************
                    SUBROUTINE USER_FWSPEC
!                   **********************
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
!warning  THIS SUBROUTINE IS MERELY AN EXAMPLE;
!+            MUST BE CODED BY THE USER
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
      USE INTERFACE_ARTEMIS, EX_USER_FWSPEC => USER_FWSPEC
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
!
!-----------------------------------------------------------------------
!
      RETURN
      END

