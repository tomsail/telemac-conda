!                   ******************************
                    SUBROUTINE GAIA_BEDLOAD_UPDATE
!                   ******************************
!
!***********************************************************************
! GAIA
!***********************************************************************
!
!>@brief Reconstitude the bedload and/or suspension data
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      USE INTERFACE_GAIA
      USE BIEF
      USE DECLARATIONS_TELEMAC
      USE DECLARATIONS_GAIA
      USE INTERFACE_HERMES
      USE DECLARATIONS_SPECIAL
!
      IMPLICIT NONE
!
!!-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
!
!!-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER I
!
!-----------------------------------------------------------------------
!
!
! RECONSTITUTES THE BEDLOAD AND/OR SUSPENSION DATA
! -----------------------------------------------------
!
      IF(DEBUG.GT.0) WRITE(LU,*) 'QS_RESULT'
!
      DO I = 1, NSICLA
      CALL OS('X=Y     ',X=T1,Y=QSCLXC%ADR(I)%P)
      CALL OS('X=Y     ',X=T2,Y=QSCLYC%ADR(I)%P)
      CALL OS('X=N(Y,Z)', X=QSCL%ADR(I)%P,Y=T1,Z=T2)
        IF(I.EQ.1) THEN
          CALL OS('X=Y     ', X=QSX, Y=T1)
          CALL OS('X=Y     ', X=QSY, Y=T2)
        ELSE
          CALL OS('X=X+Y   ', X=QSX, Y=T1)
          CALL OS('X=X+Y   ', X=QSY, Y=T2)
        ENDIF
      ENDDO
      CALL OS('X=N(Y,Z)', X=QS, Y=QSX, Z=QSY)
      IF(DEBUG.GT.0) WRITE(LU,*) 'END_QS_RESULT'
!
!-----------------------------------------------------------------------
!
      RETURN
      END
