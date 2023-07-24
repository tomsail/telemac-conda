!                   *************************
                    SUBROUTINE AD_SET_SISYPHE
!                   *************************
!
     &(IVAR,ADOBJ)
!
!***********************************************************************
! SISYPHE   V7P2
!***********************************************************************
!
!brief    ASSOCIATE THE ARRAY ADOBJ%R TO THE APPROPRIATE
!         DIFFERENTED VARIBALE.
!
!history  S.E.BOURBAN (HRW)
!+        20/06/2016
!+        V7P2
!+   First version for differentiated variables
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| IVAR       |-->| ONE SPECIFIC DIFERENTIABLE VARIABLE
!| ADOBJ      |<->| MOST LIKELY ONE OF THE VARSOR%ADR(I_ADVAR)%P
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_SPECIAL
      USE DECLARATIONS_SISYPHE
!
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)             :: IVAR
      TYPE(BIEF_OBJ)  , INTENT(INOUT) :: ADOBJ
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
!     INTEGER :: IPOIN
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      SELECT CASE (IVAR)
!       JUST TESTING IO OF DIFFERENTIATED VARIABLES
!
!   THIS IS JUST AN EXAMPLE, TO HELP USERS LINKING ADVAR WITH THEIR
!     OWN DIRIVATIVES. HERE, ADVAR(1), (2) and (3) ARE THE
!     DIFFERENTIATED VARIABLES OF TRACER(1), U AND V RESPECTIVELY.
!
#if defined COMPAD_DCO_T1S
!      CASE(1)
!        CALL DCO_T1S_SET( QS%R, ADOBJ%R, 1 )
!      CASE(2)
!        CALL DCO_T1S_SET( QSX%R, ADOBJ%R, 1 )
!      CASE(3)
!        CALL DCO_T1S_SET( QSY%R, ADOBJ%R, 1 )
!      CASE(4)
!        CALL DCO_T1S_SET( ESOMT%R, ADOBJ%R, 1 )
!      CASE(5)
!        CALL DCO_T1S_SET( H%R, ADOBJ%R, 1 )
!
!    JUST IN CASE T1S IS NOT AVAILABLE, HERE BELOW FOR TESTING
!#else
!        T%ADR(1)%P%R = ADOBJ%R
!      CASE(1)
!        QS%R = ADOBJ%R
!      CASE(2)
!        QSX%R = ADOBJ%R
!      CASE(3)
!        QSY%R = ADOBJ%R
!      CASE(4)
!        ESOMT%R = ADOBJ%R
!      CASE(5)
!        H%R = ADOBJ%R
#endif
      CASE DEFAULT
        WRITE(LU,*) 'WARNING: UNKNOWN DIFFERENTIATED VARIABLE ',IVAR
        WRITE(LU,*) '  UPDATE EITHER SUBROUTINE AD_SET_SISYPHE'
        WRITE(LU,*) '  OR THE NUMBER OF DIFERENTIATED  VARIABLES'
        WRITE(LU,*) '  -- OR ALGORITHMIC DIFFERENTIATION MIGHT NOT BE'
        WRITE(LU,*) '  CONFIGURED.'
      END SELECT
!
!-----------------------------------------------------------------------
!
      RETURN
      END SUBROUTINE AD_SET_SISYPHE
