!                   *************************
                    SUBROUTINE AD_GET_GAIA
!                   *************************
!
     &(IVAR,ADOBJ)
!
!***********************************************************************
! GAIA   V7P2
!***********************************************************************
!
!>@brief    ASSOCIATE THE ARRAY ADOBJ%R TO THE APPROPRIATE
!         DIFFERENTED VARIBALE.
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!>@param[in] IVAR  ONE SPECIFIC DIFERENTIABLE VARIABLE
!>@param[in,out] ADOBJ  MOST LIKELY ONE OF THE VARSOR%ADR(I_ADVAR)%P
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_SPECIAL
      USE DECLARATIONS_GAIA
!
      IMPLICIT NONE
!
!!-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)             :: IVAR
      TYPE(BIEF_OBJ)  , INTENT(INOUT) :: ADOBJ
!
!!-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
!      SELECT CASE (IVAR)
!       UST TESTING IO OF DIFFERENTIATED VARIABLES
!
!   THIS IS JUST AN EXAMPLE, TO HELP USERS LINKING ADVAR WITH THEIR
!     OWN DIRIVATIVES. HERE, ADVAR(1), (2) and (3) ARE THE
!     DIFFERENTIATED VARIABLES OF TRACER(1), U AND V RESPECTIVELY.
!
#if defined COMPAD_DCO_T1S
!      CASE(1)
!        CALL DCO_T1S_GET( QS%R, ADOBJ%R, 1 )
!      CASE(2)
!        CALL DCO_T1S_GET( QSX%R, ADOBJ%R, 1 )
!      CASE(3)
!        CALL DCO_T1S_GET( QSY%R, ADOBJ%R, 1 )
!      CASE(4)
!        CALL DCO_T1S_GET( ESOMT%R, ADOBJ%R, 1 )
!      CASE(5)
!        CALL DCO_T1S_GET( H%R, ADOBJ%R, 1 )
!
!   JUST IN CASE T1S IS NOT AVAILABLE, HERE BELOW FOR TESTING
!     WITHOUT DIFFERENTIATED VARIABLES.
!
!#else
!      CASE(1)
!        ADOBJ%R = QS%R
!      CASE(2)
!        ADOBJ%R = QSX%R
!      CASE(3)
!        ADOBJ%R = QSY%R
!      CASE(4)
!        ADOBJ%R = ESOMT%R
!      CASE(5)
!        ADOBJ%R = H%R
#endif
!      CASE DEFAULT
!        IF(LNG.EQ.1) THEN
!          WRITE(LU,*) 'ATTENTION : IDENTIFIANT DERIVEE INCONNU ',IVAR
!          WRITE(LU,*) '  LA ROUTINE AD_GET_GAIA OU LE NOMBRE DE'
!          WRITE(LU,*) '  VARIABLES DERIVEES SONT A METTRE A JOUR'
!          WRITE(LU,*) '  -- OU BIEN LA CONFIGURATION NE S''APPLIQUE PAS'
!          WRITE(LU,*) '  A LA DIFFERENTIATION ALGORITHMIQUE.'
!        ELSEIF(LNG.EQ.2) THEN
!          WRITE(LU,*) 'WARNING: UNKNOWN DIFFERENTIATED VARIABLE ',IVAR
!          WRITE(LU,*) '  UPDATE EITHER SUBROUTINE AD_GET_GAIA'
!          WRITE(LU,*) '  OR THE NUMBER OF DIFERENTIATED  VARIABLES'
!          WRITE(LU,*) '  -- OR ALGORITHMIC DIFFERENTIATION MIGHT NOT BE'
!          WRITE(LU,*) '  CONFIGURED.'
!        ENDIF
!      END SELECT
!
!-----------------------------------------------------------------------
!
      RETURN
      END SUBROUTINE AD_GET_GAIA
