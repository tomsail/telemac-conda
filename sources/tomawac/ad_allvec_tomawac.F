!                   ****************************
                    SUBROUTINE AD_ALLVEC_TOMAWAC
!                   ****************************
!
     &(IVAR,ADOBJ)
!
!***********************************************************************
! TOMAWAC   V7P2
!***********************************************************************
!
!brief    ASSOCIATE THE PRINOUT ARRAY ADOBJ%R TO THE APPROPRIATE
!         DIFFERENTED VARIBALE.
!
!history  S.E.BOURBAN (HRW)
!+        20/06/2016
!+        V7P2
!+   First version for gradients
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| IVAR       |-->| ONE SPECIFIC DIFERENTIABLE VARIABLE
!| ADOBJ      |<->| MOST LIKELY ONE OF THE VARSOR%ADR(I_ADVAR)%P
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_SPECIAL
      USE DECLARATIONS_TOMAWAC
      USE INTERFACE_TOMAWAC, EX_AD_ALLVEC_TOMAWAC => AD_ALLVEC_TOMAWAC
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
!! COMPAD-DCO  BEGIN  JR2016
!! COMPAD-DCO  original code
!!      SELECT CASE (IVAR)
!!#if defined COMPAD_DCO_T1S
!!      CASE(1)
!!!?        CALL DCO_T1S_GET( SF%R, ADOBJ%R, 1 )
!!        ADOBJ%R => SF%R
!!      CASE(2)
!!!?        CALL DCO_T1S_GET( SB%R, ADOBJ%R, 1 )
!!        ADOBJ%R => SB%R
!!      CASE(3)
!!!?        CALL DCO_T1S_GET( SFR%R, ADOBJ%R, 1 )
!!        ADOBJ%R => SFR%R
!!      CASE(4)
!!!?        CALL DCO_T1S_GET( H%R, ADOBJ%R, 1 )
!!!        ADOBJ%R => H%R%val
!!      CASE(5)
!!!?        CALL DCO_T1S_GET( H%R, ADOBJ%R, 1 )
!!!        ADOBJ%R => H%R%val
!!#endif
!!      CASE DEFAULT
!!        WRITE(LU,*) 'ERROR: UNKNOWN DIFFERENTIATED VARIABLE ',IVAR
!!        WRITE(LU,*) '  UPDATE EITHER SUBROUTINE AD_ALLVEC_TOMAWAC'
!!        WRITE(LU,*) '  OR THE NUMBER OF DIFERENTIATED  VARIABLES'
!!        WRITE(LU,*) '  -- OR ALGORITHMIC DIFFERENTIATION MIGHT NOT BE'
!!        WRITE(LU,*) '  CONFIGURED.'
!!        CALL PLANTE(1)
!!        STOP
!!      END SELECT
!! COMPAD-DCO  END    JR2016
!
!-----------------------------------------------------------------------
!
      RETURN
      END SUBROUTINE AD_ALLVEC_TOMAWAC
