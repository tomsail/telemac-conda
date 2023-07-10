!                   ************************
                    SUBROUTINE SOURCE_MOMENT
!                   ************************
!
     &(UA,YASMO)
!
!***********************************************************************
! TELEMAC2D
!***********************************************************************
!
!>@brief    COMPUTES SOURCE TERMS FOR MOMENTUM EQUATION (CASE OF FINITE
!!          VOLUMES). ARE CONSIDERED:
!!           - FRICTION TERM.
!!           - CORIOLIS FORCE
!!           - ALL OTHER TERMS ARE COMPUTED BEFORE IN PROSOU_FV
!
!>@history  R. ATA (EDF CHATOU LAB)
!!
!!        CREATION
!!
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!>@param  [in,out]  UA        (H,HU,HV) AT TN+1
!>@param  [in]      YASMO     LOGIC: IF YES CONIDER REMAINING FORCES
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE DECLARATIONS_SPECIAL
      USE BIEF
      USE INTERFACE_TELEMAC2D, EX_SOURCE_MOMENT => SOURCE_MOMENT
      USE DECLARATIONS_TELEMAC2D, ONLY:NPOIN,DT,H
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      LOGICAL, INTENT(IN)             :: YASMO
      DOUBLE PRECISION, INTENT(INOUT) :: UA(3,NPOIN)

!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER IS
      DOUBLE PRECISION AKAP
!
!-----------------------------------------------------------------------
!
      DO IS =1,NPOIN
!
        IF((H%R(IS)   .LE.1.D-12).OR.
     &     (UA(1,IS).LE.1.D-12)) THEN
          AKAP=0.D0
        ELSE
!       POISEUILLE FRICTION LAW
          AKAP= DT*3.D-6/(UA(1,IS)**2.D0)
        ENDIF
!
        AKAP=1.D0/(1.D0+AKAP)
        UA(2,IS) = AKAP*UA(2,IS)
        UA(3,IS) = AKAP*UA(3,IS)
!
      ENDDO
!
!-----------------------------------------------------------------------
!
      RETURN
      END
