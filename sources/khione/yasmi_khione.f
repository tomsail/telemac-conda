!                       ***********************
                        SUBROUTINE YASMI_KHIONE
!                       ***********************
     &  (YASMI)
!
!
!***********************************************************************
! TELEMAC2D   V8P1
!***********************************************************************
!
!brieF tells which tracers will have implicit source terms
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| YASMI          |<--| LOGICS FOR IMPLICIT SOURCE TERMS
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_SPECIAL
      USE DECLARATIONS_KHIONE, ONLY : THERMAL_BUDGET, ITGM,
     & NC_FRA, IND_FRA, IND_T
      USE INTERFACE_KHIONE, EX_YASMI_KHIONE => YASMI_KHIONE
!
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
!
      LOGICAL, INTENT(INOUT)::  YASMI(*)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER :: K, J
!
!-----------------------------------------------------------------------
!
      IF(THERMAL_BUDGET) THEN
        IF(ITGM.EQ.2) THEN
          YASMI(IND_T) = .TRUE.
          DO K=1,NC_FRA
            J = IND_FRA+K-1
            YASMI(J) = .TRUE.
          ENDDO
        ENDIF
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
