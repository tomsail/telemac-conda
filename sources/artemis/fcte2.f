!                 *******************************
                  DOUBLE PRECISION FUNCTION FCTE2
!                 *******************************
!
     &(XX)
!
!***********************************************************************
! ARTEMIS   V6P1                                   31/05/2011
!***********************************************************************
!
!brief    EVALUATE FUNCTION E2(KH) FOR SECOND
!+        ORDER BOTTOM EFFECTS (CURVATURE)
!
!history  C.PEYRARD
!+        31/05/2011
!+        V6P1
!+
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| XX             |-->| K*H
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE INTERFACE_ARTEMIS, EX_FCTE2 => FCTE2
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      DOUBLE PRECISION, INTENT(IN) :: XX
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      DOUBLE PRECISION  CHHX,SHHX , THHX,XXX
      INTRINSIC         SINH, COSH
!
      XXX = 2.D0*XX
!
      IF(XXX.LT.0.001D0) THEN
        FCTE2=-1.D0/12.D0
      ELSE
        SHHX = SINH(XXX)
        CHHX = COSH(XXX)
        THHX = SHHX/CHHX
        FCTE2=(THHX-XXX)*CHHX/(XXX*(SHHX+XXX)**2)
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
