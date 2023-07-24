!                 *******************************
                  DOUBLE PRECISION FUNCTION FCTE1
!                 *******************************
!
     &(XX)
!
!***********************************************************************
! ARTEMIS   V6P1                                   31/05/2011
!***********************************************************************
!
!brief    EVALUATE FUNCTION E1(KH) FOR SECOND
!+        ORDER BOTTOM EFFECTS (GRADIENT)
!
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
      USE INTERFACE_ARTEMIS, EX_FCTE1 => FCTE1
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      DOUBLE PRECISION, INTENT(IN) :: XX
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      DOUBLE PRECISION SHHX , SHH2X , CHHX ,XXX
      INTRINSIC          SINH, COSH
!
      XXX=2.D0*XX
!
      IF(XXX.LT.0.001D0) THEN
        FCTE1=-1.D0/6.D0
      ELSE
        SHHX  = SINH(XXX)
        SHH2X = SINH(2.D0*XXX)
        CHHX  = COSH(XXX)
        FCTE1=3.D0*( (CHHX-1.D0)**2 + 2.D0 )*(XXX + 2.D0*SHHX)*XXX
        FCTE1=FCTE1 +  SHHX*(XXX**4/SHHX +4.D0*XXX**3 -9.D0*SHH2X)
        FCTE1=FCTE1/(3.D0*(XXX+SHHX)**4)
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
