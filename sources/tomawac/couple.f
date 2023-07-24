!                       ***************
                        FUNCTION COUPLE
!                       ***************
!
     &( XK1   , YK1   , XK2   , YK2   , XK3   , YK3   , XK4   , YK4   )
!
!***********************************************************************
! TOMAWAC   V6P1                                 14/06/2011
!***********************************************************************
!
!brief   FUNCTION CALLED BY PRENL3
!+         IT COMPUTES THE COUPLING COEFFICIENT FOR THE NON-LINEAR
!+         INTERACTION TERM.
!
!history  E. GAGNAIRE-RENOU
!+        04/2011
!+        V6P1
!+   CREATED
!
!history  G.MATTAROLO (EDF - LNHE)
!+        14/06/2011
!+        V6P1
!+   Translation of French names of the variables in argument
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| XK1, YK1       |-->| COUPLING COEFFICIENT FOR QNL4 (GQM METHOD)
!| XK2, YK2       |-->| COUPLING COEFFICIENT FOR QNL4 (GQM METHOD)
!| XK3, YK3       |-->| COUPLING COEFFICIENT FOR QNL4 (GQM METHOD)
!| XK4, YK4       |-->| COUPLING COEFFICIENT FOR QNL4 (GQM METHOD)
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE INTERFACE_TOMAWAC, EX_COUPLE => COUPLE
      USE DECLARATIONS_TOMAWAC, ONLY : PI, GRAVIT                   
      IMPLICIT NONE
!
!.....VARIABLES IN ARGUMENT
!     """""""""""""""""""""
      DOUBLE PRECISION, INTENT(IN)    :: XK1   , YK1   , XK2   , YK2
      DOUBLE PRECISION, INTENT(IN)    :: XK3   , YK3   , XK4   , YK4
      DOUBLE PRECISION COUPLE
!
!.....LOCAL VARIABLES
!     """"""""""""""""""
      DOUBLE PRECISION RK1   , RK2   , RK3   , RK4   , WK1   , WK2
      DOUBLE PRECISION WK3   , WK4   , S12   , S13   , S14   , S23
      DOUBLE PRECISION S24   , S34   , W1P2  , Q12   , W1M3  , Q13
      DOUBLE PRECISION W1M4  , Q14   , DDD   , COEF  , DENO13, NUME13
      DOUBLE PRECISION DENO14, NUME14, ZERO
!
      COEF=PI*GRAVIT*GRAVIT/4.D0
      ZERO=1.D-10
!
      RK1=SQRT(XK1*XK1+YK1*YK1)
      RK2=SQRT(XK2*XK2+YK2*YK2)
      RK3=SQRT(XK3*XK3+YK3*YK3)
      RK4=SQRT(XK4*XK4+YK4*YK4)
!
      WK1=SQRT(RK1)
      WK2=SQRT(RK2)
      WK3=SQRT(RK3)
      WK4=SQRT(RK4)
!
      S12=XK1*XK2+YK1*YK2
      S13=XK1*XK3+YK1*YK3
      S14=XK1*XK4+YK1*YK4
      S23=XK2*XK3+YK2*YK3
      S24=XK2*XK4+YK2*YK4
      S34=XK3*XK4+YK3*YK4
!
      W1P2=SQRT((XK1+XK2)*(XK1+XK2)+(YK1+YK2)*(YK1+YK2))
      W1M3=SQRT((XK1-XK3)*(XK1-XK3)+(YK1-YK3)*(YK1-YK3))
      W1M4=SQRT((XK1-XK4)*(XK1-XK4)+(YK1-YK4)*(YK1-YK4))
      Q12=(WK1+WK2)*(WK1+WK2)
      Q13=(WK1-WK3)*(WK1-WK3)
      Q14=(WK1-WK4)*(WK1-WK4)
!
!.....COMPUTES THE D COEFFICIENT OF WEBB (1978)
!     """"""""""""""""""""""""""""""""""""""
      DDD=2.00D0*Q12*(RK1*RK2-S12)*(RK3*RK4-S34)/(W1P2-Q12)
     &   +0.50D0*(S12*S34+S13*S24+S14*S23)
     &   +0.25D0*(S13+S24)*Q13*Q13
     &   -0.25D0*(S12+S34)*Q12*Q12
     &   +0.25D0*(S14+S23)*Q14*Q14
     &   +2.50D0*RK1*RK2*RK3*RK4
     &   +Q12*Q13*Q14*(RK1+RK2+RK3+RK4)
      DENO13=W1M3-Q13
      NUME13=2.00D0*Q13*(RK1*RK3+S13)*(RK2*RK4+S24)
      IF (ABS(DENO13).LT.ZERO) THEN
        IF (ABS(NUME13).LT.ZERO) THEN
          WRITE(*,*) 'WARNING DANS COUPLE : (1-3) ON A : 0/0 !'
        ELSE
          WRITE(*,*) 'WARNING DANS COUPLE : (1-3) ON A : INFINI !'
        ENDIF
        WRITE(*,*) 'TERME (1-3) NON PRIS EN COMPTE DANS LE CALCUL.'
      ELSE
        DDD=DDD+NUME13/DENO13
      ENDIF
      DENO14=W1M4-Q14
      NUME14=2.00D0*Q14*(RK1*RK4+S14)*(RK2*RK3+S23)
      IF (ABS(DENO14).LT.ZERO) THEN
        IF (ABS(NUME14).LT.ZERO) THEN
          WRITE(*,*) 'WARNING DANS COUPLE : (1-4) ON A : 0/0 !'
        ELSE
          WRITE(*,*) 'WARNING DANS COUPLE : (1-4) ON A : INFINI !'
        ENDIF
        WRITE(*,*) 'TERME (1-4) NON PRIS EN COMPTE DANS LE CALCUL.'
      ELSE
        DDD=DDD+NUME14/DENO14
      ENDIF
!
!.....COMPUTES THE COUPLING COEFFICIENT FOR SPECTRAL DENSITIES EXPRESSED
!.....IN TERMS OF VARIANCE (NOT IN TERMS OF ENERGY)
!     """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
      COUPLE=COEF*DDD*DDD/(WK1*WK2*WK3*WK4)
!
      RETURN
      END
