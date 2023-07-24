!                 **********************
                  SUBROUTINE SOLVELAMBDA
!                 **********************
!
     &(XK,XUC,XVC,XKX,XKY,XH)
!
!***********************************************************************
! ARTEMIS   V6P3                                     06/2013
!***********************************************************************
!
!brief    RESOLUTION OF DISPERSION EQUATION WITH CURRENT
!+        USING  DICHOTOMIE
!
!code                          ->->
!+      Eq solved on k : omega-k.U = sqrt(gk tanh(kH))
!
!history  D.IDIER / J.PARISI / C.PEYRARD
!+        30/06/2013
!+        V6P3
!+
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| XK             |<-->| K : NOMBRE D'ONDE AU POINT I
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE DECLARATIONS_ARTEMIS
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      DOUBLE PRECISION, INTENT(INOUT) :: XK
      DOUBLE PRECISION, INTENT(IN)    :: XUC,XVC,XH,XKX,XKY
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER          NIT
      DOUBLE PRECISION K10,K20,K30,AK10,AK20,AK30,DELTA2,AK01,AK02,AK03
!
!-----------------------------------------------------------------------
!
      NIT=0
      K10=XK
! Produit scalaire K.U
      IF((XUC*XKX+XVC*XKY).GE.0D0)THEN
        K20=1.D-04*K10
      ELSE
        K20=10.D0*K10
      ENDIF
      K30 =(K10+K20)*0.5D0
      AK01=(XUC*XKX)
      AK02=(XVC*XKY)
      AK03=SQRT(XKX**2+XKY**2)
!
97    CONTINUE
      NIT=NIT+1
      IF (NIT.GT.100000) THEN
        WRITE(LU,*) 'ERREUR : ROUTINE SOLVELAMBDA !        '
        WRITE(LU,*) 'PROBLEME DANS LA DICHOTOMIE  !        '
        WRITE(LU,*) 'NB ITER MAX EXCEEDED         !        '
        WRITE(LU,*) '--------------------------------------'
        WRITE(LU,*) 'ATTENTION : SI LE COURANT EST OPPOSE A'
        WRITE(LU,*) 'LA HOULE, L EQUATION DE DISPERSION N A'
        WRITE(LU,*) 'PAS TOUJOURS UNE SOLUTION             '
        WRITE(LU,*) '--------------------------------------'
        CALL PLANTE(1)
        STOP
      ENDIF
!
      AK10=SQRT(GRAV*K10*TANH(K10*XH))-OMEGA
     & +    (AK01+AK02)*K10/AK03
      AK20=SQRT(GRAV*K20*TANH(K20*XH))-OMEGA
     & +    (AK01+AK02)*K20/AK03
      AK30=SQRT(GRAV*K30*TANH(K30*XH))-OMEGA
     & +    (AK01+AK02)*K30/AK03
!
      IF(AK30*AK20.GE.0.D0)THEN
        K20=K30
      ELSEIF(AK30*AK10.GE.0.D0)THEN
        K10=K30
      ELSE
        WRITE(LU,*) 'ERREUR : ROUTINE SOLVELAMBDA  !       '
        WRITE(LU,*) 'PROBLEME DANS LA DICHOTOMIE   !       '
        WRITE(LU,*) 'LES 2 POINTS SONT DU MEME COTE!       '
        WRITE(LU,*) '--------------------------------------'
        WRITE(LU,*) 'ATTENTION : SI LE COURANT EST OPPOSE A'
        WRITE(LU,*) 'LA HOULE, L EQUATION DE DISPERSION N A'
        WRITE(LU,*) 'PAS TOUJOURS UNE SOLUTION             '
        WRITE(LU,*) '--------------------------------------'
        CALL PLANTE(1)
        STOP
      ENDIF
      DELTA2=ABS(K20-K10)/K20
      K30=(K10+K20)*0.5D0
!
      IF(DELTA2.GT.1D-06)THEN
        GOTO 97
      ELSE
        XK=K30
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
