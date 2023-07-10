!     ******************************
      SUBROUTINE FAR_FIELD_POTENTIAL
!     ******************************

     &( X        , Y         , WNB       , PHI_RE   , PHI_IM,
     & DDXPHI_RE , DDYPHI_RE , DDXPHI_IM , DDYPHI_IM)
!
      USE DECLARATIONS_ARTEMIS, ONLY : PI,DEUPI,RADDEG,DEGRAD
      IMPLICIT NONE
!
!.....Variables transmises
!     """"""""""""""""""""
      DOUBLE PRECISION X     , Y     , WNB   , PHI_RE, PHI_IM
!
!CP
      DOUBLE PRECISION DDXPHI_RE , DDYPHI_RE , DDXPHI_IM , DDYPHI_IM
!CP
!.....Variables locales
!     """""""""""""""""
      INTEGER          IX    , II
      DOUBLE PRECISION R     , TETA  , XX    , MODZ  , ARGZ  ,
     &                 AUX1  , AUX2  , CORREC
      DOUBLE PRECISION MODZLU(38), ARGZLU(38)
!
!     A VIRER  !!!!!!!!!    A VIRER  !!!!!!!!
      DOUBLE PRECISION OMEGA , GRAVIT

!CP
      DOUBLE PRECISION PHTETA,DPHTETA,MODTETA,DMODTETA
      DOUBLE PRECISION ANG1,ANG2,AR,BR,RP12,RP32
      DOUBLE PRECISION DDTPHR,DDTPHI,DDRPHR,DDRPHI
      DOUBLE PRECISION DDTMODZ(38),DDTARGZ(38)
!CP


      GRAVIT=9.81D0
      OMEGA=DSQRT(GRAVIT*WNB)

!
      DATA MODZLU   / 0.4275048D+01, 0.4296801D+01, 0.4362129D+01,
     & 0.4471456D+01, 0.4624897D+01, 0.4822242D+01, 0.5062084D+01,
     & 0.5342195D+01, 0.5659060D+01, 0.6008002D+01, 0.6383544D+01,
     & 0.6779524D+01, 0.7189541D+01, 0.7607111D+01, 0.8025802D+01,
     & 0.8439748D+01, 0.8843442D+01, 0.9232073D+01, 0.9601530D+01,
     & 0.9948439D+01, 0.1027029D+02, 0.1056528D+02, 0.1083235D+02,
     & 0.1107121D+02, 0.1128217D+02, 0.1146607D+02, 0.1162433D+02,
     & 0.1175860D+02, 0.1187084D+02, 0.1196319D+02, 0.1203774D+02,
     & 0.1209669D+02, 0.1214191D+02, 0.1217515D+02, 0.1219779D+02,
     & 0.1221109D+02, 0.1221539D+02, 0.1221539D+02/
      DATA ARGZLU   /-0.4036163D+02,-0.4009285D+02,-0.3930756D+02,
     &-0.3806484D+02,-0.3645247D+02,-0.3457508D+02,-0.3254024D+02,
     &-0.3044616D+02,-0.2837278D+02,-0.2638076D+02,-0.2451050D+02,
     &-0.2278505D+02,-0.2121416D+02,-0.1979829D+02,-0.1853172D+02,
     &-0.1740489D+02,-0.1640648D+02,-0.1552499D+02,-0.1474856D+02,
     &-0.1406632D+02,-0.1346808D+02,-0.1294480D+02,-0.1248813D+02,
     &-0.1209076D+02,-0.1174620D+02,-0.1144857D+02,-0.1119283D+02,
     &-0.1097438D+02,-0.1078925D+02,-0.1063398D+02,-0.1050551D+02,
     &-0.1040127D+02,-0.1031883D+02,-0.1025677D+02,-0.1021342D+02,
     &-0.1018777D+02,-0.1017922D+02,-0.1017922D+02/
!
!
      CORREC=-4.0D0*PI
!      correc=1.D0
!
!      WRITE(6,*) 'ON ENTRE CHEZ MICHEL'
!.....Calcul du rayon (R) et de l'angle (TETA, en degres sur [-180;180])
!.....à partir des coordonnees cartesiennes (X et Y) du point considere.
!     """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
      R=DSQRT(X*X+Y*Y)
      TETA=DATAN2(Y,X)*RADDEG
!      WRITE(6,*) 'TETA=',  X  ,  Y ,  TETA


  201 IF (TETA.LT.-180.D0) THEN
        TETA=TETA+360.D0
        GOTO 201
      ENDIF
  202 IF (TETA.GT.180.D0) THEN
        TETA=TETA-360.D0
        GOTO 202
      ENDIF
!
!.....Calcul du module (ModZ) et de l'angle de la fonction de Kochin
!.....H(Teta) par interp. lineaire dans le vecteur des valeurs fournies.
!     """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
      XX=ABS(TETA)/5.0D0
      IX=INT(XX)+1
      XX=XX-INT(XX)
      MODZ=(1.D0-XX)*MODZLU(IX)+XX*MODZLU(IX+1)
      ARGZ=(1.D0-XX)*ARGZLU(IX)+XX*ARGZLU(IX+1)

!      WRITE(6,*) 'KOCHIN=',  ModZ  ,  ArgZ

!CP On recupere les derivees (attention : fonctions en rad)
      DO II=1,38
        IF (II.LE.37) THEN
          DDTMODZ(II)=(MODZLU(II+1)-MODZLU(II))/(5.0D0*DEGRAD)
          DDTARGZ(II)=(ARGZLU(II+1)-ARGZLU(II))/5.0D0
        ELSE
          DDTMODZ(II)=0.0D0
          DDTARGZ(II)=0.0D0
        ENDIF
      ENDDO

      DMODTETA=(1.D0-XX)*DDTMODZ(IX)+XX*DDTMODZ(IX+1)
      DPHTETA =(1.D0-XX)*DDTARGZ(IX)+XX*DDTARGZ(IX+1)
!CP


!
!.....Calcul du potentiel proprement dit (parties reelles et imaginaire)
!     """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
      AUX1=DSQRT(WNB/(DEUPI*R))*MODZ*CORREC
      AUX2 = WNB*R - PI/4.0D0 + ARGZ*DEGRAD
      PHI_RE= AUX1*COS(AUX2)
      PHI_IM= AUX1*SIN(AUX2)
!      WRITE(6,*) 'X   Y   PR   PI=', X  ,  Y  ,  Phi_Re  , Phi_Im

!

!      WRITE(6,*) 'ON SORT DE CHEZ MICHEL'
!      WRITE(6,*) 'ON ENTRE CHEZ CP'


!CP
!.....GRADIENT
!     """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
! -- DDR POTENTIEL
      ANG1=WNB*R - PI/4.0D0
      PHTETA =ARGZ*DEGRAD
      MODTETA=MODZ
      RP12=DSQRT(R)
      RP32=RP12**3


      AR= (-1.D0/(2.D0*RP32))*COS(ANG1) - (WNB/RP12)*SIN(ANG1)
      BR= (-1.D0/(2.D0*RP32))*SIN(ANG1) + (WNB/RP12)*COS(ANG1)

      AUX1  =DSQRT(WNB/DEUPI)*MODTETA*CORREC
      DDRPHR=AUX1*(AR*COS(PHTETA)-BR*SIN(PHTETA))
      DDRPHI=AUX1*(AR*SIN(PHTETA)+BR*COS(PHTETA))

! -- DDTETA POTENTIEL
      ANG2=WNB*R - PI/4.0D0 + PHTETA

      AR= DMODTETA*COS(ANG2)- MODTETA*DPHTETA*SIN(ANG2)
      BR= DMODTETA*SIN(ANG2)+ MODTETA*DPHTETA*COS(ANG2)

      AUX1=DSQRT(WNB/(DEUPI*R))*CORREC

      DDTPHR=AUX1*AR
      DDTPHI=AUX1*BR

! -- GRADIENT DANS LE REPERE ORTHO
      AUX2=TETA*DEGRAD
      DDXPHI_RE=(DDRPHR*COS(AUX2)-(1.D0/R)*DDTPHR*SIN(AUX2))
      DDYPHI_RE=(DDRPHR*SIN(AUX2)+(1.D0/R)*DDTPHR*COS(AUX2))
      DDXPHI_IM=(DDRPHI*COS(AUX2)-(1.D0/R)*DDTPHI*SIN(AUX2))
      DDYPHI_IM=(DDRPHI*SIN(AUX2)+(1.D0/R)*DDTPHI*COS(AUX2))



!CP
!      WRITE(6,*) 'ON SORT DE CHEZ CP'


      RETURN
      END

