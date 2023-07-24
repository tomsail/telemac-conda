!#######################################################################
                        SUBROUTINE FRIC3D
     &  (CFWC, NPOIN2, DIRHOU, U_TEL, V_TEL, UWBM)
!     MODIFIED FRICTION COEFFICIENT DUE TO WAVES+ CURRENTS
!      (CHRISTOFFERSEN AND JONSSON THEORY)
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| CFWC           |<--| FRICTION COEFFICIENT DUE TO WAV CURRENT
!| DIRHOU         |-->| WAVE DIRECTION ?????                        
!| NPOIN2         |-->| NUMBER OF POINTS IN 2D MESH
!| U_TEL          |-->| VELOCITY ALONG X 
!| V_TEL          |-->| VELOCITY ALONG Y 
!| UWBM           |-->| VELOCITY ON BOTTOM
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      USE BIEF
      USE DECLARATIONS_TOMAWAC, ONLY : DEPTH, DEUPI , GRAVIT, NF, FREQ 
      USE INTERFACE_TOMAWAC, EX_FRIC3D => FRIC3D

      IMPLICIT NONE
      
!.....VARIABLES IN ARGUMENT
!     """"""""""""""""""""
      INTEGER, INTENT(IN) :: NPOIN2
      TYPE(BIEF_OBJ),   INTENT(IN) :: U_TEL,V_TEL
      TYPE(BIEF_OBJ),   INTENT(INOUT)   :: CFWC
      DOUBLE PRECISION, INTENT(IN) :: UWBM(NPOIN2)
      DOUBLE PRECISION, INTENT(IN) :: DIRHOU(NPOIN2)
!.....LOCAL VARIABLES
!     """""""""""""""""
      DOUBLE PRECISION VITCOU,DIRCOU
      DOUBLE PRECISION OMEGAA
      DOUBLE PRECISION XKN   ,  FW, FC, XKAPPA
      INTEGER          MODEL , LUMES
      INTEGER          JF    , I
      INTEGER          ITERM , ITER  , KONVER
      DOUBLE PRECISION TOLF  , TOLX  , ERRF  , ERRX  , F1MJ, F2MJ,
     &                 DF1DFC, DF1DFW, DF2DFC, DF2DFW, DFC, DFW,
     &                 COEF1 , FCINIT, FWINIT, DET, XJ,
     &                 BETAMJ  , ZETA  , VITNUL
!
!.....VARIABLES LOCALES
!     """""""""""""""""
      DOUBLE PRECISION R, COSDIR, COEFJ ,
     &                 MMM   , MMMDFC, MMMDFW, JJJ   , JJJDFC, JJJDFW,
     &                 SIG   , SIGDFC, SIGDFW, XKA   , XKADFC, XKADFW,
     &                 TAW   , TAWDFC, TAWDFW, COEFTA, COEFKA, COEF2 ,
     &                 AUXI  , COEFN 

!.....VARIABLES LOCALES-FWMOD2

      DOUBLE PRECISION USKA, C1, YO
      DOUBLE PRECISION YN , RAC2, C2,G

      G = SQRT (GRAVIT)
      LUMES = 0
!.....PARAMETRES DE L'ALGORITHME ITERATIF
!     """""""""""""""""""""""""""""""""""
!!!!!!!!!!!!!!!!!!!!!!!!!!comeca da subrotina chris2!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      VITNUL= 1.D-6
      ITERM = 100
      TOLF  = 1.D-8
      TOLX  = 1.D-6
      XKAPPA = 0.40D0
      BETAMJ   = 0.0747D0
      ZETA   = 0.5013D0
      RAC2=DSQRT(2.D0)
      C1  =1.D0/(XKAPPA*RAC2)
!""""""""""""""""""""""""""""""""""""""""""""
!  0  C AFFECTATION DES CONSTANTES DU MODELE.
!=====C======================================

      R      = 0.45D0
      COEFN  = 0.367D0
      DO I=1,NPOIN2
        VITCOU=SQRT(U_TEL%R(I)**2.D0+V_TEL%R(I)**2.D0)
        IF(VITCOU.GE.VITNUL) THEN 
          DIRCOU=ATAN2(U_TEL%R(I),V_TEL%R(I))
        ELSE 
          DIRCOU=0
        ENDIF
        XKN = 11.D0*MAX(DEPTH(I),1.D-9) /EXP(0.41D0*73.D0/G)
        DO JF = 1,NF
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!ATENÇAO!TOU EU A DAR XKN
          OMEGAA=DEUPI*FREQ(JF)
!.....DETERMINATION DES SOLUTIONS INITIALES (SANS EFFET MUTUEL)
!     """""""""""""""""""""""""""""""""""""""""""""""""""""""""
          XJ=(DSQRT(BETAMJ)*UWBM(I)/(XKN*OMEGAA))**(2.D0/3.D0)
          COEF1  = 30.D0/DEXP(1.D0)
          FCINIT = 2.D0*(XKAPPA/DLOG(COEF1*DEPTH(I)/XKN))**2
          IF (UWBM(I).LT.VITNUL.OR.XJ.LT.VITNUL) THEN
            FWINIT=1.D-20
          ELSEIF (XJ.LT.3.47D0) THEN
            FWINIT=2.D0*BETAMJ/XJ
          ELSE
!     FUNCAO FWMOD2!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!11
            USKA=UWBM(I)/(XKN*OMEGAA)
!     
            C2=C1*DLOG(30.D0*XKAPPA*DEXP(-2.D0*ZETA)*USKA/RAC2)
            YO=(USKA/BETAMJ)**(1.D0/3.D0)/RAC2
            DO ITER=1,ITERM
              YN=C2-C1*DLOG(YO)
              IF (DABS(YN-YO).LT.1.D-5) GOTO 205
              YO=YN
            ENDDO
            WRITE(6,*) '/!/ STOP IN  FWSEUL - NO  CONVERGENCE'
            WRITE(6,*) 'Uwbm/(Kn.Omega) = ', USKA,'XKN,OMEGAA',
     &           XKN,OMEGAA,'I',I
            STOP
 205        CONTINUE
            FWINIT=1.D0/(YN*YN)
!FUNCAO final FWMOD2!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
          ENDIF
!
!.....AFFECTATION DE LA SOLUTION DE DEPART
!     """"""""""""""""""""""""""""""""""""
          FC=FCINIT
          FW=FWINIT
!.....ON S'ARRETE A CES SOLUTIONS SI UNE DES DEUX VITESSES EST
!     NULLE OU SI LE RAPPORT DES VITESSES EST TRES GRAND OU PETIT.
!     """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""
          IF ((VITCOU.LT.VITNUL).OR.(UWBM(I).LT.VITNUL)
     &         .OR.(VITCOU/UWBM(I).GT.1.D3).OR.
     &         (UWBM(I)/VITCOU.GT.1.D3)) THEN
            CFWC%R(I)=FC
          ELSE
! 
!.....ALGORITHME ITERATIF DE NEWTON-RAPHSON.
!     """"""""""""""""""""""""""""""""""""""
            ITER=0
 500        CONTINUE
            ITER=ITER+1
!!!!!!!!!!!!!!!!!!!!!!!!!!final da subrotina chris2!!!!!!!!!!!!!!!
!.......CALCUL DES MATRICES ALFA ET BETA POUR X DONNE.
!CJSUBR!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
!=====C
!  1  C CALCUL DE SIGMA  ET DE SES DERIVEES (COMMUN AUX 2 MODELES)
!=====C===========================================================
            SIG    =  (FC/FW)*(VITCOU/UWBM(I))**2
            SIGDFC =  SIG/FC
            SIGDFW = -SIG/FW
!=====C
!  2  C CALCUL DE M      ET DE SES DERIVEES (COMMUN AUX 2 MODELES)
!=====C===========================================================
            COSDIR = DABS(COS(DIRHOU(I)-DIRCOU))
            MMM    = DSQRT(1.D0+SIG*SIG+2.D0*SIG*COSDIR)
            MMMDFC = (SIG+COSDIR)/MMM*SIGDFC
            MMMDFW = (SIG+COSDIR)/MMM*SIGDFW     
!=====C
!  3  C CALCUL DE J      ET DE SES DERIVEES (COMMUN AUX 2 MODELES)
!=====C===========================================================
            COEFJ  = UWBM(I)/(XKN*OMEGAA*DSQRT(2.D0))
            JJJ    = COEFJ*DSQRT(MMM*FW)
            JJJDFC = 0.5D0*JJJ/MMM*MMMDFC
            JJJDFW = 0.5D0*JJJ*(1.D0/FW+1.D0/MMM*MMMDFW)
!=====C
!  4  C SELECTION DU MODELE EN FONCTION DE LA VALEUR DE J
!=====C==================================================
            IF (JJJ.GT.3.47D0) THEN
              MODEL=2
            ELSE
              MODEL=1
            ENDIF
!=====C
!  5  C CALCUL DE DELTAW ET DE SES DERIVEES (SELON LE MODELE CHOISI)
!=====C=============================================================
            IF (MODEL.EQ.1) THEN
              COEFTA = XKN*R*DEUPI/2.D0*DSQRT(BETAMJ*0.5D0)
              TAW    = COEFTA*DSQRT(JJJ)
              TAWDFC = 0.5D0*TAW/JJJ*JJJDFC
              TAWDFW = 0.5D0*TAW/JJJ*JJJDFW
            ELSE
              COEFTA = XKN*COEFN*XKAPPA
              TAW    = COEFTA*JJJ
              TAWDFC = COEFTA*JJJDFC
              TAWDFW = COEFTA*JJJDFW
            ENDIF
!=====C
!  6  C CALCUL DE KA     ET DE SES DERIVEES (SELON LE MODELE CHOISI)
!=====C=============================================================
            IF (MODEL.EQ.1) THEN
              COEFKA = XKAPPA/(BETAMJ*XKN)
              XKA    = 30.D0*TAW*DEXP(-COEFKA*TAW*DSQRT(SIG/MMM))
              XKADFC = XKA*(TAWDFC/TAW-COEFKA*TAW*DSQRT(SIG/MMM)
     &             *(TAWDFC/TAW+0.5D0*SIGDFC/SIG-0.5D0*MMMDFC/MMM))
              XKADFW = XKA*(TAWDFW/TAW-COEFKA*TAW*DSQRT(SIG/MMM)
     &             *(TAWDFW/TAW+0.5D0*SIGDFW/SIG-0.5D0*MMMDFW/MMM))
            ELSE
              AUXI   = DSQRT(SIG/MMM)
              XKA    = XKN*(30.D0*TAW/XKN)**(1.D0-AUXI)
              XKADFC = XKA*( -0.5D0/AUXI*(SIGDFC-SIG*MMMDFC/MMM)/MMM
     &             *DLOG(30.D0*TAW/XKN) + (1.D0-AUXI)*TAWDFC/TAW )
              XKADFW = XKA*( -0.5D0/AUXI*(SIGDFW-SIG*MMMDFW/MMM)/MMM
     &             *DLOG(30.D0*TAW/XKN) + (1.D0-AUXI)*TAWDFW/TAW )
            ENDIF
!=====C
!  7  C CALCUL DE F1     ET DE SES DERIVEES (SELON LE MODELE CHOISI)
!=====C=============================================================
            IF (MODEL.EQ.1) THEN
              F1MJ   = FW -2.D0*BETAMJ*MMM/JJJ
              DF1DFC =      -2.D0*BETAMJ*(MMMDFC/JJJ-JJJDFC*MMM/JJJ**2)
              DF1DFW = 1.D0 -2.D0*BETAMJ*(MMMDFW/JJJ-JJJDFW*MMM/JJJ**2)
            ELSE
              COEF1  = 30.D0*XKAPPA*DEXP(-2.D0*ZETA)
              AUXI   = DLOG(COEF1*JJJ)
              F1MJ   = FW  - 2.D0*MMM*(XKAPPA/AUXI)**2
              DF1DFC =     - 2.D0*XKAPPA**2*(MMMDFC/AUXI**2 
     &             - 2.D0*MMM/JJJ*JJJDFC/AUXI**3)
              DF1DFW = 1.D0 - 2.D0*XKAPPA**2*(MMMDFW/AUXI**2 
     &             - 2.D0*MMM/JJJ*JJJDFW/AUXI**3)
            ENDIF
!=====C
!  8  C CALCUL DE F2     ET DE SES DERIVEES (COMMUN AUX 2 MODELES)
!=====C===========================================================
            COEF2  = 30.D0*DEPTH(I)/DEXP(1.D0)
            AUXI   = DLOG(COEF2/XKA)
            F2MJ     = DSQRT(2.D0/FC) - AUXI/XKAPPA
            DF2DFC = XKADFC/(XKA*XKAPPA)-1.D0/DSQRT(2.D0*FC**3)
            DF2DFW = XKADFW/(XKA*XKAPPA)

!CJSUBR!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
!.......CALCUL DE L'ERREUR SUR LA FONCTION
!       """"""""""""""""""""""""""""""""""
            ERRF=ABS(F1MJ)+ABS(F2MJ)
            IF (ERRF.LT.TOLF) THEN
              KONVER=1
              GOTO 201
            ENDIF
!
!.......CALCUL DE DX
!       """"""""""""
            DET=DF1DFC*DF2DFW-DF1DFW*DF2DFC
            IF (ABS(DET).LT.1.D-10) THEN
              WRITE(6,*) '/!/ ARRET DANS CJSUBR : DETERMINAT NUL DET='
              STOP
            ENDIF
            DFC=(-F1MJ*DF2DFW+F2MJ*DF1DFW)/DET
            DFW=(-F2MJ*DF1DFC+F1MJ*DF2DFC)/DET
!     
!.......MISE A JOUR DE LA SOLUTION
!       """"""""""""""""""""""""""
            FC=FC+DFC
            FW=FW+DFW
!     
!.......TEST SUR LA VALEUR DE DX
!       """"""""""""""""""""""""
            ERRX=ABS(DFC)+ABS(DFW)
            IF (ERRX.LT.TOLX) THEN
              KONVER=2
              GOTO 201
            ENDIF
!
            IF (ITER.LT.ITERM) GOTO 500
!
            KONVER=0
!
 201        CONTINUE
            IF (LUMES.GT.0) WRITE(LUMES,2000) KONVER,ITER,FC,FW
 2000       FORMAT(
     &     'CONVERGENCE ',I1,' APRES',I5,' ITERATIONS   =>  FC =',
     &     E11.4,'  ET FW =',E11.4)
          ENDIF
        ENDDO
      ENDDO
      RETURN
      END

