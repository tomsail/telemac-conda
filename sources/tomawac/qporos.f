!                   *****************
                    SUBROUTINE QPOROS
!                   *****************

     &( TSTOT , TSDER , F , CG,  LT,XK,
     &  NF    , NDIRE  , NPOIN2   , AMORP )

!***********************************************************************
! TOMAWAC   V7P3
!***********************************************************************
!
!brief    Takes into account the friction due to porous media
!
!history  THIERRY FOUQUET (EDF - LNHE)
!+        27/03/2018
!+        V7P3
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| AMORP          |<->| DUMPING DUE TO POROUS MEDIA
!| CG             |<--| DISCRETIZED GROUP VELOCITY
!| F              |-->| DIRECTIONAL SPECTRUM
!| LT             |-->| TIME STEP
!| NF             |-->| NUMBER OF FREQUENCIES
!| NDIRE          |-->| NUMBER OF DIRECTIONS
!| NPOIN2         |-->| NUMBER OF POINTS IN 2D MESH
!| TSDER          |<->| DERIVED PART OF THE SOURCE TERM CONTRIBUTION
!| TSTOT          |<->| TOTAL PART OF THE SOURCE TERM CONTRIBUTION
!| XK             |<--| WAVE NUMBER
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!

      USE BIEF
      USE DECLARATIONS_TOMAWAC, ONLY: DEUPI, GRAVIT, FREQ, DEPTH, X, Y,
     &                                WAC_FILES, WACZON
!
      USE INTERFACE_TOMAWAC, EX_POROS => QPOROS
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      INTEGER, INTENT(IN)             :: NF, NDIRE, NPOIN2, LT
      DOUBLE PRECISION, INTENT(INOUT) :: AMORP(NPOIN2,NF)
      DOUBLE PRECISION, INTENT(INOUT) :: TSTOT(NPOIN2,NDIRE,NF)
      DOUBLE PRECISION, INTENT(INOUT) :: TSDER(NPOIN2,NDIRE,NF)
      DOUBLE PRECISION, INTENT(INOUT) :: CG(NPOIN2,NF)
      DOUBLE PRECISION, INTENT(INOUT) :: XK(NPOIN2,NF)
      DOUBLE PRECISION, INTENT(IN)    :: F(NPOIN2,NDIRE,NF)
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

      INTEGER N
! THe complexe dispersive function and its derivative
      COMPLEX(KIND=R8) FONC, DERFONC, SECONDFONC, DERTROIS
      EXTERNAL FONC, DERFONC, SECONDFONC, DERTROIS
! ALPHA MIGHT BE READ IN A FILE OR SET BY POLYNOME
      DOUBLE PRECISION GAMMA, ALPHA, INIT
      COMPLEX(KIND=R8) X0, X1, X2, X3, PHI, PHI1
      COMPLEX(KIND=R8) LPRIM, INVLPRIM, LSECOND, LTIERCE
      COMPLEX(KIND=R8) TEMP
      COMPLEX(KIND=R8) FPRIM, FSECOND
      DOUBLE PRECISION TAU, COEFF, CA, S, HW
      INTEGER IP, JF, JP, POLY
      INTEGER,SAVE:: NPOLY
      INTEGER, ALLOCATABLE,DIMENSION(:),SAVE :: NSOM
      DOUBLE PRECISION, ALLOCATABLE,SAVE :: XSOM(:,:), YSOM(:,:)
      IF (LT.EQ.1) THEN
!     TAUX DE POROSITE
        TAU=0.8D0
!     COEFFICIENT DE MASSE AJOUTEE
        CA=2.5D0
!     COEFFICIENT D'AMORTISSEMENT
        COEFF=1.D0
!     AUTRE COEFF POROSITE
        S=1+CA*(1.D0-TAU)/TAU
        TEMP=CMPLX(S,1.D0, KIND=R8)
!     COEFFICIENT DE POROSITE
        PHI=TAU*TEMP/(S**2+COEFF**2)
        ALPHA=0.8D0
        PHI1=CMPLX(1.D0,0.D0,KIND=R8)
        CALL READ_POLY(WAC_FILES(WACZON),NPOLY,NSOM,XSOM,YSOM)
        DO IP=1,NPOIN2
          DO JF=1,NF
            AMORP(IP,JF)=0.D0
          ENDDO
          DO POLY=1,NPOLY
            IF (INPOLY(X(IP), Y(IP), XSOM(:,POLY), YSOM(:,POLY),
     &          NSOM(POLY))) THEN
              ALPHA=0.8D0
              HW=(1-ALPHA)*DEPTH(IP)
              DO JF=1,NF
                GAMMA=(DEUPI*FREQ(JF))**2*DEPTH(IP)/GRAVIT
                INIT=1.D0
                DO N=1,10
                  INIT=GAMMA/TANH(INIT)
                ENDDO
                X0=CMPLX(INIT,0.D0, KIND=R8)
!     CALCULATION OF COMPLEXE WAVE NUMBER ACCORDING TO LIOU 2006
                LPRIM=DERFONC(X0,GAMMA,PHI1,ALPHA)
                INVLPRIM=1.D0/LPRIM
                FPRIM=DERFONC(X0,GAMMA,PHI,ALPHA)
                FSECOND=SECONDFONC(X0,GAMMA,PHI,ALPHA)
                LSECOND=SECONDFONC(X0,GAMMA,PHI1,ALPHA)
                LTIERCE=DERTROIS(X0, GAMMA, PHI1,ALPHA)
                X1=-FONC(X0,GAMMA,PHI,ALPHA)*INVLPRIM
                X2=-INVLPRIM*(0.5D0*LSECOND*X1**2+(FPRIM-LPRIM)*X1)
                X3=-INVLPRIM*(LSECOND*X1*X2+(FPRIM-LPRIM)*X2
     &           +0.5D0*(FSECOND-LSECOND)*X1**2+1.D0/6.D0*X1**3*LTIERCE)
!     CALCULATION OF CELERITY FOR THE REAL WATER DEPTH
                GAMMA=(DEUPI*FREQ(JF))**2*HW/GRAVIT
                INIT=DEUPI*FREQ(JF)*HW/SQRT(GRAVIT*HW)
                DO N=1,50
                  INIT=GAMMA/TANH(INIT)
                ENDDO
                CG(IP,JF)=0.5*(1+2*INIT/SINH(2*INIT))
     &               *DEUPI*FREQ(JF)*HW/INIT
                XK(IP,JF)=INIT/DEPTH(IP)

                AMORP(IP,JF)=2.D0*CG(IP,JF)*AIMAG(X0+X1+X2+X3)
              ENDDO             ! NF
            ENDIF               ! INPOLY
          ENDDO                 !POLY
        ENDDO                   !NPOIN2
      ENDIF                     !LT.LT.1
      DO IP=1,NPOIN2
        DO JF=1,NF
          DO JP=1,NDIRE
            TSTOT(IP,JP,JF) = TSTOT(IP,JP,JF)+AMORP(IP,JF)*F(IP,JP,JF)
            TSDER(IP,JP,JF) = TSDER(IP,JP,JF)+AMORP(IP,JF)
          ENDDO
        ENDDO
      ENDDO
      END

!-----------------------------------------------------------------------
      FUNCTION FONC(X, GAMMA, PHI, ALPHA)
!
      IMPLICIT NONE
      COMPLEX(KIND(1.D0)) FONC
      DOUBLE PRECISION, INTENT(IN) :: GAMMA, ALPHA
      COMPLEX(KIND(1.D0)), INTENT(IN) :: X, PHI
      COMPLEX(KIND(1.D0)) TANHCPLX
      EXTERNAL TANHCPLX

      DOUBLE PRECISION BETA
      BETA=1.D0-ALPHA
      FONC=GAMMA-X*TANHCPLX(BETA*X)
     &          -PHI*TANHCPLX(ALPHA*X)*(X-GAMMA*TANHCPLX(BETA*X))
      RETURN
      END
!-----------------------------------------------------------------------
      FUNCTION DERFONC(X, GAMMA, PHI, ALPHA)
!
      IMPLICIT NONE
      COMPLEX(KIND(1.D0)) DERFONC
      DOUBLE PRECISION, INTENT(IN) :: GAMMA, ALPHA
      COMPLEX(KIND(1.D0)), INTENT(IN) :: X, PHI
      COMPLEX(KIND(1.D0)) TANHCPLX,COSHCPLX
      EXTERNAL TANHCPLX,COSHCPLX

      DOUBLE PRECISION BETA
      BETA=1.D0-ALPHA
      DERFONC=-PHI*TANHCPLX(ALPHA*X)
     &            *(1.D0-GAMMA*BETA/COSHCPLX(BETA*X)**2)
     &        -PHI*ALPHA*(X-GAMMA*TANHCPLX(BETA*X))/COSHCPLX(ALPHA*X)**2
     &     -BETA*X/COSHCPLX(BETA*X)**2-TANHCPLX(BETA*X)
      RETURN
      END
!-----------------------------------------------------------------------
      FUNCTION SECONDFONC(X, GAMMA, PHI,  ALPHA)
!
      IMPLICIT NONE
      COMPLEX(KIND(1.D0)) SECONDFONC
      DOUBLE PRECISION, INTENT(IN) :: GAMMA, ALPHA
      COMPLEX(KIND(1.D0)), INTENT(IN) :: X, PHI
      COMPLEX(KIND(1.D0)) TANHCPLX,COSHCPLX
      EXTERNAL TANHCPLX,COSHCPLX

      DOUBLE PRECISION BETA
      BETA=1-ALPHA

      SECONDFONC=
     & -2.D0*PHI*ALPHA*(1.D0-BETA*GAMMA/COSHCPLX(BETA*X)**2)
     &                          /COSHCPLX(ALPHA*X)**2
     &        -2.D0*PHI*GAMMA*BETA**2*TANHCPLX(ALPHA*X)*TANHCPLX(BETA*X)
     &                          /COSHCPLX(BETA*X)**2
     &   +2.D0*ALPHA**2*PHI*(X-GAMMA*TANHCPLX(BETA*X))*TANHCPLX(ALPHA*X)
     &     /COSHCPLX(ALPHA*X)**2
     &  +(2.D0*X*BETA**2*TANHCPLX(BETA*X)-2.D0*BETA)/COSHCPLX(BETA*X)**2

      RETURN
      END
!-----------------------------------------------------------------------
      FUNCTION DERTROIS(X, GAMMA, PHI,  ALPHA)
!
      IMPLICIT NONE
      COMPLEX(KIND(1.D0)) DERTROIS
      DOUBLE PRECISION, INTENT(IN) :: GAMMA, ALPHA
      COMPLEX(KIND(1.D0)), INTENT(IN) :: X, PHI
      COMPLEX(KIND(1.D0)) TANHCPLX,COSHCPLX
      EXTERNAL TANHCPLX,COSHCPLX

      DOUBLE PRECISION BETA
      BETA=1.D0-ALPHA

      DERTROIS=
     & -4.D0*PHI*ALPHA*BETA*GAMMA*(BETA*TANHCPLX(BETA*X)
     &                              +ALPHA*TANHCPLX(ALPHA*X))
     &                       /(COSHCPLX(ALPHA*X)**2*COSHCPLX(BETA*X)**2)
     &       +4.D0*PHI*ALPHA**2*TANHCPLX(ALPHA*X)/(COSHCPLX(ALPHA*X)**2)
     &           -2.D0*PHI*GAMMA*BETA**2/(COSHCPLX(BETA*X)**2)*
     &              ( ALPHA*TANHCPLX(BETA*X)/(COSHCPLX(ALPHA*X)**2)
     &                +BETA*TANHCPLX(ALPHA*X)/(COSHCPLX(BETA*X)**2)
     &                -2.D0*BETA*TANHCPLX(ALPHA*X)*TANHCPLX(BETA*X)**2 )
     &           +2.D0*PHI*ALPHA**2/(COSHCPLX(ALPHA*X)**2)*
     &       ( (1.D0-GAMMA*BETA/(COSHCPLX(BETA*X)**2))*TANHCPLX(ALPHA*X)
     &                +(X-GAMMA*TANHCPLX(BETA*X))*
     &                 (ALPHA/(COSHCPLX(ALPHA*X)**2)
     &                  -2.D0*ALPHA*TANHCPLX(ALPHA*X)**2) )
     &            +(2.D0*X*BETA**3/(COSHCPLX(BETA*X)**2)
     &               +6.D0*BETA**2*TANHCPLX(BETA*X)
     &               -4.D0*X*BETA**3*TANHCPLX(BETA*X)**2
     &              )/(COSHCPLX(BETA*X)**2)
      RETURN
      END
!-----------------------------------------------------------------------
      FUNCTION TANHCPLX(Z)
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
      COMPLEX(KIND(1.D0)) TANHCPLX,III
      COMPLEX(KIND(1.D0)), INTENT(IN) :: Z

      III = CMPLX(0.D0,1.D0,KIND=R8)

      TANHCPLX= (EXP(III*Z)+EXP(-III*Z))/(EXP(III*Z)-EXP(-III*Z))
      RETURN
      END
!-----------------------------------------------------------------------
      FUNCTION COSHCPLX(Z)
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
      COMPLEX(KIND(1.D0)) COSHCPLX,III
      COMPLEX(KIND(1.D0)), INTENT(IN) :: Z

      III = CMPLX(0.D0,1.D0,KIND=R8)

      COSHCPLX= (EXP(III*Z)+EXP(-III*Z))*0.5D0
      RETURN
      END
