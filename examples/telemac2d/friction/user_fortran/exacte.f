!                   *****************
                    SUBROUTINE EXACTE
!                   *****************
!
     &(H,X,NPOIN)
!
!***********************************************************************
!
! DIRECT INTEGRATION OF THE EQUATION OF GRADUALLY VARIED FLOW
! REF
! 1.Restoration of the contact surface in FORCE-type centred schemes II
! Canestrelli, Toro 2012
! 2.Direct integration of the equation of gradually varied flow
! Venutelli 2004
!
! WARNING: THIS IS THE STEADY STATE SOLUTION
!          HOWEVER IT IS ADDED AT EVERY TIME
!          AND STOCKED IN THE OUTPUT FILE
!
!-----------------------------------------------------------------------
!                             ARGUMENTS
! .________________.____.______________________________________________.
! |      NOM       |MODE|                   ROLE                       |
! |________________|____|______________________________________________|
! |     H          |<-- |  WATER DEPTH                                 |
! |     X          | -->|  ABSCISSAE                                   |
! |   NPOIN        | -->|  NUMBER OF MESH POINTS                       |
! |________________|____|______________________________________________|
!**********************************************************************
!
      USE DECLARATIONS_SPECIAL
      USE DECLARATIONS_TELEMAC2D, ONLY: GRAV
      IMPLICIT NONE
!
      INTEGER I,J,NPOIN
      INTEGER, PARAMETER :: ITMAX = 4000
      DOUBLE PRECISION  H(NPOIN),X(NPOIN)
      DOUBLE PRECISION, PARAMETER :: UNT=1.D0/3.D0
      DOUBLE PRECISION, PARAMETER :: UNDIX=1.D0/10.D0
      DOUBLE PRECISION, PARAMETER :: UNQ=1.D0/4.D0
      DOUBLE PRECISION, PARAMETER :: S=1.8536585365*1.D-5
      DOUBLE PRECISION, PARAMETER :: N=0.0143D0,DOMAIN_L=410000.D0
      DOUBLE PRECISION, PARAMETER :: YOUT_INI=11.6D0
      DOUBLE PRECISION, PARAMETER :: EPSIL = 1.D-5
      DOUBLE PRECISION Q,YC,YN,YOUT,XOUT,DY,ETA,ETAC,ETAT
      DOUBLE PRECISION ALFA1,ALFA2,BETA1,BETA2
      DOUBLE PRECISION F1,F2,F3,F4,Z1,Z2,Z3,Z4
      DOUBLE PRECISION F1T,F2T,F3T,F4T,Z1T,Z2T,Z3T,Z4T
      DOUBLE PRECISION F,G,FT,GT,GAMMA,DXIN
      DOUBLE PRECISION XIN(ITMAX),YIN(ITMAX)
!
      INTRINSIC SQRT,ABS,LOG,ATAN
!
! VARIABLES: S -> BED SLOPE; N -> COEFFICIENT OF MANNING
!-----------------------------------------------------------------------
!
! INITIALIZE X ET YIN
!
      DO I=1,ITMAX
        XIN(I)=1.D10
        YIN(I)=0.D0
      ENDDO
!
! SCALAR FLOWRATE (m2/s) IN THE FLUME
!
      Q=6000.D0/450.D0
!
! CRITICAL WATER DEPTH
!
      YC=(Q**2/GRAV)**UNT
!
! WATER DEPTH FOR UNIFORM REGIME
!
      YN=((Q*N)/SQRT(S))**(3.D0/5.D0)
!
! SUBCRITICAL REGIME: WE START COMPUTATION FROM DOWNSTREAM
!
! WATER DEPTH AND X(FLUME LENGTH) AT THE OUTLET (GIVEN DATA)
!
      YOUT = YOUT_INI
      XOUT = DOMAIN_L
!
! INCREMENT OF WATER DEPTH
!
      DY = 0.0008D0
      ETAC=YC/YN
      YIN(1)=YOUT
      XIN(1)=XOUT
! ALPHAs AND BETAs
      ALFA1=UNDIX*SQRT(0.5D0*(5.D0+SQRT(5.D0)))
      ALFA2=UNDIX*SQRT(0.5D0*(5.D0-SQRT(5.D0)))
!
      BETA1=1.D0-SQRT(5.D0)
      BETA2=1.D0+SQRT(5.D0)
!
      DO 10 I=2,ITMAX
!
        YIN(I)=YOUT-DY
        ETA=YIN(I)/YN
        ETAT=YOUT/YN
!
        F1=2.D0*SQRT(2.D0/(5.D0-SQRT(5.D0)))*(ETA**UNT-UNQ*BETA2)
        F4=2.D0*SQRT(2.D0/(5.D0-SQRT(5.D0)))*(ETA**UNT+UNQ*BETA2)
!
        F2=2.D0*SQRT(2.D0/(5.D0+SQRT(5.D0)))*(ETA**UNT+UNQ*BETA1)
        F3=2.D0*SQRT(2.D0/(5.D0+SQRT(5.D0)))*(ETA**UNT-UNQ*BETA1)
!
        Z1=1.D0+0.5D0*BETA2*ETA**UNT+ETA**(2.D0/3.D0)
        Z4=1.D0-0.5D0*BETA2*ETA**UNT+ETA**(2.D0/3.D0)
!
        Z2=1.D0-0.5D0*BETA1*ETA**UNT+ETA**(2.D0/3.D0)
        Z3=1.D0+0.5D0*BETA1*ETA**UNT+ETA**(2.D0/3.D0)
!
        F1T=2.D0*SQRT(2.D0/(5.D0-SQRT(5.D0)))*(ETAT**UNT-UNQ*BETA2)
        F4T=2.D0*SQRT(2.D0/(5.D0-SQRT(5.D0)))*(ETAT**UNT+UNQ*BETA2)
!
        F2T=2.D0*SQRT(2.D0/(5.D0+SQRT(5.D0)))*(ETAT**UNT+UNQ*BETA1)
        F3T=2.D0*SQRT(2.D0/(5.D0+SQRT(5.D0)))*(ETAT**UNT-UNQ*BETA1)
!
        Z1T=1.D0+0.5D0*BETA2*ETAT**UNT+ETAT**(2.D0/3.D0)
        Z4T=1.D0-0.5D0*BETA2*ETAT**UNT+ETAT**(2.D0/3.D0)
!
        Z2T=1.D0-0.5D0*BETA1*ETAT**UNT+ETAT**(2.D0/3.D0)
        Z3T=1.D0+0.5D0*BETA1*ETAT**UNT+ETAT**(2.D0/3.D0)
!
        F=ALFA1*(ATAN(F1)+ATAN(F4))-ALFA2*(ATAN(F2)+ATAN(F3))+
     &    (1.D0/40.D0)*(BETA1*(LOG(ABS(Z1))-LOG(ABS(Z4)))-
     &    BETA2*(LOG(ABS(Z2))-LOG(ABS(Z3))))-
     &    UNDIX*(LOG(ABS(ETA**UNT-1.D0))-LOG(ABS(ETA**UNT+1.D0)))
!
        G=ALFA2*(ATAN(F1)-ATAN(F4))-ALFA1*(ATAN(F2)-ATAN(F3))
     &    +(1.D0/40.0D0)*(BETA2*(LOG(ABS(Z1))+LOG(ABS(Z4)))
     &    +BETA1*(LOG(ABS(Z2))+LOG(ABS(Z3))))
     &    -UNDIX*(LOG(ABS(ETA**UNT-1.D0))+LOG(ABS(ETA**UNT+1.D0)))
!
        FT=ALFA1*(ATAN(F1T)+ATAN(F4T))-ALFA2*(ATAN(F2T)+ATAN(F3T))
     &    +(1.D0/40.D0)*(BETA1*(LOG(ABS(Z1T))-LOG(ABS(Z4T)))
     &    -BETA2*(LOG(ABS(Z2T))-LOG(ABS(Z3T))))
     &    -UNDIX*(LOG(ABS(ETAT**UNT-1.D0))-LOG(ABS(ETAT**UNT+1.D0)))
!
        GT=ALFA2*(ATAN(F1T)-ATAN(F4T))-ALFA1*(ATAN(F2T)-ATAN(F3T))
     &    +(1.D0/40.0D0)*(BETA2*(LOG(ABS(Z1))+LOG(ABS(Z4)))
     &    +BETA1*(LOG(ABS(Z2T))+LOG(ABS(Z3T))))
     &    -UNDIX*(LOG(ABS(ETAT**UNT-1.D0))+LOG(ABS(ETAT**UNT+1.D0)))
!
        GAMMA=ETA-ETAT+3.D0*(FT-F)+3.D0*(ETAC**3)*(G-GT)
        XIN(I)=XOUT+(YN/S)*GAMMA
        IF(XIN(I).LT.EPSIL) GO TO 10
        XOUT=XIN(I)
        YOUT=YIN(I)
!
10    CONTINUE
!
! LINEAR APPROXIMATION FOR ALL MESH POINTS
!
      DO I=1,NPOIN
!       FIRST OUTPUT POINTS
        IF(ABS(X(I)-DOMAIN_L).LE.EPSIL)THEN
          H(I)=YOUT_INI
          CYCLE
        ENDIF
        DO J=1,ITMAX-1
!
          IF(X(I).LT.XIN(J).AND.X(I).GT.XIN(J+1))THEN
            DXIN=XIN(J)-XIN(J+1)
            H(I)=(1.D0/DXIN)*((XIN(J)-X(I))*YIN(J+1)+
     &                        (X(I)-XIN(J+1))*YIN(J))
            CYCLE
!           DEBUG
            IF(H(I).LE.0.D0)THEN
              WRITE(LU,*)'NEGATIVE H FOR I:',I
              WRITE(LU,*)'H IS EQUAL :',H(I)
              WRITE(LU,*)'BORDED BY: ', XIN(J),XIN(J+1)
              CALL PLANTE(1)
              STOP
            ENDIF
          ENDIF
        ENDDO
      ENDDO
!
!-----------------------------------------------------------------------
!
      RETURN
      END

