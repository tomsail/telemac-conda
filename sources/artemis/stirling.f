!                       *******************
                        SUBROUTINE STIRLING
!                       *******************
!
     &(NI,XI,YI,NO,XOSTEP,YO)
!
!***********************************************************************
! ARTEMIS   V7P0                                   June 2014
!***********************************************************************
!
!brief    INTERPOLATES A FUNCTION TO DESIRED RESOLUTION.
!
!history  N.DURAND (HRW)
!+        March 2001
!+
!+   Ported to V7P0
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| NI XI YI       |   | INPUT NUMBER OF DISCRETE VALUES,
!|                |   | INPUT X AND Y VALUES
!| NO XOSTEP YO   |   | OUTPUT NUMBER OF DISCRETE VALUES,
!|                |   | CORRESPONDING OUTPUT RESOLUTION IN X,
!|                |   | AND OUTPUT Y VALUES
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN) :: NI,NO
      DOUBLE PRECISION, INTENT(IN) :: XI(NI),YI(NI)
      DOUBLE PRECISION, INTENT(INOUT) :: XOSTEP,YO(NO)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER          :: I,IV,IVP
      DOUBLE PRECISION :: A1,A2,A3,A4,A5,A6
      DOUBLE PRECISION :: RMIN,R,R2,U,XO
!
      INTRINSIC FLOAT, DABS
!
!-----------------------------------------------------------------------
      XOSTEP = (XI(NI) - XI(1)) / FLOAT(NO-1)
      YO(1)  = YI(1)
      YO(NO) = YI(NI)
!
!-----------------------------------------------------------------------
!     PARAMETERS FOR THE PARABOLA FITTING
!     OUTSIDE DO AND IF LOOPS FOR OPTIMISATION PURPOSES
!     ESTIMATED ONLY ONCE
      A3 = ( YI(3)-YI(1) )/( XI(3)-XI(1) )/( XI(3)-XI(2) ) -
     &     ( YI(2)-YI(1) )/( XI(2)-XI(1) )/( XI(3)-XI(2) )
      A2 = ( YI(2)-YI(1) )/( XI(2)-XI(1) ) - A3*( XI(2)+XI(1) )
      A1 = YI(1) - ( A2 + A3*XI(1) )*XI(1)
!
      A4 = ( YI(NI-2)-YI(NI) )/( XI(NI-2)-XI(NI) )/( XI(NI-2)-XI(NI-1) )
     &   - ( YI(NI-1)-YI(NI) )/( XI(NI-1)-XI(NI) )/( XI(NI-2)-XI(NI-1) )
      A5 = ( YI(NI-1)-YI(NI) )/( XI(NI-1)-XI(NI) )
     &   - A4*( XI(NI-1)+XI(NI) )
      A6 = YI(NI) - ( A5 + A4*XI(NI) )*XI(NI)
!
!-----------------------------------------------------------------------
!     LOOP ON THE INTERPOLATION POINTS IN BETWEEN
      IVP = 2
      DO I = 2,(NO-1)
        RMIN = 1.D0
        XO = XI(1)+FLOAT(I-1)*XOSTEP
!
!-----------------------------------------------------------------------
!       DETERMINES IVP SUCH THAT XO IS CLOSEST TO XI(IVP)
!       R<0 FOR XO BEFORE XI(IVP); R>0 FOR XO AFTER XI(IVP)
        DO IV = IVP,(NI-1)
          R = ( XO - XI(IV) ) / ( XI(IV+1) - XI(IV) )
          IF( DABS(R).LE.DABS(RMIN) ) THEN
            RMIN = R
            IVP = IV
          ENDIF
        ENDDO ! IV = IVP,(NI-1)
        R = ( XO - XI(NI) ) / ( XI(NI) - XI(NI-1) )
        IF( DABS(R).LE.DABS(RMIN) ) THEN
          RMIN = R
          IVP = NI
        ENDIF
        R = RMIN
!
!-----------------------------------------------------------------------
!       XO CLOSEST TO XI(2) :
!       YO COMPUTED FROM PARABOLA USING 1ST 3 POINTS
        IF( IVP.EQ.2 ) THEN
          YO(I) = A1 + A2*XO + A3*XO**2
!
!-----------------------------------------------------------------------
!       XO CLOSEST TO XI(3) OR XI(NI-2) :
!       2ND ORDER STIRLING WITH CONSTANT STEP
        ELSEIF( IVP.EQ.3 .OR. IVP.EQ.NI-2 ) THEN
          R2 = R**2
          YO(I) = ( R2-R )*YI(IVP-1)/2.D0 +
     &        ( 1.D0-R2 )*YI(IVP)  +  ( R2+R )*YI(IVP+1)/2.D0
!
!-----------------------------------------------------------------------
!       XO CLOSEST TO XI(NI-1) OR XI(NI) :
!       YO COMPUTED FROM PARABOLA USING LAST 3 POINTS
        ELSEIF( IVP.EQ.NI-1 .OR. IVP.EQ.NI ) THEN
          YO(I) = A6 + A5*XO + A4*XO**2
!
!-----------------------------------------------------------------------
!       SUBSEQUENT POINTS
!       4TH ORDER STIRLING WITH CONSTANT STEP
        ELSE
          U = R*( (R**2)-1.D0 )/12.D0
          YO(I) =     U*( (R/2.D0)-1.D0 ) * YI(IVP-2) +
     &        (1.D0-R)*(2.D0*U-(R/2.D0))  * YI(IVP-1) +
     &          ( R*(3.D0*U-R) + 1.D0 )   * YI(IVP)   +
     &        (1.D0+R)*(-2.D0*U+(R/2.D0)) * YI(IVP+1) +
     &                U*( (R/2.D0)+1.D0 ) * YI(IVP+2)
        ENDIF
!
!-----------------------------------------------------------------------
!     END OF THE LOOP ON THE INTERPOLATION POINTS
      ENDDO ! I = 2,(NO-1)
!
      RETURN
      END
