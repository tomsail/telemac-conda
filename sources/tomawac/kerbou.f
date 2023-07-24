!                   ********************************
                    DOUBLE PRECISION FUNCTION KERBOU
!                   ********************************
!
     &( XK1   , XK2   , FREQ1  , FREQ2  , DEPTH , TETA1 , TETA2 )
!
!***********************************************************************
! TOMAWAC   V6P3                                   20/06/2011
!***********************************************************************
!
!brief    COMPUTES THE COUPLING COEFFICIENT.
!
!history  EDF/DER/LNH
!+        11/06/98
!+        V1P1
!+
!
!history  N.DURAND (HRW), S.E.BOURBAN (HRW)
!+        13/07/2010
!+        V6P0
!+   Translation of French comments within the FORTRAN sources into
!+   English comments
!
!history  N.DURAND (HRW), S.E.BOURBAN (HRW)
!+        21/08/2010
!+        V6P0
!+   Creation of DOXYGEN tags for automated documentation and
!+   cross-referencing of the FORTRAN sources
!
!history  G.MATTAROLO (EDF - LNHE)
!+        20/06/2011
!+        V6P1
!+   Translation of French names of the variables in argument
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| DEPTH          |-->| WATER DEPTH
!| FREQ1          |-->| FREQUENCY OF COMPONENT 1
!| FREQ2          |-->| FREQUENCE OF COMPONENT 2
!| TETA1          |-->| DIRECTION OF COMPONENT 1
!| TETA2          |-->| DIRECTION OF COMPONENT 2
!| XK1            |-->| WAVE NUMBER OF COMPONENT 1
!| XK2            |-->| WAVE NUMBER OF COMPONENT 2
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE DECLARATIONS_TOMAWAC, ONLY : DEUPI,GRAVIT
!
      USE INTERFACE_TOMAWAC, EX_KERBOU => KERBOU
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      DOUBLE PRECISION, INTENT(IN) :: XK1,XK2,FREQ1,FREQ2,TETA1,TETA2
      DOUBLE PRECISION, INTENT(IN) :: DEPTH
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      DOUBLE PRECISION VAR1,VAR2,VAR3,VAR4,DANG
!
!-----------------------------------------------------------------------
!
      VAR1  = XK1**2
      VAR2  = XK2**2
      VAR3  = XK1*XK2
      VAR4  = DEUPI**2*FREQ1*FREQ2
      DANG  = COS(TETA1-TETA2)
!
      KERBOU = GRAVIT*0.5D0*(VAR1+VAR2+2.D0*VAR3*DANG) +
     &         (VAR4/VAR3)*((VAR1+VAR2)*DANG+VAR3*(1.D0+DANG**2))/DEPTH
!
!-----------------------------------------------------------------------
!
      RETURN
      END
