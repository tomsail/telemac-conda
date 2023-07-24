!                   ***************
                    FUNCTION GAMMLN
!                   ***************
!
     &( XX    , DEUPI )
!
!***********************************************************************
! TOMAWAC   V6P1                                   15/06/2011
!***********************************************************************
!
!brief    COMPUTES THE NATURAL LOGARITHM FOR THE GAMMA FUNCTION
!+                (EULER FUNCTION OF SECOND-KIND).
!
!note     IF XX IS AN INTEGER NOTED N, GAMMA(N) = (N-1)!
!
!reference  "NUMERICAL RECIPES. THE ART OF SCIENTIFIC COMPUTING",
!+                       PRESS ET AL. (1989). (CF. PP 156-157)
!
!history  M. BENOIT
!+        15/11/95
!+        V1P0
!+   CREATED
!
!history  M. BENOIT
!+        07/11/96
!+        V1P2
!+   MODIFIED
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
!+        15/06/2011
!+        V6P1
!+   Translation of French names of the variables in argument
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| DEUPI          |-->| 2.PI
!| XX             |-->| VALUE AT WHICH LOG(GAMMA) IS CALCULATED
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE INTERFACE_TOMAWAC, EX_GAMMLN => GAMMLN
      IMPLICIT NONE
!
!.....VARIABLES IN ARGUMENT
!     """"""""""""""""""""
      DOUBLE PRECISION GAMMLN
      DOUBLE PRECISION,INTENT(IN)    ::  XX    , DEUPI
!
!.....LOCAL VARIABLES
!     """""""""""""""""
      INTEGER  J
      DOUBLE PRECISION STP   , X     , XC    , TMP   , SER   , AUX
      DOUBLE PRECISION COF(6)
!
!
      COF(1)= 76.180091730D0
      COF(2)=-86.505320330D0
      COF(3)= 24.014098220D0
      COF(4)= -1.231739516D0
      COF(5)=  0.001208580D0
      COF(6)= -0.000005364D0
      STP   =  2.506628275D0
!
      IF (XX.LT.1.D0) THEN
        XC=2.D0-XX
      ELSE
        XC=XX
      ENDIF
      X=XC-1.D0
      TMP=X+5.5D0
      TMP=(X+0.5D0)*LOG(TMP)-TMP
      SER=1.D0
      DO J=1,6
        X=X+1.D0
        SER=SER+COF(J)/X
      ENDDO ! J
      GAMMLN=TMP+LOG(STP*SER)
      IF (XX.LT.1D0) THEN
        AUX=0.5D0*DEUPI*(1.D0-XX)
        GAMMLN=LOG(AUX/SIN(AUX))-GAMMLN
      ENDIF
!
      RETURN
      END
