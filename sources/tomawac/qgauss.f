!                   ***************
                    FUNCTION QGAUSS
!                   ***************
!
     &( B     , N     , A     , XM    )
!
!***********************************************************************
! TOMAWAC   V6P1                                   23/06/2011
!***********************************************************************
!
!brief    COMPUTES THE INTEGRAL (0 TO INFINITY) OF THE FUNCTION
!+                GIVEN BY 'FONCRO', USING GAUSS QUADRATURES.
!
!history  F. BECQ (EDF/DER/LNH)
!+        26/03/96
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
!+        23/06/2011
!+        V6P1
!+   Translation of French names of the variables in argument
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| A              |-->| PARAMETER A OF THE FUNCTION TO BE INTEGRATED
!| B              |-->| PARAMETER B OF THE FUNCTION TO BE INTEGRATED
!| N              |-->| EXPONENT N OF THE FUNCTION TO BE INTEGRATED
!| XM             |-->| PARAMETER M OF THE FUNCTION TO BE INTEGRATED
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE INTERFACE_TOMAWAC, EX_QGAUSS => QGAUSS
      IMPLICIT NONE
!
!     VARIABLES IN ARGUMENT
!     """""""""""""""""""""
      INTEGER, INTENT(IN)    ::  N
      DOUBLE PRECISION QGAUSS
      DOUBLE PRECISION, INTENT(IN)    :: B , A , XM
!
!     LOCAL VARIABLES
!     """"""""""""""""""
      INTEGER  J     , I     , NFOIS
      DOUBLE PRECISION :: XB    , XR    , DX    , DA    , SS    , W(5)
      DOUBLE PRECISION :: A1    , A2    , A3    , Y2    , X(5)
      PARAMETER ( X = (/ .1488743389D0,.4333953941D0,.6794095682D0,
     &                   .8650633666D0,.9739065285D0 /) )
      PARAMETER ( W = (/ .2955242247D0,.2692667193D0,.2190863625D0,
     &                   .1494513491D0,.0666713443D0 /) )
!
      NFOIS = 1
!
      CALL BORNES
     &( B     , N     , A     , XM    , A2    , A3    )
      QGAUSS = 0.D0
      DA = (A3-A2)/DBLE(NFOIS)
!
      DO I=1,NFOIS
        A1 = A2
        A2 = A2+DA
        XB = 0.5D0*(A1+A2)
        XR = 0.5D0*(A2-A1)
        SS = 0.D0
        DO J=1,5
          DX = XR*X(J)
          SS = SS + W(J)*(FONCRO(XB+DX,B,N,A,XM)
     &                   +FONCRO(XB-DX,B,N,A,XM))
        ENDDO
        Y2 = XR*SS
        QGAUSS = QGAUSS + Y2
      ENDDO
!
      RETURN
      END
