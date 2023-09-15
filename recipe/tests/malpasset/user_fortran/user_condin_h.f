!                   ************************
                    SUBROUTINE USER_CONDIN_H
!                   ************************
!
!
!***********************************************************************
! TELEMAC2D   V7P3
!***********************************************************************
!
!brief    USER INITIALISES THE PHYSICAL PARAMETERS
!
!history  J-M HERVOUET (LNHE)
!+        30/08/2007
!+        V6P0
!+
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_TELEMAC
      USE DECLARATIONS_TELEMAC2D, ONLY:  X, Y, H, ZF, NPOIN
      USE TPXO
      USE OKADA
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER I
      DOUBLE PRECISION DISTAN,X1,X2,Y1,Y2,HD
      EXTERNAL DISTAN
!
!-----------------------------------------------------------------------
!
!     INITIALISATION DES VARIABLES POUR LE CALCUL DE LA SITUATION DU
!     POINT X1,Y1,X2,Y2 POINT DEFINISANT LA DROITE DE LIMITE DE BARRAGE
!     X3,Y3 POINT DEFINISANT LES COORDONNEES D POINT A DROITE DE LIMITE
!     DE
!
      X1 = 4701.183D0
      Y1 = 4143.407D0
      X2 = 4655.553D0
      Y2 = 4392.104D0
!
!     ZERO ELEVATION
      CALL OS('X=0     ' , X=H )
      CALL OS('X=X-Y   ' , X=H , Y=ZF )
!
      DO I=1,NPOIN
        HD = DISTAN(X1,Y1,X2,Y2,X(I),Y(I))
        IF(HD.GT.0.001D0) THEN
          H%R(I) = 100.D0 - ZF%R(I)
        ENDIF
!
!       ZONE DERRIERE LE BARRAGE MAIS QUI N'EST PAS DANS
!       LA RETENUE.
!
        IF((X(I)-4500.D0)**2+(Y(I)-5350.D0)**2.LT.200.D0**2) THEN
          H%R(I) = 0.D0
        ENDIF
!
      ENDDO
!
!-----------------------------------------------------------------------
!
      RETURN
      END
