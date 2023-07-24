!== Copyright (C) 2000-2022 EDF-CEREMA ==
!
!   This file is part of MASCARET.
!
!   MASCARET is free software: you can redistribute it and/or modify
!   it under the terms of the GNU General Public License as published by
!   the Free Software Foundation, either version 3 of the License, or
!   (at your option) any later version.
!
!   MASCARET is distributed in the hope that it will be useful,
!   but WITHOUT ANY WARRANTY; without even the implied warranty of
!   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
!   GNU General Public License for more details.
!
!   You should have received a copy of the GNU General Public License
!   along with MASCARET.  If not, see <http://www.gnu.org/licenses/>
!

subroutine  SING3       ( &
            AS,BS,CS    , & ! Coeff de l'equ discretisee de la singularite
            ZAM,ZAV     , & !
            BAM,VAM     , & ! largeur au miroir et vitesse amont
            CoteCrete   , & ! Cote de crete de la singularite
            DXP,DYP     , & ! Points X et Y de la crete
            CoeffDebit  , & ! Coefficient de debit
        Epaisseur_seuil , & ! seuil epais ou mince
            Erreur        & ! Erreur
                        )

! *********************************************************************
! PROGICIEL : MASCARET        A. LEBOSSE
!                             P. CHERUBINI
!                             S. MANDELKERN
!
! VERSION : V8P4R0               EDF-CEREMA
! *********************************************************************
!  FONCTION :
!  --------
!
!   CALCUL DES COEFFICIENTS DE L'EQUATION DISCRETISEE D'UN SINGULARITE
!   DEFINI DE MANIERE STANDARD ( SINGULARITE DE TYPE 3 ) .
!
!
!-----------------------------------------------------------------------
!
!   FICHIERS ENTREE/SORTIE :
!   ----------------------
!
!   SOUS PROGRAMME APPELANT :  KSING
!   -------------------------
!   SOUS PROGRAMMES APPELES :  ---
!   -------------------------
!
!   COMMENTAIRES :
!   ------------
!
! . CALCUL DE LA LOI Q(Z) SUR UNE SINGULARITE DONT LA GEOMETRIE DE LA
!   CRETE EST DONNEE PAR DES POINTS
!
! . Q EST > 0 DE L'INDICE 1 VERS L'INDICE 2
!   EN REGIME DENOYE , LA LOI STANDARD EST APPLIQUEE
!   EN REGIME NOYE , LA CORRECTION EST DONNEE PAR LE COEFFICIENT C :
!
! . RH=(ZAVAL-CoteCrete)/(ZAMONT-CoteCrete)
!      ---          RH < 0.8   C= +1
!      ---   0.8  < RH < 1.0   C=  C1*RH**3 + C2*RH**2 + C3 *RH + C4
!
! . AS,BS ET CS SONT LES COEFFICIENTS DE L'EQUATION DISCRETISEE :
!   Q = AS*DZAMONT + BS*DZAVAL + CS
!   DZAMONT REPRESENTE DZ1 SI L'ECOULEMENT A LIEU DANS LE SENS 1->2
!   DZAMONT REPRESENTE DZ2 SI L'ECOULEMENT A LIEU DANS LE SENS 2->1
!   DZAVAL  REPRESENTE DZ2 SI L'ECOULEMENT A LIEU DANS LE SENS 1->2
!   DZAVAL  REPRESENTE DZ1 SI L'ECOULEMENT A LIEU DANS LE SENS 2->1
!
!------------------------------------------------------------------------

   !============================ Declarations ==============================
   use M_PRECISION        ! Type DOUBLE
   use M_PARAMETRE_C      ! Parametres de calcul
   use M_MESSAGE_C        ! Liste des messages d'erreur
   use M_ERREUR_T         ! Definition du type ERREUR_T
   use M_TRAITER_ERREUR_I ! Traitement des erreurs

   implicit none

   !.. Formal Arguments ..
   real(DOUBLE)              , intent(  out) :: AS, BS, CS
   real(DOUBLE)              , intent(in   ) :: ZAM, ZAV
   real(DOUBLE)              , intent(in   ) :: BAM, VAM
   real(DOUBLE), dimension(:), intent(in   ) :: DXP, DYP
   real(DOUBLE)              , intent(in   ) :: CoeffDebit
   real(DOUBLE)              , intent(in   ) :: CoteCrete
   integer                   , intent(in   ) :: Epaisseur_Seuil
   type(ERREUR_T)            , intent(inout) :: Erreur
   !.. Local Scalars ..
   integer      :: N,sens_ecoul 
   real(DOUBLE) :: C1 =   0._DOUBLE ! Coefficients de la fonction
   real(DOUBLE) :: C2 = -25._DOUBLE ! permettant le passage
   real(DOUBLE) :: C3 =  40._DOUBLE ! du regime denoye
   real(DOUBLE) :: C4 = -15._DOUBLE ! au regime noye pour un seuil epais
   !
   real(DOUBLE) :: D1 = 0.385_DOUBLE ! Coefficients de la fonction
   real(DOUBLE) :: D2 = 1.5_DOUBLE   ! permettant le passage du regime
   real(DOUBLE) :: D3 = 0.5_DOUBLE   ! denoye au regime noye pour un
   real(DOUBLE) :: D4 = 0.615_DOUBLE ! seuil mince
   !
   real(DOUBLE)   :: C,CH,CH1,CT,DCDRH,DQDZ,H1,H2,PAS,Q,Q1
   real(DOUBLE)   :: RH,S,S1,VAM1,Z,Z1,ZA,ZAMONT,ZB
   !character(132) :: arbredappel_old

   !.. Intrinsic Functions ..
   intrinsic DMAX1 , DSQRT

   !============================= Instructions =============================
   ! INITIALISATIONS
   ! ---------------
   Erreur%Numero = 0
   !arbredappel_old    = trim(Erreur%arbredappel)
   !Erreur%arbredappel = trim(Erreur%arbredappel)//'=>SING3'

   CT = CoeffDebit * DSQRT( 2._DOUBLE * GPES )

   AS = 0._DOUBLE
   BS = 0._DOUBLE
   CS = 0._DOUBLE

   ZAMONT = DMAX1( ZAM , ZAV )

   H1 = ZAM - CoteCrete
   H2 = ZAV - CoteCrete

   if( H1 <= 0._DOUBLE ) then
      H1 = 0._DOUBLE
   end if

   if( H2 <= 0._DOUBLE ) then
      H2 = 0._DOUBLE
   end if

   Q  = 0._DOUBLE
   Q1 = 0._DOUBLE

   ! calcul du sens de l ecoulement
   if( h1 >= h2 ) then
      sens_ecoul = 1
   else
      sens_ecoul = -1
   end if

   ! CALCUL DU DEBIT
   ! ---------------
   do N = 2 , size(DXP)

      PAS = DXP(N) - DXP(N-1)
      ZA  = DYP(N-1)
      ZB  = DYP(N)
      Z   = ZAMONT - ( ( ZA + ZB ) / 2._DOUBLE )

      if( Z > 0._DOUBLE ) then
         CH = Z
         Q  = Q + CT * PAS * ( CH**1.5_DOUBLE )
      end if

   end do

   if( VAM < 0.0001_DOUBLE ) then
      VAM1 = 0._DOUBLE
   else
      S    = Q / VAM
      S1   = S + BAM * EPS2
      VAM1 = Q / S1
   end if

   do N = 2 , size(DXP)
      PAS = DXP(N) - DXP(N-1)
      ZA  = DYP(N-1)
      ZB  = DYP(N)
      Z1  = ZAMONT + EPS2 - ( ( ZA + ZB ) / 2._DOUBLE )

      if( Z1 > 0._DOUBLE ) then
         CH1 = Z1
         Q1  = Q1 + CT * PAS * ( CH1**1.5_DOUBLE )
      end if
   end do

   DQDZ = ( Q1 - Q ) / EPS2

   ! CALCUL DES COEFFICIENTS
   ! -----------------------
   if( ZAM >= ZAV ) then
      RH = H2 / H1
      !
      ! ECOULEMENT NOYE DE (1) VERS (2)
      !
      If( epaisseur_seuil == 1 ) then
         if( RH >= 0.8_DOUBLE ) then
            !
            ! cas du seuil epais
            !
            C     = C1 * ( RH**3 ) + C2 * ( RH**2 ) + C3 * RH + C4
            DCDRH = 3._DOUBLE * C1 * ( RH**2 ) + 2._DOUBLE * C2 * RH + C3
            AS    = C * DQDZ + Q * DCDRH * ( -H2 / ( H1**2 ) )
            BS    = Q * DCDRH * (1/H1)
            CS    = C * Q
         else
            C  = 1._DOUBLE
            AS = C * DQDZ
            BS = 0._DOUBLE
            CS = C * Q
         endif
      else
         if( H2 > 0._DOUBLE ) then
            !
            !   cas du seuil mince
            !
            C     = ( 1._DOUBLE - RH**D2 )**D1
            DCDRH =  D1 * ( -D2 * RH**D3 ) * ( 1._DOUBLE - RH**D2 )**( -D4 )
            AS    = C * DQDZ + Q * DCDRH * ( -H2 / ( H1**2 ) )
            BS    = Q * DCDRH * (1._DOUBLE / H1 )
            CS    = C * Q
         else
            C  = 1._DOUBLE
            AS = C * DQDZ
            BS = 0._DOUBLE
            CS = C * Q
         endif
      end if

   elseif( ZAV >= ZAM ) then
      RH = H1 / H2
      !
      ! ECOULEMENT NOYE DE (2) VERS (1)
      !
      If( epaisseur_seuil == 1 ) then
         if( RH >= 0.8_DOUBLE ) then
            !
            !   cas du seuil epais
            !
            C     = -( C1 * (RH**3) + C2 * (RH**2) + C3 * RH + C4)
            DCDRH = -( 3._DOUBLE * C1 * (RH**2) + 2._DOUBLE * C2 * RH + C3 )
            AS    = C * DQDZ + Q * DCDRH * ( -H1 / ( H2**2 ) )
            BS    = Q * DCDRH * (1._DOUBLE / H2 )
            CS    = C * Q
         else
            C  = -1._DOUBLE
            AS = C * DQDZ
            BS = 0._DOUBLE
            CS = C * Q
         endif
      else
         if( H1 > 0._DOUBLE ) then
            !
            !   cas du seuil mince
            !
            C     = -(1._DOUBLE -RH**D2 )**D1
            DCDRH =  -D1 * ( -D2 * RH**D3 ) * ( 1._DOUBLE - RH**D2 )**( -D4 )
            AS    = C * DQDZ + Q * DCDRH * ( -H1 / ( H2**2 ) )
            BS    = Q * DCDRH * (1._DOUBLE / H2 )
            CS    = C * Q
         else
            C  = -1._DOUBLE
            AS = C * DQDZ
            BS = 0._DOUBLE
            CS = C * Q
         endif
      end if
   end if

   !Erreur%arbredappel = arbredappel_old

   return

end subroutine SING3
