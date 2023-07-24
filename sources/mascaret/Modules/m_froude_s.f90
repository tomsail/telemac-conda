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

module M_FROUDE_S
!***********************************************************************
! PROGICIEL : MASCARET
!                     A. LEBOSSE    P. CHERUBINI    S. MANDELKERN
!
! VERSION : V8P4R0              EDF-CEREMA
!***********************************************************************
   contains

   subroutine FROUDE_S ( &
        Froude       , & ! Nombre de froude
        Beta         , & ! Coefficient de repartition des vitesses
        V            , & ! Vitesse dans la section de calcul
        H            , & ! Hauteur d'eau dans la section de calcul
        Section      , & ! Numero de la section de calcul
        Connect      , & ! Table de connectivite
        X            , & ! Abscisses des sections de calcul
        Erreur         & ! Erreur
                      )
! **********************************************************************
!
!   FONCTION :
!   --------
!
!   CALCUL DE LA VALEUR DU NOMBRE DE FROUDE
!
! ----------------------------------------------------------------------
!
!   FICHIERS  ENTREE/SORTIE :
!   -----------------------
!
!   SOUS PROGRAMMES APPELANTS :  - PERMAT (SARAP)
!   --------------------------   - REZO (REZO)
!
!   SOUS PROGRAMMES APPELES :    ---
!   -------------------------
!   COMMENTAIRES :
!   ------------
!                  - CALCUL EN LITS SIMPLE OU COMPOSE, FORMULE ETABLIE
!                    A PARTIR DE LA METHODE DES CARACTERISTIQUES.
!                  - H EST DEFINI PAR H = SECTION MOUILLEE / LARGEUR AU
!                    MIROIR .
! ----------------------------------------------------------------------
!
!============================= Declarations ===========================

   !.. Modules importes ..
   !----------------------
   use M_PRECISION
   use M_PARAMETRE_C         ! Constante GPES
   use M_MESSAGE_C           ! Messages d'erreur
   use M_CONSTANTES_CALCUL_C ! Constantes du calcul (FROUDEMAX)
   use M_ERREUR_T            ! Type ERREUR_T
   use M_CONNECT_T           ! Type CONNECT_T
   use M_TRAITER_ERREUR_I    ! Traitement de l'erreur
   use M_NUM_BIEF_S          ! Calcul du numero du bief d'une section

   !.. Declarations explicites ..
   !-----------------------------
   implicit none

   !.. Arguments ..
   !---------------
   real(DOUBLE)              , intent(  out) :: Froude
   real(DOUBLE)              , intent(in   ) :: Beta
   real(DOUBLE)              , intent(in   ) :: V
   real(DOUBLE)              , intent(in   ) :: H
   integer                   , intent(in   ) :: Section
   type(CONNECT_T)           , intent(in   ) :: Connect
   real(DOUBLE), dimension(:), intent(in   ) :: X
   type(ERREUR_T)            , intent(inout) :: Erreur

   !.. Variables locales ..
   !-----------------------
   !character(132) :: !arbredappel_old

   !============================= Instructions ===========================
   Erreur%Numero = 0
   !arbredappel_old    = trim(!Erreur%arbredappel)
   !Erreur%arbredappel = trim(!Erreur%arbredappel)//'=>FROUDE'

   if( H.GT.EPS3 ) then
      Froude = ( Beta * V ) / dsqrt( ( Beta - W1 ) * Beta * V**2 + GPES * H )
   else
      Froude = 0._DOUBLE
   endif

   return

   end subroutine FROUDE_S

end module M_FROUDE_S
