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

module M_CALC_CONNECT_I
!***********************************************************************
! PROGICIEL : MASCARET        F. MAUREL       N. GOUTAL
!
! VERSION : V8P4R0              EDF-CEREMA
!***********************************************************************
   interface

   subroutine CALC_CONNECT  ( &
     Connect             , & ! Table de connectivite
     NbBief              , & ! Nombre de biefs
     AbscAbsExtDebBief   , & ! Abscisse abs de l'extremite debut du bief
     AbscAbsExtFinBief   , & ! Abscisse abs de l'extremite debut du bief
     X                   , & ! Abscisses de sections de calcul
     NbNoeud             , & ! Nombre de noeuds
     NbExtNoeud          , & ! Nombre d'extremite relie a chaque noeud
     ExtDebBief          , & ! Numero de l'extremite debut de chaque bief
     ExtFinBief          , & ! Numero de l'extremite fin de chaque bief
     ExtNoeud            , & ! Numero d'extremite lie a un noeud
     NbExtLibre          , & ! Nombre d'extremites libres
     NumExtLibre         , & ! Numero d'extremite libre
     Erreur                & ! Erreur
                         )

   !========================= Declarations ===========================

   use M_PRECISION
   use M_CONNECT_T           ! Type CONNECT
   use M_ERREUR_T            ! Type ERREUR_T
   use M_PROFIL_T            ! Type  PROFIL_T
   use M_MESSAGE_C           ! Messages d'erreur
   use M_CONSTANTES_CALCUL_C ! Constantes num, phys et info
   use M_TRAITER_ERREUR_I    ! Traitement de l'errreur
   use M_XINDIC_S            ! Calc de l'indice corresp a une absc

   implicit none

   ! Arguments
   type(CONNECT_T)                   , intent(  out) :: Connect
   integer                           , intent(in   ) :: NbBief
   real(DOUBLE)      , dimension(:)  , pointer       :: AbscAbsExtDebBief
   real(DOUBLE)      , dimension(:)  , pointer       :: AbscAbsExtFinBief
   real(DOUBLE)      , dimension(:)  , intent(in   ) :: X
   integer                           , intent(in   ) :: NbNoeud
   integer           , dimension(:)  , pointer       :: NbExtNoeud
   integer           , dimension(:)  , pointer       :: ExtDebBief
   integer           , dimension(:)  , pointer       :: ExtFinBief
   integer           , dimension(:,:), pointer       :: ExtNoeud
   integer                           , intent(in   ) :: NbExtLibre
   integer           , dimension(:)  , pointer       :: NumExtLibre
   type(ERREUR_T)                    , intent(inout) :: Erreur

   end subroutine CALC_CONNECT

   end interface

end module M_CALC_CONNECT_I
