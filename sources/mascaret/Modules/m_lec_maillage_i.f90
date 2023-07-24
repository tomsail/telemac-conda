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

module M_LEC_MAILLAGE_I
!***********************************************************************
! PROGICIEL : MASCARET        S. MANDELKERN
!
! VERSION : V8P4R0              EDF-CEREMA
!***********************************************************************
   interface

   subroutine LEC_MAILLAGE  ( &
                              X , & ! Tableau des abscisses
                       maille_e , & !
                       maille_r , & !
                   TypeMaillage , & ! Type de calcul du maillage
                        fichier , & ! Fichier du maillage
                   UniteListing , & ! Unite logique fichier listing
                         Erreur & ! Erreur
                             )

   !========================= Declarations ===========================
   use M_PRECISION
   use M_ERREUR_T            ! Type ERREUR_T
   use M_FICHIER_T           ! Type FICHIER_T
   use M_MAILLE_T            ! Types MAILLE_E_T et MAILLE_R_T
   use M_MESSAGE_C           ! Messages d'erreur
   use M_CONSTANTES_CALCUL_C ! Constantes num, phys et info
   use M_LIRE_CHAINE_S       ! Sous programme de lecture de chaine
   use M_TRAITER_ERREUR_I    ! Traitement de l'errreur

   implicit none

   ! Arguments
   real(DOUBLE)      , dimension(:), pointer       :: X
   integer                         , intent(  out) :: TypeMaillage
   type(FICHIER_T)                 , intent(in   ) :: fichier
   type(MAILLE_R_T), dimension(:)  , pointer       :: maille_r
   type(MAILLE_E_T), dimension(:)  , pointer       :: maille_e
   integer                         , intent(in   ) :: UniteListing
   type(ERREUR_T)                  , intent(inout) :: Erreur

   end subroutine LEC_MAILLAGE

   end interface

end module M_LEC_MAILLAGE_I
