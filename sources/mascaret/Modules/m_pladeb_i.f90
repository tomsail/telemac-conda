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

module M_PLADEB_I
!***********************************************************************
! PROGICIEL : MASCARET        A. LEBOSSE
!                             P. CHERUBINI
!                             S. PERON
!                             S. MANDELKERN
!
! VERSION : V8P4R0              EDF-CEREMA
!***********************************************************************
   interface

   subroutine  PLADEB   ( &
                          ! Resultats
                    Section , & ! Section sous la cote de debordement
                          ! Donnees
              LimiteInterne , & ! Limite interieur du lit
              LimiteExterne , & ! Limite exterieur du lit
                    NumProf , & ! Numero du profil
                   DXP, DYP , & ! Points geometriques du profil
        FrottParoiVerticale , & ! Conservation du frottement sur les parois verticales
            ImpressionPlani , & ! Impression du planimetrage
               UniteListing , & ! Unite logique listing
                     Erreur  & ! Erreur
                             )

   !***********************************************************************
   !  FONCTION :
   !
   !           PLANIMETRAGE DU LIT MAJEUR LORSQUE LA COTE EST EGALE A LA
   !           COTE DE DEBORDEMENT , EN RIVE DROITE OU EN RIVE GAUCHE
   !
   !-----------------------------------------------------------------------
   !   FICHIERS  ENTREE/SORTIE :   - UniteListing  : IMPRESSION LISTING
   !   -------------------------
   !   SOUS-PROGRAMME APPELANT :   - PLANIM
   !   -------------------------
   !   SOUS-PROGRAMMES APPELES :   - COMPT_CHENAUX
   !   -------------------------
   !
   !   COMMENTAIRES  :
   !   ---------------
   !
   !   DOCUMENTATION EXTERNE :
   !   ---------------------
   !***********************************************************************

   !============================= Declarations ===========================
   use M_PRECISION           ! Type DOUBLE
   use M_PARAMETRE_C         ! Parametres de calcul
   use M_MESSAGE_C           ! Liste des messages d'erreur
   use M_ERREUR_T         ! Definition du type ERREUR_T
   use M_TRAITER_ERREUR_I ! Traitement des erreurs
   use M_COMPT_CHENAUX_I  ! Interface de sous-programme

   !.. Implicit Declarations .. 
   implicit none

   !.. Parameters .. 
   integer, parameter :: NB_MAX_CHENAUX = 100

   !.. Formal Arguments .. 
   real(DOUBLE), dimension(2), intent(  out) :: Section
   integer     , dimension(2), intent(in   ) :: LimiteInterne
   integer     , dimension(2), intent(in   ) :: LimiteExterne
   integer                   , intent(in   ) :: NumProf
   real(DOUBLE), dimension(:), intent(in   ) :: DXP, DYP
   logical                   , intent(in   ) :: FrottParoiVerticale
   logical                   , intent(in   ) :: ImpressionPlani
   integer                   , intent(in   ) :: UniteListing
   type(ERREUR_T)            , intent(inout) :: Erreur

   end subroutine PLADEB

   end interface

end module M_PLADEB_I
