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

module M_LEC_HYDRAU_I
!***********************************************************************
! PROGICIEL : MASCARET        S. MANDELKERN
!                             D. POIZAT
!
! VERSION : V8P4R0              EDF-CEREMA
!***********************************************************************
   interface

   subroutine LEC_HYDRAU( &
                          ! Resultats
                        loi , & ! Tableau des lois hydrauliques
                 UniteTemps , & ! unite de temps des chroniques temporelles
                          ! Donnees
                    Fichier , & ! Fichier contenant la loi
          impression_hydrau , & ! Flag d'impression de la loi
               UniteListing , & ! Unite logique fichier listing
                     Erreur & ! Erreur
                            )

   ! .....................................................................
   !  FONCTION : LECTURE DES FICHIERS DE DONNEES HYDRAULIQUE
   !  --------   ET FUSION DES TABLEAUX AVEC CEUX ENTREES EN
   !             ARGUMENT
   !
   !----------------------------------------------------------------------------
   !
   !   FICHIERS ENTREE/SORTIE :       - Fichier listing (UniteListing)
   !   ----------------------         - Fichier des lois
   !
   !   SOUS-PROGRAMME(S) APPELANT(S) : - PRETRAIT
   !   -----------------------------
   !   SOUS-PROGRAMME(S) APPELE(S)   : --
   !   ---------------------------      
   !   COMMENTAIRES :
   !   ------------
   !
   !   DOCUMENTATION EXTERNE :
   !   ---------------------
   !
   !***********************************************************************

   !============================= Declarations ============================
   use M_PRECISION
   use M_PARAMETRE_C
   use M_LOI_T               ! Definition du type LOI_T
   use M_ERREUR_T            ! Definition du type ERREUR_T
   use M_FICHIER_T           ! Definition du type FICHIER_T
   use M_MESSAGE_C           ! Definition des messages d'erreur
   use M_CONSTANTES_CALCUL_C ! Constantes LIMNIGRAMME, HYDROGRAMME, etc.
   use M_TRAITER_ERREUR_I    ! Inteface generique de gestion des erreurs
   use M_LIRE_CHAINE_S       ! Lecture des lignes commentaires

   !.. Declarations explicites ..
   implicit none

   !.. Arguments ..
   type(LOI_T)                  , intent(inout) :: loi
   integer                      , intent(  out) :: UniteTemps
   type(FICHIER_T)              , intent(in   ) :: fichier
   logical                      , intent(in   ) :: impression_hydrau
   integer                      , intent(in   ) :: UniteListing
   type(ERREUR_T) ,               intent(inout) :: Erreur

   end subroutine LEC_HYDRAU

   end interface

end module M_LEC_HYDRAU_I
