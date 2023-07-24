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

module M_LEC_GEOM_CASIER2_I
!***********************************************************************
! PROGICIEL : MASCARET       C. RISSOAN
!
! VERSION : V8P4R0              EDF-CEREMA
!***********************************************************************
   interface

   subroutine LEC_GEOM_CASIER2( &
                           Casier , & ! Tableau des casiers
                          Fichier , & ! Fichier contenant les casiers
                           Option , &
                           Erreur &   ! Erreur
                                )

   ! .....................................................................
   !  FONCTION : LECTURE DES FICHIERS DE DONNEES HYDRAULIQUE
   !  --------   ET FUSION DES TABLEAUX AVEC CEUX ENTREES EN
   !             ARGUMENT
   !
   !----------------------------------------------------------------------------
   !
   !   FICHIERS ENTREE/SORTIE :       - Fichier listing (UniteListing)
   !   ----------------------         - Fichier des profils
   !
   !   SOUS-PROGRAMME(S) APPELANT(S) : - LEC_GEOM
   !   -----------------------------
   !   SOUS-PROGRAMME(S) APPELE(S)   : - LIRE_CHAINE_S (module)
   !   ---------------------------     - DECODER_GEOM_CASIER
   !   COMMENTAIRES :
   !   ------------
   !
   !   DOCUMENTATION EXTERNE :
   !   ---------------------
   !
   !***********************************************************************

   !============================= Declarations ============================
   use M_PRECISION
   use M_FICHIER_T                ! Definition du type FICHIER_T
   use M_CASIER_T                 ! Definition du type PROFIL_T
   use M_ERREUR_T                 ! Definition du type ERREUR_T
   use M_MESSAGE_CASIER_C         ! Definition des messages d'erreur
   use M_TRAITER_ERREUR_CASIER_I
   use M_DECODER_GEOM_CASIER_I    ! Interface de  sous-programme
   use M_LIRE_CHAINE_S            ! lecture de lignes de commentaire
   use M_PLANIM_CASIER_I

   !.. Declarations explicites ..
   implicit none

   !.. Arguments ..
   type(CASIER_T) , dimension(:), pointer       :: Casier
   type(FICHIER_T)              , intent(in   ) :: fichier
   integer                      , intent(in   ) :: Option
   type(ERREUR_T)               , intent(inout) :: Erreur

   !.. Constantes ..
   integer     , parameter :: LEN_CHAINE = 80

   end subroutine LEC_GEOM_CASIER2

   end interface

end module M_LEC_GEOM_CASIER2_I
