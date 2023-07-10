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

module M_MAILLER_I
!***********************************************************************
! PROGICIEL : MASCARET       D. ROUGE
!                            S. MANDELKERN
!
! VERSION : V8P4R0              EDF-CEREMA
!***********************************************************************
   interface

   subroutine  MAILLER ( &
                         X , & ! maillage
                  maille_r , & ! mailles reelles
                  maille_e , & ! mailles entieres
                    Profil , & ! tableau des profils
              TypeMaillage , & ! type de calcul du maillage
            impression_geo , & ! test d'impression de la geometrie
              UniteListing , & ! Unite logique fichier listing
                    Erreur & ! Erreur
                       )

   !***********************************************************************
   !  FONCTION :
   !  --------
   !
   !          DEFINITION DES ABSCISSES DES SECTIONS DE CALCUL
   !          SUR L'ENSEMBLE DU FICHIER .
   !
   !----------------------------------------------------------------------
   !
   !   FICHIERS ENTREE/SORTIE :
   !   ----------------------
   !                FichMaillage  : Fichier de definition du maillage
   !                UniteListing  : Fichier listing
   !
   !   SOUS PROGRAMME APPELANT :
   !   -------------------------
   !   SOUS PROGRAMMES APPELES :
   !   -------------------------
   !   COMMENTAIRES :
   !   ------------
   !
   !           DEFINITION DES ABSCISSES DES SECTIONS DE CALCUL
   !
   !   **  TypeMaillage = 1 **   LES SECTIONS SONT LES PROFILS DE DONNEES
   !
   !
   !   **  TypeMaillage = 2 **   LES SECTIONS SONT DEFINIES PAR SERIES
   !
   !                         XD      XF
   !           !     !        !       !
   !       ----!-----!--:--:--!-:-:-:-!---!------------!------
   !  -->      !     !  :  :  ! : : : !
   !  -->      !     !  :  :  ! : : : !
   !       ----!-----!--:--:--!-:-:-:-!---!------------!------
   !       IPX !  0  !    2   !  3    ! 0 !     0
   !
   !
   !         XD   ABSCISSE DE DEPART D'UNE SERIE
   !         XF   ABSCISSE DE FIN D'UNE SERIE
   !        IPX   NOMBRE DE SECTIONS INTERMEDIAIRES
   !
   !   **  TypeMaillage = 3 **   LES SECTIONS SONT DEFINIES UNE A UNE
   !
   !
   !   **  TypeMaillage = 4 **   LES SECTIONS ONT ETE DEFINIES DANS LE CALCUL
   !                       PERMANENT ET SERONT LUES DANS LE FICHIER
   !                       CONTENANT LA LIGNE D'EAU INITIALE
   !                       ( SOUS-PROGRAMME LEC_LIGNE )
   !   **  TypeMaillage = 5 **   LES SECTIONS SONT DEFINIES PAR SERIE.
   !                       ON DONNE ENTRE 2 PROFILS, LA TAILLE MAXIMALE 
   !                       D UNE MAILLE
   !***********************************************************************

   !=========================== Declarations ================================
   use M_PRECISION
   use M_PARAMETRE_C
   use M_CONSTANTES_CALCUL_C ! Type de maillage
   use M_PROFIL_T            ! Definition du type PROFIL_T
   use M_MAILLE_T            ! Definition du type MAILLE_R_T
                             !                 et MAILLE_E_T
   use M_ERREUR_T              ! Type ERREUR_T
   use M_MESSAGE_C           ! Messages d'erreur
   use M_TRAITER_ERREUR_I    ! Interface de traitement des erreurs

   !.. Implicit Declarations .. 
   implicit none

   !.. Formal Arguments .. 
   real(DOUBLE), dimension(:), pointer :: X
   type(MAILLE_R_T), dimension(:), intent(in   ) :: Maille_r
   type(MAILLE_E_T), dimension(:), intent(in   ) :: Maille_e
   type(PROFIL_T)  , dimension(:), intent(in   ) :: Profil
   logical                       , intent(in   ) :: impression_geo
   integer                       , intent(in   ) :: UniteListing
   integer                       , intent(in   ) :: TypeMaillage
   type(ERREUR_T)            , intent(inout) :: Erreur

   end subroutine MAILLER

   end interface

end module M_MAILLER_I
