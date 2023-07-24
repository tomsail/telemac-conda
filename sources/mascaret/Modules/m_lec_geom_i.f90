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

module M_LEC_GEOM_I
!***********************************************************************
! PROGICIEL : MASCARET        S. MANDELKERN
!
! VERSION : V8P4R0              EDF-CEREMA
!***********************************************************************
   interface

   subroutine LEC_GEOM( &
                   Profil , & ! Profils geometriques
                   NbBief , &
              ProfDebBief , & ! Premiers profils de chaque bief
              ProfFinBief , & ! Derniers profils de chaque bief
                    Ecart , & ! Ecart entre branches
      FrottParoiVerticale , & ! Frottement sur les parois verticales
                 Prof_Abs , & ! Abscisse absolue
                    Noyau , & ! Noyau de calcul
           impression_geo , & ! Impression de la geometrie
             UniteListing , & ! Unite logique fichier listing
              FichierGeom , & ! Fichier geometrie
               FormatGeom , & ! Format du fichier geometrie
                   Erreur ) ! Erreur

   !***********************************************************************
   !  FONCTION :
   !  --------
   !
   !          LECTURE DU FICHIER GEOMETRIE
   !
   !
   !   FICHIERS ENTREE/SORTIE :
   !   ----------------------
   !                FichierGeo    : Fichier des profils geometriaues
   !                UniteListing  : Fichier listing
   !
   !   SOUS PROGRAMME APPELANT :
   !   -------------------------
   !   SOUS PROGRAMMES APPELES :
   !   -------------------------
   !                LEC_GEOM_V2P0 : LECTURE DES PROFILS AU FORMET LIDO 2.0
   !                LEC_GEOM_V3P0 : LECTURE DES PROFILS AU FORMET LIDO 3.0
   !
   !   COMMENTAIRES :
   !   ------------

   !=========================== Declarations ================================
   use M_PRECISION
   use M_MESSAGE_C           ! Messages d'erreur
   use M_PARAMETRE_C         ! Parametres
   use M_CONSTANTES_CALCUL_C ! formats geometrie disponibles
   use M_PROFIL_T         ! Definition du type PROFIL_T
   use M_FICHIER_T        ! Definition du type FICHIER_T
   use M_ERREUR_T         ! type ERREUR_T
   use M_TRAITER_ERREUR_I ! Module-procedure de traitement des erreurs
   use M_LEC_GEOM_V2P0_I  ! Interface de sous-programme
   use M_LEC_GEOM_V3P0_I  ! Interface de sous-programme
   use M_INTERPOLATION_S  ! 

   !.. Implicit Declarations .. 
   implicit none

   !.. Formal Arguments .. 
   type(PROFIL_T), dimension(:) , pointer       :: Profil
   integer                      , intent(  out) :: NbBief
   integer       , dimension(:) , pointer       :: ProfDebBief
   integer       , dimension(:) , pointer       :: ProfFinBief
   type(FICHIER_T)              , intent(in   ) :: FichierGeom
   logical                      , intent(in   ) :: FrottParoiVerticale
   logical                      , intent(in   ) :: Prof_Abs
   logical                      , intent(in   ) :: impression_geo
   integer                      , intent(in   ) :: UniteListing
   real(DOUBLE)                 , intent(in   ) :: Ecart ! denominateur commun
                                                         ! des abscisses de debut de bief
   integer                      , intent(in   ) :: FormatGeom
   integer                      , intent(in   ) :: Noyau
   type(ERREUR_T)               , intent(inout) :: Erreur

   end subroutine LEC_GEOM

   end interface

end module M_LEC_GEOM_I
