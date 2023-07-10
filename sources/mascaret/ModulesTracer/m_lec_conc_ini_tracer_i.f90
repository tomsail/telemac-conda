!== Copyright (C) 2000-2022 EDF-CEREMA ==
!
!   This file is part of MASCARET-TRACER.
!
!   MASCARET-TRACER is free software: you can redistribute it and/or modify
!   it under the terms of the GNU General Public License as published by
!   the Free Software Foundation, either version 3 of the License, or
!   (at your option) any later version.
!
!   MASCARET-TRACER is distributed in the hope that it will be useful,
!   but WITHOUT ANY WARRANTY; without even the implied warranty of
!   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
!   GNU General Public License for more details.
!
!   You should have received a copy of the GNU General Public License
!   along with MASCARET-TRACER.  If not, see <http://www.gnu.org/licenses/>
!

module M_LEC_CONC_INI_TRACER_I
!***********************************************************************
! PROGICIEL : TRACER         S.MANDELKERN - M.LUCK
!
! VERSION : V8P4R0              EDF-CEREMA
!***********************************************************************

   interface

   subroutine LEC_CONC_INI_TRACER( &
                    UniteListing , &
               ImpressionConcIni , &
                  FichierConcIni , &
                               X , & ! Maillage
                    TypeMaillage , & ! Mode de calcul du maillage
                               C , & ! Concentrations initiales des traceurs
                         nb_trac , & ! nombre de traceurs
                         Connect , & ! Table de connectivite du reseau
                          Profil , & ! Profils geometriques
                         unitNum , & ! Unite logique du fichier .xcas
                          Erreur )   ! Erreur

   ! .....................................................................
   !  FONCTION : LECTURE DU FICHIER DES CONCENTRATIONS INITIALES
   !  --------
   !             INTERPOLATION AUX SECTIONS DE CALCUL SI NECESSAIRE
   !             SI TypeMaillage = TYPE_MAILLAGE_PRECEDENT :
   !             DEFINITION DES ABSCISSES DES SECTIONS DE CALCUL .
   !
   !----------------------------------------------------------------------
   !
   !   FICHIERS ENTREE/SORTIE :       - Fichier listing (UniteListing)
   !   ----------------------
   !
   !   SOUS-PROGRAMME(S) APPELANT(S) :  - PRETRAIT_TRACER
   !   -----------------------------
   !   SOUS-PROGRAMME(S) APPELE(S)   :  - LEC_FIC_CONC_INI_TRACER
   !   ---------------------------      - INTERPOLATION_S
   !
   !***********************************************************************

   !============================= Declarations ============================
   use M_PRECISION
   use M_FICHIER_T            ! Definition du type FICHIER_T et UniteListing
   use M_ERREUR_T             ! Definition du type ERREUR_T
   use M_CONNECT_T            ! Type CONNECT_T
   use M_PROFIL_T             ! Type PROFIL_T
   use M_PARAMETRE_C
   use M_MESSAGE_C            ! Definition des messages d'erreur
   use M_MESSAGE_TRACER_C
   use M_CONSTANTES_CALCUL_C  ! Definition des formats de fichiers de sortie
   use M_LEC_FIC_CONC_INI_TRACER_I
   use M_INTERPOLATION_S       ! Interface du sous programme INTERPOLATION_S
   use M_ABS_ABS_S             ! Calcul de l'abscisse absolue
   use M_TRAITER_ERREUR_I      ! Traitement des erreurs
   use M_XCAS_S

   !.. Declarations explicites ..
   implicit none

   !.. Arguments ..
   real(DOUBLE)  , dimension(:)   , pointer     :: X
   real(DOUBLE)  , dimension(:,:) , pointer     :: C
   type(CONNECT_T)              , intent(in   ) :: Connect
   type(PROFIL_T), dimension(:) , intent(in   ) :: Profil
   integer                                      :: nb_trac  ! nombre de traceurs
   type(FICHIER_T)              , intent(inout) :: FichierConcIni
   integer                      , intent(in   ) :: TypeMaillage
   logical                      , intent(in   ) :: ImpressionConcIni
   integer                      , intent(in   ) :: UniteListing
   integer, intent(in)                          :: unitNum
   type(ERREUR_T)               , intent(inout) :: Erreur

   end subroutine LEC_CONC_INI_TRACER

   end interface

end module M_LEC_CONC_INI_TRACER_I
