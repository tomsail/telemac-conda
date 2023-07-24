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

module M_PRETRAIT_TRACER_INTERFACE_I
!***********************************************************************
! PROGICIEL : TRACER         S.MANDELKERN - M.LUCK
!
! VERSION : V8P4R0              EDF-CEREMA
!***********************************************************************

   Interface

   subroutine  PRETRAIT_Tracer_INTERFACE( &
                FichierMotCle , & ! Fichier des mots-cles
                      Connect , & ! Table de connectivite
                       Apport , & ! Apports hydrauliques
                       Profil , & ! Profils geometriques
                            X , & ! Abscisses des sections de calcul
                    Extremite , & ! Extremites libres
                 TempsMaximum , & ! Temps maximum du calcul
                              ! Lecture des parametres de Tracer
                 OptionTracer , & ! Choix d'un calcul avec TRACER
                       Nbtrac , & ! Nombre de traceurs
                     ConsTrac , & ! Constantes pour TRACER
                 FreqCouplage , & ! Frequence de couplage hydraulique/tracer
                              ! Conc init, CL, sources, lois tracer
             Presence_ConcIni , & ! Presence de conditions initiales
                     Cond_Lim , & ! Conditions aux limites
               Sources_tracer , & ! Sources pour le traceur
                    LoiTracer , & ! Lois Tracer (CL ou sources)
           FichiersLoisTracer , & ! Liste de fichiers lois Tracer
                              ! Lecture des parametres de QE
              Modele_Qual_Eau , & ! Modele de QE
                       ParPhy , & ! Parametres de modele de QE
                        Meteo , & ! Donnees meteo
               Fichier_Parphy , & ! Fichier des parametres de QE
                Fichier_Meteo , & ! Fichier meteo
                              ! Impression des parametres et resultats
            FichierResuTracer , & ! Fichier resultats
             FormatResuTracer , &
         FichierListingTracer , & ! Fichier listing
        ImpressionConcListing , & ! Logique pour les impressions
            ImpressionConcIni , & ! Logique pour les impressions initiales
        ImpressionBilanTracer , & ! Logique pour les impressions
                  PasStockage , & ! Pas de stockage  (hydraulique)
                PasImpression , & ! Pas d'impression (hydraulique)
                   Impression , &
                              ! Traitement des erreurs
                       Erreur )

   ! **********************************************************************
   !  FONCTION : LECTURE DU FICHIER CAS PAR APPEL DU LOGICIEL DAMOCLES
   !             POUR LE MODULE TRACER
   ! ----------------------------------------------------------------------
   !
   ! SOUS-PROGRAMME APPELANT : - SUPER_TRACER
   !
   ! **********************************************************************

   !=========================== Declarations ================================
   use M_PRECISION            ! Definition de la precision DOUBLE ou SIMPLE
   use M_CONSTANTES_CALCUL_C  ! Constantes num, phys et info
   use M_PARAMETRE_C
   use M_FICHIER_T
   use M_CONNECT_T            ! Type CONNECT_T : connectivite du reseau
   use M_APPORT_T             ! Definition du type APPORT_T
   use M_PROFIL_T             ! Definition du type PROFIL_T
   use M_EXTREMITE_T          ! Definition du type EXTREMITE_T
   use M_TRAITER_ERREUR_I            ! Traitement de l'errreur
   use M_PARAMETRES_QUALITE_EAU_T    ! Donnees physiques du traceur
   use M_METEO_T                     ! Donnees Meteo
   use M_SOURCE_TRACER_T             ! Sources de traceurs
   use M_LOI_TRACER_T                ! Lois tracer
   use M_CONSTANTES_TRACER_T
   use M_COND_LIM_TRACER_T           ! Conditions aux limites pour les traceurs
   use M_CONSTANTES_CALCUL_TRACER_C
   use M_LEC_CONC_INI_TRACER_I
   use M_LEC_LOI_TRACER_I
   use M_LEC_SOURCE_I
   use M_ERREUR_T                    ! Traitement des erreurs
   use M_MESSAGE_C
   use M_MESSAGE_TRACER_C
   use M_XCAS_S

   !.. Implicit Declarations ..
   implicit none

   !.. Gestion des mots-cles ..
   type(FICHIER_T), intent(inout) :: FichierMotCle
   !.. Variables d'entree (maillage et hydraulique) ..
   type(CONNECT_T) , intent(in   ) :: Connect
   type(APPORT_T) ,dimension(:), intent(in   ) :: Apport
   type(PROFIL_T) ,dimension(:), pointer       :: Profil       ! Profils geometriques
   real(DOUBLE) ,dimension(:), pointer         :: X            ! Maillage
   Type(EXTREMITE_T) , dimension(:) , pointer  :: Extremite
   real(DOUBLE)                , intent(in   ) :: TempsMaximum
   !.. Traceurs ..
   logical                       , intent(  out) :: OptionTracer
   integer                                       :: Nbtrac
   integer                       , intent(  out) :: Modele_Qual_Eau
   integer                       , intent(  out) :: FreqCouplage
   type(CONSTANTES_TRACER_T), dimension(:)  , pointer :: ConsTrac
   type(COND_LIM_TRACER_T)  , dimension(:)  , pointer :: Cond_Lim
   type(SOURCE_TRACER_T)    , dimension(:)  , pointer :: Sources_tracer
   type(LOI_TRACER_T)       , dimension(:)  , pointer :: LoiTracer
   type(PARAMETRES_QUALITE_EAU_T)                     :: ParPhy
   type(METEO_T)                                      :: Meteo
   type(FICHIER_T)           , intent(inout)   :: FichierResuTracer
   type(FICHIER_T)           , intent(inout)   :: FichierListingTracer
   logical                   , intent(  out)   :: Presence_ConcIni
   type(FICHIER_T)           , intent(inout)   :: Fichier_Meteo
   type(FICHIER_T)           , intent(inout)   :: Fichier_Parphy
   logical                   , intent(  out)   :: ImpressionConcListing
   logical                   , intent(  out)   :: ImpressionConcIni
   logical                   , intent(  out)   :: ImpressionBilanTracer
   integer                   , intent(  out)   :: FormatResuTracer
   integer                   , intent(in   )   :: PasStockage
   integer                   , intent(in   )   :: PasImpression
   !.. Traitement des erreurs ..
   type(ERREUR_T)            , intent(inout)   :: Erreur

   ! Arguments specifiques pour l'interface
   type(FICHIER_T),  dimension(:), pointer       :: FichiersLoisTracer
   logical                       , intent(in   ) :: Impression

end subroutine PRETRAIT_TRACER_INTERFACE

   end interface

end module M_PRETRAIT_TRACER_INTERFACE_I
