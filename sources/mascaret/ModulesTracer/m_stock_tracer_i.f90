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

module M_STOCK_TRACER_I
!***********************************************************************
! PROGICIEL : TRACER         S.MANDELKERN
!
! VERSION : V8P4R0              EDF-CEREMA
!***********************************************************************

   interface

   subroutine STOCK_TRACER( X , &
                         ZREF , &
                      QT , AT , &
                     Ctraceur , &
                      nb_trac , &
                        TEMPS , &
                              ! Modele
                      Connect , &
                              ! Parametre
              FichierResultat , &
                    OptionSto , &
                   FormatResu , &
              PhaseSimulation , &
               NumeroPasTemps , &
                       VarSto , &
                   SectionSto , &
                FichierMotCle , &
                       Erreur )

   !***********************************************************************
   !
   !  FONCTION : 
   !  --------
   !
   !             STOCKAGE DES RESULTATS SUR FICHIER
   !
   !-----------------------------------------------------------------------
   !                             VARIABLES LOCALES
   ! .______________________.____._______________________________________________
   ! !    NOM          !TYPE!MODE!                   ROLE
   ! !_________________!____!____!_______________________________________________
   ! ! retour          ! R  !    ! Variable contenant le code de retour de fonctions
   ! ! NbSectEff       ! R  !    ! Dimension de la liste des sections a stocker
   ! ! isec            ! R  !    ! Compteur sur les secteurs
   ! ! SectionStoEff   ! R  !    ! Numeros des sections effectives a sortir (suivant option)
   ! !_________________!____!____!_______________________________________________
   !
   !  MODE: -->(DONNEE NON MODIFIEE),<--(RESULTAT),<-->(DONNEE MODIFIEE)
   !               (ENTREE)              (SORTIE)       (ENTREE/SORTIE)
   !-------------------------------------------------------------------------
   !
   !   FICHIERS ENTREE/SORTIE :       FichierResultat
   !   ----------------------
   !
   !   SOUS-PROGRAMME(S) APPELANT(S) :  Neant
   !   -----------------------------
   !   SOUS-PROGRAMME(S) APPELE(S) :    STO_OPTHYCA, STO_NONPER, STO_PER
   !   ---------------------------      (sous-programmes internes)
   !                                    INIT_VAR_SORTIE_S (sous-programme de module)
   !***********************************************************************

   !============================= Declarations ============================
   !.. Modules importes ..
   !----------------------
   use M_PRECISION
   use M_CONNECT_T           ! Definition du type CONNECT_T
   use M_FICHIER_T           ! Definition du type FICHIER_T
   use M_ERREUR_T            ! Definition du type ERREUR_T
   use M_CONSTANTES_CALCUL_C ! Constantes servant a reperer la phase de calcul
   use M_INDEX_VARIABLE_TRACER_C    ! Constantes servant a reperer les variables
   use M_PARAMETRE_C         ! Epsilon pour la difference de 2 temps
   use M_INIT_VAR_SORTIE_TRACER_S   ! Initialisation des structures des variables a sortir
   use M_TRAITER_ERREUR_I    ! Traitement des erreurs
   use M_MESSAGE_C           ! Liste des messages d'erreur

   !.. Declarations explicites .. 
   !-----------------------------
   implicit none

   !.. Arguments .. 
   !---------------
   real(DOUBLE)   , dimension(:)  , pointer :: X
   real(DOUBLE)   , dimension(:)  , pointer :: ZREF
   real(DOUBLE)   , dimension(:)  , pointer :: QT,AT
   real (DOUBLE)  , dimension(:,:), pointer :: Ctraceur
   integer                        , intent(in   ) :: nb_trac
   real(DOUBLE)                   , intent(in   ) :: TEMPS
   ! Modele
   type(CONNECT_T)                , intent(in   ) :: Connect
   ! Parametres
   type(FICHIER_T)                , intent(in   ) :: FichierResultat
   type(FICHIER_T)                , intent(in   ) :: FichierMotCle
   integer                        , intent(in   ) :: OptionSto
   integer                        , intent(in   ) :: FormatResu
   integer                        , intent(in   ) :: PhaseSimulation, NumeroPasTemps
   logical        , dimension(:)  , intent(in   ) :: VarSto
   integer        , dimension(:)  , pointer       :: SectionSto
   type(ERREUR_T)                 , intent(inout) :: Erreur

   end subroutine STOCK_TRACER

   end interface

end module M_STOCK_TRACER_I
