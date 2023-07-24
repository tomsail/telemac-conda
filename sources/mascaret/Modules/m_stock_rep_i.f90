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

module M_STOCK_REP_I
!***********************************************************************
! PROGICIEL : MASCARET     A. LEBOSSE
!                          P. CHERUBINI
!                          S. PERON
!                          S. MANDELKERN
!
! VERSION : V8P4R0             EDF-CEREMA
!***********************************************************************
   interface
   subroutine STOCK_REP ( X               , &
                          ZREF            , &
                          RGC  , RDC      , &
                          CF1  , CF2      , &
                          Z               , &
                          Q               , &
                          Q1   , Q2       , &
                          DebitFlux       , &
                          S1   , S2       , &
                          B1   , B2       , &
                          BS              , &
                          P1   , P2       , &
                          RH1  , RH2      , &
                          FR   , BETA     , &
                          TAUF            , &
                          Y    , HMOY     , &
                          Q2G  , Q2D      , &
                          VOL  , VOLS     , &
                          CHARG           , &
                          SS              , &
                          V1   , V2       , &
                          ZMAX , TZMAX    , &
                          VZMAX           , &
                          ZMIN , TZMIN    , &
                          V1MIN, V1MAX    , &
                          BMAX            , &
                          TOND            , &
                          QMAX , TQMAX    , &
                          EMAX            , &
                          ZVRAI , QVRAI   , &
                          XFRON           , &
                          Qdev            , &
                          TEMPS           , &
! Modele
                          Connect         , &
! Parametre
                          FichierResultat , &
                          VarSto          , &
                          SectionSto      , &
                          Erreur          )

   ! .....................................................................
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
   !   COMMENTAIRES :
   !   ------------
   !                     Comment rajouter une variable a stocker ?
   !                     -----------------------------------------
   !
   !   Soit VARNEW le nom de la nouvelle variable a stocker.
   !   A. Il faut modifier 1 ligne :
   !      I. Dans le module M_INDEX_VARIABLE_C :
   !         1. Rajouter 1 unite a la constante NB_TOT_VAR
   !            qui represente le nombre total de variables stockables
   !            `  integer , parameter :: NB_TOT_VAR = ...'
   !   B. Il faut introduire 3 lignes :
   !      I.  Dans le module M_INDEX_VARIABLE_C :
   !          2. Introduire la constante definissant l'indice de la
   !             nouvelle variable dans les tableaux de variables
   !             `  integer , parameter :: VAR_VARNEW = ...'
   !      II. Dans le sous-programme STOCK :
   !          3. Definir l'initialisation des informations constantes
   !             de cette nouvelle variable
   !             a. Si son stokage est obligatoire
   !             b. Son nom long
   !             c. Son nom sur 4 caracteres
   !             d. Le nom de son unite physique
   !             e. La precision souhaitee pour sa valeur numerique
   !             f. Si elle est dependante du temps
   !             Par exemple :
   !             `var_nom(VAR_VARNEW )=VAR_NOM_T(.false.,"Variable Nouvelle","VNEW","m/s   ",3,.true. )'
   !          4. Definir ses valeurs dans la structure contenant les
   !             informations dependantes de la simulation
   !             `gdr(VAR_VARNEW)%Valeur     => ...'
   !             ou l'objet pointe est le tableau des valeurs par section
   !             de cette nouvelle variables.
   !             (Bien entendu le tableau des valeurs par section devra
   !              etre defini, par exemple en le passant en argument
   !              de la routine STOCK)
   !
   !***********************************************************************

   !============================= Declarations ============================

   !.. Modules importes ..
   !----------------------
   use M_PRECISION
   use M_CONNECT_T           ! Definition du type CONNECT_T
   use M_FICHIER_T           ! Definition du type FICHIER_T
   use M_ERREUR_T            ! Definition du type ERREUR_T
   use M_CONSTANTES_CALCUL_C ! Constantes servant a reperer la phase de calcul
   use M_INDEX_VARIABLE_C    ! Constantes servant a reperer les variables
   use M_PARAMETRE_C         ! Epsilon pour la difference de 2 temps
   use M_INIT_VAR_SORTIE_S   ! Initialisation des structures des variables a sortir
   use M_TRAITER_ERREUR_I    ! Traitement des erreurs
   use M_MESSAGE_C           ! Liste des messages d'erreur

   !.. Declarations explicites .. 
   !-----------------------------
   implicit none

   !.. Arguments .. 
   !---------------
   real(DOUBLE)   , dimension(:), pointer :: X
   real(DOUBLE)   , dimension(:), pointer :: ZREF
   real(DOUBLE)   , dimension(:), pointer :: RGC
   real(DOUBLE)   , dimension(:), pointer :: RDC
   real(DOUBLE)   , dimension(:), pointer :: CF1
   real(DOUBLE)   , dimension(:), pointer :: CF2
   real(DOUBLE)   , dimension(:), pointer :: Z
   real(DOUBLE)   , dimension(:), pointer :: Q
   real(DOUBLE)   , dimension(:), pointer :: Q1
   real(DOUBLE)   , dimension(:), pointer :: Q2
   real(DOUBLE)   , dimension(:), pointer :: DebitFlux
   real(DOUBLE)   , dimension(:), pointer :: S1
   real(DOUBLE)   , dimension(:), pointer :: S2
   real(DOUBLE)   , dimension(:), pointer :: B1
   real(DOUBLE)   , dimension(:), pointer :: B2
   real(DOUBLE)   , dimension(:), pointer :: BS
   real(DOUBLE)   , dimension(:), pointer :: P1
   real(DOUBLE)   , dimension(:), pointer :: P2
   real(DOUBLE)   , dimension(:), pointer :: RH1
   real(DOUBLE)   , dimension(:), pointer :: RH2
   real(DOUBLE)   , dimension(:), pointer :: FR
   real(DOUBLE)   , dimension(:), pointer :: BETA
   real(DOUBLE)   , dimension(:), pointer :: TAUF
   real(DOUBLE)   , dimension(:), pointer :: Y
   real(DOUBLE)   , dimension(:), pointer :: HMOY
   real(DOUBLE)   , dimension(:), pointer :: Q2G
   real(DOUBLE)   , dimension(:), pointer :: Q2D
   real(DOUBLE)   , dimension(:), pointer :: VOL
   real(DOUBLE)   , dimension(:), pointer :: VOLS
   real(DOUBLE)   , dimension(:), pointer :: CHARG
   real(DOUBLE)   , dimension(:), pointer :: SS
   real(DOUBLE)   , dimension(:), pointer :: V1
   real(DOUBLE)   , dimension(:), pointer :: V2
   real(DOUBLE)   , dimension(:), pointer :: ZMAX
   real(DOUBLE)   , dimension(:), pointer :: TZMAX
   real(DOUBLE)   , dimension(:), pointer :: VZMAX
   real(DOUBLE)   , dimension(:), pointer :: ZMIN
   real(DOUBLE)   , dimension(:), pointer :: TZMIN
   real(DOUBLE)   , dimension(:), pointer :: V1MIN
   real(DOUBLE)   , dimension(:), pointer :: V1MAX
   real(DOUBLE)   , dimension(:), pointer :: BMAX
   real(DOUBLE)   , dimension(:), pointer :: TOND
   real(DOUBLE)   , dimension(:), pointer :: QMAX
   real(DOUBLE)   , dimension(:), pointer :: TQMAX
   real(DOUBLE)   , dimension(:), pointer :: EMAX
   real(DOUBLE)   , dimension(:), pointer :: ZVRAI
   real(DOUBLE)   , dimension(:), pointer :: QVRAI
   real(DOUBLE)   , dimension(:), pointer :: XFRON
   real(DOUBLE)   , dimension(:), pointer :: Qdev
   real(DOUBLE)                         , intent(in   ) :: TEMPS
! Modele
   type(CONNECT_T),                       intent(in   ) :: Connect
! Parametres
   type(FICHIER_T)                      , intent(in   ) :: FichierResultat
   logical        , dimension(:)        , intent(in   ) :: VarSto
   integer        , dimension(:)        , pointer       :: SectionSto
   type(ERREUR_T)                       , intent(inout) :: Erreur

   end subroutine STOCK_REP

   end interface

end module M_STOCK_REP_I
