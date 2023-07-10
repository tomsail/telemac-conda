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

module M_QREPAR_I
!***********************************************************************
! PROGICIEL : MASCARET      A. LEBOSSE
!                           P. CHERUBINI
!                           S. PERON
!                           S. MANDELKERN
!
! VERSION : V8P4R0              EDF-CEREMA
!***********************************************************************
   interface

   subroutine QREPAR (   &
        SommeDebitance , & !/RESULTATS/
        ZAval          , &
        NumPassage     , & !/DONNEES MODIFIEES/
        Q              , &
        Z              , &
        ZREF           , & !/DONNEES NON MODIFIEES/
        X              , &
        CF1            , &
        CF2            , &
        IDT            , &
        XDT            , &
        Profil         , &
        Profil_plan    , &
        NumConfluence  , &
        Connect        , &
        ModeleLit      , &
        Epsil          , &
        DZPREV         , &
        UniteListing   , & ! Unite logisue du fichier listing
        LoiFrottement  , &
        Erreur           & !/Erreur/
                     )
   !
   ! **********************************************************************
   !
   !   FONCTION :
   !   --------
   !
   !   REPARTITION DES DEBITS AU NOEUD NumConfluence
   !   CAS DE PLUSIEURS BRANCHES AVAL
   !
   ! ----------------------------------------------------------------------
   ! ARGUMENTS
   ! ._______________.____.____._______________________________________________
   ! ! NOM           !TYPE!MODE! ROLE
   ! !_______________!____!____!______________________________________________
   ! ! SommeDebitance! R  !<-- ! SOMME DES DEBITANCES DES BRANCHES AVAL
   ! ! ZAval         ! R  !<-- ! TABLEAU DE COTE (INTERNE A QREPAR)
   ! ! NumPassage    ! I  !<-->! VARIABLE INDICATRICE :
   ! !               !    !    !  = 1  CALCUL DU DEBIT DANS LA BRANCHE AVAL
   ! !               !    !    !  = 2  CALCUL DES COTES DANS LES BRANCHES AMONT
   ! ! Q             ! R  !<-->! TABLEAU DES DEBITS
   ! ! Z             ! R  ! -->! TABLEAU DES COTES DE LA SURFACE LIBRE
   ! ! ZREF          ! R  ! -->! COTES DU FOND DU LIT
   ! ! X             ! R  ! -->! ABSCISSES DES SECTIONS DE CALCUL
   ! ! CF1           ! R  ! -->! COEF. DE FROTTEMENT , LIT MINEUR
   ! ! CF2           ! R  ! -->! COEF. DE FROTTEMENT , LIT MAJEUR
   ! ! IDT           ! I  ! -->!  )PERMET DE DETERMINER LA SECTION DE CALCUL A
   ! ! XDT           ! R  ! -->!  )PARTIR DES SECTIONS DE DONNEES
   ! ! Profil        ! T  ! -->! Caracteristiques du planimetrage d'un profil
   ! ! Profil_plan   ! R  ! -->! Variables de profil planimetrees
   ! ! NumConfluence ! I  ! -->! NUMERO DU NOEUD A TRAITER
   ! ! Connect       ! T  ! -->! Structure contenant la table de connectivite
   ! ! ModeleLit     ! I  ! -->! Modele du lit
   ! ! Erreur        ! T  !<-->! Erreur
   ! !_______________!____!____!______________________________________________
   !
   ! VARIABLES LOCALES
   ! ._______________.____.____.______________________________________________
   ! !               !    !    !
   ! !_______________!____!____!______________________________________________
   !  MODE: -->(DONNEE NON MODIFIEE),<--(RESULTAT),<-->(DONNEE MODIFIEE)
   !               (ENTREE)              (SORTIE)       (ENTREE/SORTIE)
   !-----------------------------------------------------------------------
   !
   !   FICHIERS ENTREE/SORTIE :
   !   ----------------------
   !
   !   SOUS PROGRAMME APPELANT :  PERSAR
   !   -------------------------
   !   SOUS PROGRAMMES APPELES :  
   !   -------------------------
   !
   !   INTERPOLATION_S : INTERPOLATION DE LAGRANGE D'ODRE N
   !
   !   REPAR           : CALCUL DE LA REPARTITION DES DEBITS ENTRE
   !                     LE LIT MINEUR ET LE LIT MAJEUR
   !
   !   RHSBP_S :  CALCUL DES PARAMETRES DE LA GEOMETRIE D'UNE
   !              SECTION
   !
   !   COMMENTAIRES :
   !   ------------
   !
   ! ----------------------------------------------------------------------
   !

   !============================= Declarations ===========================
   !.. Modules importes ..
   !----------------------
   use M_PRECISION
   ! Types derives
   use M_CONNECT_T         ! Type CONNECT_T
   use M_PROFIL_T          ! Type PROFIL_T
   use M_PROFIL_PLAN_T     ! Type PROFIL_PLAN_T
   use M_ERREUR_T          ! Type ERREUR_T
   ! Procedures-module
   use M_INTERPOLATION_S   ! Sous-programme INTERPOLATION_S
   use M_RHSBP_S           ! Sous-programme RHSBP_GENERIQUE_S
   use M_TRAITER_ERREUR_I  ! Traitement de l'erreur 
   ! Interfaces
   use M_REPAR_I

   !.. Declarations explicites ..
   !-----------------------------
   implicit none
   !.. Arguments ..
   ! --------------
   ! TABLEAUX DIMENSIONNES A NMPLAN
   real(DOUBLE)       , dimension(:)  , intent(  out) :: SommeDebitance
   real(DOUBLE)       , dimension(:)  , intent(  out) :: ZAval
   integer            ,                 intent(inout) :: NumPassage
   ! TABLEAU  DIMENSIONNE  A NbSect
   real(DOUBLE)       , dimension(:)  , intent(inout) :: Q
   real(DOUBLE)       , dimension(:)  , intent(inout) :: Z
   real(DOUBLE)       , dimension(:)  , intent(in   ) :: ZREF
   real(DOUBLE)       , dimension(:)  , intent(in   ) :: X
   real(DOUBLE)       , dimension(:)  , intent(in   ) :: CF1
   real(DOUBLE)       , dimension(:)  , intent(in   ) :: CF2
   integer            , dimension(:)  , intent(in   ) :: IDT
   real(DOUBLE)       , dimension(:)  , intent(in   ) :: XDT
   type(PROFIL_T)     , dimension(:)  , intent(in   ) :: Profil
   type(PROFIL_PLAN_T),                 intent(in   ) :: Profil_plan
   integer            ,                 intent(in   ) :: NumConfluence
   type(CONNECT_T)    ,                 intent(in)    :: Connect
   integer            ,                 intent(in   ) :: ModeleLit
   real(DOUBLE)       ,                 intent(  out) :: Epsil
   real(DOUBLE)       , dimension(:)  , intent(  out) :: DZPREV
   integer            ,                 intent(in   ) :: UniteListing
   integer                            , intent(in)    :: LoiFrottement
   type(ERREUR_T)     ,                 intent(inout) :: Erreur

   end subroutine QREPAR

   end interface

end module M_QREPAR_I
