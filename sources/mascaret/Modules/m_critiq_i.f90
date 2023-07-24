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

module M_CRITIQ_I
!***********************************************************************
! PROGICIEL : MASCARET
!                          A. LEBOSSE      P. CHERUBINI
!                          S. PERON        S. MANDELKERN
! VERSION : V8P4R0              EDF-CEREMA
!***********************************************************************
   interface

   subroutine CRITIQ ( &
     ZCRIT        , & ! /RESULTATS/
     Section      , & ! /DONNEES NON MODIFIEES/
     ZREF         , &
     Q            , & ! /DONNEES NON MODIFIEES
     CF1          , & !  (ARGUMENTS DE S.P APPELES)/
     CF2          , &
     IDT          , &
     XDT          , &
     Profil       , &
     Profil_plan  , &
     ModeleLit    , &
     LoiFrottement, & ! Loi de frottement
     UniteListing , & ! Unite logique fichier listing
     Erreur         & ! /Erreur/
     )
!
! **********************************************************************
!
!   FONCTION :
!   --------
!
!   CALCUL DE LA COTE CRITIQUE DANS LA SECTION Section
!
! ----------------------------------------------------------------------
! ARGUMENTS
! .__________________.____._______________________________________________
! !    NOM      !TYPE!MODE!                   ROLE
! !_____________!____!____!______________________________________________
! ! ZCRIT       ! R  !<-- ! COTE CRITIQUE
! ! Section           ! I  ! -->! SECTION DU CALCUL DE LA COTE CRITIQUE
! ! ZREF        ! R  ! -->! COTE DU FOND
! ! Q           ! R  ! -->! DEBIT GLOBAL
! ! CF1         ! R  ! -->! COEF. DE FROTTEMENT , LIT MINEUR
! ! CF2         ! R  ! -->! COEF. DE FROTTEMENT , LIT MAJEUR
! ! IDT         ! I  ! -->!  )PERMET DE DETERMINER LA SECTION DE CALCUL A
! ! XDT         ! R  ! -->!  )PARTIR DES SECTIONS DE DONNEES
! ! Profil      ! T  ! -->! Caracteristiques du planimetrage d'un profil
! ! Profil_plan ! T  ! -->! Variables de profil planimetrees
! ! ModeleLit   ! I  ! -->! Modele du lit
! ! Erreur      ! T  !<-->! Erreur
! !_____________!____!____!______________________________________________
!
! VARIABLES LOCALES
! .___________.________________________________________________________
! !           !    !    !
! !___________!____!____!______________________________________________
!  MODE: -->(DONNEE NON MODIFIEE),<--(RESULTAT),<-->(DONNEE MODIFIEE)
!               (ENTREE)              (SORTIE)       (ENTREE/SORTIE)
!-----------------------------------------------------------------------
!
!   FICHIERS  ENTREE/SORTIE :
!   -----------------------
!
!   SOUS PROGRAMME APPELANT :  PERMAT
!   -------------------------
!   SOUS PROGRAMMES APPELES :
!   -------------------------
!
!   RHSBP_S : CALCUL DES PARAMETRES DE LA GEOMETRIE D'UNE SECTION
!
!   REPAR : CALCUL DE LA REPARTITION DES DEBITS ENTRE LE LIT MINEUR
!           ET LE LIT MAJEUR ACTIF
!
!   COMMENTAIRES :
!   ------------
!
! ----------------------------------------------------------------------

   !============================= Declarations ===========================
   !.. Modules importes ..
   !----------------------
   use M_PRECISION
   ! Constantes nommees
   use M_PARAMETRE_C
   ! Types derives
   use M_PROFIL_T          ! Type PROFIL_T
   use M_PROFIL_PLAN_T     ! Type PROFIL_PLAN_T
   use M_ERREUR_T          ! Type ERREUR_T
   ! Procedures-module
   use M_RHSBP_S           ! Sous-programme RHSBP_S
   use M_TRAITER_ERREUR_I  ! Traitement des erreurs
   ! Interfaces
   use M_REPAR_I

   !.. Declarations explicites ..
   !-----------------------------
   implicit none

   !.. Arguments ..
   ! --------------
   real(DOUBLE)                       , intent(out)   :: ZCRIT
   integer            ,                 intent(in)    :: Section
   ! TABLEAUX DIMENSIONNES A NMSCAL
   real(DOUBLE)       , dimension(:)  , intent(in)    :: ZREF
   real(DOUBLE)       ,                 intent(in)    :: Q
   real(DOUBLE)       ,                 intent(in)    :: CF1
   real(DOUBLE)       ,                 intent(in)    :: CF2
   integer            , dimension(:)  , intent(in)    :: IDT
   real(DOUBLE)       , dimension(:)  , intent(in)    :: XDT
   type(PROFIL_T)     , dimension(:)  , intent(in)    :: Profil
   type(PROFIL_PLAN_T),                 intent(in)    :: Profil_plan
   integer            ,                 intent(in)    :: ModeleLit
   integer                            , intent(in)    :: LoiFrottement
   integer            ,                 intent(in   ) :: UniteListing
   type(ERREUR_T)     ,                 intent(inout) :: Erreur

   end subroutine CRITIQ

   end interface

end module M_CRITIQ_I
