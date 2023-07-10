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

module M_REPAR_I
!***********************************************************************
! PROGICIEL : MASCARET      A. LEBOSSE
!                           P. CHERUBINI
!                           S. MANDELKERN
!
! VERSION : V8P4R0              EDF-CEREMA
!***********************************************************************
   interface

   subroutine REPAR ( &
       DEB         , & ! Resultats
       VMOY        , &
       BETA        , &
       Q1          , &
       Q2          , &
       S1          , & ! Donnees modifiees
       S2          , &
       RH1         , &
       RH2         , &
       P1          , & ! Donnees non modifiees
       P2          , &
       Q           , &
       CF1         , &
       CF2         , &
       ModeleLit   , &
       LoiFrottement, & ! Loi de frottement
       NomProfil   , & ! Nom du profil pour erreur
      Erreur         & ! Erreur
                   )

   ! **********************************************************************
   !   FONCTION :
   !   --------
   !   CALCUL DE LA REPARTITION DE DEBIT
   !   MODELE DEBORD : REPARTITION LIT MINEUR/LIT MAJEUR , UTILISANT LE
   !                   PARAMETRE DE MODIFICATION DES DEBITANCES A
   !   MODELE CRUGOS : APPLICATION DE LA FORMULE DE COMPOSITION DES
   !   ou Fond Berge   RUGOSITES D'EINSTEIN -  LE LIT EST ALORS CONSIDERE
   !                   COMME UNIQUE, AVEC UN COEFFICIENT DE RUGOSITE
   !                   FONCTION DES COEFFICIENTS INITIAUX, ET DES
   !                   PARAMETRES GEOMETRIQUES
   !   SI AUNCUN MODELE N'A ETE RETENU, LE MODELE DEBORD EST APPLIQUE EN
   !   PRENANT LE PARAMETRE A EGAL A 1
   !
   ! ----------------------------------------------------------------------
   ! ARGUMENTS
   ! .________________.____._______________________________________________
   ! !    NOM    !TYPE!MODE!                   ROLE
   ! !___________!____!____!______________________________________________
   ! !  DEB      ! R  !<-- ! DEBITANCE
   ! !  VMOY     ! R  !<-- ! VITESSE MOYENNE
   ! !  BETA     ! R  !<-- ! COEFFICIENT DE REPARTITION DE VITESSES MIN/MAJ
   ! !  Q1,Q2    ! R  !<-- ! DEBIT
   ! !  S1,S2    ! R  !<-->! SECTION MOUILLEE    ) INDICE 1 LIT MINEUR
   ! !  RH1,RH2  ! R  !<-->! RAYON HYDRAULIQUE   )        2 LIT MAJEUR
   ! !  Q        ! R  ! -->! DEBIT GLOBAL
   ! !  P1,P2    ! R  ! -->! PERIMETRE MOUILLE   )
   ! !  CF1,CF2  ! R  ! -->! COEF. DE FROTTEMENT )
   ! !  ModeleLit! I  ! -->! Type du modele du lit
   ! !  Erreur   ! T  ! -->! Erreur
   ! !___________!____!____!______________________________________________
   !  VARIABLES LOCALES
   ! .___________.____.____.______________________________________________
   ! !  RH       ! R  !<-- ! RAYON HYDRAULIQUE
   ! !   A       ! R  ! -- ! PARAMETRE DU MODELE DEBORD
   ! !   PUT     ! R  ! -- ! VALEUR SEUIL POUR LE CALCUL DE A
   ! !___________!____!____!______________________________________________
   !  MODE: -->(DONNEE NON MODIFIEE),<--(RESULTAT),<-->(DONNEE MODIFIEE)
   !               (ENTREE)              (SORTIE)       (ENTREE/SORTIE)
   ! ----------------------------------------------------------------------
   !
   !   FICHIERS  ENTREE/SORTIE :
   !   -------------------------
   !   SOUS PROGRAMMES APPELANTS :  REZO, CRITIQ, PERMAT, QREPAR, SARAP
   !   ---------------------------
   !   SOUS PROGRAMMES APPELES :
   !   -------------------------
   !   COMMENTAIRES :
   !   --------------
   !   POUR TRAITER UN LIT COMPOSE EN LIT UNIQUE, IL SUFFIT DE DEMANDER
   !   LE MODELE CRUGOS, AVEC DES COEFFICIENTS DE RUGOSITE EGAUX
   ! ----------------------------------------------------------------------

   !============================ Declarations ==============================
   use M_PRECISION
   use M_PARAMETRE_C
   use M_CONSTANTES_CALCUL_C ! MODELE_LIT
   use M_ERREUR_T            ! type ERREUR_T
   use M_TRAITER_ERREUR_I    ! Traitement de l'erreur

   implicit none

   !.. Formal Arguments .. 
   real(DOUBLE)  , intent(out)   :: DEB
   real(DOUBLE)  , intent(out)   :: VMOY
   real(DOUBLE)  , intent(out)   :: BETA
   real(DOUBLE)  , intent(out)   :: Q1
   real(DOUBLE)  , intent(out)   :: Q2
   real(DOUBLE)  , intent(inout) :: S1
   real(DOUBLE)  , intent(inout) :: S2
   real(DOUBLE)  , intent(inout) :: RH1
   real(DOUBLE)  , intent(inout) :: RH2
   real(DOUBLE)  , intent(in)    :: Q
   real(DOUBLE)  , intent(in)    :: P1
   real(DOUBLE)  , intent(in)    :: P2
   real(DOUBLE)  , intent(in)    :: CF1
   real(DOUBLE)  , intent(in)    :: CF2
   integer       , intent(in)    :: ModeleLit
   integer       , intent(in)    :: LoiFrottement
   Character(30) , intent(in)    :: NomProfil
   type(ERREUR_T), intent(inout) :: Erreur

   end subroutine REPAR

   end interface

end module M_REPAR_I
