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

module M_PERSAR_I
!***********************************************************************
! PROGICIEL : MASCARET         A. LEBOSSE
!                              P.CHERUBINI
!                              S. MANDELKERN
!
! VERSION : V8P4R0              EDF-CEREMA
!***********************************************************************
   interface

   subroutine PERSAR(        &
                         Z , & !/RESULTATS/
                         Q , &
                         X , & !/DONNEES NON MODIFIEES/
                      ZREF , &
                       CF1 , &
                       CF2 , &
                    PCSing , &
                       IDT , &
                       XDT , &
                    Profil , &
                ProfilPlan , &
                        F1 , &
                    QInjec , &
                   Connect , &
               Singularite , &
                 Extremite , &
                 ModeleLit , &
                 Confluent , & ! Caracteristiques des confluences
                    Abaque , & ! Abaques des pertes de charges aux confluences
                Impression , &
              UniteListing , & ! Unite logique du fichier listing
                     Temps , &
                Algorithme , &
             LoiFrottement , & ! Loi de frottement
      PerteChargeConfluent , & ! Perte de charge automatique aux confluents
                      CQMV , & ! Apport de debit quantite de mvt
              decentrement , &
                    Erreur & !/Erreur/
                        )

   ! **********************************************************************
   !
   !   FONCTION :
   !   ----------
   !
   !   CALCUL DE LA LIGNE D'EAU
   !
   !-----------------------------------------------------------------------
   ! ARGUMENTS
   ! .________________.____.____._______________________________________________
   ! !    NOM         !TYPE!MODE!                   ROLE
   ! !________________!____!____!_______________________________________________
   ! ! Z              ! R  !<-- ! COTE DE LA SURFACE LIBRE
   ! ! Q              ! R  !<-- ! DEBITS
   ! ! X              ! R  ! -->! ABSCISSE DES SECTIONS DE CALCUL(SUR LE BIEF)
   ! ! ZREF           ! R  ! -->! COTE DU FOND
   ! ! CF1,CF2        ! R  ! -->! COEF. DE FROTTEMENT ,  1= MINEUR   2= MAJEUR
   ! ! PCSing         ! R  ! -->! PERTE DE CHARGE SINGULIERE
   ! ! IDT            ! I  ! -->!  )PERMET DE DETERMINER LA SECTION DE CALCUL A
   ! ! XDT            ! R  ! -->!  )PARTIR DES SECTIONS DE DONNEES
   ! ! Profil         ! R  ! -->! Caracteristiques du planimetrage d'un profil
   ! ! ProfilPlan     ! R  ! -->! SECTION MOUILLEE  ZONE DE STOCKAGE
   ! ! QInjec         ! R  ! -->! DEBIT APPORTE OU SOUTIRE
   ! ! Connect        ! T  ! -->! Structure contenant la table de connectivite
   ! ! Singularite    ! T  ! -->! Structure decrivant la singularite
   ! ! Extremite      ! T  ! -->! Structure decrivant les extremites
   ! ! ModeleLit      ! I  ! -->! Modele du lit
   ! ! Temps          ! R  ! -->! Temps
   ! ! Algorithme     ! I  ! -->! TABLEAUX DES APPELS AUX S.P. RESOLVANT LE
   ! !                !    !    ! CALCUL DE LIGNES D'EAU :
   ! !                !    !    !  Algorithme = ISUB + I
   ! !                !    !    !   AVEC  ISUB = 100  ==>       QBIEF
   ! !                !    !    !         ISUB = 200  ==>  CALL PERMAT
   ! !                !    !    !         ISUB = 300  ==>  CALL QNODE (1ER PAS)
   ! !                !    !    !         ISUB = 400  ==>  CALL QNODE (2EM PAS)
   ! !                !    !    !         ISUB = 500  ==>  CALL QREPAR(1ER PAS)
   ! !                !    !    !         ISUB = 600  ==>  CALL QREPAR(2EM PAS)
   ! !                !    !    !     ET   I = LE NUMERO DU NOEUD OU DU BIEF
   ! ! Erreur         ! T  !<-->! Erreur
   ! !________________!____!____!______________________________________________
   !
   ! VARIABLES LOCALES
   ! .______________________________________________________________________
   ! !   IA        ! I  ! -- ! COMPTEUR DU TABLEAU DES APPELS
   ! !   ICOMPT    ! I  ! -- ! COMPTEUR LIMITANT LE NOMBRE D'ITERATION
   ! !   NumPassage! I  ! -- ! INDICATEUR POUR LES APPELS A QNODE ET QREPAR
   ! !   noeud_bief! I  ! -- ! NUMERO DU NOEUD OU DU BIEF CONSIDERE
   ! !SommeDebitance R  ! -->! SOMME DES DEBITANCES DES BRANCHES AVAL
   ! !   ZAval     ! R  ! -->! TABLEAU DE COTE (INTERNE A QREPAR)
   ! !_____________!____!____!______________________________________________
   !  MODE: -->(DONNEE NON MODIFIEE),<--(RESULTAT),<-->(DONNEE MODIFIEE)
   !               (ENTREE)              (SORTIE)       (ENTREE/SORTIE)
   !-----------------------------------------------------------------------
   !
   !   FICHIERS ENTREE/SORTIE :
   !   ------------------------
   !   SOUS PROGRAMME APPELANT :  SARAP
   !   -------------------------
   !   SOUS PROGRAMMES APPELES :
   !   -------------------------
   !
   !   - PERMAT :  SOUS-PROGRAMME DU CALCUL DE LA LIGNE D'EAU
   !               DANS UN BIEF
   !   - QNODE  :  REPARTITION DES DEBITS A UN NOEUD AVEC
   !               EGALITE DES COTES ( UNE SEULE BRANCHE AVAL)
   !   - QREPAR :  REPARTITION DES DEBITS A UN NOEUD AVEC
   !               EGALITE DES COTES (PLUSIEURS BRANCHES AVAL)
   !
   !   COMMENTAIRES :
   !   --------------
   ! **********************************************************************

   !============================= Declarations ===========================
   !
   !.. Modules importes ..
   !----------------------
   use M_PRECISION
   use M_MESSAGE_C         ! Messages d'erreur
   ! Types derives
   use M_CONFLUENT_T       ! Type confluent
   use M_CONNECT_T         ! Definition du Type CONNECT_T
   use M_EXTREMITE_T       ! Definition du Type EXTREMITE_T
   use M_PROFIL_T          ! Definition du Type PROFIL_T
   use M_PROFIL_PLAN_T     ! Definition du Type PROFIL_PLAN_T
   use M_ERREUR_T          ! Definition du type ERREUR_T
   use M_SINGULARITE_T     ! Definition du Type SINGULARITE_T
   ! Interfaces
   use M_CALC_PC_CONFLU_I    ! Calcul des pertes de charge auto aux confluents
   use M_PERMAT_I
   use M_QNODE_I
   use M_QREPAR_I
   use M_TRAITER_ERREUR_I  ! Traitement de l'erreur

   !.. Declarations explicites ..
   !-----------------------------
   implicit none

   !.. Arguments ..
   ! --------------
   ! TABLEAUX DIMENSIONNES A NMSCAL
   real(DOUBLE)       , dimension(:)  , intent(out)   :: Z
   real(DOUBLE)       , dimension(:)  , intent(out)   :: Q
   ! TABLEAUX DIMENSIONNES A NMSCAL
   real(DOUBLE)       , dimension(:)  , intent(in)    :: X
   real(DOUBLE)       , dimension(:)  , intent(in)    :: ZREF
   real(DOUBLE)       , dimension(:)  , intent(in)    :: CF1
   real(DOUBLE)       , dimension(:)  , intent(in)    :: CF2
   real(DOUBLE)       , dimension(:)  , intent(in)    :: PCSing
   integer            , dimension(:)  , intent(in)    :: IDT
   real(DOUBLE)       , dimension(:)  , intent(in)    :: XDT
   type(PROFIL_T)     , dimension(:)  , intent(in)    :: Profil
   type(PROFIL_PLAN_T)                , intent(in)    :: ProfilPlan
   real (Double)      , dimension(:,:), intent(in)    :: F1
   real(DOUBLE)       , dimension(:)  , intent(in)    :: QInjec
   type(CONNECT_T)                    , intent(in)    :: Connect
   type(SINGULARITE_T), dimension(:)  , intent(in)    :: Singularite
   type(EXTREMITE_T)  , dimension(:)  , intent(in)    :: Extremite
   integer                            , intent(in)    :: ModeleLit
   type(CONFLUENT_T)   , dimension(:) , intent(in   ) :: Confluent
   real(DOUBLE)    , dimension(:,:,:) , intent(in   ) :: Abaque
   logical                            , intent(in)    :: Impression
   integer            ,                 intent(in)    :: UniteListing
   real(DOUBLE)                       , intent(in)    :: Temps
   integer            , dimension(:)  , intent(in)    :: Algorithme
   integer                            , intent(in)    :: LoiFrottement
   integer                            , intent(in)    :: CQMV
   logical                            , intent(in)    :: PerteChargeConfluent
   logical                            , intent(in)    :: decentrement
   type(ERREUR_T)                     , intent(inout) :: Erreur

   end subroutine PERSAR

   end interface

end module M_PERSAR_I
