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

module M_PERMAT_I
!***********************************************************************
! PROGICIEL : MASCARET        A. LEBOSSE
!                             P. CHERUBINI
!                             S. PERON
!                             S. MANDELKERN
!
! VERSION : V8P4R0              EDF-CEREMA
!***********************************************************************
   interface

   subroutine PERMAT( &
                      Z , & ! Cotes de la surface libre
                      Q , & ! Debit
                  ZINIT , & ! Cote initiale
                      X , & ! Maillage
                   ZREF , & ! Cote de reference
                    CF1 , & ! Coeff de frottement mineur
                    CF2 , & ! Coeff de frottement majeur
                 PCSing , & ! Pertes de charge singuliere
                    IDT , & ! Positionnnement des sections/profils
                    XDT , & ! idem
                 Profil , & ! Profils geometriques
             ProfilPlan , & ! Tableaux de planimetrage
                     F1 , & ! Fonction impulsion
                Connect , & ! Table de connectivite
                NumBief , & ! Numero du bief
                 Nbsect , & ! Nombre de sections
            Singularite , & ! Singularites
              ModeleLit , & ! Modele du lit Debord/Fond/Berge
             Impression , & ! Flag d'impression
           UniteListing , & ! Unite logique du fichier listing
                  Temps , & ! Temps
          LoiFrottement , & ! Loi de frottement
                  CQMV  , & ! qmv des debits d'apport
           decentrement , & !  option decentrement
                 Erreur &  !/ERREUR/
                    )

   ! **********************************************************************
   !   FONCTION :
   !   --------
   !
   !   CALCUL DE LIGNE D'EAU DANS UN BIEF EN REGIME PERMANENT
   !   MODELISATION UNIDIMENSIONNELLE
   !   LITS MINEUR ET MAJEUR - HYPOTHESES DEBORD OU PRADO
   !   SINGULARITES
   !
   ! ----------------------------------------------------------------------
   !
   !   FICHIERS  ENTREE/SORTIE :
   !   -----------------------
   !
   !   - UniteListing   : IMPRESSION DES RESULTATS GLOBAUX
   !
   !   SOUS PROGRAMMES APPELANTS :  PERSAR
   !   ---------------------------
   !   SOUS PROGRAMMES APPELES :
   !   -------------------------
   !
   !   CRITIQ : CALCUL DE LA COTE CRITIQUE
   !   FROUDE : CALCUL DU NOMBRE DE FROUDE
   !   PSING  : TRAITEMENT DES SINGULARITES EN PERMANENT
   !   REPAR  : CALCUL DE LA REPARTITION DES DEBITS ENTRE LE LIT MINEUR
   !            ET LE LIT MAJEUR ACTIF, HYPOTHESES 'DEBORD'
   !   RHSBP_S: CALCUL DES PARAMETRES DE LA GEOMETRIE D'UNE SECTION
   !
   !   COMMENTAIRES :
   !   ------------
   !
   !   LE TERME REPRESENTANT LES PERTES DE CHARGE REGULIERES EST PRIS
   !   SELON LA MOYENNE HARMONIQUE DES
   !   COEFFICIENTS CALCULES DANS CHACUNE DES SECTIONS AMONT ET AVAL:
   !                  2      1     1
   !                ----- = --- + ---
   !                JAVAM   JAV   JAM
   !
   !   LES PERTES DE CHARGE SINGULIERES SONT :
   !      - RALENTISSEMENT : JS=CPCS*(BETAAM*VAM-BETAAV*VAV)**2/2./G
   !      - OBSTACLE  EN A : JS=PCSing(A)*BETAAM*VAM**2/2./G
   !
   !   LE CALCUL EST SUPPOSE SE FAIRE EN REGIME FLUVIAL.
   !   TOUTEFOIS SI AU COURS DES ITERATIONS ON DETECTE UN REGIME FLUVIAL
   !   A L'AVAL ET TORRENTIEL A L'AMONT , ON IMPOSE ALORS Z=ZCRITIQUE A
   !   LA SECTION AMONT , CECI TANT QU'ON N'A PAS RETROUVE UN ECOULEMENT
   !   FLUVIAL. DE PLUS UN TEST EST REALISE POUR VERIFIER QUE CE PASSAGE
   !   FLUVIAL AMONT - CRITIQUE AVAL N'EST PAS TROP BRUTAL
   !
   !   DOCUMENTATION EXTERNE :
   !   ---------------------
   !
   !   NOTE D'UTILISATION       : RAPPORT HE-43/92.19
   !   NOTE DE PRINCIPE         : RAPPORT HE-43/92.64
   !
   ! ----------------------------------------------------------------------
   !

   !============================= Declarations ===========================

   !.. Modules importes ..
   !----------------------
   use M_PRECISION
   ! Constantes nommees
   use M_PARAMETRE_C       ! GPES
   use M_MESSAGE_C         ! Messages d'erreur
   ! Types derives
   use M_PROFIL_T          ! Type PROFIL_T
   use M_PROFIL_PLAN_T     ! Type PROFIL_PLAN_T
   use M_SINGULARITE_T     ! Type SINGULARITE_T
   use M_ERREUR_T          ! Type EREUR_T
   use M_CONNECT_T         ! Type CONNECT_T
   ! Procedures-module
   use M_RHSBP_S           ! Sous-programme RHSBP_S
   use M_RHSB1_S          ! Sous programme RHSBP1
   use M_NUM_BIEF_S        ! Numero de bief d'une section
   use M_FROUDE_S          ! Calcul du nombre de Froude
   use M_TRAITER_ERREUR_I  ! Interface generique de traitement des erreurs
   ! Interfaces
   use M_REPAR_I
   use M_PSING_I
   use M_CRITIQ_I

   !.. Declarations explicites ..
   !-----------------------------
   implicit none

   !.. Arguments ..
   !---------------
   ! TABLEAU  DIMENSIONNE  A NbSect
   real(DOUBLE)       , dimension(:)  , intent(out)   :: Z
   ! TABLEAU  DIMENSIONNE  A NbSect
   real(DOUBLE)       , dimension(:)  , intent(in)    :: Q
   real(DOUBLE)       ,                 intent(in)    :: ZINIT
   ! TABLEAUX DIMENSIONNES A NMSCAL
   real(DOUBLE)       , dimension(:)  , intent(in)    :: X
   real(DOUBLE)       , dimension(:)  , intent(in)    :: ZREF
   real(DOUBLE)       , dimension(:)  , intent(in)    :: CF1
   real(DOUBLE)       , dimension(:)  , intent(in)    :: CF2
   real(DOUBLE)       , dimension(:)  , intent(in)    :: PCSing
   integer            , dimension(:)  , intent(in)    :: IDT
   real(DOUBLE)       , dimension(:)  , intent(in)    :: XDT
   type(PROFIL_T)     , dimension(:)  , intent(in)    :: Profil
   type(PROFIL_PLAN_T),                 intent(in)    :: ProfilPlan
   real(Double)       , dimension(:,:), intent(in)    :: F1
   integer            ,                 intent(in)    :: NumBief
   type(CONNECT_T)    ,                 intent(in)    :: Connect
   type(SINGULARITE_T), dimension(:)  , intent(in)    :: Singularite
   integer            ,                 intent(in)    :: ModeleLit
   logical                            , intent(in)    :: Impression
   integer            ,                 intent(in)    :: UniteListing
   real(DOUBLE)       ,                 intent(in)    :: Temps
   integer            ,                 intent(in)    :: LoiFrottement
   integer            ,                 intent(in)    :: Nbsect
   integer            ,                 intent(in)    :: CQMV
   logical                            , intent(in)    :: decentrement
   type(ERREUR_T)     ,                 intent(inout) :: Erreur

   end subroutine PERMAT

   end interface

end module M_PERMAT_I
