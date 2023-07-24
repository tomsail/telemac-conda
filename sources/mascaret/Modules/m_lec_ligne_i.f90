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

module M_LEC_LIGNE_I
!***********************************************************************
! PROGICIEL : MASCARET        S. MANDELKERN
!
! VERSION : V8P4R0              EDF-CEREMA
!***********************************************************************
   interface

   subroutine LEC_LIGNE( &
                         Z , & ! Cote initiale
                         Q , & ! debit initial
                       CF1 , & ! Coefficient de frottement mineur
                       CF2 , & ! Coefficient de frottement majeur
                         X , & ! maillage
           TypeEntreeLigne , & ! format LIDO/OPTHYCA
              FichierLigne , & ! fichier l.e. initial
               FormatLigne , & ! format LIDO/OPTHYCA
              TypeMaillage , & ! Mode de calcul du maillage
           ImpressionLigne , & ! test d'impression
              UniteListing , & ! Unite logique fichier listing
                    Profil , & ! Profils geometriques
                  Prof_Abs , & ! Profils Absolus
               ProfDebBief , & ! Premiers profils des biefs
               ProfFinBief , & ! Derniers profils des biefs
             AbsRelDebBief , & ! Abscisse relative de debut de bief
             AbsRelFinBief , & ! Abscisse relative de fin de bief
                    Erreur ) ! Erreur

   ! .....................................................................
   !  FONCTION : LECTURE DU FICHIER DE LA LIGNE D'EAU INITIALE
   !  --------
   !             INTERPOLATION AUX SECTIONS DE CALCUL SI NECESSAIRE
   !             SI TypeMaillage = TYPE_MAILLAGE_PRECEDENT :
   !             DEFINITION DES ABSCISSES DES SECTIONS DE CALCUL .
   !
   !----------------------------------------------------------------------------
   !
   !   FICHIERS ENTREE/SORTIE :       - Fichier listing (UniteListing)
   !   ----------------------
   !
   !   SOUS-PROGRAMME(S) APPELANT(S) :  - LECDON
   !   -----------------------------
   !   SOUS-PROGRAMME(S) APPELE(S)   :  - LEC_LIGNE_OPT
   !   ---------------------------      - LEC_LIGNE_LIDO
   !                                    - INTERPOLATION_S
   !                                    - ABS_ABS_S
   !
   !   COMMENTAIRES :
   !   ------------
   !
   !   DOCUMENTATION EXTERNE :
   !   ---------------------
   !
   !***********************************************************************

   !============================= Declarations ============================
   use M_PRECISION
   use M_FICHIER_T            ! Definition du type FICHIER_T et UniteListing
   use M_ERREUR_T             ! Definition du type ERREUR_T
   use M_PROFIL_T             ! Type  PROFIL_T
   use M_PARAMETRE_C
   use M_MESSAGE_C            ! Definition des messages d'erreur
   use M_CONSTANTES_CALCUL_C  ! Definition des formats de fichiers de sortie
   use M_LEC_LIGNE_LIDO_I
   use M_LEC_LIGNE_OPT_I
   use M_INTERPOLATION_S       ! Interface du sous programme INTERPOLATION_S
   use M_ABS_ABS_S             ! Calcul de l'abscisse absolue
   use M_TRAITER_ERREUR_I      ! Traitement des erreurs

   !.. Declarations explicites ..
   implicit none

   type SECTION_REL_T 
      sequence
      integer      :: Branche     ! Numero de branche
      real(DOUBLE) :: AbscisseRel ! Abscisse relative
   end type SECTION_REL_T

   !.. Arguments ..
   real(DOUBLE), dimension(:)   , pointer       :: X
   real(DOUBLE), dimension(:)   , pointer       :: Z
   real(DOUBLE), dimension(:)   , pointer       :: Q
   real(DOUBLE), dimension(:)   , pointer       :: CF1
   real(DOUBLE), dimension(:)   , pointer       :: CF2
   integer                      , intent(in   ) :: TypeEntreeLigne
   type(FICHIER_T)              , intent(in   ) :: FichierLigne
   integer                      , intent(in   ) :: FormatLigne
   integer                      , intent(in   ) :: TypeMaillage
   logical                      , intent(in   ) :: ImpressionLigne 
   logical                      , intent(in   ) :: Prof_Abs 
   integer                      , intent(in   ) :: UniteListing
   type(PROFIL_T), dimension(:) , intent(in   ) :: Profil
   integer       , dimension(:) , intent(in   ) :: ProfDebBief
   integer       , dimension(:) , intent(in   ) :: ProfFinBief
   real(DOUBLE)  , dimension(:) , intent(in   ) :: AbsRelDebBief
   real(DOUBLE)  , dimension(:) , intent(in   ) :: AbsRelFinBief
   type(ERREUR_T)               , intent(inout) :: Erreur
   integer, parameter :: ORDRE_INTERP = 1  ! ordre d'interpolation  

   end subroutine LEC_LIGNE

   end interface

end module M_LEC_LIGNE_I
