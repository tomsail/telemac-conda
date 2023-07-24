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

module M_LEC_FROTTEMENT_I
!***********************************************************************
! PROGICIEL : MASCARET       S. MANDELKERN
!
! VERSION : V8P4R0              EDF-CEREMA
!***********************************************************************
   interface

   subroutine LEC_FROTTEMENT ( &
                             CF1 , & ! Coefficient de frottement Mineur
                             CF2 , & ! Coefficient de frottement Majeur
                               X , & ! Abscisse des sections de calcul
                         ZoneFrot, & ! limite des zones de frottement
                             XDT , &
                          Profil , & ! Profils geometriques
                     ProfDebBief , & ! Premiers profils des biefs
                     ProfFinBief , & ! Derniers profils des biefs
               AbscRelExtDebBief , & ! Abscisse de l'extremite debut du bief
               AbscRelExtFinBief , & ! Abscisse de l'extremite debut du bief
             InterpLinCoeffFrott , &
                    UniteListing , & ! Unite logique fichier listing
                        unitNum  , & ! Unite logique .xcas
                          Erreur & ! Erreur
                               )

   !========================= Declarations ===========================
   use M_PRECISION
   use M_ERREUR_T            ! Type ERREUR_T
   use M_PARAMETRE_C
   use M_PROFIL_T            ! Type  PROFIL_T
   use M_ZONE_FROT_T         ! Type Zone frottement
   use M_MESSAGE_C           ! Messages d'erreur
   use M_CONSTANTES_CALCUL_C ! Constantes num, phys et info
   use M_TRAITER_ERREUR_I    ! Traitement de l'errreur
   use M_XINDIC_S            ! Calc de l'indice corresp a une absc
   use M_ABS_ABS_S           ! Calcul de l'abscisse absolue
   use M_XCAS_S

   implicit none

   ! Arguments
   real(DOUBLE)      , dimension(:)  , pointer       :: CF1
   real(DOUBLE)      , dimension(:)  , pointer       :: CF2
   real(DOUBLE)      , dimension(:)  , intent(in   ) :: X
   real(DOUBLE)      , dimension(:)  , intent(in   ) :: XDT
   type(PROFIL_T)    , dimension(:)  , intent(in   ) :: Profil
   type(Zone_Frot_T) , dimension(:)  , pointer       :: ZoneFrot
   integer           , dimension(:)  , intent(in   ) :: ProfDebBief
   integer           , dimension(:)  , intent(in   ) :: ProfFinBief
   real(DOUBLE)      , dimension(:)  , intent(in   ) :: AbscRelExtDebBief
   real(DOUBLE)      , dimension(:)  , intent(in   ) :: AbscRelExtFinBief
   logical                           , intent(in   ) :: InterpLinCoeffFrott
   integer                           , intent(in   ) :: UniteListing
   integer, intent(in)                               :: unitNum
   type(ERREUR_T)                    , intent(inout) :: Erreur
   ! Variables locales
   integer      :: izone              ! compteur sur les zones de frottement
   integer      :: isect              ! compteur sur les sections
   integer      :: indice             ! indice de debut de zone
   integer      :: indice2            ! indice de fin de zone
   integer      :: indice2_prec       ! indice de fin de zone precedente
   integer      :: nb_zone_frottement
   real(DOUBLE) :: valeur_coeff_min
   real(DOUBLE) :: valeur_coeff_maj
   real(DOUBLE) :: valeur_coeff_min_prec
   real(DOUBLE) :: valeur_coeff_maj_prec
   integer      :: branche_zone_frott
   real(DOUBLE) :: abscdeb_zone_frott
   real(DOUBLE) :: abscfin_zone_frott
   real(DOUBLE) :: abscfin_zone_frott_prec
   integer      :: branche_zone_frott_prec
   logical      :: chevauchement
   integer      :: retour              ! code de retour des fonctions
                                      ! intrinseques
   integer, allocatable :: itab(:)
   real(double), allocatable :: rtab1(:),rtab2(:),rtab3(:),rtab4(:)
   character(len=256)  :: pathNode
   character(len=8192) :: line
   !character(132) :: !arbredappel_old

   end subroutine LEC_FROTTEMENT

   end interface

end module M_LEC_FROTTEMENT_I
