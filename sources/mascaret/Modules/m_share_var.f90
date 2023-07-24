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

MODULE M_SHARE_VAR
!***********************************************************************
! PROGICIEL : MASCARET        F. DEMANGEON
!                             F. ZAOUI
!
! VERSION : V8P4R0              EDF-CEREMA
!***********************************************************************
! Commentaire : ce module constitue un regroupement de variables
!               globales pour un acces direct depuis le simulateur
!               du solveur N2QN1
!***********************************************************************
!
! Types derivees
!
   USE M_APPORT_T
   USE M_CONFLUENT_T
   USE M_CONNECT_T
   USE M_EXTREMITE_T
   USE M_SINGULARITE_T
   USE M_DEVERSOIR_T
   USE M_PROFIL_T
   USE M_PROFIL_PLAN_T
   USE M_ERREUR_T
!TAPENADE--
   USE M_DONNEES_CALAGE_T
   USE M_DONNEES_CRUES_CALAGE_T
   USE M_CONSTANTES_CALAGE_T
!--TAPENADE
   USE M_FICHIER_T
   IMPLICIT NONE
!
! Variables concernant SARAP
!
   TYPE(EXTREMITE_T), DIMENSION(:), POINTER :: extremite  => null()
   TYPE(ERREUR_T) :: erreur
   TYPE(PROFIL_T), DIMENSION(:), POINTER :: profil => null()
   TYPE(PROFIL_PLAN_T),SAVE :: profilplan
   TYPE(APPORT_T), DIMENSION(:), POINTER :: apport => null()
   TYPE(CONNECT_T),SAVE :: connect
   TYPE(SINGULARITE_T), DIMENSION(:), POINTER, SAVE :: singularite=>NULL()
   TYPE(CONFLUENT_T), DIMENSION(:), POINTER :: confluent => null()
   TYPE(DEVERSOIR_T), DIMENSION(:), POINTER :: deversoir => null()
   TYPE(FICHIER_T) :: fichierlisting
   DOUBLE PRECISION, DIMENSION(:), POINTER :: x => null(), cf1 => null(), cf2 => null()
   DOUBLE PRECISION, DIMENSION(:), POINTER :: zref => null()
   DOUBLE PRECISION, DIMENSION(:), POINTER :: xdt => null()
   DOUBLE PRECISION, DIMENSION(:, :), POINTER :: f1 => null()
   DOUBLE PRECISION, DIMENSION(6, 6, 5) :: abaque
   DOUBLE PRECISION :: temps
   INTEGER, DIMENSION(:), POINTER :: idt => null()
   INTEGER, DIMENSION(:), POINTER :: algorithme => null()
   INTEGER :: nb_sect
   INTEGER :: modelelit
   INTEGER :: unitelisting
   INTEGER :: loifrottement
   INTEGER :: cqmv
   LOGICAL :: decentrement
   LOGICAL :: impressioncalcul
   LOGICAL :: pertechargeconfluent
!TAPENADE--
!
! Variables concernant le calage automatique
!
  TYPE(FICHIER_T) :: FichierResultatCalage, FichierResultatCalage1
  TYPE(DONNEES_CALAGE_T), DIMENSION(:), POINTER :: calage_frott => null()
  TYPE(DONNEES_CRUES_CALAGE_T),SAVE :: calage_crues
  TYPE(CONSTANTES_CALAGE_T) :: Constantes_Calage
  TYPE(APPORT_T), DIMENSION(:), POINTER :: apport_cal => null()
  DOUBLE PRECISION, DIMENSION(:,:), POINTER :: z_mesu, pond => null()
  INTEGER :: nb_crue, max_mes
  INTEGER , DIMENSION(:), POINTER :: nb_mes => null()
  INTEGER, DIMENSION(:,:), POINTER:: i_mesu => null()
  INTEGER :: iestim
  INTEGER :: nb_zone_frottement
!--TAPENADE
END MODULE M_SHARE_VAR

