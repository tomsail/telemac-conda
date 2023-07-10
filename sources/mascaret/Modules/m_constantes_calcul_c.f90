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

module M_CONSTANTES_CALCUL_C
!***********************************************************************
! PROGICIEL : MASCARET        N. GOUTAL
!
! VERSION : V8P4R0              EDF-CEREMA
!***********************************************************************

   !=========================== Declarations ==============================
   use M_PRECISION

   !***********************************************************************
   !
   ! Constantes reperant la phase de la simulation
   !
   !***********************************************************************
   integer     , parameter :: PHASE_INITIALISATION = 1
   integer     , parameter :: PHASE_CALCUL         = 2
   integer     , parameter :: PHASE_TERMINAISON    = 3
   integer     , parameter :: PHASE_ARRET          = 4

   !***********************************************************************
   !
   ! Constantes reperant le critere d'arret du calcul
   !
   !***********************************************************************
   integer     , parameter :: TEMPS_MAXIMUM                      = 1
   integer     , parameter :: NOMBRE_DE_PAS_TEMPS_MAXIMUM        = 2
   integer     , parameter :: COTE_MAXIMALE_AU_POINT_DE_CONTROLE = 3

   !***********************************************************************
   !
   ! Constantes reperant le type de maillage
   !
   !***********************************************************************
   integer      , parameter :: TYPE_MAILLAGE_PROFIL        = 1
   integer      , parameter :: TYPE_MAILLAGE_SERIE         = 2
   integer      , parameter :: TYPE_MAILLAGE_INDIVIDUELLE  = 3
   integer      , parameter :: TYPE_MAILLAGE_PRECEDENT     = 4
   integer      , parameter :: TYPE_MAILLAGE_SERIE_PROFIL  = 5
   integer      , parameter :: TYPE_MAILLAGE_NB_MAX        = 5

   !***********************************************************************
   !
   ! Constantes reperant le type de format geometrie
   !
   !***********************************************************************
   integer, parameter :: FORMAT_GEOM_LIDOV2P0 = 1
   integer, parameter :: FORMAT_GEOM_LIDOV3P0 = 2

   !***********************************************************************
   !
   ! Constantes reperant le type de d'entree des donnees
   !
   !***********************************************************************
   integer, parameter :: SAISIE_PAR_FICHIER = 1
   integer, parameter :: SAISIE_PAR_CLAVIER = 2

   !***********************************************************************
   !
   ! Constantes reperant le type de format de sortie
   !
   !***********************************************************************
   integer     , parameter :: FORMAT_STO_OPTHYCA       = 1
   integer     , parameter :: FORMAT_STO_PERMANENT     = 2
   integer     , parameter :: FORMAT_STO_NONPERMANENT  = 3
   integer     , parameter :: FORMAT_STO_LIDOP         = 4

   !***********************************************************************
   !
   ! Constantes reperant le type d'apport
   !
   !***********************************************************************
   integer, parameter :: TYPE_APPORT_PONCTUEL = 1
   integer, parameter :: TYPE_APPORT_LINEIQUE = 2

   !***********************************************************************
   !
   ! Constantes reperant le type de rupture du barrage principal
   !
   !***********************************************************************
   integer, parameter :: TYPE_RUPTURE_PROGRESSIVE = 1
   integer, parameter :: TYPE_RUPTURE_INSTANTANEE = 2

   !***********************************************************************
   !
   ! Constantes reperant le type de regime
   !
   !***********************************************************************
   integer     , parameter :: REGIME_PERMANENT     = 1
   integer     , parameter :: REGIME_NON_PERMANENT = 2

   !***********************************************************************
   !
   ! Constantes reperant le  modele du lit
   !
   !***********************************************************************
   integer     , parameter :: MODELE_LIT_DEBORD     = 1
   integer     , parameter :: MODELE_LIT_FOND_BERGE = 2

   !***********************************************************************
   !
   ! Constantes reperant le noyau de calcul utilise
   !
   !***********************************************************************
   integer     , parameter :: NOYAU_SARAP    = 1
   integer     , parameter :: NOYAU_REZODT   = 2
   integer     , parameter :: NOYAU_MASCARET = 3
   integer     , parameter :: NB_NOYAU       = 3

   character(8), dimension(NB_NOYAU), parameter :: NOM_NOYAU = &
       (/           &
       "SARAP   " , &
       "REZODT  " , &
       "MASCARET"   &
       /)

   !***********************************************************************
   !
   ! Constantes reperant la valeur limite du nombre de Froude en Non permanent
   !
   !***********************************************************************
   real(DOUBLE), parameter :: FROUDEMAX = 5._DOUBLE

   !***********************************************************************
   !
   ! Constante coefficient d'implicitation pour la discretisation
   !
   !***********************************************************************
   real(DOUBLE), parameter :: TETA = 0.6_DOUBLE

   !***********************************************************************
   !
   ! Constante choix du mode de stockage dans le fichier resultat
   !
   !***********************************************************************
   integer     , parameter :: STOCKAGE_TOUTES_SECTION = 1
   integer     , parameter :: STOCKAGE_LISTE_SECTION  = 2

   !***********************************************************************
   !
   ! Constantes reperant la loi de frottement
   !
   !***********************************************************************
   integer     , parameter :: LOI_FROTTEMENT_STRICKLER = 1
   integer     , parameter :: LOI_FROTTEMENT_CHEZY     = 2
   integer     , parameter :: LOI_FROTTEMENT_COLEBROOK = 3
   integer     , parameter :: LOI_FROTTEMENT_BAZIN     = 4
   integer     , parameter :: LOI_FROTTEMENT_NB_MAX    = 4
   
   !***********************************************************************
   !
   ! Constantes reperant le  modele du talus (calcul Courlis)
   !
   !***********************************************************************
 
   integer     , parameter :: MODELE_TALUS_PENTE      = 1
   integer     , parameter :: MODELE_TALUS_GLISSEMENT = 2

end module M_CONSTANTES_CALCUL_C
