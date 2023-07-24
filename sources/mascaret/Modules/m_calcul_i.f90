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

module M_CALCUL_I
!***********************************************************************
! PROGICIEL : MASCARET        A. LEBOSSE       S. MANDELKERN
!
! VERSION : V8P4R0              EDF-CEREMA
!***********************************************************************

interface

subroutine CALCUL ( &

! Resultats
  Z                             , & ! Cote
  Q                             , & ! Debit
  Matrice                       , & ! Matrice du probleme a resoudre
! Donnees non modifiees
  P1, P2                        , & ! Perimetres mouillees
  B1, B2, BS                    , & ! Largeurs au miroir
  S1, S2                        , & ! Sections mouillees
  R1, R2                        , & ! Rayons hydrauliaues
  DPDZ1, DPDZ2                  , & ! Gradients du perimetre/Z
  X                             , & ! Maillage
  CF1, CF2                      , & ! Coefficients de frottement
  QINJEC                        , & ! Debits d'apport
  Connect                       , & ! Table de connectivite
  RDLIM,SDLIM,TDLIM             , & ! Conditions aux limites
  PCSing                        , & ! Pertes de charges singulieres
  ModeleLit                     , & ! Modele du lit (Debord/Crugos)
  DT                            , & ! Pas de temps
  NumeroPas                     , & ! numero du pas
  Impression                    , & ! Flag d'impression
  UniteListing                  , & ! Unite logique listing
  LoiFrottement                 , & ! Loi de frottement
  ASING,BSING,CSING,DSING       , & ! Coefficients de l'equation discretisee d'une singularites
  OptionCasier                  , & ! Flag presence Casier
  Aliai,Bliai                   , & ! Coefficients de l'equation discretisee d'une liaison
  Cliai,Dliai                   , &
  Qliai                         , &
  ApportPluie                   , &
  Liaison                       , &
  Casier                        , &
  NoConvection                  , & ! Attenuation de la convection
  CQMV                          , & ! constante pour l'apport de debit dans la quantite de mvt
  Erreur                          & ! Erreur
)

   use M_PRECISION
   use M_CONSTANTES_CALCUL_C  ! Constantes parametres de calcul (TETA)
   use M_PARAMETRE_C          ! Parametres de calcul
   use M_MESSAGE_C            ! Liste des messages d'erreur
   use M_ERREUR_T             ! Definition du type ERREUR_T
   use M_CONNECT_T            ! Definition du type CONNECT_T
   use M_TRAITER_ERREUR_I     ! Traitement des erreurs
   use M_CONNECT_T            ! Type CONNECT_T
   use M_DEBITANCE_S          ! Calcul des coefficients de Striclkler
   use M_TRAITER_ERREUR_I     ! Traitement des erreurs
   use M_REZOMAT_T            ! Definition du type REZOMAT_T
   use M_APPORT_PLUIE_T
   use M_LIAISON_T            ! Definition du type LIAISON_T
   use M_CASIER_T             ! Definition du type CASIER_T
   use M_TRAITER_ERREUR_I     ! Traitement des erreurs
   use M_SURVOL_I             ! Interafce du sous-programme SURVOL
   use M_BILVOL_I             ! Interafce du sous-programme BILVOL
   use M_CONSTANTES_CASIER_C  ! Constantes casiers

  implicit none

  !
  ! Arguments
  !

  real(DOUBLE) , dimension(:) , intent(out)   :: Z, Q
  real(DOUBLE) , dimension(:) , intent(in)    :: P1, P2
  real(DOUBLE) , dimension(:) , intent(in)    :: B1, B2, BS
  real(DOUBLE) , dimension(:) , intent(in)    :: R1, R2
  real(DOUBLE) , dimension(:) , intent(in)    :: S1, S2
  real(DOUBLE) , dimension(:) , intent(in)    :: DPDZ1, DPDZ2
  real(DOUBLE) , dimension(:) , intent(in)    :: QINJEC
  real(DOUBLE) , dimension(:) , intent(in)    :: X                   ! Maillage
  real(DOUBLE) , dimension(:) , intent(in)    :: CF1, CF2
  real(DOUBLE) , dimension(:) , intent(in)    :: PCSing              ! Pertes de charge singulieres
  real(DOUBLE) , dimension(:) , intent(in)    :: RDLIM, SDLIM, TDLIM ! Conditions aux limites
  real(DOUBLE)                , intent(in)    :: DT
  real(DOUBLE),  dimension(:),  intent(in)    :: ASING, BSING, CSING, DSING
  real(DOUBLE),  dimension(:),  intent(in)    :: Aliai, Bliai, Cliai, Dliai, Qliai
  logical                     , intent(in)    :: Impression
  logical                     , intent(in   ) :: OptionCasier
  logical                     , intent(in)    :: NoConvection
  integer                     , intent(in)    :: UniteListing
  integer                     , intent(in)    :: LoiFrottement
  integer                     , intent(in)    :: NumeroPas
  integer                     , intent(in)    :: ModeleLit
  integer                     , intent(in)    :: CQMV
  type(ERREUR_T)              , intent(inout) :: Erreur
  type(CONNECT_T)             , intent(in)    :: Connect             ! Table de connectivite du reseau
  type(REZOMAT_T)             , intent(inout) :: Matrice             ! Matrice du reseau
  type(LIAISON_T), dimension(:), pointer, intent(inout) :: Liaison
  type(CASIER_T) , dimension(:), pointer, intent(inout) :: Casier
  type(APPORT_PLUIE_T), dimension(:), pointer, intent(inout   ) :: ApportPluie

end subroutine CALCUL

end interface

end module M_CALCUL_I
