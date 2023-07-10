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

module M_CALC_PC_CONFLU_I
!***********************************************************************
! PROGICIEL : MASCARET        F. MAUREL       N. GOUTAL
!
! VERSION : V8P4R0              EDF-CEREMA
!***********************************************************************

   interface

   subroutine CALC_PC_CONFLU ( &
    PCSing                , &
    Z                     , &
    Q                     , &
    X                     , &
    ZREF                  , &
    Profil                , &
    ProfilPlan            , &
    Confluent             , &
    Abaque                , &
    IDT                   , &
    XDT                   , &
    Connect               , &
    UniteListing          , &
    Erreur                  &
                          )

   !============================== Instructions ================================

   use M_PRECISION     ! type DOUBLE
   use M_PARAMETRE_C   ! EPS3
   use M_MESSAGE_C     ! Liste des messages d'erreur
   use M_CONNECT_T     ! type CONNECT_T
   use M_CONFLUENT_T   ! type CONFLUENT_T
   use M_ERREUR_T      ! type ERREUR_T
   use M_PROFIL_T      ! type PROFIL_T
   use M_PROFIL_PLAN_T ! type PROFIL_T
   use M_TRAITER_ERREUR_I ! Traitement des erreurs
   use M_RHSBP_S       ! sous-programme RHSBP_SECTION_S
   use M_INTERPOLATION_S

   ! Arguments
   real(DOUBLE)        , dimension(:)  , intent(  out) :: PCSing
   real(DOUBLE)        , dimension(:)  , intent(in   ) :: Z
   real(DOUBLE)        , dimension(:)  , intent(in   ) :: Q
   real(DOUBLE)        , dimension(:)    , intent(in   ) :: X
   real(DOUBLE)        , dimension(:)    , intent(in   ) :: ZREF
   type(PROFIL_T)      , dimension(:)    , intent(in   ) :: Profil
   type(PROFIL_PLAN_T)                   , intent(in   ) :: ProfilPlan
   type(CONFLUENT_T)   , dimension(:)    , intent(in   ) :: Confluent
   real(DOUBLE)        , dimension(:,:,:), intent(in   ) :: Abaque
   integer             , dimension(:)    , intent(in   ) :: IDT
   real(DOUBLE)        , dimension(:)    , intent(in   ) :: XDT
   type(CONNECT_T)                       , intent(in   ) :: Connect
   integer                               , intent(in   ) :: UniteListing
   type(ERREUR_T)                        , intent(inout) :: Erreur

   end subroutine CALC_PC_CONFLU

   end interface

end module M_CALC_PC_CONFLU_I
