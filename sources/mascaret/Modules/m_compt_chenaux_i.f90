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

module M_COMPT_CHENAUX_I
!***********************************************************************
! PROGICIEL : MASCARET        N. GOUTAL
!
! VERSION : V8P4R0              EDF-CEREMA
!***********************************************************************
   interface

   subroutine  COMPT_CHENAUX  ( &
            NbChenaux      , & ! Nombre de chenaux
            LimChenal      , & ! points limites des chenaux
            XLimChenal     , & ! Abscisses des limites des chenaux
            Borne          , & ! Bornes de calcul
            Cote           , & ! Cote pour laquelle le calcul est fait
            DXP, DYP       , & ! Points geometriques du profil
            Erreur           & ! Erreur
                           )

   !============================= Declarations ===========================
   use M_PRECISION           ! Type DOUBLE
   use M_PARAMETRE_C         ! Parametres de calcul
   use M_MESSAGE_C           ! Liste des messages d'erreur
   use M_ERREUR_T            ! Definition du type ERREUR_T
   use M_TRAITER_ERREUR_I    ! Traitement des erreurs

   !.. Implicit Declarations .. 
   implicit none

   !.. Parameters .. 
   integer, parameter :: NB_MAX_CHENAUX = 100

   !.. Formal Arguments .. 
   integer                                  , intent(  out) :: NbChenaux
   integer     , dimension(2,NB_MAX_CHENAUX), intent(  out) :: LimChenal
   real(DOUBLE), dimension(2,NB_MAX_CHENAUX), intent(  out) :: XLimChenal
   real(DOUBLE), dimension(:)               , intent(in   ) :: DXP, DYP
   integer     , dimension(2)               , intent(in   ) :: Borne
   real(DOUBLE)                             , intent(in   ) :: Cote
   type(ERREUR_T)                           , intent(inout) :: Erreur

   end subroutine COMPT_CHENAUX

   end interface

end module M_COMPT_CHENAUX_I
