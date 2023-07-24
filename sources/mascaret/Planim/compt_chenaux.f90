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

subroutine  COMPT_CHENAUX  ( &
            NbChenaux      , & ! Nombre de chenaux
            LimChenal      , & ! points limites des chenaux
            XLimChenal     , & ! Abscisses des limites des chenaux
            Borne          , & ! Bornes de calcul
            Cote           , & ! Cote pour laquelle le calcul est fait
            DXP, DYP       , & ! Points geometriques du profil
            Erreur           & ! Erreur
                           )
! *********************************************************************
! PROGICIEL : MASCARET         N. GOUTAL
!
! VERSION : V8P4R0                EDF-CEREMA
! *********************************************************************

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
   real(DOUBLE), dimension(:)               , intent(inout) :: DXP, DYP
   integer     , dimension(2)               , intent(in   ) :: Borne
   real(DOUBLE)                             , intent(in   ) :: Cote
   type(ERREUR_T)                           , intent(inout) :: Erreur
   !.. Local Scalars .. 
   real(DOUBLE)   :: expression
   integer        :: ichenal         ! Compteur sur les chenaux
   integer        :: ipoint          ! Compteur sur les points
   !character(132) :: !arbredappel_old

   !============================ Instructions ============================== 
   ! INITIALISATIONS
   ! ---------------
   Erreur%Numero = 0
   !arbredappel_old    = trim(!Erreur%arbredappel)
   !Erreur%arbredappel = trim(!Erreur%arbredappel)//'=>COMPT_CHENAUX'
   LimChenal (:,:) = 0
   XLimChenal(:,:) = W0
   ichenal = 0
   ipoint  = Borne(1)

   ! RIVE GAUCHE D'UN CHENAL
   LOOP1 : do while( ipoint < Borne(2).and.ichenal < NB_MAX_CHENAUX )
      ipoint  = ipoint  + 1
      if( DYP(ipoint) < cote ) then
         ichenal = ichenal + 1
         !--------------------------------------------------------
         ! Calcul de l'abscisse de l'intersection plan d'eau - sol
         ! = expression
         !--------------------------------------------------------
         if( ABS(DYP(ipoint)-DYP(ipoint-1)).LT.EPS15 ) then
            DYP(ipoint-1) = DYP(ipoint) + EPS2
         end if

         expression = (DXP(ipoint-1) * (DYP(ipoint  ) - Cote) -   &
                       DXP(ipoint  ) * (DYP(ipoint-1) - Cote)) /  &
                      (DYP(ipoint  ) -  DYP(ipoint-1))

         LimChenal (1,ichenal) = ipoint - 1
         XLimChenal(1,ichenal) = expression

         do while( ipoint < Borne(2) )
            ! RIVE DROITE D'UN CHENAL
            ipoint = ipoint + 1
            if( DYP(ipoint) >= Cote ) exit
         end do

         if( ABS(DYP(ipoint)-DYP(ipoint-1)).LT.EPS15 ) then
            DYP(ipoint) = DYP(ipoint - 1) + EPS2
         end if

         expression = (DXP(ipoint-1) * (DYP(ipoint  ) - Cote) -   &
                       DXP(ipoint  ) * (DYP(ipoint-1) - Cote)) /  &
                      (DYP(ipoint  ) -  DYP(ipoint-1))

         LimChenal (2,ichenal) = ipoint - 1
         XLimChenal(2,ichenal) = expression

      end if

   end do LOOP1

   NbChenaux = ichenal

  ! Fin des traitements
  ! -------------------

  !Erreur%arbredappel = !arbredappel_old

  return

end subroutine COMPT_CHENAUX
