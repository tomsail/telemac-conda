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

module M_CCL_I
!***********************************************************************
! PROGICIEL : MASCARET        S. MANDELKERN
!
! VERSION : V8P4R0              EDF-CEREMA
!***********************************************************************
   interface

   subroutine      CCL( &

                RDLIM, SDLIM, TDLIM     , & ! Conditions aux limites calcul
                Z, Q1, Q2               , & ! Cote, debit mineur et majeur
                ZREF                    , & ! Cote de reference
                CF1                     , & ! Coeff de frottement mineur
                IDT                     , & ! Positionnement des section/profils
                XDT                     , & ! Positionnement des section/profils
                Profil                  , & ! Profils geometriques
                ProfilPlan              , & ! Profils planimetres
                Extremite               , & ! Extremites libres
                Connect                 , & ! table de connectivite
                Temps                   , & ! Temps
                LoiFrottement           , & ! Loi de frottement
                Erreur                    & ! Erreur
                      )

! **********************************************************************
!
!   FONCTION : CALCUL DES CONDITIONS AUX LIMITES
!   --------
!
!
!
!   FICHIERS ENTREE/SORTIE :  --
!   ----------------------      
!   SOUS PROGRAMMES APPELANTS : - REZO
!   ---------------------------
!   SOUS PROGRAMMES APPELES :    - LOILIM
!   -------------------------
!------------------------------------------------------------------------ 

   !========================== Declarations ==============================
   use M_PRECISION           ! Type DOUBLE
   use M_CONSTANTES_CALCUL_C ! Consatantes calcul
   use M_MESSAGE_C           ! Liste des messages d'erreur
   use M_PARAMETRE_C         ! Constantes numeriques 2/3
   use M_CONNECT_T           ! Definition du type CONNECT_T
   use M_ERREUR_T            ! Definition du type ERREUR_T
   use M_EXTREMITE_T         ! Definition du type EXTREMITE_T
   use M_PROFIL_PLAN_T       ! Profils planimetres
   use M_PROFIL_T            ! Profils geometriques
   use M_RHSBP_S             ! Sous programme RHSBP_SECTION_S
   use M_DEBITANCE_S         ! Sous programme DEBITANCE_S
   use M_LOILIM_I            ! Interface du sous-programme LOILIM
   use M_TRAITER_ERREUR_I    ! Traitement des erreurs

   implicit none

   !.. Arguments .. 
   real(DOUBLE)     , dimension(:), intent(  out) :: RDLIM, SDLIM, TDLIM
   real(DOUBLE)     , dimension(:), intent(in   ) :: Z
   real(DOUBLE)     , dimension(:), intent(in   ) :: Q1
   real(DOUBLE)     , dimension(:), intent(in   ) :: Q2
   real(DOUBLE)     , dimension(:), intent(in   ) :: ZREF
   real(DOUBLE)     , dimension(:), intent(in   ) :: CF1
   real(DOUBLE)     , dimension(:), intent(in   ) :: XDT
   integer          , dimension(:), intent(in   ) :: IDT
   type(PROFIL_T)   , dimension(:), intent(in   ) :: Profil
   type(PROFIL_PLAN_T)            , intent(in   ) :: ProfilPlan
   type(EXTREMITE_T), dimension(:), intent(in   ) :: Extremite
   type(CONNECT_T)                , intent(in   ) :: Connect
   real(DOUBLE)                   , intent(in   ) :: Temps
   integer                        , intent(in   ) :: LoiFrottement
   type(ERREUR_T)                 , intent(inout) :: Erreur

   end subroutine CCL

   end interface

end module M_CCL_I
