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

module M_DEBITANCE_S
!***********************************************************************
! PROGICIEL : MASCARET        N. GOUTAL
!
! VERSION : V8P4R0              EDF-CEREMA
!***********************************************************************
   contains

   subroutine DEBITANCE_S ( &
             DEB1         , & ! Debitance
             ST1          , & ! Strickler mineur
             RH1          , & ! Rayon hydraulique mineur
             S1           , & ! Section mouillee mineur
             LoiFrottement, & ! Loi de frottement
             CF1          , & ! Coefficient de frottement mineur
             Erreur         &
                          )

   !============================= Declarations ===========================
   use M_PRECISION
   use M_CONSTANTES_CALCUL_C ! LOI_FROTTEMENT_STRICKLER
   use M_PARAMETRE_C         ! GPES, W23,
   use M_ERREUR_T            ! Definition du type Erreur

   implicit none

   ! Arguments
   !----------
   real(DOUBLE), intent(  out) :: DEB1
   real(DOUBLE), intent(  out) :: ST1
   real(DOUBLE), intent(in   ) :: RH1
   real(DOUBLE), intent(in   ) :: S1
   integer     , intent(in   ) :: LoiFrottement
   real(DOUBLE), intent(in   ) :: CF1
   type(ERREUR_T), intent(inout) :: Erreur

   ! Variables locales
   !------------------
   real(DOUBLE)   :: chezy
   !character(132) :: !arbredappel_old ! arbre d'appel precedent

   ! Constantes
   !-----------

   !============================= Instructions ===========================

   ! INITIALISATION
   !===============
   Erreur%Numero = 0
   !arbredappel_old    = trim(!Erreur%arbredappel)
   !Erreur%arbredappel = trim(!Erreur%arbredappel)//'=>DEBITANCE'

   IF( RH1.LT.EPS6 ) THEN
      ST1 = CF1
      DEB1 = 0._DOUBLE
      Return
   ENDIF

   select case (LoiFrottement)
   !----------------------------------------------------
   ! Strickler fixe
   !----------------------------------------------------
      case(LOI_FROTTEMENT_STRICKLER)

         chezy = CF1 * ( RH1**W16 )

      !----------
      ! Chezy fixe
      !----------
      case(LOI_FROTTEMENT_CHEZY)

         chezy = CF1

      !----------------------------------------------------
      ! COLEBROOK   x(alpha[ks])
      !----------------------------------------------------
     case(LOI_FROTTEMENT_COLEBROOK)

        chezy = 2._DOUBLE *                             &
                ( 0.190_DOUBLE * CF1 + 0.972_DOUBLE ) * &
                 dsqrt( 8._DOUBLE * GPES ) *            &
                 DLOG10( 12._DOUBLE * RH1 / CF1 )

      !----------------------------------------------------
      ! BAZIN   Chezy(i) en fonction de RH et mb (fixe)
      !----------------------------------------------------
      case(LOI_FROTTEMENT_BAZIN)

         chezy = 45._DOUBLE / ( 1._DOUBLE + CF1 / RH1 )

   end select

   ST1 = chezy  / ( RH1**W16 )

   DEB1 = ST1 * S1 * RH1**W23

   !------------------
   ! Fin du traitement
   !------------------

   !Erreur%arbredappel = !arbredappel_old

   return

   end subroutine DEBITANCE_S

end module M_DEBITANCE_S
