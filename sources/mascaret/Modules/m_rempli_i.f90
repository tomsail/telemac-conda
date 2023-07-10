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

module M_REMPLI_I
!***********************************************************************
! PROGICIEL : MASCARET      A. LEBOSSE
!                           P. CHERUBINI
!                           S. PERON
!                           S. MANDELKERN
!
! VERSION : V8P4R0              EDF-CEREMA
!***********************************************************************
   interface

   subroutine  REMPLI(            &
            DZB                 , & ! Matrice du systeme lineaire
            ZCA,ZCB,ZCC         , & ! Coefficients de la rel de condensation
            ZTA,ZTB,ZTC         , & ! Coefficients de la rel de condensation
            RDLIM,SDLIM,TDLIM   , & ! Coeff des rel discretisees des cond aux limites
            Connect             , & ! Table de connectivite
            Erreur                & ! Erreur
                                )

   ! .....................................................................
   !  FONCTION :
   !  --------
   ! .                                                                   .
   ! . CONSTITUTION DE LA MATRICE DZB DU SYSTEME LINEAIRE SERVANT A      .
   ! . CALCULER LES DZ DANS LES SECTIONS EXTREMES DES BIEFS              .
   ! .....................................................................
   ! . ZC*    .  --> . COEFFICIENT DE LA RELATION DE CONDENSATION :      .
   ! .        .      . DQ(ORIGINE)=                                      .
   ! .        .      . ZCA*DZ(ORIGINE)+ZCB+ZCC*DZ(EXTREMITE)             .
   ! . ZT*    .  --> . COEFFICIENT DE LA RELATION DE CONDENSATION :      .
   ! .        .      . DQ(EXTREMITE)=                                    .
   ! .        .      . ZTA*DZ(EXTREMITE)+ZTB +ZTC*DZ(ORIGINE)
   ! . Relation discretisee de la condition a la limite i :
   ! . RDLIM(i)*DeltaQ + SDLIM(i)*DeltaZ = TDLIM(i)                      .
   ! .....................................................................
   ! . LES COEFFICIENTS DE CETTE MATRICE SONT OBTENUS AU MOYEN DES       .
   ! . COEFFICIENTS ZCA,...,ZTA,..., CALCULES DANS LE SOUS-PROGRAMME     .
   ! . CONDEN , ET DES COEFFICIENTS RDLIM,SDLIM,TDLIM, CALCULES DANS LE  .
   ! . SOUS-PROGRAMME LOILIM                                             .
   ! .....................................................................

   !============================ Declarations ==============================
   use M_PRECISION        ! Type DOUBLE
   use M_PARAMETRE_C      ! Parametres de calcul
   use M_MESSAGE_C        ! Liste des messages d'erreur
   use M_ERREUR_T         ! Definition du type ERREUR_T
   use M_CONNECT_T        ! Definition du type CONNECT_T
   use M_TRAITER_ERREUR_I ! Traitement des erreurs

   implicit none

   !.. Formal Arguments .. 
   real(DOUBLE), dimension(:,:), intent(inout) :: DZB
   real(DOUBLE), dimension(:), intent(in   ) :: ZCA, ZCB, ZCC
   real(DOUBLE), dimension(:), intent(in   ) :: ZTA, ZTB, ZTC
   real(DOUBLE), dimension(:), intent(in   ) :: RDLIM, SDLIM, TDLIM
   type(CONNECT_T)           , intent(in   ) :: Connect
   type(ERREUR_T)             , intent(inout) :: Erreur

   end subroutine REMPLI

   end interface

end module M_REMPLI_I
