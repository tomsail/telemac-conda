!== Copyright (C) 2000-2022 EDF-CEREMA ==
!
!   This file is part of MASCARET-TRACER.
!
!   MASCARET-TRACER is free software: you can redistribute it and/or modify
!   it under the terms of the GNU General Public License as published by
!   the Free Software Foundation, either version 3 of the License, or
!   (at your option) any later version.
!
!   MASCARET-TRACER is distributed in the hope that it will be useful,
!   but WITHOUT ANY WARRANTY; without even the implied warranty of
!   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
!   GNU General Public License for more details.
!
!   You should have received a copy of the GNU General Public License
!   along with MASCARET-TRACER.  If not, see <http://www.gnu.org/licenses/>
!

module M_CALK_I
!***********************************************************************
! PROGICIEL : TRACER         S.MANDELKERN - E.LEHMANN
!
! VERSION : V8P4R0              EDF-CEREMA
!***********************************************************************
   Interface

   SUBROUTINE CALK ( RK , RKC ,       &
                     U , ST , B , H , &
                     Nbsect , NOPTK   )
   !
   !***********************************************************************
   !
   !  FONCTION :
   !  --------
   !            CALCUL DU COEFFICIENT DE DIFFUSION
   !
   !_________________________________________________________________________________
   !   NOM   !TYPE!MODE!                      ROLE                                  !
   !_________!____!____!____________________________________________________________!
   !                                                                                !
   !                              PARAMETRES D'APPEL                                !
   !________________________________________________________________________________!
   !   RK    ! R  !    ! COEFFICIENT DE DIFFUSION                                   !
   !   IM    ! E  !    ! Dimension du systeme                                       !
   !   X     ! TR !    ! Position de la section (abscisse)                          !
   !   H     ! TR !    ! Hauteur d'eau                                              !
   !   A     ! TR !    ! Section mouillee                                           !
   !   U     ! TR !    ! Champ du vecteur vitesse                                   !
   !________________________________________________________________________________!
   !  MODE: -->(DONNEE NON MODIFIEE),<--(RESULTAT),<-->(DONNEE MODIFIEE)
   !               (ENTREE)              (SORTIE)       (ENTREE/SORTIE)
   !-----------------------------------------------------------------------
   !
   !   FICHIERS ENTREE/SORTIE :
   !   ----------------------
   !
   !   SOUS-PROGRAMMES APPELANT : TRACER
   !   ------------------------
   !***********************************************************************
   !
   !
   use M_PRECISION
   use M_PARAMETRE_C
   !
   ! * DECLARATION DES ARGUMENTS *
   real (DOUBLE) , dimension(Nbsect) , intent(inout) :: RK 
   real (DOUBLE) , dimension(2)      , intent(inout) :: RKC
   real (DOUBLE) , dimension(Nbsect) , intent(in   ) :: U ,ST, B, H
   integer Nbsect , NOPTK

   end subroutine CALK

   end interface

end module M_CALK_I
