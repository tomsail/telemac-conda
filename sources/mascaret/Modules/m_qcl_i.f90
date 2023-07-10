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

module M_QCL_I
!***********************************************************************
! PROGICIEL : MASCARET      A. LEBOSSE
!                           P. CHERUBINI
!                           S. PERON
!                           S. MANDELKERN
!
! VERSION : V8P4R0              EDF-CEREMA
!***********************************************************************
   interface

   subroutine QCL    ( &
   ! Resultats : Objets dont l'etat est mis a jour
     Apport          , & ! tableau des Apports
     Singularite     , & ! tableau des singularites
     Extremite       , & ! tableau des Extremites libres
! Donnees
     LoiHydrau       , & ! tableau des lois hydrauliques
     Temps           , & ! Temps
     np              , & ! numero du pas de temps
     Q1              , & ! Debits mineurs dans les sections de calcul
     Froude          , & ! Nombre de Froude
! Modele
     Connect         , & ! Connectivite du reseau
! Parametre
     NoyauCalcul     , & ! Noyau de calcul utilise
     Erreur            & ! Erreur
                     )

   ! .....................................................................
   !  FONCTION :
   !  --------
   !
   !  La fonctionnalite QCL fournit les valeurs au temps t :
   !  - de chaque loi aux limites Q, ou Z, ou Q et Z, ou loi Z(Q)
   !  - des debits d'apport
   !  - des cotes amont des singularites obeissant a une loi Z(t)
   !
   !----------------------------------------------------------------------------
   !
   !   FICHIERS ENTREE/SORTIE :        Neant
   !   ----------------------
   !
   !   SOUS-PROGRAMME(S) APPELANT(S) :
   !   -----------------------------
   !   SOUS-PROGRAMME(S) APPELE(S)   : - INTERPOLATION_S (dans module)
   !   ---------------------------
   !
   !   COMMENTAIRES :
   !   ------------
   !
   !***********************************************************************

   !============================= Declarations ============================

   !.. Modules importes ..
   !----------------------
   use M_PRECISION
   use M_CONSTANTES_CALCUL_C  ! Constantes pour les phases et modeles de calcul
   use M_MESSAGE_C            ! Messages d'erreur
   use M_APPORT_T             ! Definition du type APPORT_T
   use M_CONNECT_T            ! Definition du type CONNECT_T
   use M_ERREUR_T             ! Definition du type ERREUR_T
   use M_EXTREMITE_T          ! Definition du type EXTREMITE_T
   use M_LOI_T                ! Definition du type LOI_T
   use M_SINGULARITE_T        ! Definition du type SINGULARITE_T
   use M_TRAITER_ERREUR_I     ! Interface generique de gestion des erreurs
   use M_INTERPOLATION_S      ! Sous-programme INTERPOLATION_S

   !.. Declaration Explicite .. 
   !---------------------------
   implicit none

   !.. Arguments .. 
   !---------------
   type(APPORT_T)     , dimension(:), intent(inout) :: Apport
   type(SINGULARITE_T), dimension(:), pointer       :: Singularite
   type(EXTREMITE_T)  , dimension(:), intent(inout) :: Extremite
   ! Donnees
   type(LOI_T)        , dimension(:), intent(in   ) :: LoiHydrau
   real(DOUBLE)                     , intent(in   ) :: Temps
   integer                          , intent(in   ) :: Np
   real(DOUBLE)       , dimension(:), intent(in   ) :: Q1
   real(DOUBLE)       , dimension(:), intent(in   ) :: Froude
   ! Modele
   type(CONNECT_T)                  , intent(in   ) :: Connect
   ! Parametres
   integer                          , intent(in   ) :: NoyauCalcul
   ! Gestion des erreurs
   type(ERREUR_T)                   , intent(inout) :: Erreur

   end subroutine QCL

   end interface

end module M_QCL_I
