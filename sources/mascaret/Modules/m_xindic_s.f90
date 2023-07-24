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

module M_XINDIC_S
!***********************************************************************
! PROGICIEL : MASCARET     A. LEBOSSE
!                          P. CHERUBINI
!                          S. MANDELKERN
!
! VERSION : V8P4R0             EDF-CEREMA
!***********************************************************************
   contains

   subroutine XINDIC_S        ( &
              INDICE          , & ! section de calcul correspondant a XLU
              XLU             , & ! Abscisse donnee
              X               , & ! Tableau des esctions de calcul
              Erreur          )   ! Erreur

   !***********************************************************************
   !  FONCTION :
   !  --------
   !
   !         CE SOUS PROGRAMME DEFINIT L'INDICE DE CALCUL
   !         DE L'ABSCISSE XLU
   !
   !-----------------------------------------------------------------------
   !
   !   FICHIERS ENTREE/SORTIE :
   !   ----------------------
   !
   !   SOUS PROGRAMMES APPELANTS :
   !   ---------------------------
   !   SOUS PROGRAMMES APPELES :    ---
   !   -------------------------
   !
   !   COMMENTAIRES :
   !   ------------
   !
   !***********************************************************************

      !=========================== Declarations ================================
      use M_PRECISION
      use M_MESSAGE_C        ! Messages d'erreur
      use M_ERREUR_T         ! Definition du type ERREUR_T
      use M_TRAITER_ERREUR_I ! Traitement de l'erreur

      !.. Implicit Declarations ..
      implicit none

      !.. Formal Arguments ..
      integer                    , intent(  out) :: INDICE
      real(DOUBLE)               , intent(in   ) :: XLU
      real(DOUBLE), dimension(:) , intent(in   ) :: X
      type(ERREUR_T)             , intent(inout) :: Erreur

      !.. Local Scalars ..
      integer        :: IMAX , J
      !character(132) :: !arbredappel_old

      !========================== Instructions =============================

      ! INITIALISATION
      ! --------------
      Erreur%Numero = 0
      !arbredappel_old    = trim(!Erreur%arbredappel)
      !Erreur%arbredappel = trim(!Erreur%arbredappel)//'=>XINDIC_S'

      IMAX = size(X)

      !   CALCUL DE L'INDICE
      !---------------------
      if( dabs( XLU - X(IMAX) ) <= 0.000001D0 ) then
         INDICE = IMAX
      else if( XLU > X(IMAX) .or. XLU < X(1) ) then
         Erreur%Numero = 25
         Erreur%ft   = err_25
         Erreur%ft_c = err_25c
         call TRAITER_ERREUR (Erreur, XLU)
         return
      else
         do J = 1 , IMAX - 1
            if( XLU >= X(J) .and. XLU < X(J+1) ) then
               INDICE = J
               exit
            end if
         end do
      endif

     !Erreur%arbredappel = !arbredappel_old

   end subroutine XINDIC_S

end module M_XINDIC_S
