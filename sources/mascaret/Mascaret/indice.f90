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

subroutine INDICE( &
                   I , &
                ABSC , &
                   X , &
              NBSECT , &
              Erreur )

!***********************************************************************
! PROGICIEL : MASCARET        N. GOUTAL
!
! VERSION : V8P4R0              EDF-CEREMA
!***********************************************************************
!
! FONCTION :        RECHERCHE PAR DICHOTOMIE L'INDICE DE CALCUL
!
!
!-----------------------------------------------------------------------
!                             ARGUMENTS
! .___________.____.____.______________________________________________.
! !    NOM    !TYPE!MODE!                   ROLE                       !
! !___________!____!____!______________________________________________!
! !  I        !  I !  A ! INDICE                                       !
! !  ABSC     !  R !  D ! ABSCISSE                                     !
! !  X        ! TR !  D ! ABSCISSE DU MAILLAGE                         !
! !  NBSECT   !  I !    ! Nombre de sections                           !
! !___________!____!____!______________________________________________!
!
!     TYPE : I (ENTIER), R (REEL), A (ALPHANUMERIQUE), T (TABLEAU)
!            L (LOGIQUE)   .. ET TYPES COMPOSES (EX : TR TABLEAU REEL)
!     MODE : D (DONNEE NON MODIFIEE), R (RESULTAT), M (DONNEE MODIFIEE)
!            A (AUXILIAIRE MODIFIE)
!
!***********************************************************************

   !============================= Declarations ===========================

   !.. Modules importes ..
   !----------------------
   use M_PRECISION
   use M_MESSAGE_C           ! Messages d'erreur
   use M_PARAMETRE_C         ! EPS2
   use M_ERREUR_T            ! ERREUR
   use M_TRAITER_ERREUR_I    ! Traitement des erreurs

   !.. Declarations explicites ..
   !-----------------------------
   implicit none

   !.. Arguments ..
   !---------------
   integer     ,                   intent(  out) :: I
   real(DOUBLE),                   intent(in)    :: ABSC
   ! 1ere dimension IM
   real(DOUBLE), dimension(:)    , intent(in)    :: X
   integer     ,                   intent(in)    :: NBSECT
   Type (ERREUR_T)               , intent(inout) :: ERREUR

   !.. Variables locales ..
   !-----------------------
   integer IG,ID
   integer IMIL,NMAX,NT,IGP1
   real(DOUBLE) XG
   !character(132) :: !arbredappel_old ! arbre d'appel precedent

   !============================= Instructions ===========================

   ! INITIALISATION
   !===============
   Erreur%Numero = 0
   !   !arbredappel_old    = trim(!Erreur%arbredappel)
   !   !Erreur%arbredappel = trim(!Erreur%arbredappel)//'=>INDICE'

   ! INITIALISATIONS
   ! ---------------
   IG   = 1
   ID   = NBSECT
   NMAX = NBSECT / 2
   NT   = 0

   do while( NT <= NMAX )

      NT   = NT + 1
      IMIL = ( ID + IG ) / 2
      XG   = X(IMIL)
      if( ABSC >= XG - EPS2 ) then
         IG = IMIL
      else
         ID = IMIL
      endif

      ! TEST SUR LA FIN DE LA DICHOTOMIE
      ! --------------------------------
      IGP1 = IG + 1
      if( IGP1 >= ID )  exit

   end do

   I = IG

   ! IMPRESSION DE CONTROLE EN CAS DE NON CONVERGENCE
   ! ------------------------------------------------
   if( NT > NMAX ) then
      Erreur%Numero = 106
      Erreur%ft     = err_106
      Erreur%ft_c   = err_106c
      call TRAITER_ERREUR( Erreur )
      return
   end if

   !------------------
   ! Fin du traitement
   !------------------

   !Erreur%arbredappel = !arbredappel_old

   return

end subroutine INDICE
