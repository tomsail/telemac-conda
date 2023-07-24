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

subroutine BORNE( &
               SFIN , &
               QFIN , &
              INDIC , &
              IBIEF , &
               IFIN , &
               IDEB , &
              XFRON , &
                  X , &
              SNODE , &
              QNODE , &
             Erreur )

!***********************************************************************
! PROGICIEL : MASCARET        N. GOUTAL
!
! VERSION : V8P4R0              EDF-CEREMA
!
!             CALCUL DES BORNES AMONT et AVAL POUR UN BIEF
!
!-----------------------------------------------------------------------
!                             ARGUMENTS
! .___________.____.____.______________________________________________.
! !    NOM    !TYPE!MODE!                   ROLE                       !
! !___________!____!____!______________________________________________!
! !  IBIEF    !  I !    !                                              !
! !  IDEB     !  I !  A ! INDICE DE DEBUT D'UN BIEF                    !
! !  IFIN     !  I !  D ! INDICE DE FIN D'UN BIEF                      !
! !  XFRON    !  R !  D ! POSITION DU FRONT D'ONDE                     !
! !  X        ! TR !  M ! ABSCISSE DE CALCUL                           !
! !  NBARA    !  I !    !                                              !
! !  SFIN     !  R !  D ! SECTION MOUILLEE DE FIN DE BIEF              !    
! !  QFIN     !  R !  D ! DEBIT DE FIN DE BIEF                         !
! !  SNODE    ! TR !  D ! SECTION MOUILLEE                             !
! !  INDIC    ! TI !    ! INDICATEUR DE BIEF                           ! 
! !  NBSING   !  I !    ! Nombre de singularites                       !
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
   use M_MESSAGE_C         ! Messages d'erreur
   use M_ERREUR_T          ! Type ERREUR_T
   use M_INDICE_I          ! Interface du sous-programme INDICE
   use M_TRAITER_ERREUR_I  ! Traitement des erreurs

   !.. Declarations explicites ..
   !-----------------------------
   implicit none

   !.. Arguments ..
   !---------------
   integer     , dimension(:), intent(  out) :: INDIC
   integer     ,               intent(inout) :: IDEB,IFIN
   integer     ,               intent(in)    :: IBIEF
   real(DOUBLE),               intent(in)    :: XFRON
   real(DOUBLE),               intent(inout) :: SFIN,QFIN
   ! 1ere dimension IM
   real(DOUBLE), dimension(:), intent(in)    :: X
   ! 1ere dimension IM
   real(DOUBLE), dimension(:), intent(in)    :: SNODE,QNODE
   Type (ERREUR_T)           , intent(inout) :: ERREUR

   !.. Variables locales ..
   !-----------------------
   integer :: IL
   integer :: IFRON
   integer ::  NBSECT 
   !character(132) :: arbredappel_old ! arbre d'appel precedent

   !============================= Instructions ===========================
   ! INITIALISATION
   !===============
   Erreur%Numero = 0
   !arbredappel_old    = trim(Erreur%arbredappel)
   !Erreur%arbredappel = trim(Erreur%arbredappel)//'=>BORNE'

   !   DETERMINATION DES LIMITES DANS UN BIEF PAR RAPPORT AU FRONT
   !       LA LIMITE AVAL DU DOMAINE EST SITUE 5 MAILLES  A l'AVAL
   !                     DU FRONT OU AU BARRAGE
   NBSECT = size( X )

   if( ( XFRON <= X(IFIN) ).and.( XFRON >= X(IDEB) ) ) then
      call INDICE( IFRON , XFRON , X , NBSECT , ERREUR )
      IL = IFRON + 10
      if( IL < IFIN ) then

         IFIN         = IL
         INDIC(IBIEF) = 1
         SFIN         = SNODE(IFIN)
         QFIN         = QNODE(IFIN)
      else
         INDIC(IBIEF) = 1
      end if
   else
      INDIC(IBIEF) = -1
      IFIN         = IDEB + 5
      SFIN         = SNODE(IFIN)
      QFIN         = 0._DOUBLE
   end if

   !------------------
   ! Fin du traitement
   !------------------
   !Erreur%arbredappel = !arbredappel_old

   return

end subroutine BORNE
