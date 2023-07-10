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

module M_CALCFL_I
! *********************************************************************
! PROGICIEL : MASCARET         F. MAUREL
!
! VERSION : V8P4R0                EDF-CEREMA
! *********************************************************************
   interface

   subroutine CALCFL ( &
        DT           , &
        DTI          , &
        IFIGE,ICOMPT , &
        Opt          , &
        UNODE        , &
        CNODE        , &
        X            , &
        I1           , &
        I2           , &
        CFL          , &
        NBBIEF       , &
        Phase_Post_Imp   , & ! Flag d'impression
        UniteListing , & ! Unite logique fichier listing
        DTVAR        , &
        Erreur         & ! Erreur
                  )

!***********************************************************************
!  FONCTION :
!  --------
!
!      AJUSTEMENT DU PAS DE TEMPS EN FONCTION DE LA CONDITION CFL
!
!-----------------------------------------------------------------------
!
!                             ARGUMENTS
! .___________.____.____.______________________________________________.
! !    NOM    !TYPE!MODE!                   ROLE                       !
! !___________!____!____!______________________________________________!
! !  DT       !  R !  M ! PAS DE TEMPS                                 !
! !  UNODE    ! TR !  D ! VITESSE                                      !
! !  CNODE    ! TR !  D ! CELERITE                                     !
! !  X        ! TR !  D ! ABSCISSES DES POINTS DU MAILLAGE             !
! !  I1,I2    ! TI !  D ! INDICE DES EXTREMITES DE CHAQUE BIEF         !
! !  CFL      !  R !  M ! NOMBRE DE COURANT DESIRE                     !
! !  NBBIEF   !  I !    ! Nombre de biefs                              !
! !  DTVAR    !  L !  D ! LOGIQUE D'AJUSTEMENT DU PAS DE TEMPS         !
! !___________!____!____!______________________________________________!
!
!                             VARIABLES LOCALES
! .___________.____.____.______________________________________________.
! !  CMAX     !  R !  M ! NOMBRE DE COURANT MAXIMAL                    !
! !  NMAX     !  I !  M ! POINT OU LE NB DE CORANT EST MAXIMAL         !
! !  CNODE1   !  R !  A ! VITESSE + CELERITE (U+C)                     !
! !  CNODE2   !  R !  A ! VITESSE - CELERITE (U-C)                     !
! !  COUR     !  R !  A ! NB. DE COURANT DANS LA CELLULE               !
! !  COU      !  R !  A ! (NB. DE COURANT DANS LA CELLULE)*DT          !
! !  CMA      !  R !  A ! (NB. DE COURANT MAX)*DT                      !
! !  NOEUD    !  I !  A ! POINT COURANT DU MAILLAGE                    !
! !  IBIEF    !  I !  A ! NUMERO DU BIEF COURANT                       !
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
   use M_CONSTANTES_CALCUL_C ! phase du calcul
   use M_ERREUR_T  ! ERREUR

   !.. Declarations explicites ..
   !-----------------------------
   implicit none

   !.. Arguments ..
   !---------------
   real(DOUBLE),               intent(inout) ::  DT
   real(DOUBLE),dimension(:), intent(inout) :: DTI
   ! 1ere dimension IM
   real(DOUBLE), dimension(:), intent(in)    :: UNODE,CNODE,X
   integer     , dimension(:), intent(in)    :: I1,I2
   integer     , dimension(:),intent(inout)  :: IFIGE
   real(DOUBLE),               intent(in)    :: CFL
   integer     ,               intent(in)    :: NBBIEF
   integer     ,               intent(inout) :: Icompt
   Integer     ,               intent(in)    :: Phase_Post_Imp
   integer     ,               intent(in   ) :: UniteListing
   logical     ,               intent(in)    :: DTVAR, OPT
   Type (ERREUR_T)           , intent(inout) :: ERREUR

   end subroutine CALCFL

   end interface

end module M_CALCFL_I
