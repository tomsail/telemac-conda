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

function CELE( &
           NOEUD , &
            SURF , &
            SGEO , &
           ALGEO , &
          ERREUR )

!***********************************************************************
! PROGICIEL : MASCARET        N. GOUTAL
!
! VERSION : V8P4R0              EDF-CEREMA
!***********************************************************************
!     FONCTION : CALCUL DE LA SURFACE MOUILLEE
!                     EN FONCTION DU TIRANT D'EAU SUR MAILLAGE INITIAL
!
!-----------------------------------------------------------------------
!                             ARGUMENTS
! .___________.____.____.______________________________________________.
! !    NOM    !TYPE!MODE!                   ROLE                       !
! !___________!____!____!______________________________________________!
! !  NOEUD    !  I !  D ! NOEUD CONSIDERE DU MAILLAGE                  !
! !  SURF     !  R !  D ! SECTION MOUILLEE POUR LAQUELLE ON CALCULE    !
! !           !    !    !      LA CELERITE                             !
! !  SGEO     ! TR !  D ! SURFACE MOUILLE PLANIMETREE                  !
! !  ALGEO    ! TR !  D ! LARGEUR AU MIROIR PLANIMETREE                !
! !___________!____!____!______________________________________________!
!
!                             VARIABLES LOCALES
! .___________.____.____.______________________________________________.
! !  JG       !  I !  A ! BORNE GAUCHE DE L'INTERVALLE CONTENANT SURF  !
! !  JD       !  I !  A ! BORNE DROITE DE L'INTERVALLE CONTENANT SURF  !
! !  SG       !  R !  A ! SURFACE MOUILLE POUR LA BORNE GAUCHE         !
! !  SD       !  R !  A ! SURFACE MOUILLE POUR LA BORNE DROITE         !
! !  ALG      !  R !  A ! LARGEUR AU MIROIR POUR LA BORNE GAUCHE       !
! !  ALD      !  R !  A ! LARGEUR AU MIROIR POUR LA BORNE DROITE       !
! !  ALARG    !  R !  A ! LARGEUR AU MIROIR INTERPOLEE                 !
! !___________!____!____!______________________________________________!
!
!     TYPE : I (ENTIER), R (REEL), A (ALPHANUMERIQUE), T (TABLEAU)
!            L (LOGIQUE)   .. ET TYPES COMPOSES (EX : TR TABLEAU REEL)
!     MODE : D (DONNEE NON MODIFIEE), R (RESULTAT), M (DONNEE MODIFIEE)
!            A (AUXILIAIRE MODIFIE)
!
!***********************************************************************
!  SGEO et ALGEO font partie d'une structure de donnees

   !============================= Declarations ===========================

   !.. Modules importes ..
   !----------------------
   use M_PRECISION
   use M_PARAMETRE_C      ! GPES, EPS10
   use M_MESSAGE_C        ! Messages d'erreur
   use M_ERREUR_T         ! Erreur
   use M_DICHO_I          ! Interface du sous-programme DICHO
   use M_TRAITER_ERREUR_I ! Traitement des erreurs

   !.. Declarations explicites ..
   !-----------------------------
   implicit none

   !.. Arguments ..
   !---------------
   real(DOUBLE)                                  :: CELE
   integer     ,                   intent(in)    :: NOEUD
   real(DOUBLE),                   intent(in)    :: SURF
   ! 1ere dimension IM
   real(DOUBLE), dimension(:,:)  , intent(in)    :: SGEO,ALGEO
   type (ERREUR_T)               , intent(inout) :: ERREUR

   !.. Variables locales ..
   !-----------------------
   integer      :: JG,JD
   real(DOUBLE) :: SG,SD,ALG,ALD
   real(DOUBLE) :: ALARG
   !character(132) :: !arbredappel_old ! arbre d'appel precedent

   !============================= Instructions ===========================

   ! INITIALISATION
   !===============
   Erreur%Numero = 0

   ! RECHERCHE DE L'INTERVALLE CONTENANT SURF PAR DICHOTOMIE
   call DICHO( JG , JD , SURF , SGEO(NOEUD,:) , ERREUR )

   if( Erreur%Numero /= 0 ) then
      CELE = 0.
      return
   endif

   ! CALCUL DE LA LARGEUR ET DE LA SECTION MOUILLEE AUX BORNES
   ALG = ALGEO(NOEUD,JG)
   ALD = ALGEO(NOEUD,JD)
   SG  = SGEO(NOEUD,JG)
   SD  = SGEO(NOEUD,JD)

   ! INTERPOLATION DE LA LARGEUR
   ALARG = ( ALD * ( SURF - SG ) + ALG * ( SD - SURF ) ) / ( SD - SG )
   if( ALARG <= EPS10 ) then
      Erreur%Numero = 104
      Erreur%ft     = err_104
      Erreur%ft_c   = err_104c
      !arbredappel_old    = trim(!Erreur%arbredappel)
      !Erreur%arbredappel = trim(!Erreur%arbredappel)//'=>CELE'
      CELE = 0.
      call TRAITER_ERREUR( Erreur , ALARG , NOEUD , JG , JD )
      return
   endif

   ! CALCUL DE LA CELERITE
   ! ---------------------
   CELE = dsqrt( GPES * SURF / ALARG )

   !------------------
   ! Fin du traitement
   !------------------

   !Erreur%arbredappel = !arbredappel_old

   return

end function CELE
