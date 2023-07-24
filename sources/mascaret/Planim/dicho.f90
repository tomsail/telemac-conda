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

subroutine DICHO              ( &
          PasBas              , & ! BORNE basse DE L'INTERV CONTENANT SurfaceMouillee
          PasHaut             , & ! BORNE haute DE L'INTERV CONTENANT SurfaceMouillee
          SurfaceMouillee     , & ! surface mouillee
          SurfaceMouilleePlani, & ! Surface mouillee planimetree
          Erreur                & ! Erreur
                              )
! *********************************************************************
! PROGICIEL : MASCARET         N. GOUTAL       F. MAUREL
!
! VERSION : V8P4R0                EDF-CEREMA
! *********************************************************************
!
! FONCTION :        RECHERCHE PAR DICHOTOMIE DES BORNES DE L'INTERVALLE
!                   CONTENANT LA SURFACE MOUILLEE SurfaceMouillee SUR LE MAILLAGE
!                   INITIAL
!-----------------------------------------------------------------------
!                             VARIABLES LOCALES
! .___________.____.____.______________________________________________.
! !  SurfaceMouilleeBasse !  R !  A ! SURFACE MOUILLE POUR LA BORNE GAUCHE  !
! !  NMAX                 !  I !  A !                                       !
! !  NT                   !  I !  A ! NOMBRE D'ITERATIONS DEJA EFFECTUEES   !
! !  JGP1                 !  I !  A ! PasBas+1 (<PasHaut TANT QU ON ITERE)  !
! !___________!____!____!______________________________________________!
!
!     TYPE : I (ENTIER), R (REEL), A (ALPHANUMERIQUE), T (TABLEAU)
!            L (LOGIQUE)   .. ET TYPES COMPOSES (EX : TR TABLEAU REEL)
!     MODE : D (DONNEE NON MODIFIEE), R (RESULTAT), M (DONNEE MODIFIEE)
!            A (AUXILIAIRE MODIFIE)
!
!***********************************************************************
!  SurfaceMouilleePlani fait partie d'une structure de donnees  STRUCTURES_SECTION

   !============================= Declarations ===========================

   !.. Modules importes ..
   !----------------------
   use M_PRECISION
   use M_MESSAGE_C        ! Messages d'erreurs
   use M_ERREUR_T         ! Type ERREUR_T
   use M_TRAITER_ERREUR_I ! Traitement des erreurs

   !.. Declarations explicites ..
   !-----------------------------
   implicit none

   !.. Arguments ..
   !---------------
   integer     ,                   intent(  out) :: PasBas
   integer     ,                   intent(  out) :: PasHaut
   real(DOUBLE),                   intent(in   ) :: SurfaceMouillee
   real(DOUBLE), dimension(:)    , intent(in   ) :: SurfaceMouilleePlani
   type(ERREUR_T),                 intent(inout) :: Erreur

   !.. Variables locales ..
   !-----------------------
   integer        :: nombre_pas
   integer        :: pas_milieu      ! MILIEU DE L'INTERVALLE [PasBas,PasHaut]
   integer        :: NMAX            ! NOMBRE MAXIMAL D'ITERATION POUR LA DICHOTOMIE
   integer        :: NT,JGP1
   real(DOUBLE)   :: SurfaceMouilleeBasse ! surface mouillee pas bas
   !character(132) :: !arbredappel_old ! arbre dappel en entree

   !============================= Instructions ===========================

   ! INITIALISATIONS
   ! ---------------
   Erreur%Numero = 0
   nombre_pas    = size(SurfaceMouilleePlani(:))
   PasBas        = 1
   PasHaut       = nombre_pas
   NMAX          = nombre_pas / 2
   NT            = 0

   do while( NT + 1 <= NMAX )
      NT                   = NT + 1
      pas_milieu           = ( PasHaut + PasBas ) / 2
      SurfaceMouilleeBasse = SurfaceMouilleePlani(pas_milieu)
      if( SurfaceMouillee > SurfaceMouilleeBasse ) then
         PasBas  = pas_milieu
      else
         PasHaut = pas_milieu
      endif

      ! TEST SUR LA FIN DE LA DICHOTOMIE
      ! --------------------------------
      JGP1 = PasBas + 1
      if (JGP1 >= PasHaut) exit
   end do

   ! IMPRESSION DE CONTROLE EN CAS DE NON CONVERGENCE
   ! ------------------------------------------------
   if (NT > NMAX) then
      Erreur%Numero = 206
      Erreur%ft     = err_206
      Erreur%ft_c   = err_206c
      call TRAITER_ERREUR( Erreur )
      !    !arbredappel_old    = trim(!Erreur%arbredappel)
      !    !Erreur%arbredappel = trim(!Erreur%arbredappel)//'=>DICHO'
      return
   end if

   ! Fin des traitements
   ! -------------------

   ! !Erreur%arbredappel = !arbredappel_old

   return

end subroutine DICHO
