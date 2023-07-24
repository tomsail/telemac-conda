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

subroutine CALDYG ( &
     DYGEO        , &
     NOEUD        , &
     JSEC         , &
     SGEO         , &
     X            , &
     DDZPR        , &
     IDT          , &
     XDT          , &
     NSECG        , &
     NSECD        , &
     NBPAS        , &
     Erreur         &
     )

! *********************************************************************
! PROGICIEL : MASCARET         F. MAUREL       N. GOUTAL
!
! VERSION : V8P4R0                EDF-CEREMA
! *********************************************************************
!  FONCTION : CALCUL DE LA VARIABLE DYGEO = (DY/DX) A S CONSTANT
!-----------------------------------------------------------------------
!                             ARGUMENTS
! .___________.____.____.______________________________________________.
! !    NOM    !TYPE!MODE!                   ROLE                       !
! !___________!____!____!______________________________________________!
! !  DYGEO    ! TR !    !                                              !
! !  NOEUD    !  I !  A ! NOEUD CONSIDERE DU MAILLAGE                  !
! !  JSEC     !  I !  A ! INDICE DU PLANIMETRAGE                       !
! !  SGEO     ! TR !    !                                              !
! !  X        ! TR !    !                                              !
! !  DDZPR    ! TR !    ! Pas de planimetrage des profils de donnees   !
! !  IDT      ! TR !    !                                              !
! !  XDT      ! TR !    !                                              !
! !  NSECG    !  I !  A ! NUMERO DE LA SECTION LIMITE DU BIEF A GAUCHE !
! !  NSECD    !  I !  A ! NUMERO DE LA SECTION LIMITE DU BIEF A DROITE !
! !  NBPAS    !  I !  D !                                              !
! !  Erreur   !  I !  R ! Code de retour                               !
! !___________!____!____!______________________________________________!
!
!                             VARIABLES LOCALES
! .___________.____.____.______________________________________________.
! !  SURFI    !  R !  A ! SURFACE MOUILLEE AU NOEUD COURANT            !
! !  JG       !  I !  A ! BORNE GAUCHE DE L'INTERVALLE CONTENANT SURF  !
! !  JD       !  I !  A ! BORNE DROITE DE L'INTERVALLE CONTENANT SURF  !
! !  SEG      !  R !  A ! SURFACE MOUILLE POUR JG                      !
! !  SED      !  R !  A ! SURFACE MOUILLE POUR JD                      !
! !  YEG      !  R !  A ! TIRANT D'EAU POUR L'INDICE JG                !
! !  YED      !  R !  A ! TIRANT D'EAU POUR L'INDICE JD                !
! !  YNM1     !  R !  A ! TIRANT D'EAU AU NOEUD N-1                    !
! !  YNP1     !  R !  A ! TIRANT D'EAU AU NOEUD N+1                    !
! !  YNOEU    !  R !  A ! TIRANT D'EAU AU NOEUD N                      !
! !  DZNOEU   !  R !  A ! PAS DE PLANIMETRAGE AU NOEUD N               !
! !  DZNM1    !  R !  A ! PAS DE PLANIMETRAGE AU NOEUD N-1             !
! !  DZNP1    !  R !  A ! PAS DE PLANIMETRAGE AU NOEUD N+1             !
! !  PROFIG   !  I !  A ! PROF. DE DONNEE GAUCHE DE LA SECT. DE CALCUL !
! !  PROFID   !  I !  A ! PROF. DE DONNEE DROIT  DE LA SECT. DE CALCUL !
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
   use M_DICHO_I     ! Interface du sous-programme DICHO
   use M_ERREUR_T    ! Type ERREUR_T

   !.. Declarations explicites ..
   !-----------------------------
   implicit none

   !.. Arguments ..
   !---------------
   ! 1ere dimension IM
   real(DOUBLE), dimension(:,:)  , intent(  out) :: DYGEO
   integer     ,                   intent(in)    :: NOEUD,JSEC
   ! 1ere dimension IM
   real(DOUBLE), dimension(:,:)  , intent(in)    :: SGEO
   real(DOUBLE), dimension(:)    , intent(in)    :: X
   real(DOUBLE), dimension(:)    , intent(in)    :: DDZPR
   integer     , dimension(:)    , intent(in)    :: IDT
   real(DOUBLE), dimension(:)    , intent(in)    :: XDT
   integer     ,                   intent(in)    :: NSECG,NSECD
   integer     ,                   intent(in)    :: NBPAS
   type(ERREUR_T),                 intent(inout) :: Erreur

   !.. Variables locales ..
   !-----------------------
   integer      :: JG,JD,PROFIG,PROFID
   real(DOUBLE) :: SURFI,SEG,SED,YEG,YED,YNM1,YNP1
   real(DOUBLE) :: DZNOEU,DDZNP1,DDZNM1,YNOEU
   !character(132) :: !arbredappel_old

   !============================= Instructions ===========================
   ! INITIALISATION
   ! --------------
   Erreur%Numero = 0
   !arbredappel_old    = trim(!Erreur%arbredappel)
   !Erreur%arbredappel = trim(!Erreur%arbredappel)//'-CALDYG'

   ! TRAITEMENT DE L'INTERIEUR DU DOMAINE
   ! ************************************
   if( ( NOEUD /= NSECG ).and.( NOEUD /= NSECD ) ) then
      SURFI = SGEO(NOEUD,JSEC)
      ! CALCUL DE Y EN NOEUD-1 POUR SURFI
      call DICHO( JG , JD , SURFI , SGEO(NOEUD-1,:) , Erreur )
      if( Erreur%Numero /= 0 ) then
         return
      endif

      SEG   = SGEO(NOEUD-1,JG)
      SED   = SGEO(NOEUD-1,JD)

      PROFIG = IDT(NOEUD-1)
      PROFID = IDT(NOEUD-1) + 1
      DDZNM1 = DDZPR(PROFIG) + ( DDZPR(PROFID) - DDZPR(PROFIG) ) * XDT(NOEUD-1)

      YEG   = ( JG - 1 ) * DDZNM1
      YED   = ( JD - 1 ) * DDZNM1
      YNM1  = ( YED * ( SURFI - SEG ) + YEG * ( SED - SURFI ) ) / ( SED - SEG )

      ! CALCUL DE Y EN NOEUD+1 POUR SURFI
      call DICHO( JG , JD , SURFI , SGEO(NOEUD+1,:) , Erreur )
      if( Erreur%Numero /= 0 ) then
         return
      endif

      SEG   = SGEO(NOEUD+1,JG)
      SED   = SGEO(NOEUD+1,JD)

      PROFIG = IDT(NOEUD+1)
      PROFID = IDT(NOEUD+1) + 1
      DDZNP1 = DDZPR(PROFIG) + ( DDZPR(PROFID) - DDZPR(PROFIG) ) * XDT(NOEUD+1)

      YEG   = ( JG - 1 ) * DDZNP1
      YED   = ( JD - 1 ) * DDZNP1
      YNP1  = ( YED * ( SURFI - SEG ) + YEG * ( SED - SURFI ) ) / ( SED - SEG )
      DYGEO(NOEUD,JSEC) = ( YNP1 - YNM1 ) / ( ( X(NOEUD+1) - X(NOEUD-1) ) )
   endif

   ! TRAITEMENT DE L'EXTREMITE GAUCHE DU BIEF
   ! ****************************************
   if( NOEUD == NSECG ) then
      SURFI  = SGEO(NOEUD,JSEC)
      PROFIG = IDT(NOEUD)
      PROFID = IDT(NOEUD) + 1
      DZNOEU = DDZPR(PROFIG) + ( DDZPR(PROFID) - DDZPR(PROFIG) ) * XDT(NOEUD)
      YNOEU  = ( JSEC - 1 ) * DZNOEU

      ! CALCUL DE Y EN NOEUD+1 POUR SURFI
      call DICHO( JG , JD , SURFI , SGEO(NOEUD+1,:) , Erreur )
      if( Erreur%Numero /= 0 ) then
         return
      endif

      SEG   = SGEO(NOEUD+1,JG)
      SED   = SGEO(NOEUD+1,JD)

      PROFIG = IDT(NOEUD+1)
      PROFID = IDT(NOEUD+1) + 1
      DDZNP1 = DDZPR(PROFIG) + ( DDZPR(PROFID) - DDZPR(PROFIG) ) * XDT(NOEUD+1)

      YEG   = real( JG - 1 , DOUBLE ) * DDZNP1
      YED   = real( JD - 1 , DOUBLE ) * DDZNP1
      YNP1  = ( YED * ( SURFI - SEG ) + YEG * ( SED - SURFI ) ) / ( SED - SEG )

      DYGEO(NOEUD,JSEC) = ( YNP1 - YNOEU ) / ( ( X(NOEUD+1) - X(NOEUD) ) )
   endif

   if( NOEUD == NSECD ) then
      ! TRAITEMENT DE L'EXTREMITE DROITE DU BIEF
      ! ****************************************
      SURFI = SGEO(NOEUD,JSEC)

      PROFIG = IDT(NOEUD)
      PROFID = IDT(NOEUD) + 1
      DZNOEU = DDZPR(PROFIG) + ( DDZPR(PROFID) - DDZPR(PROFIG) ) * XDT(NOEUD)

      YNOEU = real( JSEC - 1 , DOUBLE ) * DZNOEU

      ! CALCUL DE Y EN NOEUD+1 POUR SURFI
      call DICHO( JG , JD , SURFI , SGEO(NOEUD-1,:) , Erreur )
      if( Erreur%Numero /= 0 ) then
         return
      endif

      SEG   = SGEO(NOEUD-1,JG)
      SED   = SGEO(NOEUD-1,JD)

      PROFIG = IDT(NOEUD-1)
      PROFID = IDT(NOEUD-1) + 1
      DDZNM1 = DDZPR(PROFIG) + ( DDZPR(PROFID) - DDZPR(PROFIG) ) * XDT(NOEUD-1)

      YEG   = real( JG - 1 , DOUBLE ) * DDZNM1
      YED   = real( JD - 1 , DOUBLE ) * DDZNM1
      YNM1  = ( YED * ( SURFI - SEG ) + YEG * ( SED - SURFI ) ) / ( SED - SEG )

      DYGEO(NOEUD,JSEC) = ( YNOEU - YNM1 ) / ( ( X(NOEUD) - X(NOEUD-1) ) )
   endif

   !Erreur%arbredappel = !arbredappel_old

   return

end subroutine CALDYG
