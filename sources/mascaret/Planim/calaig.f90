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

subroutine CALAIG ( &
     AIGEO        , &
     NOEUD        , &
     JSEC         , &
     X            , &
     SGEO         , &
     CGEO         , &
     ALGEO        , &
     NSECG        , &
     NSECD        , &
     Trav1        , &
     NBPAS        , &
     Erreur         &
     )
! *********************************************************************
! PROGICIEL : MASCARET         N. GOUTAL
!
! VERSION : V8P4R0                EDF-CEREMA
! *********************************************************************
!-----------------------------------------------------------------------
!                             ARGUMENTS
! .___________.____.____.______________________________________________.
! !    NOM    !TYPE!MODE!                   ROLE                       !
! !___________!____!____!______________________________________________!
! !  AIGEO    ! TR !    !                                              !
! !  NOEUD    !  I ! -->! NOEUD CONSIDERE DU MAILLAGE                  !
! !  JSEC     !  I ! -->! INDICE DU PLANIMETRAGE                       !
! !  X        ! TR ! -->!                                              !
! !  SGEO     ! TR !    !                                              !
! !  CGEO     ! TR !    !                                              !
! !  ALGEO    ! TR !    !                                              !
! !  NSECG    !  I !<-->! NUMERO DE LA SECTION LIMITE DU BIEF A GAUCHE !
! !  NSECD    !  I !<-->! NUMERO DE LA SECTION LIMITE DU BIEF A DROITE !
! !  NBPAS    !  I ! -->!                                              !
! !  Erreur   !  I !<-->! Retour                                       !
! !___________!____!____!______________________________________________!
!
!                             VARIABLES LOCALES
! .___________.____.____.______________________________________________.
! !  TRAV1    ! TR !    !                                              !
! !  ALG      !  R !  A ! LARGEUR AU MIROIR  AU NOEUD GAUCHE           !
! !  ALD      !  R !  A ! LARGEUR AU MIROIR  AU NOEUD DROIT            !
! !  SURFI    !  R !  A ! SURFACE MOUILLEE AN NOEUD COURANT            !
! !  JG       !  I !  A ! BORNE GAUCHE DE L'INTERVALLE CONTENANT SURF  !
! !  JD       !  I !  A ! BORNE DROITE DE L'INTERVALLE CONTENANT SURF  !
! !  SEG      !  R !  A ! SURFACE MOUILLE POUR JG                      !
! !  SED      !  R !  A ! SURFACE MOUILLE POUR JD                      !
! !  CELEG    !  R !  A ! CELERITE POUR JG                             !
! !  CELED    !  R !  A ! CELERITE POUR JD                             !
! !  CELNM1   !  R !  A ! CELERITE AU NOEUD N-1                        !
! !  CELNP1   !  R !  A ! CELERITE AU NOEUD N+1                        !
! !  ALNM1    !  R !  A ! LARGEUR AU MIROIR AU NOEUD N-1               !
! !  ALNP1    !  R !  A ! LARGEUR AU MIROIR AU NOEUD N+1               !
! !  DERL     !  R !  A ! DERIVEE DE LA LARGEUR PAR RAPPORT A X        !
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
   use M_ERREUR_T  ! Definition du type ERREUR_T
   use M_DICHO_I   ! Interface du sous-programme DICHO

   !.. Declarations explicites ..
   !-----------------------------
   implicit none

   !.. Arguments ..
   !---------------
   real(DOUBLE), dimension(:,:)  , intent(  out) :: AIGEO
   integer     ,                   intent(in)    :: NOEUD,JSEC
   real(DOUBLE), dimension(:)    , intent(in)    :: X
   ! 1ere dimension IM
   real(DOUBLE), dimension(:,:)  , intent(in)    :: SGEO
   real(DOUBLE), dimension(:,:)  , intent(in)    :: CGEO,ALGEO
   integer     ,                   intent(in)    :: NSECG,NSECD
   integer     ,                   intent(in)    :: NBPAS
   type(ERREUR_T),                 intent(inout) :: Erreur
   real(DOUBLE), dimension(:)     , intent(inout) :: TRAV1

   !.. Variables locales ..
   !-----------------------
   real(DOUBLE)                    :: SURFI,SEG,SED,ALG,ALD,ALNM1,ALNP1,DERL
   real(DOUBLE)                    :: CELEG,CELED,CELNP1,CELNM1
   integer                         :: JG,JD
   !character(132) :: !arbredappel_old

   !============================= Instructions ===========================
   ! INITIALISATION
   ! --------------
   Erreur%Numero = 0
   !arbredappel_old    = trim(!Erreur%arbredappel)
   !Erreur%arbredappel = trim(!Erreur%arbredappel)//'-CALAIG'

   if( ( NOEUD /= NSECG ).and.( NOEUD /= NSECD ) ) then
      ! TRAITEMENT DE L'INTERIEUR DU DOMAINE
      ! ************************************
      SURFI = SGEO(NOEUD,JSEC)

      ! CALCUL DE LA CELERITE EN (NOEUD-1) POUR SURFI
      call DICHO( JG , JD , SURFI , SGEO(NOEUD-1,:) , Erreur )
      if( Erreur%Numero /= 0 ) then
         return
      endif

      SEG    = SGEO(NOEUD-1,JG)
      SED    = SGEO(NOEUD-1,JD)
      CELEG  = CGEO(NOEUD-1,JG)
      CELED  = CGEO(NOEUD-1,JD)
      CELNM1 = ( CELED * ( SURFI - SEG ) + CELEG * ( SED - SURFI ) ) / ( SED - SEG )

      ! CALCUL DE LA CELERITE EN (NOEUD+1) POUR SURFI
      call DICHO( JG , JD , SURFI , SGEO(NOEUD+1,:) , Erreur )
      if( Erreur%Numero /= 0 ) then
         return
      endif

      SEG    = SGEO(NOEUD+1,JG)
      SED    = SGEO(NOEUD+1,JD)
      CELEG  = CGEO(NOEUD+1,JG)
      CELED  = CGEO(NOEUD+1,JD)
      CELNP1 = ( CELED * ( SURFI - SEG ) + CELEG * ( SED - SURFI ) ) / ( SED - SEG )

      TRAV1(JSEC) = ( CELNP1- CELNM1 ) / ( ( X(NOEUD+1) - X(NOEUD-1) ) * SGEO(NOEUD,JSEC) )
      SURFI       = SGEO(NOEUD,JSEC)

      ! CALCUL DE LA CELERITE EN (NOEUD-1) POUR SURFI
      call DICHO( JG , JD , SURFI , SGEO(NOEUD-1,:) , Erreur )
      if( Erreur%Numero /= 0 ) then
         return
      endif

      SEG    = SGEO(NOEUD-1,JG)
      SED    = SGEO(NOEUD-1,JD)
      CELEG  = CGEO(NOEUD-1,JG)
      CELED  = CGEO(NOEUD-1,JD)
      CELNM1 = ( CELED * ( SURFI - SEG ) + CELEG * ( SED - SURFI ) ) / ( SED - SEG )

      ! CALCUL DE LA CELERITE EN (NOEUD+1) POUR SURFI
      call DICHO( JG , JD , SURFI , SGEO(NOEUD+1,:) , Erreur )
      if( Erreur%Numero /= 0 ) then
         return
      endif

      SEG    = SGEO(NOEUD+1,JG)
      SED    = SGEO(NOEUD+1,JD)
      CELEG  = CGEO(NOEUD+1,JG)
      CELED  = CGEO(NOEUD+1,JD)
      CELNP1 = ( CELED * ( SURFI - SEG ) + CELEG * ( SED - SURFI ) ) / ( SED - SEG )

      TRAV1(JSEC) = ( CELNP1- CELNM1 ) / ( (X(NOEUD+1) - X(NOEUD-1) ) * SGEO(NOEUD,JSEC) )

      if( JSEC > 2 ) then
         AIGEO(NOEUD,JSEC) = AIGEO(NOEUD,JSEC-1) -             &
                           ( TRAV1(JSEC) + TRAV1(JSEC-1) ) *   &
                           ( SGEO (NOEUD,JSEC) - SGEO (NOEUD,JSEC-1) ) / 2._DOUBLE
      else
         TRAV1(1) = 0._DOUBLE
         ! CALCUL DE AIGEO(NOEUD,2)
         SURFI = SGEO(NOEUD,JSEC)
         ! LARGEUR EN NOEUD-1 POUR SURFI
         call DICHO( JG , JD , SURFI , SGEO(NOEUD-1,:) , Erreur )
         if( Erreur%Numero /= 0 ) then
            return
         endif

         SEG    = SGEO(NOEUD-1,JG)
         SED    = SGEO(NOEUD-1,JD)
         ALG    = ALGEO(NOEUD-1,JG)
         ALD    = ALGEO(NOEUD-1,JD)
         ALNM1  = ( ALD * ( SURFI - SEG ) + ALG * ( SED - SURFI ) ) / ( SED - SEG )

         ! LARGEUR EN NOEUD+1 POUR SURFI
         call DICHO( JG , JD , SURFI , SGEO(NOEUD+1,:) , Erreur )
         if( Erreur%Numero /= 0 ) then
            return
         endif

         SEG    = SGEO(NOEUD+1,JG)
         SED    = SGEO(NOEUD+1,JD)
         ALG    = ALGEO(NOEUD+1,JG)
         ALD    = ALGEO(NOEUD+1,JD)
         ALNP1  = ( ALD * ( SURFI - SEG ) + ALG * ( SED - SURFI ) ) / ( SED - SEG )
         DERL   = ( ALNP1 - ALNM1 ) / ( ( X(NOEUD+1) - X(NOEUD-1) ) * ALGEO(NOEUD,JSEC) )
         AIGEO(NOEUD,JSEC) = DERL * CGEO(NOEUD,JSEC)
      endif
   endif

   ! TRAITEMENT DE L'EXTREMITE GAUCHE DU BIEF
   ! ****************************************
   if( NOEUD == NSECG ) then
      SURFI = SGEO(NOEUD,JSEC)
      ! CELERITE EN NOEUD+1 POUR SURFI
      call DICHO( JG , JD , SURFI , SGEO(NOEUD+1,:) , Erreur )
      if( Erreur%Numero /= 0 ) then
         return
      endif

      SEG     = SGEO(NOEUD+1,JG)
      SED     = SGEO(NOEUD+1,JD)
      CELEG   = CGEO(NOEUD+1,JG)
      CELED   = CGEO(NOEUD+1,JD)
      CELNP1  = ( CELED * ( SURFI - SEG ) + CELEG * ( SED - SURFI ) ) / ( SED - SEG )
      TRAV1(JSEC) = ( CELNP1 - CGEO(NOEUD,JSEC) ) / ( ( X(NOEUD+1) - X(NOEUD) ) * SGEO(NOEUD,JSEC) )

      if( JSEC > 2 ) then
         AIGEO(NOEUD,JSEC) = AIGEO(NOEUD,JSEC-1) -           &
             ( TRAV1(JSEC) + TRAV1(JSEC-1) )     *           &
             ( SGEO (NOEUD,JSEC  ) - SGEO (NOEUD,JSEC-1) ) / 2._DOUBLE
      else
         TRAV1(1) = 0._DOUBLE
         ! CALCUL AIGEO(NOEUD,2)
         SURFI    = SGEO(NOEUD,JSEC)
         ! LARGEUR EN NOEUD+1 POUR SURFI
         call DICHO( JG , JD , SURFI , SGEO(NOEUD+1,:) , Erreur )
         if( Erreur%Numero /= 0 ) then
            return
         endif

         SEG     = SGEO(NOEUD+1,JG)
         SED     = SGEO(NOEUD+1,JD)
         ALG     = ALGEO(NOEUD+1,JG)
         ALD     = ALGEO(NOEUD+1,JD)
         ALNP1   = ( ALD*(SURFI-SEG) + ALG*(SED-SURFI) ) / ( SED - SEG )
         DERL    = (ALNP1 - ALGEO(NOEUD,JSEC)) / ( ( X(NOEUD+1) - X(NOEUD) ) * ALGEO(NOEUD,JSEC) )
         AIGEO(NOEUD,JSEC) = DERL * CGEO(NOEUD,JSEC)
      endif
   endif

   ! TRAITEMENT DE L'EXTREMITE DROITE DU BIEF
   ! ****************************************
   if( NOEUD == NSECD ) then
      SURFI   = SGEO(NOEUD,JSEC)
      !           CELERITE EN NOEUD-1 POUR SURFI
      call DICHO( JG , JD , SURFI , SGEO(NOEUD-1,:) , Erreur )
      if( Erreur%Numero /= 0 ) then
         return
      endif

      SEG     = SGEO(NOEUD-1,JG)
      SED     = SGEO(NOEUD-1,JD)
      CELEG   = CGEO(NOEUD-1,JG)
      CELED   = CGEO(NOEUD-1,JD)
      CELNM1  = ( CELED*(SURFI-SEG) + CELEG*(SED-SURFI) ) / ( SED - SEG )
      TRAV1(JSEC) = ( CGEO(NOEUD,JSEC)- CELNM1 ) / ( ( X(NOEUD) - X(NOEUD-1) ) * SGEO(NOEUD,JSEC) )

      if( JSEC > 2 ) then
         AIGEO(NOEUD,JSEC) = AIGEO(NOEUD,JSEC-1) -                        &
                             ( TRAV1(JSEC) + TRAV1(JSEC-1) ) *            &
                             ( SGEO (NOEUD,JSEC  ) - SGEO (NOEUD,JSEC-1)) &
                              / 2._DOUBLE
      else
         !              CALCUL AIGEO(NOEUD,2)
         SURFI   = SGEO(NOEUD,JSEC)
         !              LARGEUR EN NOEUD-1 POUR SURFI
         call DICHO( JG , JD , SURFI , SGEO(NOEUD-1,:) , Erreur )
         if( Erreur%Numero /= 0 ) then
            return
         endif

         SEG     = SGEO(NOEUD-1,JG)
         SED     = SGEO(NOEUD-1,JD)
         ALG     = ALGEO(NOEUD-1,JG)
         ALD     = ALGEO(NOEUD-1,JD)
         ALNM1   = ( ALD * ( SURFI - SEG ) + ALG * ( SED - SURFI ) ) / ( SED - SEG )
         DERL    = (ALGEO(NOEUD,JSEC) - ALNM1) /                     &
                  ( ( X(NOEUD) - X(NOEUD-1) ) * ALGEO(NOEUD,JSEC) )
         AIGEO(NOEUD,JSEC) = DERL * CGEO(NOEUD,JSEC)
      endif
   endif

   !Erreur%arbredappel = !arbredappel_old
   return

end subroutine CALAIG
