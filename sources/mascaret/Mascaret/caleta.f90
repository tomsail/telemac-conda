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

subroutine CALETA( &
                   S , &
                   Q , &
                   U , &
                  FR , &
                   C , &
                   H , &
                   Z , &
                  PR , &
                BETA , &
               INDIC , &
               SNODE , &
               QNODE , &
               UNODE , &
               ZNODE , &
               YNODE , &
              JGNODE , &
              JDNODE , &
               FROUD , &
               CNODE , &
               BETAN , &
                COTR , &
                  DZ , &
                 DZD , &
                SGEO , &
               ALGEO , &
               SGEOD , &
              PRGEOD , &
                HEPS , &
               NOEUD , &
              CELGAU , &
              NMLARG , &
              ERREUR )

!***********************************************************************
! PROGICIEL : MASCARET        F. MAUREL
!
! VERSION : V8P4R0              EDF-CEREMA
!***********************************************************************
!
!  FONCTION : CALCUL DE L'ETAT DANS UNE CELLULE POUR UN SOLVEUR DE ROE
!
!
!-----------------------------------------------------------------------
!                             ARGUMENTS
! .___________.____.____.______________________________________________.
! !    NOM    !TYPE!MODE!                   ROLE                       !
! !___________!____!____!______________________________________________!
! !  S        !  R !  M ! SECTION MOUILLEE                             !
! !  Q        !  R !  M ! DEBIT                                        !
! !  U        !  R !  M ! VITESSE                                      !
! !  FR       !  R !  M ! FROUDE                                       !
! !  C        !  R !  M ! CELERITE                                     !
! !  H        !  R !  M ! TIRANT D'EAU                                 !
! !  Z        !  R !  M ! COTE SURFACE LIBRE                           !
! !  PR       !  R !  M ! PRESSION                                     !
! !  BETA     !  R !  M !                                              !
! !  INDIC    !  I !  M !                                              !
! !  SNODE    ! TR !  D ! SURFACE MOUILLEE                             !
! !  QNODE    ! TR !  D ! DEBIT                                        !
! !  UNODE    ! TR !  D ! VITESSE                                      !
! !  ZNODE    ! TR !  D !                                              !
! !  YNODE    ! TR !  D !                                              !
! !  FROUD    ! TR !  D !                                              !
! !  CNODE    ! TR !  D !                                              !
! !  BETAN    ! TR !  D !                                              !
! !  COTR     ! TR !  D ! COTE DU RADIER                               !
! !  DZ       ! TR !  D ! PAS DE PLANIMETRAGE                          !
! !  DZD      ! TR !  D ! PAS DE PLANIMETRAGE AUX INTERFACES           !
! !  SGEO     ! TR !  D ! SURFACES PLANIMETREES                        !
! !  ALGEO    ! TR !  D ! LARGEUR PLANIMETREE                          !
! !  SGEOD    ! TR !  D ! SURFACES PLANIMETREES(Maillage decale)       !
! !  PRGEOD   ! TR !  D ! PRESSION PLANIMETREE (MAILLAGE DECALE)       !
! !  HEPS     !  R !  D ! VALEUR MINIMALE DU TIRANT D'EAU              !
! !  NOEUD    !  I !  D ! NUMERO DE CELLULE COURANT                    !
! !  CELGAU   !  I !  D ! = 1 SI NOEUD : CELLULE GAUCHE DE L INTERFACE !
! !           !    !    ! = 0 SI NOEUD : CELLULE DROITE DE L INTERFACE !
! !  NMLARG   !    !  D !                                              !
! !___________!____!____!______________________________________________!
!
!     TYPE : I (ENTIER), R (REEL), A (ALPHANUMERIQUE), T (TABLEAU)
!            L (LOGIQUE)   .. ET TYPES COMPOSES (EX : TR TABLEAU REEL)
!     MODE : D (DONNEE NON MODIFIEE), R (RESULTAT), M (DONNEE MODIFIEE)
!            A (AUXILIAIRE MODIFIE)
!
!***********************************************************************
!  SGEO, ALGEO,SGEOD  et PRGEOD font partie d'une structure donnees

   !============================= Declarations ===========================

   !.. Modules importes ..
   !----------------------
   use M_PRECISION
   use M_ERREUR_T    ! ERREUR
   use M_PARAMETRE_C ! ERREUR
   use M_CELE_I      ! Interface de la fonction    CELE
   use M_CSUR_I      ! Interface de la fonction    CSUR
   use M_PRESD_I     ! Interface de la fonction    PRESD
   use M_DICHODM_I
   use M_DICHOD_I

   !.. Declarations explicites ..
   !-----------------------------
   implicit none

   !.. Arguments ..
   !---------------
   real(DOUBLE),                 intent(  out)   :: S,Q,U,FR,C,H,Z,PR,BETA
   integer     ,                 intent(inout)   :: INDIC
   ! 1ere dimension IM
   real(DOUBLE), dimension(:)  , intent(in)      :: SNODE,QNODE,UNODE
   real(DOUBLE), dimension(:)  , intent(in)      :: ZNODE,YNODE
   integer     , dimension(:)  , intent(in)      :: JGNODE,JDNODE
   real(DOUBLE), dimension(:)  , intent(in)      :: FROUD,CNODE,BETAN
   real(DOUBLE), dimension(:)  , intent(in)      :: COTR,DZ
   ! 1ere dimension IM1
   real(DOUBLE), dimension(:)  , intent(in)      :: DZD
   ! 1ere dimension IM , 2nde dimension NMLARG
   real(DOUBLE), dimension(:,:), intent(in)      :: SGEO,ALGEO
   ! 1ere dimension IM1, 2nde dimension NMLARG
   real(DOUBLE), dimension(:,:), intent(in)      :: SGEOD,PRGEOD
   real(DOUBLE),                 intent(in)      :: HEPS
   integer     ,                 intent(in)      :: NOEUD,CELGAU
   integer     ,                 intent(in)      :: NMLARG
   Type (ERREUR_T)              , intent(inout)  :: ERREUR

   !.. Variables locales ..
   !-----------------------
   integer        :: NINTER,JG,JD
   Real(DOUBLE)   :: ALG,ALD,SG,SD,PRG,ALARG
   !character(132) :: !arbredappel_old ! arbre d'appel precedent


   !============================= Instructions ===========================

   ! INITIALISATION
   !===============
   Erreur%Numero = 0

   ! DEFINITION DE L'INTERFACE PAR RAPPORT A LA CELLULE
   ! ==================================================
   if( CELGAU == 1 ) then
      NINTER = NOEUD
   else
      NINTER = NOEUD - 1
   endif

   H = YNODE(NOEUD)

   if( H < HEPS ) then
      !      SECTION VIDE
      !      ============
      H = HEPS
      S = CSUR( NOEUD , HEPS , DZ , SGEO , NMLARG , ERREUR )

      if( Erreur%Numero /= 0 ) then
         !arbredappel_old    = trim(!Erreur%arbredappel)
         !Erreur%arbredappel = trim(!Erreur%arbredappel)//'=>CALETA'
         return
      endif

      Q = QNODE(NOEUD)
      U = Q / S

      !
      ! CALCUL DE LA CELERITE ET DE LA SECTION MOUILLEE AUX BORNES
      !
      JG  = JGNODE(NOEUD)
      JD  = JDNODE(NOEUD)
      ALG = ALGEO(NOEUD,JG)
      ALD = ALGEO(NOEUD,JD)
      SG  = SGEO(NOEUD,JG)
      SD  = SGEO(NOEUD,JD)

      ! INTERPOLATION DE LA LARGEUR
      ALARG = ( ALD * ( S - SG ) + ALG * ( SD - S ) ) / ( SD - SG )

      if( ALARG <= EPS10 ) then
         !arbredappel_old    = trim(!Erreur%arbredappel)
         !Erreur%arbredappel = trim(!Erreur%arbredappel)//'=>CALETA'
         return
      endif

      !
      ! CALCUL DE LA CELERITE
      ! ---------------------
      C  = dsqrt( GPES * S / ALARG )
      FR = dabs( U ) / C
      Z  = H + COTR(NOEUD)
      call DICHODM( JG , JD , S , NINTER , SGEOD , JDNODE(NINTER) , NMLARG , ERREUR )

      if( Erreur%Numero /= 0 ) then
         !arbredappel_old    = trim(!Erreur%arbredappel)
         !Erreur%arbredappel = trim(!Erreur%arbredappel)//'=>CALETA'
         return
      endif

      !
      ! SECTION MOUILLEES ET  PRESSION AUX BORNES
      !
      PRG = PRGEOD(NINTER,JG)
      SG  = SGEOD(NINTER,JG)
      SD  = SGEOD(NINTER,JD)

      !
      ! INTERPOLATION DU TERME DE PRESSION
      ! ----------------------------------
      PR = PRG + 0.5_DOUBLE * GPES * DZD(NINTER) * ( ( S * S - SG * SG ) / ( SD - SG ) )
      BETA = 1._DOUBLE
      if( CELGAU == 1 ) then
         INDIC = INDIC + 2
      else
         INDIC = INDIC + 1
      endif
   else
      !      SECTION MOUILLEE
      !      ================
      S    = SNODE(NOEUD)
      Q    = QNODE(NOEUD)
      U    = UNODE(NOEUD)
      BETA = BETAN(NOEUD)
      C    = CNODE(NOEUD)
      FR   = FROUD(NOEUD)
      Z    = ZNODE(NOEUD)

      !
      !   RECHERCHE DES BORNES
      !
      call DICHODM( JG , JD , SNODE(NOEUD) , NINTER , SGEOD , JDNODE(NINTER) , NMLARG , ERREUR )

      !
      ! SECTION MOUILLEE ET PRESSION AUX BORNES
      !
      !   PRG = PRGEOD(NINTER,JG)
      !   SG  = SGEOD(NINTER,JG)
      !   SD  = SGEOD(NINTER,JD)

      !
      ! INTERPOLATION DU TERME DE PRESSION
      ! ----------------------------------
      PR = PRGEOD(NINTER,JG) + 0.5_DOUBLE * GPES * DZD(NINTER) * &
           ( ( SNODE(NOEUD)**2 - SGEOD(NINTER,JG)**2 ) / ( SGEOD(NINTER,JD) - SGEOD(NINTER,JG) ) )

   endif

   !------------------
   ! Fin du traitement
   !------------------

   !  !Erreur%arbredappel = !arbredappel_old

   return

end subroutine CALETA
