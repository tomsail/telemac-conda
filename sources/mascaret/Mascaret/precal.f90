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

subroutine PRECAL( &
               SNODE , &
         SMIN , SMAJ , &
       YNODE , FROUD , &
        VOLS , UNODE , &
       CNODE , ZNODE , &
               QNODE , &
     JGNODE , JDNODE , &
         QMIN , QMAJ , &
        BETA , ZINIT , &
               XFRON , &
               IBARP , &
           confluent , &
            W , AIRS , &
       QVRAI , SVRAI , &
       UVRAI , YVRAI , &
               ZVRAI , &
                   X , &
             I1 , I2 , &
     NBBIEF , NBNOEU , &
              NBSECT , &
              NMLARG , &
                  DZ , &
                COTR , &
         sectionPlan , &
         REP , SUITE , &
                SEPS , &
     CALCOS , CALCVA , &
     STOCKAGE , AVAL , &
              Erreur &
                     )

!***********************************************************************
! PROGICIEL : MASCARET        F. MAUREL        N. GOUTAL
!
! VERSION : V8P4R0              EDF-CEREMA
!***********************************************************************
!
! FONCTION : PREPARATION DU CALCUL,
!            INITIALISATION DES DIFFERENTES VARIABLES
!
!-----------------------------------------------------------------------
!                             ARGUMENTS
! .___________.____.____.______________________________________________.
! !    NOM    !TYPE!MODE!                   ROLE                       !
! !___________!____!____!______________________________________________!
! !  SNODE    ! TR !  R ! SURFACE MOUILLEE                             !
! !  SMIN     ! TR !  D ! COEFFFICIENTS DE STRICKLER (AUX NOEUDS)      !
! !  SMAJ     ! TR !  D !                                              !
! !  YNODE    ! TR !  R ! TIRANT D'EAU                                 !
! !  FROUD    ! TR !  R ! NOMBRE DE FROUDE                             !
! !  VOLS     ! TR !    !                                              !
! !  UNODE    ! TR !  R ! VITESSE                                      !
! !  CNODE    ! TR !  R ! CELERITE DES ONDES                           !
! !  ZNODE    ! TR !  D ! COTE DE LA SURFACE LIBRE                     !
! !  QNODE    ! TR !  D ! DEBIT                                        !
! !  QMIN     ! TR !    !                                              !
! !  QMAJ     ! TR !    !                                              !
! !  BETA     ! TR !    !                                              !
! !  ZINIT    ! TR !  R ! COTE DE LA SURFACE LIBRE A L'INSTANT INITIAL !
! !  QMAX     ! TR !  R ! DEBIT MAXIMAL                                !
! !  TQMAX    ! TR !  R ! INSTANT OU LE DEBIT MAXIMAL EST OBSERVE      !
! !  ZMAX     ! TR !  R ! COTE DE LA SURFACE LIBRE MAXIMALE            !
! !  TZMAX    ! TR !  R ! INSTANT OU ZMAX EST OBSERVE                  !
! !  UTZMAX   ! TR !  R ! VITESSE LORSQU'ON ATTEINT LA HAUTEUR MAXIMALE!
! !  NRJMAX   ! TR !  R ! ENERGIE CINETIQUE MAXIMALE                   !
! !  TFRON    ! TR !  R ! TEMPS DE L'ARRIVEE DE L'ONDE                 !
! !  XFRON    ! TR !  R ! POSITION DU FRONT D'ONDE DANS CHAQUE BIEF    !
! !  XBARP    !  R !  D ! POSITION DU BARRAGE PRINCIPAL                !
! !  W        ! TR !  D ! VARIABLE D'ETAT DANS LE CONFLUENT            !
! !  AIRS     ! TR !  D ! SURFACE DES CELLULES 2D                      !
! !XCONF,YCONF! TR !  D ! COORDONEES DES EXTREMITES DU CONFLUENT       !
! !  TETACO   ! TR !  D ! DIRECTION DE CHAQUE BIEF AU CONFLUENT        !
! !  ISEC     ! TI !  D ! NUMERO SECTION CALCUL 1D LIMITE A UN CONF    !
! !  ISECVO   ! TI !  D ! NUMERO DE SA VOISINE                         !
! !  FINBIE   ! TI !  D ! INDICATEUR DE DEBUT ET FIN DE BIEF           !
! !  QVRAI    ! TR !  R ! DEBIT VRAI LORS DE LA VALIDATION             !
! !  SVRAI    ! TR !  R ! SURFACE VRAIE LORS DE LA VALIDATION          !
! !  UVRAI    ! TR !  R ! VITESSE VRAIE LORS DE LA VALIDATION          !
! !  YVRAI    ! TR !  R ! TIRANT D'EAU VRAI LORS DE LA VALIDATION      !
! !  ZVRAI    ! TR !  R ! SURFACE LIBRE VRAIE LORS DE LA VALIDATION    !
! !  X        ! TR !  D ! ABSCISSE DES SECTION DE CALCUL               !
! !  I1,I2    ! TI !  D ! INDICE DES EXTREMITES DE CHAQUE BIEF         !
! !  NBBIEF   !  I !  A ! NOMBRE DE BIEF                               !
! !  NBNOEU   !  I !  A ! NOMBRE DE NOEUD                              !
! !  NBSECT   !  I !    !                                              !
! !  COTR     ! TR !  D ! COTE DU FOND                                 !
! !  SGEO     ! TR !    !                                              !
! !  SGEO1    ! TR !    !                                              !
! !  SSGEO    ! TR !    !                                              !
! !  ALGEO    ! TR !  D ! LARGEUR AU MIROIR PLANIMETREE                !
! !  DEBGE1   ! TR !    !                                              !
! !  DEBGE2   ! TR !    !                                              !
! !  DZ       ! TR !  D ! PAS DE PLANIMETRAGE                          !
! !  TINIT    !  R !  D ! INSTANT INITIAL DU CALCUL                    !
! !  REP      !  L !    !                                              !
! !  SUITE    !  L !    !                                              !
! !  SEPS     !  R !  D ! VALEUR MINIMALE DE LA SECTION MOUILLEE       !
! !  CALCOS   !  L !  A ! CALCUL D'une OS                              !
! !  CALCVA   !  L !  A ! CALCUL D'UNE SOL. ANAL                       !
! !  STOCKAGE !  L !  A !                                              !
! !  AVAL     !  L !  A ! CALCUL AVAL d'UNE OS                         !
! !  NFREPL   !  I !    !                                              !
! !  NMLARG   !  I !  D !                                              !
 ! !___________!____!____!______________________________________________!
!
!                             VARIABLES LOCALES
! .___________.____.____.______________________________________________.
! !  NOEUD    !  I !  A ! NUMERO DU NOEUD COURANT                      !
! !  IBIEF    !  I !  A ! NUMERO DU BIEF COURANT                       !
! !___________!____!____!______________________________________________!
!
!     TYPE : I (ENTIER), R (REEL), A (ALPHANUMERIQUE), T (TABLEAU)
!            L (LOGIQUE)   .. ET TYPES COMPOSES (EX : TR TABLEAU REEL)
!     MODE : D (DONNEE NON MODIFIEE), R (RESULTAT), M (DONNEE MODIFIEE)
!            A (AUXILIAIRE MODIFIE)
!
!*************************************************************************
!  SGEO, SGEO1, SSGEO,ALGEO,DEBGE1,DEBGE2 font partie d'une structure
!             de donnees  STRUCTURE_SECTION

   !============================= Declarations ===========================

   !.. Modules importes ..
   !----------------------
   use M_PRECISION
   use M_MESSAGE_C
   use M_SECTION_PLAN_T
   use M_CONFLUENT_T
   use M_ERREUR_T    ! Definition du type d'erreur
   use M_CELE_I      ! Interface de la fonction    CELE
   use M_CSUR_I      ! Interface de la fonction    CSUR
   use M_INIGEO_I    ! Interface du sous-programme INIGEO
   use M_MINMAJ_I    ! Interface du sous-programme MINMAJ
   use M_DICHO_I
   use M_TRAITER_ERREUR_I ! Interface de traitement des erreurs

   !.. Declarations explicites ..
   !-----------------------------
   implicit none

   !.. Arguments ..
   !---------------
   type (SECTION_PLAN_T)                ,intent(in) :: SECTIONPLAN
   type (confluent_T)     ,dimension(:), intent(in) :: CONFLUENT
   real(DOUBLE), dimension(:)    , intent(  out) :: SNODE
   real(DOUBLE), dimension(:)    , intent(  out) :: SMIN,SMAJ
   real(DOUBLE), dimension(:)    , intent(  out) :: YNODE
   real(DOUBLE), dimension(:)    , intent(  out) :: FROUD,VOLS,UNODE
   real(DOUBLE), dimension(:)    , intent(  out) :: CNODE
   real(DOUBLE), dimension(:)    , intent(in)    :: ZNODE
   real(DOUBLE), dimension(:)    , intent(in)    :: QNODE
   integer     , dimension(:)    , intent(  out) :: JGNODE,JDNODE
   real(DOUBLE), dimension(:)    , intent(  out) :: QMIN,QMAJ,BETA
   real(DOUBLE), dimension(:)    , intent(inout) :: ZINIT
   real(DOUBLE), dimension(:)    , intent(  out) :: XFRON
   integer      ,                   intent(in)    :: IBARP
   ! 1ere dimension 3, 2nde dimension 12
   real(DOUBLE), dimension(:,:,:), intent(  out) :: W
   ! 1ere dimension 12
   real(DOUBLE), dimension(:,:)  , intent(  out) :: AIRS
   real(DOUBLE), dimension(:)    , intent(  out) :: QVRAI,SVRAI,UVRAI,YVRAI,ZVRAI
   real(DOUBLE), dimension(:)    , intent(in)    :: X
   integer     , dimension(:)    , intent(in)    :: I1,I2
   integer     ,                   intent(in)    :: NBBIEF,NBNOEU
   integer     ,                   intent(in)    :: NBSECT
   integer     ,                   intent(in)    :: NMLARG
   real(DOUBLE), dimension(:)    , intent(in)    :: COTR
   real(DOUBLE) ,dimension(:)    , intent(in)    :: DZ
   real(DOUBLE)                  , intent(in)    :: SEPS
   logical     ,                   intent(in)    :: REP,SUITE
   logical     ,                   intent(in)    :: CALCOS,CALCVA,STOCKAGE,AVAL
   type (erreur_T)               , intent(inout) :: Erreur

   !.. Variables locales ..
   !-----------------------
   integer      :: NOEUD,IBIEF,INOEU,IEL,IFONC,IS
   integer      :: NELMIN(12,2),NFRELM(18)
   integer      :: NSEGP,NSEGR
   integer      :: ICONF
   real(DOUBLE) :: BXY(12,2),ZF(12)
   real(DOUBLE) :: VNOIN(3,12),VNOFR(3,18)
   ! 1ere dimension Nbsect
   real(DOUBLE), dimension(Nbsect,Nmlarg)         :: SGEO,SGEO1,SSGEO
   real(DOUBLE), dimension(Nbsect,Nmlarg)         :: ALGEO
   real(DOUBLE), dimension(Nbsect,Nmlarg)         :: DEBGE1,DEBGE2
   ! 1 ere dimension 3 , 2 ieme Size (confluent)
   integer     , dimension(3,size(Confluent))     :: ISEC,ISECVO,FINBIE
   real (DOUBLE)  , dimension(3,size(Confluent))     :: XCONF,YCONF, TETACO
   !character(132) :: !arbredappel_old ! arbre d'appel precedent

   !============================= Instructions ===========================

   ! INITIALISATION
   !===============
   Erreur%Numero = 0
   !arbredappel_old    = trim(!Erreur%arbredappel)
   !Erreur%arbredappel = trim(!Erreur%arbredappel)//'=> ERREUR SUR L INITIALISATION'

   !Affectation des variables
   sgeo(:,:)   = SectionPlan%S(:,:)
   sgeo1(:,:)  = SectionPlan%S1(:,:)
   algeo(:,:)  = SectionPlan%B(:,:)
   debge1(:,:) = SectionPlan%DEB1(:,:)
   debge2(:,:) = SectionPlan%DEB2(:,:)

   label_boucle_confluent : do Iconf = 1 , size(Confluent)
      isec  (:,iconf) = Confluent(iconf)%ISEC(:)
      isecvo(:,iconf) = Confluent(iconf)%ISECVO(:)
      finbie(:,iconf) = Confluent(iconf)%FINBIE(:)
      XCONF (:,iconf) = Confluent(iconf)%AbscisseAfflu(:)
      YCONF(:,iconf)  = Confluent(iconf)%OrdonneeAfflu(:)
      TETACO(:,iconf) = Confluent(iconf)%AngleAfflu(:)
   enddo label_boucle_confluent

   label_boucle_bief : do IBIEF = 1 , NBBIEF

      label_boucle_noeud : do NOEUD = I1(IBIEF) , I2(IBIEF)

         ! 1 ) CALCUL DU TIRANT D'EAU
         ! --------------------------
         YNODE(NOEUD) = ZNODE(NOEUD) - COTR(NOEUD)
         if( YNODE(NOEUD) < 0._DOUBLE )  then
            Erreur%Numero = 108
            Erreur%ft   = err_108
            Erreur%ft_c = err_108c
            call TRAITER_ERREUR( Erreur , NOEUD , X(NOEUD) )
            return
          endif

         ! 2 ) CALCUL DE LA SURFACE MOUILLEE
         ! ---------------------------------

         ! a ) TOTALE
         SNODE(NOEUD) = CSUR( NOEUD , YNODE(NOEUD) , DZ , SGEO , NMLARG , ERREUR )
         if( Erreur%Numero /= 0 ) then
            return
         endif

         call DICHO( JGNODE(Noeud)          , &
                     JDNODE(Noeud)          , &
                     SNODE (Noeud)          , &
                     SectionPlan%S(Noeud,:) , &
                     Erreur                 )

         call MINMAJ( &
                  SMIN , &
                  QMIN , &
                  SMAJ , &
                  QMAJ , &
                  BETA , &
                 SNODE , &
                 QNODE , &
                 NOEUD , &
         JGNODE(NOEUD) , &
         JDNODE(NOEUD) , &
                 SGEO1 , &
                  SGEO , &
                DEBGE1 , &
                DEBGE2 , &
                NMLARG , &
                ERREUR &
                       )

         if( Erreur%Numero /= 0 ) then
            !Print *,'erreur minmaj'
            return
         endif

         ! b ) STOCKAGE
         if( STOCKAGE ) then
            VOLS(NOEUD) = CSUR( NOEUD , YNODE(NOEUD) , DZ , SSGEO , NMLARG , ERREUR )
            if( Erreur%Numero /= 0 ) then
            !print *,'erreur stockage'
               return
            endif
         endif

         ! 3 ) CALCUL DE LA VITESSE
         ! ------------------------
         if( SNODE(NOEUD) < SEPS ) then
            UNODE(NOEUD) = QNODE(NOEUD) / SEPS
         else
            UNODE(NOEUD) = QNODE(NOEUD) / SNODE(NOEUD)
         endif

         ! 4 ) CALCUL DE LA CELERITE DES ONDES
         ! -----------------------------------
         CNODE(NOEUD) = CELE( NOEUD , SNODE(NOEUD) , SGEO , ALGEO , ERREUR )
         if( Erreur%Numero /= 0 ) then
            return
         endif

         ! 5 ) CALCUL DU NOMBRE DE FROUDE
         ! ------------------------------
         FROUD(NOEUD) = dabs( UNODE(NOEUD) ) / CNODE(NOEUD)

      end do label_boucle_noeud

   end do label_boucle_bief


   ! 8 ) INITIALISATION DES PARAMETRES LIES AUX ONDES DE SUBMERSION
   if( CALCOS.AND..not.REP ) then
      do IBIEF = 1 , NBBIEF
         XFRON(IBIEF) = X(IBARP)
      end do
   endif

   ! 11 ) INITIALISATION DES VALEURS UTILISEES POUR LA VALIDATION
   if( CALCVA ) then
      do NOEUD = 1 , NBSECT
         QVRAI(NOEUD) = QNODE(NOEUD)
         SVRAI(NOEUD) = SNODE(NOEUD)
         YVRAI(NOEUD) = YNODE(NOEUD)
         ZVRAI(NOEUD) = ZNODE(NOEUD)
         UVRAI(NOEUD) = UNODE(NOEUD)
      end do
   endif

   ! INITIALISATION DES VARIABLES CONFLUENT
   ! --------------------------------------
   label_boucle_noeud2 : do INOEU = 1 , NBNOEU

      call INIGEO(          &
           AIRS(:,INOEU)  , &
           BXY            , &
           ZF             , &
           NELMIN         , &
           NFRELM         , &
           NSEGP          , &
           NSEGR          , &
           VNOIN          , &
           VNOFR          , &
           XCONF(:,INOEU) , &
           YCONF(:,INOEU) , &
           TETACO(:,INOEU), &
           SNODE          , &
           ZNODE          , &
           COTR           , &
           X              , &
           DZ             , &
           SGEO           , &
           ISEC(:,INOEU)  , &
           ISECVO(:,INOEU), &
           INOEU          , &
           NMLARG         , &
           ERREUR           &
                          )

      if( Erreur%Numero /= 0 ) then
         !print *,'erreur inigeo'
         return
      endif

      do IEL = 1 , 3
         IS = ISEC(IEL,INOEU)
         W(1,IEL,INOEU) = ZNODE(IS) - COTR(IS)
         if( FINBIE(IEL,INOEU) == 0 ) then
            W(2,IEL,INOEU) = QNODE(IS) * VNOFR(1,IEL) / VNOFR(3,IEL)
            W(3,IEL,INOEU) = QNODE(IS) * VNOFR(2,IEL) / VNOFR(3,IEL)
         else
            W(2,IEL,INOEU) = -QNODE(IS) * VNOFR(1,IEL) / VNOFR(3,IEL)
            W(3,IEL,INOEU) = -QNODE(IS) * VNOFR(2,IEL) / VNOFR(3,IEL)
         endif
      end do

      !    A L'INTERIEUR DU CONFLUENT
      do IFONC = 1 , 3
         W(IFONC,7,INOEU ) = ( 7._DOUBLE * W(IFONC,1,INOEU) + &
                               1._DOUBLE * W(IFONC,2,INOEU) + &
                               1._DOUBLE * W(IFONC,3,INOEU) ) / 9._DOUBLE
        W(IFONC,9,INOEU )  = ( 1._DOUBLE * W(IFONC,1,INOEU) + &
                               7._DOUBLE * W(IFONC,2,INOEU) + &
                               1._DOUBLE * W(IFONC,3,INOEU) ) / 9._DOUBLE
        W(IFONC,11,INOEU)  = ( 1._DOUBLE * W(IFONC,1,INOEU) + &
                               1._DOUBLE * W(IFONC,2,INOEU) + &
                               7._DOUBLE * W(IFONC,3,INOEU) ) / 9._DOUBLE
        W(IFONC,8,INOEU )  = ( 4._DOUBLE * W(IFONC,1,INOEU) + &
                               4._DOUBLE * W(IFONC,2,INOEU) + &
                               1._DOUBLE * W(IFONC,3,INOEU) ) / 9._DOUBLE
        W(IFONC,10,INOEU)  = ( 1._DOUBLE * W(IFONC,1,INOEU) + &
                               4._DOUBLE * W(IFONC,2,INOEU) + &
                               4._DOUBLE * W(IFONC,3,INOEU) ) / 9._DOUBLE
        W(IFONC,12,INOEU)  = ( 4._DOUBLE * W(IFONC,1,INOEU) + &
                               1._DOUBLE * W(IFONC,2,INOEU) + &
                               4._DOUBLE * W(IFONC,3,INOEU) ) / 9._DOUBLE
      end do

   end do label_boucle_noeud2

  !------------------
  ! Fin du traitement
  !------------------

  !Erreur%arbredappel = !arbredappel_old

  return

end subroutine PRECAL
