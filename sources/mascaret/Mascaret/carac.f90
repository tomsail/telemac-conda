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

subroutine CARAC( &
               FREM , &
              INDIC , &
             IPOINT , &
              UNODE , &
              SNODE , &
              QNODE , &
                  X , &
                 DT , &
              NSECG , &
              NSECD , &
               COTR , &
               FROT , &
               SGEO , &
              AIGEO , &
              DYGEO , &
             NBSECT , &
             NMLARG , &
             ERREUR )

!***********************************************************************
! PROGICIEL : MASCARET        N. GOUTAL
!
! VERSION : V8P4R0              EDF-CEREMA
!***********************************************************************
!     FONCTION : REMONTEE FAIBLE DES INVARIANTS DE RIEMANN POUR LES
!                     CONDITIONS AUX LIMITES
!
!-----------------------------------------------------------------------
!                             ARGUMENTS
! .___________.____.____.______________________________________________.
! !    NOM    !TYPE!MODE!                   ROLE                       !
! !___________!____!____!______________________________________________!
! !  FREM     !  R ! R  ! VALEUR REMONTEE PAR LA CARACTERISTIQUE       !
! !  INDIC    !  I ! D  !                                              !
! !  IPOINT   !  I ! D  ! POINT A PARTIR DUQUEL ON TRACE LA            !
! !           !    !    ! CARACTERISTIQUE                              !
! !  UNODE    ! TR ! D  ! VITESSES                                     !
! !  SNODE    ! TR ! D  ! SECTION MOUILLEE                             !
! !  QNODE    ! TR ! D  ! DEBIT                                        !
! !  X        ! TR ! D  ! TABLEAU DES COORDONNEES                      !
! !  DT       !  R ! D  ! PAS DE TEMPS                                 !
! !  NSECG    !  I ! D  ! INDICE DE LA SECTION GAUCHE DU DOMAINE ETUDIE!
! !  NSECD    !  I ! D  ! INDICE DE LA SECTION DROITE DU DOMAINE ETUDIE!
! !  COTR     ! TR ! D  ! PRAD TABLEAU DE DIMENSION NBSECT-1           !
! !           !    !    ! PRAD(I) PENTE DU RADIER ENTRE I ET I+1       !
! !  FROT     ! TR ! D  ! FROTTEMENT                                   !
! !  SGEO     ! TR !    !                                              !
! !  AIGEO    ! TR ! D  ! AIGEO  PLANIMETREE SUR LE MAILLAGE           !
! !  DYGEO    ! TR ! D  ! DYGEO  PLANIMETREE SUR LE MAILLAGE           !
! !  NBSECT   !  I ! D  ! Nombre de sections                           !
! !  NMLARG   !    ! D  !                                              !
! !___________!____!____!______________________________________________!
!
!     TYPE : I (ENTIER), R (REEL), A (ALPHANUMERIQUE), T (TABLEAU)
!            L (LOGIQUE)   .. ET TYPES COMPOSES (EX : TR TABLEAU REEL)
!     MODE : D (DONNEE NON MODIFIEE), R (RESULTAT), M (DONNEE MODIFIEE)
!            A (AUXILIAIRE MODIFIE)
!-----------------------------------------------------------------------
!     SOUS PROGRAMME APPELE :  BISSND
!
!***********************************************************************
! SGEO, AIGEO  et DYGEO font partie d'une structure de donnees

   !============================= Declarations ===========================

   !.. Modules importes ..
   !----------------------
   use M_PRECISION
   use M_PARAMETRE_C ! GPES, EPS2, EPS5, EPS10
   use M_ERREUR_T    ! Message d'erreur
   use M_AINTDC_I    ! Interface de la fonction    AINTDC
   use M_BISSND_I    ! Interface du sous-programme BISSND
   use M_DYDXSC_I    ! Interface de la fonction    DYDXSC

   !.. Declarations explicites ..
   !-----------------------------
   implicit none

   !.. Arguments ..
   !---------------
   real(DOUBLE),                   intent(  out) :: FREM
   integer     ,                   intent(in)    :: INDIC
   integer     ,                   intent(in)    :: IPOINT
   real(DOUBLE), dimension(:)    , intent(in)    :: UNODE,SNODE,QNODE
   real(DOUBLE), dimension(:)    , intent(in)    :: X
   real(DOUBLE),                   intent(in)    :: DT
   integer     ,                   intent(in)    :: NSECG,NSECD
   real(DOUBLE), dimension(:)    , intent(in)    :: COTR
   real(DOUBLE), dimension(:)    , intent(in)    :: FROT
   ! 1ere dimension IM
   real(DOUBLE), dimension(:,:)  , intent(in)    :: SGEO,AIGEO,DYGEO
   integer     ,                   intent(in)    :: NBSECT
   integer     ,                   intent(in)    :: NMLARG
   type (ERREUR_T)               , intent(inout) :: ERREUR

   !.. Variables locales ..
   !-----------------------
   real(DOUBLE), dimension(2*NBSECT) :: DDT,XPIED,HPIED,FNP1,FN,VIT,SLOPE,A,B,C
   real(DOUBLE)                      :: CG,CD,VITMAX,PFOND,SOURL,SOURC
   real(DOUBLE)                      :: DX,DTR,DUDX,PRODU
   real(DOUBLE)                      :: DXREF,PROREF,DREF,EPSX,EPSU,EPST
   real(DOUBLE)                      :: UCOUR,DXPIED,XPD
   real(DOUBLE)                      :: XPIEDG,XPIEDD,HDD,HDG,HGD,HGG,FG,FD,PG,PD
   real(DOUBLE)                      :: FROG,FROD
   real(DOUBLE)                      :: QNG,QND
   real(DOUBLE)                      :: PENTE,PASX,ETA1,ETA2,ETA3,Z,ZP,ZF
   integer, dimension(2*NBSECT)      :: KPIED
   integer        :: TCAR
   integer        :: IMP1,NOEUD,I,KEP,MK,MJ,IEPS,K1,KFIN
   integer        :: NG,ND,NEQ
   !character(132) :: !arbredappel_old ! arbre d'appel precedent

   !============================= Instructions ===========================

   ! INITIALISATION
   !===============
   Erreur%Numero = 0
   !arbredappel_old    = trim(!Erreur%arbredappel)
   !Erreur%arbredappel = trim(!Erreur%arbredappel)//'=>CARAC'

   ! INITIALISATIONS
   IMP1 = NSECD + 1

   !     CALCUL DES VALEURS CONVECTEES ET DES VITESSES DE CONVECTION
   do NOEUD = NSECG , NSECD
      FN(NOEUD) = QNODE(NOEUD)
     VIT(NOEUD) = UNODE(NOEUD-1)
   end do

   !     CONDITIONS AUX LIMITES (NE SERVENT PAS SI TOUT SE PASSE BIEN)
   CG     = FN(NSECG)
   CD     = FN(NSECD)
   VITMAX = 0._DOUBLE

   do NOEUD = NSECG , NSECD
     VITMAX = dmax1( dabs( VIT(NOEUD) ) , VITMAX )
   end do

   !     TERMES SOURCES AUTRES QUE LE FROTTEMENT
   TCAR = 1

   do NOEUD = NSECG , NSECD

      if( NOEUD == NSECG ) then
         PFOND = ( COTR(NSECG+1) - COTR(NSECG) ) / ( X(NSECG+1) - X(NSECG) )
      else if( NOEUD == NSECD ) then
         PFOND = ( COTR(NSECD) - COTR(NSECD-1) ) / ( X(NSECD) - X(NSECD-1) )
      else
         PFOND = ( COTR(NOEUD+1) - COTR(NOEUD-1) ) / ( X(NOEUD+1) - X(NOEUD-1) )
      endif

      SOURL = ( 1._DOUBLE / GPES ) * VIT(NOEUD) * (-TCAR) &
             * AINTDC( NOEUD , SNODE(NOEUD) , SGEO , AIGEO , NMLARG , ERREUR )
      if( Erreur%Numero /= 0 ) then
         return
      endif

      SOURC = ( -1._DOUBLE ) * DYDXSC( NOEUD , SNODE(NOEUD) , SGEO , DYGEO , NMLARG , ERREUR )
      if( Erreur%Numero /= 0 ) then
         return
      endif

      if( INDIC == 1 ) then
         SLOPE(NOEUD) = SOURL + SOURC + PFOND
      else
         SLOPE(NOEUD) = 0._DOUBLE
      endif

   end do

   !     1- DEFINITION DES GRANDEURS DE REFERENCE DU MAILLAGE
   !     ---------------------------------------------------------
   !          **************************************
   !          *  DXREF      * LONGEUR DE REFERENCE *
   !          *             *                      *
   !          *  EPS2       * EPSILON DE PRECISION *
   !          *             *                      *
   !          *  EPSX       * PRECISION SUR X      *
   !          *             *                      *
   !          *  EPSU       * PRECISION SUR U      *
   !          **************************************
   DXREF = ( X(NBSECT) - X(1) ) / NBSECT
   EPSX  = EPS2 * DXREF
   EPSU  = EPS2 * VITMAX
   EPST  = EPS2 * DT

   !     CAS D'UN PAS DE TEMPS NUL
   !     *************************
   if( DT < EPS5 ) then
      if( IPOINT == 1 ) then
         FREM  = QNODE(NSECG)
      else
         FREM  = QNODE(NSECD)
      endif
      return
   endif

   !     I.1) CARACTERISTIQUES CALCULEES ANALYTIQUEMENT DX/DT=AX+B
   !          ****************************************************
   !     1- INITIALISATION ,POUR TOUTE MAILLE  ,DU TEMPS NECESSAIRE
   !              A  SA  TRAVERSEE : DDT
   !     ---------------------------------------------------------
   !     GRANDEUR REFERENCE DU PRODUIT DES VITESSES *
   PROREF = EPSU**2
   !     GRANDEUR REFERENCE DERIVEE DE VITESSE EN X *
   DREF = EPSU / DXREF

   do I = NSECG , NSECD - 1

      DX    =   X(I+1) - X(I)
      DUDX  = ( VIT(I+1) - VIT(I) ) / DX
      PRODU =   VIT(I) * VIT(I+1)
      ! TESTS SUR LE SIGNE DU PRODUIT A PROREF PRES
      if( PRODU > PROREF ) then
         ! PRODUIT DES VITESSES STICTEMENT POSITIF
         ! SUR NULLITE DE LA DERIVEE A DREF PRES
         if( dabs(DUDX) > DREF ) then
            ! VITESSE VARIABLE SUR LA MAILLE
            if( VIT(I) < 0._DOUBLE ) KEP = -1
            if( VIT(I) > 0._DOUBLE ) KEP = +1
            DDT(I) = -KEP * dlog( -DUDX * DX / VIT(I+1) + 1._DOUBLE ) / DUDX
            if( dabs(DDT(I)) > EPST ) cycle
         end if
         ! VITESSE  CONSTANTE SUR LA MAILLE
         DDT(I) = dabs( DX / VIT(I) )
      else
         ! PRODUIT DES VITESSES NEGATIF OU NUL
         ! *   LE  TEMPS  POUR  TRAVERSEER LA  MAILLE EST INFINI
         ! &   PRIS ICI EGAL A   2*DT   *
         DDT(I) = 2._DOUBLE * DT
      end if
   end do

   !     2- CALCUL DES PIEDS DES CARACTERISTIQUES
   !     ----------------------------------------
   do 2000 I=NSECG,NSECD
      DTR = DT
      !           /  TESTS SUR LE SIGNE DE LA VITESSE AU POINT I /
      if( VIT(I) < -EPSU )                     goto 2100
      if( VIT(I) >= -EPSU.and.VIT(I) <= EPSU ) goto 2200
      if( VIT(I) >  EPSU )                     goto 2300
      !           2.1 -  INITIALISATION DE :
      !                         MK  ,INDICATEUR DE MAILLE
      !                         K1  ,INCREMENT FNP1 EXPLORATION
      !                         IEPS, TEL QUE MK+IEPS SOIT LE POINT COURANT
      !                -  VITESSE NEGATIVE -
      2100 MK   =  I
      IEPS      =  0
      K1        =  1
      goto 2500
      !                -  VITESSE NULLE    -
      2200 KPIED(I) = I
      XPIED(I)      = 0._DOUBLE
      !      <<<  LE CALCUL EST DEJA FINI ,KPIED & XPIED CONNUS >>>
      goto 2000
      !                -  VITESSE POSITIVE -
      2300 MK   =  I-1
      IEPS      =  1
      K1        = -1
      goto 2500
      !           2.2-   DESCENTE DE LA CARACTERISTIQUE
      !           //   PASSAGE A LA MAILLE SUIVANTE  //
      2400 MK = MK + K1
      !                    //  TEST SUR LA TRAVERSEE DES LIMITES DU DOMAINE//
      !                 SI ON A TRAVERSE , ALORS ON VA ECRIRE KPIED & XPIED
      2500 if( MK == NSECG - 1 .or. MK == NSECD ) goto 3100
      !                    SINON LA CARACTERISTIQUE EST INTERIEURE AU DOMAINE
      !                    LE PIED SE TROUVE T IL DANS CETTE MAILLE ?
      !                    SI OUI ,ALORS ON VA ECRIRE KPIED & XPIED
      if( DTR <= DDT(MK) ) goto 2600
      !                    SINON , ON PASSE A LA MAILLE SUIVANTE
      DTR = DTR - DDT(MK)
      goto 2400
      !            2.3-   ECRITURE DE KPIED & XPIED
      !                  2.3-1   PIED INTERIEUR AU DOMAINE
      2600 KPIED(I) = MK
      !                  *  UCOUR : VITESSE AU POINT  COURANT  *
      UCOUR = VIT(MK+IEPS)
      DX    = X(MK+1)- X(MK)
      DUDX  = ( VIT(MK+1) - VIT(MK) ) / DX
      !                         // TEST SUR NULLITE VIT COURANT //
      !                        ... SI LA VITESSE EST NON NULLE,ALORS ON SAUTE
      if( dabs(UCOUR) > EPSU ) goto 2700
      !                                .. SINON ...
      !                    // VITESSE NULLE //
      KPIED(I) = MK + IEPS
      XPIED(I) = 0._DOUBLE
      goto 2000
      !                   // VITESSE NON NULLE //
      !                         // TEST SUR NULLITE DERIVEE DE VIT EN X//
      !                        ... SI LA DERIVEE  EST NULLE , ALORS ON SAUTE
      2700 if( dabs(DUDX) < DREF )  goto 2800
      !                             .. SINON ...
      !                         // DERIVEE EST NON NULLE //
      DXPIED = UCOUR * ( dexp( -DUDX * DTR ) - 1._DOUBLE ) / DUDX
      goto 2900
      !                         // DERIVEE EST  NULLE //
      2800 DXPIED=-UCOUR*DTR
      !                            DXPIED=UCOUR*DTR
      !                         //  TEST DU SIGNE DE LA VITESSE  COURANTE //
      !                         ... SI LA VITESSE COURANTE EST NEGATIVE
      2900 if( UCOUR > EPSU ) goto 3000
      !                                 ALORS ...
      XPIED(I) = DXPIED
      !                             XPIED(I)=ABS(DXPIED)
      goto 2000
      !                         ... SINON LA VITESSE COURANTE EST POSITIVE ET
      3000 XPIED(I) = DX + DXPIED
      !                             XPIED(I)=DX-ABS(DXPIED)
      goto 2000
      !                  2.3-2   PIED EXTERIEUR AU DOMAINE
      3100 KPIED(I) = MK
      !                         SORTIE DANS L' ELEMENT GAUCHE OU DROITE ?
      if( MK /= NSECG-1 )  goto 3200
      !                         // SORTIE DANS L' ELEMENT GAUCHE //
      XPIED(I) = VIT(NSECG) * ( DT - DTR )
      goto 2000
      !                         // SORTIE DANS L' ELEMENT DROITE //
      3200 XPIED(I) = -VIT(NSECD) * DTR
   2000 continue
   !       CALCUL DES VARIABLES AU PIED DE LA CARACTERISTIQUE ISSUE DU
   !       POINT NSECG OU DU POINT NSECD
   if( IPOINT == NSECG ) then
      NG  = KPIED(NSECG)
      ND  = KPIED(NSECG) + 1
      XPD = XPIED(NSECG)
   else
      NG  = KPIED(NSECD)
      ND  = KPIED(NSECD) + 1
      XPD = XPIED(NSECD)
   endif
   if( NG == NSECG - 1 ) then
      !          SORTIE A GAUCHE
      FREM = QNODE(NSECG)
      return
   endif
   if( NG == NSECD ) then
      !          SORTIE A DROITE
      FREM = QNODE(NSECD)
      return
   endif
   DX    = X(ND) - X(NG)
   QNG   = QNODE(NG)
   QND   = QNODE(ND)
   FREM  = ( QNG * ( DX - XPD ) + QND * XPD ) / DX
   !     /ICI TOUS LES PIEDS DE  CARACTERISTIQUE SONT DETERMINES  /
   !    1.    CALCUL DES BASES GAUCHES & DES HAUTEURS DES FONCTIONS
   !                PSI    TRANSPORTEES
   !          -----------------------------------------------------
   !   <<< LES  HAUTEURS SONT STOCKEES DANS HPIED & LES PENTES DANS DDT>>
   !       1.1  BASE GAUCHE
   DDT(1) = 0._DOUBLE
   do I = NSECG + 1 , NSECD

      if( KPIED(I-1) == NSECG - 1 ) then
         XPIEDG = X(NSECG) - VIT(NSECG) * DT
      else
         XPIEDG = X(int( KPIED(I-1) ))
      endif

      if( KPIED(I) == NSECG - 1 ) then
         XPIEDD = X(NSECG) - VIT(NSECG) * DT
      else
         XPIEDD = X(int( KPIED(I) ))
      endif

      DDT(I) = XPIEDD - XPIEDG + XPIED(I) - XPIED(I-1)
      if( DDT(I) < EPSX ) DDT(I) = EPSX

   end do

   !       1.2  HAUTEUR DE LA FONCTION TRANSPORTEE
   HPIED(NSECG) = ( X(NSECG+1) - X(NSECG) ) / DDT(NSECG+1)
   do I= NSECG + 1 , NSECD - 1
      HPIED(I) = ( X(I+1) - X(I-1) ) / ( DDT(I) + DDT(I+1) )
   end do
   HPIED(NSECD) = ( X(NSECD) - X(NSECD-1) ) / DDT(NSECD)
   !    2.  CALCUL DE LA MATRICE <PHI(I),PHI(J)>
   !        ------------------------------------
   if( VIT(NSECG) >= 0._DOUBLE ) then
      A(NSECG) = 0._DOUBLE
      B(NSECG) = 1._DOUBLE
      C(NSECG) = 0._DOUBLE
   else
      A(NSECG) = 0._DOUBLE
      B(NSECG) = 2._DOUBLE * ( X(NSECG+1) - X(NSECG) )
      C(NSECG) = X(NSECG+1) - X(NSECG)
   endif
   do I = NSECG + 1 , NSECD - 1
      A(I) = X(I) - X(I-1)
      B(I) = 2._DOUBLE * ( X(I+1) - X(I-1) )
      C(I) = X(I+1) - X(I)
   end do

   if( VIT(NSECD) <= 0._DOUBLE) then
      A(NSECD) = 0._DOUBLE
      B(NSECD) = 1._DOUBLE
      C(NSECD) = 0._DOUBLE
   else
      A(NSECD) = X(NSECD) - X(NSECD-1)
      B(NSECD) = 2._DOUBLE * ( X(NSECD) - X(NSECD-1) )
      C(NSECD) = 0._DOUBLE
   endif
   !    3.  CALCUL DU SECOND MEMBRE
   !        -----------------------
   !       3.1  MISE A ZERO DU SECOND MEMBRE
   do I = NSECG , NSECD
      FNP1(I) = 0._DOUBLE
   end do
   !       3.2  CALCUL DU TERME FNP1(I)
   !          // POUR TOUTE FONCTION PSI TRANSPORTEE //
   !              INITIALISATION (EVITE DES PBS DUS A VARIABLE NON DEFINIE)
   HDD = 0._DOUBLE
   do 60 I = NSECG , NSECD
      !               3.2-1  ...  PREMIERE INITIALISATION DE :
      !                                MJ , INDICATEUR DE MAILLE
      !                                HGG , HAUTEUR DE PSI A GAUCHE
      !                                PENTE, PENTE DE PSI
      !                               ... SI I EGAL 1
      if( I /= NSECG ) goto 61
      !                                     ALORS...
      MJ    = KPIED(I)
      HGG   = HPIED(I)
      PENTE = -HPIED(I) / DDT(I+1)
      goto 62
      !                                     SINON ...
      61 MJ = KPIED(I-1)
      HGG   = 0._DOUBLE
      PENTE = HPIED(I) / DDT(I)
      !              3.2-2  ...   INITIALISATION DES VALEURS LOCALES:
      !                           PASX    , PAS D' ESPACE LOCAL
      !                              ... SI LA MAILLE EST INTERIEURE
      62 if( MJ == NSECG-1 .or. MJ == NSECD ) goto 63
      !                                       ALORS ...
      PASX = X(MJ+1) - X(MJ)
      goto 65
      !                                   ... SINON... EST-CE A GAUCHE ?
      63 if( MJ == NSECD ) goto 64
      !                                        OUI , ALORS ..
      PASX = VIT(NSECG) * DT
      !     LE GOTO 70 EST COMMENTARISE CAR L'INTEGRATION SUR PASX N'EST PAS
      !     TOUJOURS NEGLIGEABLE MEME SI PASX EST PLUS PETIT QUE EPSX
      IF( DABS(PASX) <= EPS10 ) GOTO 70
      goto 65
      !                                        NON , ALORS ..
      64 PASX = -VIT(NSECD) * DT
      IF( DABS(PASX) <= EPS10 ) GOTO 70
      !              3.2-3  ...   INITIALISATION DES VALEURS LOCALES:
      !                           ETA1 ETA 2 ETA 3, PARAM DESCRIPTIFS DE PSI
      !                        // TESTONS LES POSITIONS DES TROIS (OU MOINS)
      !                              PIEDS DE PSI PAR RAPPORT A LA MAILLE //
      !                             ... ETA2  PAR TEST SUR PIED DE I ...
      65 if( MJ < KPIED(I) ) goto 651
      if( MJ == KPIED(I) ) goto 652
      !                                     PIED A GAUCHE DE LA MAILLE
      ETA2 = 0._DOUBLE
      goto 66
      !                                     PIED DANS LA MAILLE
      652 ETA2 = XPIED(I) / PASX
      goto 66
      !                                     PIED A DROITE DE LA MAILLE
      651 ETA2 = 1._DOUBLE
      !                             ... ETA1  PAR TEST SUR PIED DE I-1 ...
      !                                   ..SI I EGAL NSECG
      66 if( I /= NSECG ) goto 661
      !                                         ALORS ..
      ETA1 = ETA2
      goto 67
      !                                         SINON ..
      !                                         ...  SI ON EST DS KPIED(I-1)
      661 ETA1 = XPIED(I-1) / PASX
      !                                         ...  SINON ...
      if( MJ /= KPIED(I-1) ) ETA1 = 0._DOUBLE
      !                             ... ETA3  PAR TEST SUR PIED DE I+1 ...
      !                                   ..SI I EGAL NSECD
      67 if( I /= NSECD ) goto 671
      !                                         ALORS ..
      ETA3 = ETA2
      goto 68
      !                                         SINON ..
      671 if( MJ < KPIED(I+1) ) goto 672
      if( MJ == KPIED(I+1) ) goto 673
      !                                     PIED A GAUCHE DE LA MAILLE
      ETA3 = 0._DOUBLE
      goto 68
      !                                     PIED DANS LA MAILLE
      673 ETA3 = XPIED(I+1) / PASX
      goto 68
      !                                     PIED A DROITE DE LA MAILLE
      672 ETA3 = 1._DOUBLE
      !              3.2-4  ...   INITIALISATION DES VALEURS LOCALES:
      !                           HGD,HDG,HDD AUXILIAIRES DE HAUTEUR DE PSI
      68 HDG = HGG + ( ETA2 - ETA1 ) * PASX * PENTE
      HGD = HDG
      !                     // Y A-T-IL CHANGEMENT DE PENTE ? //
      !                           ... SI MJ EGAL KPIED(I) (I.E. ETA3 =/= ETA2)
      if( ABS(ETA3-ETA2).LT.EPS15 ) goto 681
      !                               ALORS ...
      PENTE = -HPIED(I) / DDT(I+1)
      681 HDD = HGD + ( ETA3 - ETA2 ) * PASX * PENTE
      !        3.3   INTEGRATION PAS A PAS . FORMULE DE SIMPSON
      !              CALCUL DE L'INTEGRALE PSIN*FN (CLASSIQUE)
      !                     DE L'INTEGRALE PSIN*SLOPE*GPES*DT/2.
      !                  ET DE L'INTEGRALE PSIN*FROT*GPES*DT/2. (FROTTEMENT)
      !           POUR TOUTE FONCTION PSI CONVECTEE
      !              3.3-1  ...   INITIALISATION DES VALEURS LOCALES:
      !                           FG & FD , VALEURS DE PSI A GAUCHE & DROITE
      !                              ... SI LA MAILLE EST INTERIEURE
      if( MJ == NSECG - 1 .or. MJ == NSECD ) goto 83
      !                                       ALORS ...
      FG   = FN(MJ)
      FD   = FN(MJ+1)
      PG   = SLOPE(MJ)
      PD   = SLOPE(MJ+1)
      FROG = -FROT(MJ)
      FROD = -FROT(MJ+1)
      goto 85
      !                                   ... SINON... EST-CE A GAUCHE ?
      83 if( MJ == NSECD ) goto 84
      !                                        OUI , ALORS ..
      FG   = CG
      FD   = FN(NSECG)
      PG   = 0._DOUBLE
      PD   = 0._DOUBLE
      FROG = 0._DOUBLE
      FROD = 0._DOUBLE
      goto 85
      !                                        NON , ALORS ..
      84 FG = FN(NSECD)
      FD = CD
      PG = 0._DOUBLE
      PD = 0._DOUBLE
      FROG = 0._DOUBLE
      FROD = 0._DOUBLE
      !              3.3-2  INTEGRATION A GAUCHE
      !              CALCUL DE L'INTEGRALE PSIN*FN
      85 Z = FG * ( ( 1._DOUBLE - ETA1 ) * HGG + ( 2._DOUBLE - ETA1 - ETA2 ) &
            * ( HGG + HDG ) + ( 1._DOUBLE - ETA2 ) * HDG ) &
            + FD * ( ETA1 * HGG + ( ETA1 + ETA2 ) * ( HGG + HDG ) + ETA2 * HDG )
      Z = Z * ( ETA2 - ETA1 ) * PASX
      !                     DE L'INTEGRALE PSIN*SLOPE*GPES*DT/2.
      ZP = PG * ( ( 1._DOUBLE - ETA1 ) * HGG + ( 2._DOUBLE - ETA1 - ETA2 ) &
          * ( HGG + HDG ) + ( 1._DOUBLE - ETA2 ) * HDG ) &
          + PD * ( ETA1 * HGG + ( ETA1 + ETA2 ) * ( HGG + HDG ) + ETA2 * HDG )
      ZP = ZP * ( ETA2 - ETA1 ) * PASX * GPES *DT / 2._DOUBLE
      !                  ET DE L'INTEGRALE PSIN*FROT*GPES*DT/2.
      ZF = FROG * ( ( 1._DOUBLE - ETA1 ) * HGG + ( 2._DOUBLE - ETA1 - ETA2 ) &
          * ( HGG + HDG ) + ( 1._DOUBLE - ETA2 ) * HDG ) &
          +  FROD * ( ETA1 * HGG + ( ETA1 + ETA2 ) * ( HGG + HDG ) + ETA2 * HDG )
      ZF = ZF * ( ETA2 - ETA1 ) * PASX * GPES * DT / 2._DOUBLE
      !                 // SOMMATION DANS FNP1(I) DU TERME DE GAUCHE //
      FNP1(I) = FNP1(I) + Z + ZP + ZF
      !              3.3-2  INTEGRATION A DROITE
      !              CALCUL DE L'INTEGRALE PSIN*FN
      Z = FG * ( ( 1._DOUBLE - ETA2 ) * HGD + ( 2._DOUBLE - ETA2 - ETA3 ) &
          * ( HGD + HDD ) + ( 1._DOUBLE - ETA3 ) * HDD ) &
          + FD * ( ETA2 * HGD + ( ETA2 + ETA3 ) * ( HGD + HDD ) + ETA3 * HDD )
      Z = Z * ( ETA3 - ETA2 ) * PASX
      !                     DE L'INTEGRALE PSIN*SLOPE*GPES*DT/2.
      ZP = PG * ( ( 1._DOUBLE - ETA2 ) * HGD + ( 2._DOUBLE - ETA2 - ETA3 ) &
          * ( HGD + HDD ) + ( 1._DOUBLE - ETA3 ) * HDD ) &
          + PD * ( ETA2 * HGD + ( ETA2 + ETA3 ) * ( HGD + HDD ) + ETA3 * HDD )
      ZP = ZP * ( ETA3 - ETA2 ) * PASX * GPES * DT / 2._DOUBLE
      !                  ET DE L'INTEGRALE PSIN*FROT*GPES*DT/2.
      ZF = FROG * ( ( 1._DOUBLE - ETA2 ) * HGD + ( 2._DOUBLE - ETA2 - ETA3 ) &
          * ( HGD + HDD ) + ( 1._DOUBLE - ETA3 ) * HDD ) &
          + FROD * ( ETA2 * HGD + ( ETA2 + ETA3 ) * ( HGD + HDD ) + ETA3 * HDD )
      ZF = ZF * ( ETA3 - ETA2 ) * PASX * GPES * DT / 2._DOUBLE
      !                 // SOMMATION DANS FNP1(I) DU TERME DE DROITE //
      FNP1(I) = FNP1(I) + Z + ZP + ZF
      !       3.3-3   PASSAGE A LA MAILLE SUIVANTE
      !                                  OU ARRET INTEGRATION SUR PSI
      !                 // ON PASSE A LA MAILLE SUIVANTE //
      70 MJ = MJ + 1
      HGG   = HDD
      !                 // L' INTEGRATION SUR PSI EST ELLE FINIE ? //
      !                    ... SI LA MAILLE A DROITE DE LA FIN DE PSI ATTEINTE
      !                              ALORS , ON PASSE A LA FONCTION SUIVANTE
      if( I == NSECD ) KFIN = KPIED(I) + 1
      if( I /= NSECD ) KFIN = KPIED(I+1) + 1
      if( MJ == KFIN .or. MJ == IMP1 ) goto 60
      !                           ... SINON  , ON CONTINUE L' INTEGRATION
      goto 62

   60 continue
   !      3.4 CALCUL DE L'INTEGRALE DE PSINP1*SLOPE*GPES*DT/2.
   !              ET DE L'INTEGRALE DE PSINP1*FROT*GPES*DT/2.
   FNP1(NSECG) = FNP1(NSECG) &
                + ( X(NSECG+1) - X(NSECG) ) &
                * ( 2._DOUBLE * SLOPE(NSECG) + SLOPE(NSECG+1) ) * GPES * DT / 2._DOUBLE &
                - ( X(NSECG+1) - X(NSECG) ) &
                * ( 2._DOUBLE * FROT(NSECG) + FROT(NSECG+1) ) * GPES * DT / 2._DOUBLE

   do I = NSECG + 1 , NSECD - 1
      FNP1(I) = FNP1(I) &
               + ( X(I) - X(I-1) ) * ( 2._DOUBLE * SLOPE(I) + SLOPE(I-1) ) * GPES * DT / 2._DOUBLE &
               + ( X(I+1) - X(I) ) * ( 2._DOUBLE * SLOPE(I) + SLOPE(I+1) ) * GPES * DT / 2._DOUBLE &
               - ( X(I) - X(I-1) ) * ( 2._DOUBLE * FROT(I) + FROT(I-1) ) * GPES * DT / 2._DOUBLE &
               - ( X(I+1) - X(I  ) ) * ( 2._DOUBLE * FROT(I) + FROT(I+1) ) * GPES * DT / 2._DOUBLE
   end do

   FNP1(NSECD) = FNP1(NSECD) &
                + ( X(NSECD) - X(NSECD-1) ) &
                * ( 2._DOUBLE * SLOPE(NSECD) + SLOPE(NSECD-1) ) * GPES * DT / 2._DOUBLE &
                - ( X(NSECD) - X(NSECD-1) ) &
                * ( 2._DOUBLE * FROT(NSECD) + FROT(NSECD-1) ) * GPES * DT / 2._DOUBLE
   !       3.5  CALCUL DES POINTS DE DIRICHLET S'IL Y'A LIEU
   !            POINT DIRICHLET A L'AMONT
   if( dabs( C(NSECG) ) <= EPS10 ) FNP1(NSECG) = CG
   !            POINT DIRICHLET A L'AVAL
   if( dabs( A(NSECD) ) <= EPS10 ) FNP1(NSECD) = CD
   !       3.6  RESOLUTION
   NEQ = NSECD - NSECG + 1
   call BISSND( FNP1(NSECG:) , A(NSECG:) , B(NSECG:) , C(NSECG:) , NEQ , 1 , ERREUR )
   if( Erreur%Numero /= 0 ) then
      return
   endif

   !------------------
   ! Fin du traitement
   !------------------

   !Erreur%arbredappel = !arbredappel_old

   return

end subroutine CARAC
