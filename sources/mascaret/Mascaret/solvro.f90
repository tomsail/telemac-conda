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

subroutine SOLVRO(                   &
                             SNODE , &
                             QNODE , &
                             UNODE , &
                             ZNODE , &
                             YNODE , &
                             FROUD , &
                             CNODE , &
                             FLUX  , &
                         DebitFlux , &
  JGNODE , JDNODE , IFIGE , KTEMPS , &
                              BETA , &
                               QIN , &
                                 X , &
                              SGEO , &
                             ALGEO , &
                             SGEOD , &
                            PRGEOD , &
                            DEBGEO , &
                            DEBGED , &
                             S1GEO , &
                              COTR , &
                               DZD , &
                                DZ , &
                                DT , &
                              HEPS , &
                             NSECG , &
                             NSECD , &
                            NBARAD , &
                             NBBAR , &
                       SINGULARITE , &
                            PCSing , &
                             SPREC , &
                             QPREC , &
                            NBSECT , &
                            FRTIMP , &
                       Impli_Trans , &
           PerteElargissementTrans , &
                        Boussinesq , &
                              CQMV , &
                            NMLARG , &
                            ERREUR &
                                   )

!***********************************************************************
! PROGICIEL : MASCARET        N. GOUTAL      J. SAINTE-MARIE
!
! VERSION : V8P4R0              EDF-CEREMA
!***********************************************************************
!   FONCTION : RESOLUTION DES EQUATIONS DE SAINT VENANT
!                      PAR UN SCHEMA DE ROE
!
!-----------------------------------------------------------------------
!                             ARGUMENTS
! .___________.____.____.______________________________________________.
! !    NOM    !TYPE!MODE!                   ROLE                       !
! !___________!____!____!______________________________________________!
! !  SNODE    ! TR !  M ! SURFACE MOUILLEE                             !
! !  QNODE    ! TR !  M ! DEBIT                                        !
! !  UNODE    ! TR !  M ! VITESSE                                      !
! !  ZNODE    ! TR !    !                                              !
! !  YNODE    ! TR !    !                                              !
! !  FROUD    ! TR !    !                                              !
! !  CNODE    ! TR !    !                                              !
! !  BETA     ! TR !    !                                              !
! !  QIN      ! TR !  D ! DEBIT D'APPORT                               !
! !  X        ! TR !  D ! ABSCISSES DES POINTS DU MAILLAGE             !
! !  SGEO     ! TR !  D ! SURFACES PLANIMETREES                        !
! !  ALGEO    ! TR !  D ! LARGEUR PLANIMETREE                          !
! !  SGEOD    ! TR !    !                                              !
! !  PRGEOD   ! TR !  D ! PRESSION PLANIMETREE (MAILLAGE DECALE)       !
! !  DEBGEO   ! TR !    !                                              !
! !  DEBGED   ! TR !    !                                              !
! !  COTR     ! TR !  D ! COTE DU RADIER                               !
! !  DZD      ! TR !    !                                              !
! !  DZ       ! TR !    !                                              !
! !  DT       !  R !  D ! PAS DE TEMPS                                 !
! !  HEPS     !  R !    !                                              !
! !  SESP     !  R !    !                                              !
! !  NSECG    !  I !    !                                              !
! !  NSECD    !  I !    !                                              !
! !  NBARAD   !  I !    !                                              !
! !  NBBAR    !  I !    !                                              !
! !  IBAR     ! TI !    !                                              !
! !  ZDEV     ! TR !    !                                              !
! !  NBSECT   !  I !  A ! NOMBRE DE MAILLES                            !
! !  FRTIMP   !  L !  A ! INDIQUE SI LE FROTTEMENT EST IMPLICITE       !
! !  NMLARG   !  I !  D !                                              !
! !___________!____!____!______________________________________________!
!
!                             VARIABLES LOCALES
! .___________.____.____.______________________________________________.
! !  FLUX     ! TR !  A ! FLUX ASSEMBLE DANS CHAQUE CELLULE            !
! !  ZG       !  R !  A ! COTE SURFACE LIBRE CELLULE DE GAUCHE         !
! !  ZD       !  R !  A ! COTE SURFACE LIBRE CELLULE DE DROITE         !
! !  HG(D)    !  R !  A ! TIRANT D'EAU CELLULE DE GAUCHE (DROITE)      !
! !  PRG(D)   !  R !  A ! PRESSION CELLULE DE GAUCHE (DROITE)          !
! !  QG(D     !  R !  A ! VITESSE CELLULE DE GAUCHE (DROITE)           !
! !  SG(D)    !  R !  A ! SECTION MOUILLEE CELLULE DE GAUCHE (DROITE)  !
! !  UG(D     !  R !  A ! VITESSE CELLULE DE GAUCHE (DROITE)           !
! !  CG(D)    !  R !  A ! CELERITE CELLULE DE GAUCHE (DROITE)          !
! !  FRG(D)   !  R !  A ! FROUDE CELLULE DE GAUCHE (DROITE)            !
! !  UTILD    !  R !  A ! VITESE MOYENNE DE ROE                        !
! !  CTILD    !  R !  A ! CELERITE MOYENNE DE ROE                      !
! !  SDIFF    !  R !  A ! DIFFERENCE ENTRE SECTION DROITE ET GAUCHE    !
! !  LAMDA1   !  R !  A ! VALEUR PROPRE 1, UTILD-CTILD                 !
! !  LAMDA2   !  R !  A ! VALEUR PROPRE 2, UTILD+CTILD                 !
! !  FLULOC   ! TR !  A ! FLUX LOCAL A TRAVERS UNE INTERFACE           !
! !  T1       ! TR !  A ! VECTEUR PROPRE ASSOCIE A LA V.P 1            !
! !  T2       ! TR !  A ! VECTEUR PROPRE ASSOCIE A LA V.P 2            !
! !  TS1      ! TR !  A ! VECTEUR DE L'INVERSE DE LA MATRICE (T1,T2)-1 !
! !  TS2      ! TR !  A ! VECTEUR DE L'INVERSE DE LA MATRICE (T1,T2)-1 !
! !  SOTILD   ! TR !  A ! T. SOURCE  INTERFACE                         !
! !  SOPRIM   !  R !  A ! T. SOURCE (CENTRE DE LA CELLULE)             !
! !  FLUSOG   ! TR !  A ! FLUX DE T. SOURCE PARTIE GAUCHE DE LA CELLULE!
! !  FLUSOD   ! TR !  A ! FLUX DE T. SOURCE PARTIE DROITE DE LA CELLULE!
! !  FLUSOC   ! TR !  A ! FLUX DE T. SOURCE PARTIE CENTRE DE LA CELLULE!
! !  SOFROT   ! TR !  A ! FROTTEMENT INTERFACE                         !
! !  FLUFRG   ! TR !  A ! FLUX DE FROTTEMENT GAUCHE DE LA CELLULE      !
! !  FLUFRD   ! TR !  A ! FLUX DE FROTTEMENT DROITE DE LA CELLULE      !
! !  TIRANT   !  R !  A ! TIRANT D'EAU                                 !
! !  COEF     !  R !  A ! COEF DANS L'EQ 2EME DEGRE (CAS IMPLICITE)    !
! !  DELTA    !  R !  A ! DISCRIMINANT EQ 2EME DEGRE (CAS IMPLICITE)   !
! !  IVIDE    ! TI !  A ! INDICATEUR DE CELLULE GAUCHE VIDE            !
! !  INDIC    !  I !  A ! 0 SI LES 2 CEL. PLEINES, 1 SI CEL DROITE VIDE!
! !           !    !    ! 2 SI CEL. GAUCHE VIDE., 3 SI 2 CEL. VIDES    !
! !___________!____!____!______________________________________________!
!
!     TYPE : I (ENTIER), R (REEL), A (ALPHANUMERIQUE), T (TABLEAU)
!            L (LOGIQUE)   .. ET TYPES COMPOSES (EX : TR TABLEAU REEL)
!     MODE : D (DONNEE NON MODIFIEE), R (RESULTAT), M (DONNEE MODIFIEE)
!            A (AUXILIAIRE MODIFIE)
!
!***********************************************************************
! SGEO ALGEO SGEOD PRGEOD DEBGEO DEBGED STRUCTURE DE DONNEES SECTION

   !============================= Declarations ===========================

   !.. Modules importes ..
   !----------------------
   use M_PRECISION
   use M_PARAMETRE_C ! GPES, EPS3, EPS6, EPSN6
   use M_ERREUR_T    ! ERREUR
   use M_SINGULARITE_T ! singularites
   use M_BISSN1_I    ! Interface du sous-Programme BISSN
   use M_BOUSSI_I    ! Interface du sous-programme BOUSSI
   use M_CALETA_I    ! Interface du sous-programme CALETA
   use M_DEBITA_I    ! Interface du sous-programme DEBITA
   use M_MATRI_I     ! Interface du sous-Programme Matri
   use M_MATRIA_I     ! Interface du sous-Programme Matri
   use M_FLUDEV_I    ! Interface du sous-programme FLUDEV
   use M_FLUROE_I    ! Interface du sous-programme FLUROE
   use M_PRESD_I     ! Interface de la fonction    PRESD
   use M_SOURCE_I    ! Interface du sous-programme SOURCE
   use M_VVPROPI_I    ! Interface du sous-programme VVPROPI
   use M_VVPROP_I    ! Interface du sous-programme VVPROP
   use M_DICHOM_I
   use M_CSUR_I
   use M_CSURM1_I
   use M_BOUSSI_I

   !.. Declarations explicites ..
   !-----------------------------
   implicit none

   !.. Arguments ..
   !---------------
   ! 1ere dimension IM
   real(DOUBLE), dimension(:)    , intent(inout)     :: SNODE,QNODE
   real(DOUBLE), dimension(:)    , intent(inout)     :: UNODE
   real(DOUBLE), dimension(:)    , intent(inout)     :: ZNODE,YNODE
   real(DOUBLE), dimension(:)    , intent(inout)     :: FROUD,CNODE,BETA
   real(DOUBLE), dimension(:)    , intent(inout)     :: SPREC,QPREC
   real(DOUBLE), dimension(:,:)  , intent(inout)     :: FLUX
   real(DOUBLE), dimension(:)    , intent(inout)     :: DebitFlux
   real(DOUBLE), dimension(:)    , intent(in)        :: QIN
   real(DOUBLE), dimension(:)    , intent(in)        :: PCSing
   real(DOUBLE), dimension(:)    , intent(in)        :: X
   ! 1ere dimension IM, 2nde dimension NMLARG
   real(DOUBLE), dimension(:,:)  , intent(in)        :: SGEO
   real(DOUBLE), dimension(:,:)  , intent(in)        :: S1GEO
   real(DOUBLE), dimension(:,:)  , intent(in)        :: ALGEO
   ! 1ere dimension IM-1, 2nde dimension NMLARG
   real(DOUBLE), dimension(:,:)  , intent(in)        :: SGEOD
   real(DOUBLE), dimension(:,:)  , intent(in)        :: PRGEOD
   ! 1ere dimension IM, 2nde dimension NMLARG
   real(DOUBLE), dimension(:,:)  , intent(in)        :: DEBGEO
   ! 1ere dimension IM-1, 2nde dimension NMLARG
   real(DOUBLE), dimension(:,:)  , intent(in)        :: DEBGED
   ! 1ere dimension IM
   real(DOUBLE), dimension(:)    , intent(in)        :: COTR
   ! 1ere dimension IM-1
   real(DOUBLE), dimension(:)    , intent(in)        :: DZD
   ! 1ere dimension IM
   real(DOUBLE), dimension(:)    , intent(in)        :: DZ
   real(DOUBLE),                   intent(in)        :: DT
   real(DOUBLE),                   intent(in)        :: HEPS
   integer     ,dimension(:)      ,intent(inout)     :: JGNODE,JDNODE,IFIGE
   integer     ,                   intent(in)        :: NSECG,NSECD,KTEMPS
   integer     ,                   intent(in)        :: NBARAD,NBBAR
   ! Type singularite,
   type(SINGULARITE_T), dimension(:) , intent(inout) :: SINGULARITE
   integer     ,                   intent(in)        :: NBSECT
   logical     ,                   intent(in)        :: FRTIMP
   logical     ,                   intent(in)        :: Impli_Trans
   logical     ,                   intent(in)        :: PerteElargissementTrans
   logical     ,                   intent(in)        :: Boussinesq
   integer     ,                   intent(in)        :: NMLARG
   integer     ,                   intent(in)        :: CQMV
   Type (ERREUR_T)                ,intent(inout)     :: ERREUR

   !.. Variables locales ..
   !-----------------------
   real(DOUBLE) :: SCUBE(NBSECT)
   real(DOUBLE) :: S1D,S1G
   real(DOUBLE) :: FLULOC(NBSECT,2)
   real(DOUBLE) :: FLUSOD(NBSECT,2)
   real(DOUBLE) :: FLUSOC(NBSECT,2)
   real(DOUBLE) :: FLUFRG(NBSECT,2),FLUFRD(NBSECT,2),FLUSOG(NBSECT,2)
   real(DOUBLE) :: A(NBSECT,4),D(NBSECT,4),L(4,NBSECT)
   real(DOUBLE) :: M(4,NBSECT),N(4,NBSECT),ID(NBSECT,4)
   real(DOUBLE) :: FLULOD(NBSECT), FLULOG(NBSECT)
   real(DOUBLE) :: SOPRIM(2)
   integer      :: IVIDE(NBSECT),K
   integer      :: IB,IPOS,I,CELGAU,INDIC,J,JG,JD
   integer      :: NC,NN,Nb_Point,NOEUD
   integer      :: CORRG,NSECD0,NSECDL,ITYP,retour
   real(DOUBLE) :: BETAM
   real(DOUBLE) :: ZD,ZG,FRD,FRG,PRDPREC
   real(DOUBLE) :: HG,QG,SG,UG,PRG,HD,QD,SD,UD,PRD,CG,CD
   real(DOUBLE) :: BETAG,BETAD
   real(DOUBLE) :: UTILD,CTILD,SDIFF
   real(DOUBLE) :: LAMDA1,LAMDA2
   real(DOUBLE) :: LAMDI1,LAMDI2
   real(DOUBLE) :: T1(2),TS1(2),T2(2),TS2(2)
   real(DOUBLE) :: TI1(2),TIS1(2),TIS2(2),TI2(2)
   real(DOUBLE) :: SOTILD(2)
   real(DOUBLE) :: SOFROT(2)
   real(DOUBLE) :: COEF,DELTA
   real(DOUBLE) :: CELEGA,CELEDR,T0
   real(DOUBLE) :: DEB,V0(2),V1(2)
   real(DOUBLE) :: AI(4),DI(4),IDI(4)
   real(DOUBLE) :: WW1(2,NBSECT),S,S1,S2,S3,SSG,SSD
   !character(132) :: !arbredappel_old ! arbre d'appel precedent
   real(DOUBLE) :: DX
   real(DOUBLE) :: CE(NBSECT),ZF(NBSECT),scube1(nbsect)
   real(DOUBLE) :: HNP1(nbsect),Z(Nbsect)
   integer      :: NS

   !============================= Instructions ===========================

   ! INITIALISATION
   !===============
   Erreur%Numero = 0
   PRD = 0
   !
   If (Boussinesq) then

      DO NOEUD = NSECG , NSECD
         JG   = JGNODE(NOEUD)
         JD   = JDNODE(NOEUD)
         S1G  = S1GEO(NOEUD,JG)
         S1D  = S1GEO(NOEUD,JD)

         ! INTERPOLATION DE LA LARGEUR
         Scube(Noeud) = ( S1D * ( YNODE(NOEUD) - JG * DZ(NOEUD) ) + S1G * ( JD * DZ(NOEUD) - YNODE(Noeud) ) ) / DZ(NOEUD)

      Enddo
   endif

   NSECD0    = 0
   SOPRIM(1) = 0._DOUBLE
   SOPRIM(2) = 0._DOUBLE
   SOFROT(1) = 0._DOUBLE
   SOFROT(2) = 0._DOUBLE
   SOTILD(1) = 0._DOUBLE
   SOTILD(2) = 0._DOUBLE

   !
   !  CONDITION LIMITE SUR L'INCREMENT
   !
   V0(1) = -SNODE(NSECG) + SPREC(NSECG)
   V0(2) = -QNODE(NSECG) + QPREC(NSECG)
   V1(1) = -SNODE(NSECD) + SPREC(NSECD)
   V1(2) = -QNODE(NSECD) + QPREC(NSECD)

   ! CALCUL DES ETATS
   ! ----------------
   t0 = 0.D0
   S  = 0.D0
   S1 = 0.D0
   S2 = 0.D0
   S3 = 0.D0

   label_boucle_sect : do I = NSECG , NSECD - 1
      INDIC    = 0
      IVIDE(I) = 0
!
! Modification Nicole 22/08/2012
!
!     IF( ( IFIGE(I) == 0 ) .OR. ( IFIGE(I+1) == 0 ) ) then
         ! CALCUL DE L'ETAT HYDRAULIQUE  A DROITE
         CELGAU = 0
         PRDPREC = PRD

         call CALETA ( &
              SD     , &
              QD     , &
              UD     , &
              FRD    , &
              CD     , &
              HD     , &
              ZD     , &
              PRD    , &
              BETAD  , &
              INDIC  , &
              SNODE  , &
              QNODE  , &
              UNODE  , &
              ZNODE  , &
              YNODE  , &
     JGNODE , JDNODE , &
              FROUD  , &
              CNODE  , &
              BETA   , &
              COTR   , &
              DZ     , &
              DZD    , &
              SGEO   , &
              ALGEO  , &
              SGEOD  , &
              PRGEOD , &
              HEPS   , &
              I+1    , &
              CELGAU , &
              NMLARG , &
              ERREUR   &
                     )

         if( Erreur%Numero /= 0 ) then
            return
         endif

         ! CALCUL DE L'ETAT HYDRAULIQUE  A GAUCHE
         CELGAU = 1

         call CALETA ( &
              SG     , &
              QG     , &
              UG     , &
              FRG    , &
              CG     , &
              HG     , &
              ZG     , &
              PRG    , &
              BETAG  , &
              INDIC  , &
              SNODE  , &
              QNODE  , &
              UNODE  , &
              ZNODE  , &
              YNODE  , &
     JGNODE , JDNODE , &
              FROUD  , &
              CNODE  , &
              BETA   , &
              COTR   , &
              DZ     , &
              DZD    , &
              SGEO   , &
              ALGEO  , &
              SGEOD  , &
              PRGEOD , &
              HEPS   , &
              I      , &
              CELGAU , &
              NMLARG , &
              ERREUR   &
                     )

         if( INDIC >= 2 ) then
            IVIDE(I) = 1
         endif

         BETAM = ( BETAG + BETAD ) / 2._DOUBLE

         if( INDIC == 5 ) then

            ! ETAT SEC DES DEUX COTES
            ! =======================
            FLULOC(I,1)   = 0._DOUBLE
            FLULOC(I,2)   = 0._DOUBLE
            FLUSOG(I+1,1) = 0._DOUBLE
            FLUSOG(I+1,2) = 0._DOUBLE
            FLUSOD(I,1)   = 0._DOUBLE
            FLUSOD(I,2)   = 0._DOUBLE
            FLUSOC(I,1)   = 0._DOUBLE
            FLUSOC(I,2)   = 0._DOUBLE
            FLUFRG(I+1,1) = 0._DOUBLE
            FLUFRG(I+1,2) = 0._DOUBLE
            FLUFRD(I,1)   = 0._DOUBLE
            FLUFRD(I,2)   = 0._DOUBLE

         else
            ! AU MOINS UN CELLULE EST MOUILLEE
            ! ================================
            ! CALCUL DES MOYENNES DE ROE
            ! --------------------------
            SSD   = DSQRT( SD )
            SSG   = DSQRT( SG )
            UTILD = ( UD * SSD + UG * SSG ) / ( SSD + SSG )
            SDIFF = dabs( SD - SG )
            if( SDIFF < EPS3 ) then
               CELEDR = CNODE(I+1)
               CELEGA = CNODE(I)
               CTILD  = ( CELEDR + CELEGA ) / 2._DOUBLE
            else
               CTILD = dsqrt( ( PRD - PRG ) / ( SD - SG ) )
            endif

            ! CALCUL DES TERMES SOURCES
            ! ------------------------
            call SOURCE (  &
                 SOTILD  , &
                 SOPRIM  , &
                 SOFROT  , &
                 ZG      , &
                 ZD      , &
                 PRG     , &
                 PRDPREC , &
                 CTILD   , &
                 BETAM   , &
                 X       , &
                 SNODE   , &
 QNODE   , UNODE , JDNODE, &
                 QIN     , &
                 PCSing  , &
                 COTR    , &
                 SGEO    , &
                 SGEOD   , &
                 PRGEOD  , &
                 DEBGED  , &
                 FRTIMP  , &
                 DZD     , &
                 DZ      , &
                 I       , &
                 NSECG   , &
                 NMLARG  , &
 PerteElargissementTrans , &
                 CQMV    , &
                 ERREUR    &
                         )
            !
            ! CALCUL DES VECTEURS PROPRES ET DES VALEURS PROPRES DE LA MATRICE DE ROE
            ! -----------------------------------------------------------------------
            call VVPROPI(  &
                 LAMDA1  , &
                 LAMDA2  , &
                 T1      , &
                 T2      , &
                 TS1     , &
                 TS2     , &
                 UTILD   , &
                 CTILD   , &
                 BETAM   , &
                 FRD     , &
                 FRG     , &
                 UD      , &
                 UG      , &
                 CG      , &
                 CD      , &
                 IVIDE(I), &
                 CORRG   , &
                 Erreur    &
                        )

            if( impli_trans ) then
               !
               !   CALCUL DES COEFFICIENTS DE LA MATRICE POUR L'IMPLICITATION
               !
               call MATRI(DI , IDI , LAMDA1 , LAMDA2 , &
                          T1 , T2 , TS1 , TS2 , ERREUR)
               !
               !
               !
               label_boucle_4 : do J = 1 , 4
                  D(I,J) = DI(J)
                 ID(I,J) = IDI(J)
               end do label_boucle_4
            endif

           !
           ! CALCUL DES FLUX DE ROE
           ! ----------------------
           call FLUROE ( &
                FLULOC , &
                FLUSOD , &
                FLUSOG , &
                FLUFRG , &
                FLUFRD , &
                FLUSOC , &
                SOTILD , &
                SOPRIM , &
                SOFROT , &
                LAMDA1 , &
                LAMDA2 , &
                T1     , &
                T2     , &
                TS1    , &
                TS2    , &
                SG     , &
                SD     , &
                QG     , &
                QD     , &
                PRG    , &
                PRD    , &
                BETAG  , &
                BETAD  , &
                X      , &
                UTILD  , &
                I      , &
                INDIC  , &
                CORRG  , &
                Erreur   &
                       )
         endif
 !    endif ! fin de la reactualisation des flux
   end do label_boucle_sect

   !
   !  boucle pour calculer la matrice A de l'implicite
   !
   if( impli_trans ) then
      label_boucle_etat : do I = NSECG , NSECD
         INDIC    = 0
         IVIDE(I) = 0
         !
         !  Cacul de l'etat a gauche
         !
         UG    = UNODE (I)
         CG    = CNODE (I)
         BETAG = BETA(I)
         !
         ! CALCUL DES VECTEURS PROPRES ET DES VALEURS PROPRES en I
         ! -----------------------------------------------------------------------
         call VVPROP( &
                    TI1 , &
                    TI2 , &
                   TIS1 , &
                   TIS2 , &
                 LAMDI1 , &
                 LAMDI2 , &
                  BETAG , &
                     UG , &
                     CG , &
                 Erreur )

         !
         !   CALCUL DES COEFFICIENTS DE LA MATRICE POUR L'IMPLICITATION
         !
         call MATRIA( AI , LAMDI1 , LAMDI2 , TI1 , TI2 , TIS1 , TIS2 , ERREUR )
         !
         !
         !

         label_boucle_5 : do J = 1 , 4
            A(I,J) = AI(J)
         end do label_boucle_5
      end do label_boucle_etat
   endif
   !
   ! PRISE EN COMPTE DES BARRAGES DEVERSANTS
   ! ---------------------------------------
   if( NBARAD >= 1 ) then
      do IB = 1 , NBBAR
         IPOS = SINGULARITE(IB)%Section
         ITYP = SINGULARITE(IB)%Type
         if( ( IPOS >= NSECG ) .and. ( IPOS <= NSECD ) .and. ( ITYP >= 4 ) .and. ( ITYP /= 8 ) ) then
            PRG = PRESD( IPOS , SNODE(IPOS  ) , DZD , PRGEOD , SGEOD , NMLARG , ERREUR )
            PRD = PRESD( IPOS , SNODE(IPOS+1) , DZD , PRGEOD , SGEOD , NMLARG , ERREUR )
            if( ITYP == 6 ) then
               Nb_Point = Size( SINGULARITE(IB)%PtQ )
            else
               Nb_point = 1
               if(.not.associated(Singularite(IB)%PtQ)) allocate( Singularite(IB)%PtQ(Nb_point) , STAT = retour )
               if(.not.associated(Singularite(IB)%PtZ)) allocate( Singularite(IB)%PtZ(Nb_point) , STAT = retour )
            endif
            call FLUDEV (            &
                      FLULOC       , &
                      FLULOD       , &
                      FLULOG       , &
                      NSECD0       , &
                      ITYP         , &
                      IPOS         , &
         SINGULARITE(IB)%CoteCrete , &
             SINGULARITE(IB)%Debit , &
  SINGULARITE(IB)%Epaisseur_Seuil  , &
                      Nb_point     , &
               SINGULARITE(IB)%PtQ , &
               SINGULARITE(IB)%PtZ , &
                     SNODE(IPOS+1) , &
                     ZNODE(IPOS+1) , &
                     PRD           , &
                     SNODE(IPOS  ) , &
                     ZNODE(IPOS  ) , &
                     YNODE(IPOS  ) , &
                     PRG           , &
                     QNODE(IPOS)   , &
                     QNODE(IPOS+1) , &
                     DZ            , &
                     ALGEO         , &
                     COTR          , &
                     DT            , &
        SINGULARITE(IB)%CoeffDebit , &
                      NMLARG       , &
                      ERREUR         &
                                   )
            if( Erreur%Numero /= 0 ) then
               return
            endif

            !  MISE A ZERO DES FLUX SOURCES
            FLUSOD(IPOS,1)   = 0._DOUBLE
            FLUSOD(IPOS,2)   = 0._DOUBLE
            FLUSOG(IPOS+1,1) = 0._DOUBLE
            FLUSOG(IPOS+1,2) = 0._DOUBLE
            FLUFRG(IPOS+1,1) = 0._DOUBLE
            FLUFRG(IPOS+1,2) = 0._DOUBLE
            FLUFRD(IPOS,1)   = 0._DOUBLE
            FLUFRD(IPOS,2)   = 0._DOUBLE
         endif
      end do
   endif

   ! CALCUL DES BORNES DE CALCUL (pour l'option Onde de submersion)
   if( NSECD0 > 0 ) then
      NSECDL = NSECD0
   else
      NSECDL = NSECD
   endif

   do I = NSECG + 1 , NSECDL - 1
      QPREC(I) = QNODE(I)
      SPREC(I) = SNODE(I)
   end do

   ! ASSEMBLAGE DES FLUX
   ! -------------------
   do I = NSECG + 1 , NSECDL - 1
 !!     If( IFIGE(I) == 0 ) then
         FLUX(I,1) = ( FLULOC(I,1) - FLULOC(I-1,1) + &
                       FLUSOG(I,1) + FLUSOD(I,1) + &
                       FLUFRG(I,1) + FLUFRD(I,1) )
         FLUX(I,2) = ( FLULOC(I,2) - FLULOC(I-1,2) + &
                       FLUSOG(I,2) + FLUSOD(I,2) + &
                       FLUFRG(I,2) + FLUFRD(I,2) + FLUSOC(I,2) )
        DebitFlux (I) = FLULOC(I,1) +FLUSOD(I,1) + FLUFRD(I,1)
 !!     endif
      if( impli_trans ) then
         !
         ! ASSEMBLAGE DES MATRICES IMPLICITES
         !
         K = I - NSECG + 1
         do J = 1 , 4
            L(J,K) = -( A(I-1,J) + D(I-1,J) ) * DT / ( X(I+1) - X(I-1) )
            M(J,K) = ID(I,J) + ( D(I-1,J) + D(I,J) ) * DT / ( X(I+1) - X(I-1) )
            N(J,K) = ( A(I+1,J) - D(I,J) ) * DT / ( X(I+1) - X(I-1) )
         end do
      endif
   end do
   !
   ! INITIALISATION DES BORDS DE LA MATRICE POUR LA PRISE EN COMPTE DES CL
   !
   NC = NSECDL - NSECG + 1
   !
   do J = 1 , 4
      L(J,1)  = 0._DOUBLE
      N(J,1)  = 0._DOUBLE
      L(J,NC) = 0._DOUBLE
      N(J,NC) = 0._DOUBLE
   end do
!
   M(1,1)  = 1._DOUBLE
   M(2,1)  = 0._DOUBLE
   M(3,1)  = 0._DOUBLE
   M(4,1)  = 1._DOUBLE
   M(1,NC) = 1._DOUBLE
   M(2,NC) = 0._DOUBLE
   M(3,NC) = 0._DOUBLE
   M(4,NC) = 1._DOUBLE
   !
   ! TRAITEMENT DES BARRAGES AVAL
   !
   if( NBARAD >= 1 ) then
      do IB = 1 , NBBAR
         IPOS = SINGULARITE(IB)%Section
         if( ( IPOS >= NSECG + 1 )  &
             .and. ( IPOS <= NSECDL - 1 ) &
             .and. ( SINGULARITE(IB)%Type >= 2 ) ) then
            FLUX(IPOS,2) = FLULOD(IPOS) - FLULOC(IPOS-1,2) + &
                           FLUSOG(IPOS,2) + FLUSOD(IPOS,2) + &
                           FLUFRG(IPOS,2) + FLUFRD(IPOS,2) + &
                           FLUSOC(IPOS,2)
            FLUX(IPOS+1,2) = FLULOC(IPOS+1,2) - FLULOG(IPOS) + &
                             FLUSOG(IPOS+1,2) + FLUSOD(IPOS+1,2) + &
                             FLUFRG(IPOS+1,2) + FLUFRD(IPOS+1,2) + &
                             FLUSOC(IPOS+1,2)
         endif
      end do
   endif
   !
   !  CALCUL FINAL
   !
   WW1(1,1)  = 0._DOUBLE
   WW1(2,1)  = 0._DOUBLE
   WW1(1,NC) = 0._DOUBLE
   WW1(2,NC) = 0._DOUBLE
   !
   !
   do I = NSECG + 1 , NSECDL - 1
      K        = -NSECG + I + 1
      WW1(1,K) = -FLUX(I,1)* 2._DOUBLE * DT / ( X(I+1) - X(I-1) )
      WW1(2,K) = -FLUX(I,2)* 2._DOUBLE * DT / ( X(I+1) - X(I-1) )
   end do
   !
   !   RESOLUTION DU SYSTEME LINEAIRE
   !
   NC = NSECDL - NSECG + 1
   NN = 10
   !
   !  MODIFICATION DU SECOND MEMBRE POUR LA PRISE EN COMPTE DES CL
   !
   If( Impli_Trans )  then
      WW1(1,1)  = V0(1)
      WW1(2,1)  = V0(2)
      WW1(1,NC) = V1(1)
      WW1(2,NC) = V1(2)
      call BISSN1( WW1 , L , M , N , NC , 1 , ERREUR )
   endif

   do NOEUD = NSECG + 1 , NSECDL - 1
      K            = -NSECG + NOEUD + 1
      SNODE(NOEUD) = SPREC(NOEUD) + WW1(1,K)
   enddo

   !      Prise en compte des termes hydrostatiques
   IF( Boussinesq ) THEN
      !do NOEUD = NSECG + 1 , NSECDL - 1
      do NOEUD = NSECG , NSECDL - 1
         HNP1(NOEUD)  = CSURM1(SNODE(NOEUD), DZ(NOEUD),SGEO(NOEUD,:), Erreur)
         JG           = int(HNP1(NOEUD)/DZ(NOEUD)) + 1
         JD           = JG + 1
         S1G          = S1GEO(NOEUD,JG)
         S1D          = S1GEO(NOEUD,JD)

         ! INTERPOLATION DE LA LARGEUR
         Scube1(Noeud) = ( S1D * ( HNP1(NOEUD) - JG * DZ(NOEUD) ) + S1G * ( JD * DZ(NOEUD) - HNP1(Noeud) ) ) / DZ(NOEUD)
      enddo
      NS = NSECDL - NSECG !+ 1
      DX = X(2) - X(1)
      do NOEUD = 1 , NS
         CE(NOEUD) = WW1(2,NOEUD)
         ZF(NOEUD) = COTR(NOEUD)
         Z(NOEUD)  = HNP1(NOEUD) + COTR(NOEUD)
      enddo

      call BOUSSI (  &
           QNODE   , &
           SNODE   , &
           YNODE   , &
           Z       , &
           SCUBE1  , &
           SCUBE   , &
           SPREC   , &
           QPREC   , &
           CE      , &
           ZF      , &
           NSECG   , &
           NSECDL  , &
           X       , &
           DX      , &
           DT      , &
           NS      , &
           ERREUR    &
           )
   ELSE
      do NOEUD = NSECG + 1 , NSECDL - 1
         K            = -NSECG + NOEUD + 1
         QNODE(NOEUD) = QPREC(NOEUD) + WW1(2,K)
      enddo
   endif

   !
   ! PRISE EN COMPTE DU FROTTEMENT IMPLICITE LE CAS ECHEANT
   !
   if( FRTIMP ) then
      do I = NSECG + 1 , NSECDL - 1

         call DEBITA (  &
              DEB     , &
              I       , &
              SNODE(I), &
              SGEO    , &
              DEBGEO  , &
              NMLARG  , &
              ERREUR    &
              )
         if( Erreur%Numero /= 0 ) then
            return
         endif

         COEF  = GPES * DT * SNODE(I) / ( DEB * DEB )

         DELTA = ( 1._DOUBLE + 4._DOUBLE * COEF * dabs( QNODE(I) ) )

         if( COEF > EPS6 ) then

            if( QNODE(I) > 0._DOUBLE ) then
               QNODE(I) = ( -1._DOUBLE + dsqrt( DELTA ) ) / ( 2._DOUBLE * COEF )
               DebitFlux (I) = (-1._DOUBLE + dsqrt(DELTA ))/ (2._DOUBLE * COEF )
            else
               QNODE(I) = ( 1._DOUBLE - dsqrt( DELTA ) ) / ( 2._DOUBLE * COEF )
               DebitFlux(I) = (1._DOUBLE -dsqrt(DELTA) ) / ( 2._DOUBLE * COEF)
            endif

         else

            QNODE(I) = QNODE(I) * ( 1._DOUBLE - COEF * QNODE(I) )
            DebitFlux(I) = DebitFlux(I)* (1._DOUBLE-COEF*DebitFlux(I))

         endif

      end do

   endif

   do I = NSECG + 1 , NSECDL - 1
      ! TRAITEMENT PARTICULIER DES CELLULES SECHES
      ! ------------------------------------------
      if( IVIDE(I) == 1 ) then
         ! LE DEBIT NE S'INVERSE PAS
         ! -------------------------
         if( QNODE(I) * QPREC(I) < EPSN6 ) then
            QNODE(I) = 0._DOUBLE
         endif
      endif
   end do

   !------------------
   ! Fin du traitement
   !------------------

   !Erreur%arbredappel = !arbredappel_old

   return

end subroutine SOLVRO
