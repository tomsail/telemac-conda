!== Copyright (C) 2000-2022 EDF-CEREMA ==
!
!   This file is part of MASCARET-TRACER.
!
!   MASCARET-TRACER is free software: you can redistribute it and/or modify
!   it under the terms of the GNU General Public License as published by
!   the Free Software Foundation, either version 3 of the License, or
!   (at your option) any later version.
!
!   MASCARET-TRACER is distributed in the hope that it will be useful,
!   but WITHOUT ANY WARRANTY; without even the implied warranty of
!   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
!   GNU General Public License for more details.
!
!   You should have received a copy of the GNU General Public License
!   along with MASCARET-TRACER.  If not, see <http://www.gnu.org/licenses/>
!

subroutine HYP1FA( &
                   ! RESULTATS
            FNP1 , &   ! RESULTAT APRES CONVECTION
                   ! DONNEES NON MODIFIEES
              FN , &   ! TABLEAU DES VALEURS A CONVECTER
             VIT , &   ! TABLEAU DES VITESSES
               X , &   ! TABLEAU DES COORDONNEES
              DT , &   ! PAS DE TEMPS
            NOPT , &   ! OPTION POUR L'EQUATION A RESOUDRE :
                       !               0:FORME CONSERVATIVE
                       !               1:FORME NON CONSERVATIVE
              IM , &   ! DIMENSION DES TABLEAUX
              CG , &   ! CONDITONS LIMITES A TN+1 A GAUCHE
              CD , &   ! CONDITONS LIMITES A TN+1 A DROITE
            PRAD , &   ! PRAD(I) PENTE DU RADIER ENTRE I ET I+1
            FROT , &   ! FROT(I) COEFFICIENT DE FROTTEMENT EN I
          ERREUR )     ! Canal de sortie pour fichier d impression

!*****************************************************************************
! PROGICIEL : TRACER         S. MANDELKERN
!
! VERSION : V8P4R0              EDF-CEREMA
!*****************************************************************************
!
!  FONCTION :
!  --------
!
!            SCHEMA DE CONVECTION 1D
!            PAR LA METHODE DES CARACTERISTIQUES EN CONVECTION FAIBLES
!
!
!*****************************************************************************
!
!   FICHIERS ENTREE/SORTIE :
!   ----------------------
!
!   SOUS-PROGRAMMES APPELES :	BISSND
!   -----------------------
!
!   SOUS-PROGRAMMES APPELANT :	RESEQU
!   ------------------------
!
!   COMMENTAIRES :
!   ------------
!*****************************************************************************

   use M_PRECISION
   use M_PARAMETRE_C
   use M_BISSND_tracer_I
   use M_ERREUR_T

   !.. Implicit Declarations ..
   implicit none
   !
   !.. Formal Arguments ..
   !
   Type (ERREUR_T)           ,intent(inout)        :: ERREUR
   ! DONNEES NON MODIFIES
   integer, intent(in)                             :: NOPT
   integer, intent(in)                             :: IM
   real (DOUBLE), intent(in)                       :: DT
   real (DOUBLE), dimension(:), intent(in)         :: FROT
   real (DOUBLE), dimension(:), intent(in)         :: PRAD
   real (DOUBLE), dimension(:), intent(in)         :: X
   real (DOUBLE), dimension(:), intent(in)         :: VIT
   real (DOUBLE), intent(in)                       :: CD
   real (DOUBLE), intent(in)                       :: CG
   real (DOUBLE), dimension(:), intent(in)         :: FN
   ! RESULTATS
   real (DOUBLE), dimension(:), intent(inout)      :: FNP1
   !.. Local Scalars ..
   integer :: i,ieps,imm1,imp1,k1,kep,kfin,mj,mk,noeud,NF
   real (DOUBLE) :: dref,dtr,dudx,dx,dxpied,dxref,eps,epst,epsu,epsx,eta1,eta2,eta3,fd, &
          fg,frod,frog,g,hdd,hdg,hgd,hgg,pasx,pd,pente,pg,produ,proref,ucour, &
          vitmax,xpiedd,xpiedg,z,zf,zp
   !.. Local Arrays ..
   real (DOUBLE), dimension(IM) :: A,B,C,DDT,HPIED,XPIED
   integer , dimension(IM) :: KPIED
   !.. Intrinsic Functions ..
   intrinsic DABS, DEXP, DLOG, DMAX1

   g      = 9.81d0
   IMM1   = IM - 1
   IMP1   = IM + 1
   VITMAX = 0.d0
   do NOEUD = 1 , IM
      VITMAX = DMAX1(DABS(VIT(NOEUD)),VITMAX)
   end do

   !
   !     1- DEFINITION DES GRANDEURS DE REFERENCE DU MAILLAGE
   !     ---------------------------------------------------------
   !          **************************************
   !          *  DXREF      * LONGEUR DE REFERENCE *
   !          *             *                      *
   !          *  EPS        * EPSILON DE PRECISION *
   !          *             *                      *
   !          *  EPSX       * PRECISION SUR X      *
   !          *             *                      *
   !          *  EPSU       * PRECISION SUR U      *
   !          **************************************
   !
   DXREF = ( X(IM) - X(1) ) / IM
   EPS   = 1.d-2
   EPSX  = EPS * DXREF
   EPSU  = EPS * VITMAX
   EPST  = EPS * DT

   !
   !     CAS D'UN PAS DE TEMPS NUL
   !     *************************
   !
   if( DT < 1.d-5 ) then
      do NOEUD = 1 , IM
         FNP1(NOEUD) = FN(NOEUD)
      end do
   else
      !
      !     I.1) CARACTERISTIQUES CALCULEES ANALYTIQUEMENT DX/DT=AX+B
      !          ****************************************************
      !
      !
      !     1- INITIALISATION ,POUR TOUTE MAILLE  ,DU TEMPS NECESSAIRE
      !              A  SA  TRAVERSEE : DDT
      !     ---------------------------------------------------------
      !
      !     GRANDEUR REFERENCE DU PRODUIT DES VITESSES *
      PROREF = EPSU**2
      !     GRANDEUR REFERENCE DERIVEE DE VITESSE EN X *
      DREF = EPSU / DXREF
      !
      do I = 1 , IMM1
         DX    = X(I+1) - X(I)
         DUDX  = ( VIT(I+1) - VIT(I) ) / DX
         PRODU = VIT(I) * VIT(I+1)
         !         /    TESTS SUR LE SIGNE DU PRODUIT A PROREF PRES  /
         if( PRODU <= PROREF ) then
            !              /   PRODUIT DES VITESSES NEGATIF OU NUL      /
            !              *   LE  TEMPS  POUR  TRAVERSEER LA  MAILLE EST INFINI
            !                    &   PRIS ICI EGAL A   2*DT   *
            DDT(I) = 2.d0 * DT
         else
            !              /   PRODUIT DES VITESSES STICTEMENT POSITIF  /
            !                     /TEST SUR NULLITE DE LA DERIVEE A DREF PRES/
            if( DABS(DUDX) > DREF ) then
            !                            /   VITESSE VARIABLE SUR LA MAILLE  /
               if( VIT(I) < 0.d0 ) then
                  KEP = -1
               end if
               if( VIT(I) > 0.d0 ) then
                  KEP = +1
               end if
               DDT(I) = -KEP * DLOG( -DUDX * DX / VIT(I+1) + 1.D0 ) / DUDX
               if( DABS(DDT(I)) > EPST ) cycle
            end if
            !                            /   VITESSE  CONSTANTE SUR LA MAILLE/      
            DDT(I) = DABS( DX / VIT(I) )
         end if
      end do
      !
      !     2- CALCUL DES PIEDS DES CARACTERISTIQUES
      !     ----------------------------------------
      !
      do I = 1 , IM
         DTR = DT
         !           /  TESTS SUR LE SIGNE DE LA VITESSE AU POINT I /
         if( VIT(I) >= -EPSU ) then
            if( VIT(I) >= -EPSU .and. VIT(I) <= EPSU ) then
               !                -  VITESSE NULLE    -
               KPIED(I) = I
               !      <<<  LE CALCUL EST DEJA FINI ,KPIED & XPIED CONNUS >>>
               XPIED(I) = 0.d0
               cycle
            elseif( VIT(I) > EPSU ) then
               !                -  VITESSE POSITIVE -
               MK   = I - 1
               IEPS = 1
               K1   = -1
               goto 1000
            end if
         end if
         !
         !
         !           2.1 -  INITIALISATION DE :
         !                         MK  ,INDICATEUR DE MAILLE
         !                         K1  ,INCREMENT FNP1 EXPLORATION
         !                         IEPS, TEL QUE MK+IEPS SOIT LE POINT COURANT
         !
         !
         !                -  VITESSE NEGATIVE -
         MK   = I
         IEPS = 0
         K1   = 1
         1000 do
            !                    //  TEST SUR LA TRAVERSEE DES LIMITES DU DOMAINE//
            !
            !                 SI ON A TRAVERSE , ALORS ON VA ECRIRE KPIED & XPIED
            if( MK == 0 .or. MK == IM ) goto 1100
            !                    SINON LA CARACTERISTIQUE EST INTERIEURE AU DOMAINE
            !
            !                    LE PIED SE TROUVE T IL DANS CETTE MAILLE ?
            !                    SI OUI ,ALORS ON VA ECRIRE KPIED & XPIED
            !
            if( DTR <= DDT(MK) ) exit
            !                    SINON , ON PASSE A LA MAILLE SUIVANTE
            DTR = DTR - DDT(MK)
            !
            !           2.2-   DESCENTE DE LA CARACTERISTIQUE
            !
            !
            !           //   PASSAGE A LA MAILLE SUIVANTE  //
            MK = MK + K1
         end do
         !
         !            2.3-   ECRITURE DE KPIED & XPIED
         !
         !
         !                  2.3-1   PIED INTERIEUR AU DOMAINE
         !
         KPIED(I) = MK
         !
         !                  *  UCOUR : VITESSE AU POINT  COURANT  *
         UCOUR = VIT(MK+IEPS)
         DX    = X(MK+1) - X(MK)
         DUDX  = ( VIT(MK+1) - VIT(MK) ) / DX
         !
         !                         // TEST SUR NULLITE VIT COURANT //
         !                        ... SI LA VITESSE EST NON NULLE,ALORS ON SAUTE
         if( DABS(UCOUR) > EPSU ) then
            !
            !                   // VITESSE NON NULLE //
            !
            !                         // TEST SUR NULLITE DERIVEE DE VIT EN X//
            !                        ... SI LA DERIVEE  EST NULLE , ALORS ON SAUTE
            if( DABS(DUDX) < DREF) then
               !                         // DERIVEE EST  NULLE //
               DXPIED = -UCOUR * DTR
            else
               !                             .. SINON ...
               !                         // DERIVEE EST NON NULLE //
               DXPIED = UCOUR * ( DEXP( -DUDX * DTR ) - 1.D0 ) / DUDX
            end if
            !                            DXPIED=UCOUR*DTR
            !
            !                         //  TEST DU SIGNE DE LA VITESSE  COURANTE //
            !                         ... SI LA VITESSE COURANTE EST NEGATIVE
            if( UCOUR > EPSU ) then
               !                         ... SINON LA VITESSE COURANTE EST POSITIVE ET
               !                             XPIED(I)=DX-ABS(DXPIED)
               XPIED(I) = DX + DXPIED
            else
               !                                 ALORS ...
               !                             XPIED(I)=ABS(DXPIED)
               XPIED(I) = DXPIED
            end if
         else
            !                                .. SINON ...
            !                    // VITESSE NULLE //
            KPIED(I) = MK + IEPS
            XPIED(I) = 0.d0
         end if
         cycle
         !
         !                  2.3-2   PIED EXTERIEUR AU DOMAINE
         !
         1100 KPIED(I) = MK
         !
         !                         SORTIE DANS L' ELEMENT GAUCHE OU DROITE ?
         if( MK /= 0 ) then
            !                         // SORTIE DANS L' ELEMENT DROITE //
            XPIED(I) = -VIT(IM) * DTR
         else
            !                         // SORTIE DANS L' ELEMENT GAUCHE //
            XPIED(I) = VIT(1) * ( DT - DTR )
         end if
         !
      !
      end do
      !
      !     /ICI TOUS LES PIEDS DE  CARACTERISTIQUE SONT DETERMINES  /
      !
      !    1.    CALCUL DES BASES GAUCHES & DES HAUTEURS DES FONCTIONS
      !                PSI    TRANSPORTEES
      !          -----------------------------------------------------
      !
      !   <<< LES  HAUTEURS SONT STOCKEES DANS HPIED & LES PENTES DANS DDT>>
      !
      !       1.1  BASE GAUCHE
      DDT(1) = 0.d0
      do I = 2 , IM
         !
         if( KPIED(I-1) == 0 ) then
            XPIEDG = X(1) - VIT(1) * DT
         else
            XPIEDG = X(KPIED(I-1))
         end if
         !
         if( KPIED(I) == 0 ) then
            XPIEDD = X(1) - VIT(1) * DT
         else
            XPIEDD = X(KPIED(I))
         end if
         !
         DDT(I) = XPIEDD - XPIEDG + XPIED(I) - XPIED(I-1)
         if( DDT(I) < EPSX ) then
            DDT(I) = EPSX
         end if
      end do
      !       1.2  HAUTEUR DE LA FONCTION TRANSPORTEE
      HPIED(1) = 1.d0
      if( NOPT /= 0 ) then
         HPIED(1) = ( X(2) - X(1) ) / DDT(2)
      end if
      do I = 2 , IMM1
         HPIED(I) = 1.d0
         if( NOPT /= 0 ) then
            HPIED(I) = ( X(I+1) - X(I-1) ) / ( DDT(I) + DDT(I+1) )
         end if
      end do
      HPIED(IM) = 1.d0
      if( NOPT /= 0 ) then
         HPIED(IM) = ( X(IM) - X(IMM1) ) / DDT(IM)
      end if
      !
      !    2.  CALCUL DE LA MATRICE <PHI(I),PHI(J)>
      !        ------------------------------------
      !
      if( VIT(1) >= 0.d0 ) then
         A(1) = 0.d0
         B(1) = 1.d0
         C(1) = 0.d0
      else
         A(1) = 0.d0
         B(1) = 2.d0 * ( X(2) - X(1) )
         C(1) = X(2) - X(1)
      end if
      do I = 2 , IMM1
         A(I) = X(I) - X(I-1)
         B(I) = 2. * ( X(I+1) - X(I-1) )
         C(I) = X(I+1) - X(I)
      end do
      !
      if( VIT(IM) <= 0.d0 ) then
         A(IM) = 0.d0
         B(IM) = 1.d0
         C(IM) = 0.d0
      else
         A(IM) = X(IM) - X(IMM1)
         B(IM) = 2.d0 * ( X(IM) - X(IMM1) )
         C(IM) = 0.d0
      end if
      !
      !    3.  CALCUL DU SECOND MEMBRE
      !        -----------------------
      !
      !       3.1  MISE A ZERO DU SECOND MEMBRE
      do I = 1 , IM
         FNP1(I) = 0.d0
      end do
      !
      !       3.2  CALCUL DU TERME FNP1(I)
      !
      !          // POUR TOUTE FONCTION PSI TRANSPORTEE //
      do I = 1 , IM
         !
         !               3.2-1  ...  PREMIERE INITIALISATION DE :
         !                                MJ , INDICATEUR DE MAILLE
         !                                HGG , HAUTEUR DE PSI A GAUCHE
         !                                PENTE, PENTE DE PSI
         !
         !                               ... SI I EGAL 1
         if( I /= 1 ) then
            !                                     SINON ...
            MJ    = KPIED(I-1)
            HGG   = 0.d0
            PENTE = HPIED(I) / DDT(I)
         else
            !                                     ALORS...
            MJ    = KPIED(I)
            HGG   = HPIED(I)
            PENTE = -HPIED(I) / DDT(I+1)
         end if
         do
            !
            !              3.2-2  ...   INITIALISATION DES VALEURS LOCALES:
            !                           PASX    , PAS D' ESPACE LOCAL
            !
            !                              ... SI LA MAILLE EST INTERIEURE
            if( MJ == 0 .or. MJ == IM ) then
               !                                   ... SINON... EST-CE A GAUCHE ?
               if( MJ == IM ) then
                  !                                        NON , ALORS ..
                  PASX = -VIT(IM) * DT
               else
                  !                                        OUI , ALORS ..
                  !     LE GOTO 70 EST COMMENTARISE CAR L'INTEGRATION SUR PASX N'EST PAS
                  !     TOUJOURS NEGLIGEABLE MEME SI PASX EST PLUS PETIT QUE EPSX
                  PASX = VIT(1) * DT
               end if
            else
               !                                       ALORS ...
               PASX = X(MJ+1) - X(MJ)
            end if
            !
            !              3.2-3  ...   INITIALISATION DES VALEURS LOCALES:
            !                           ETA1 ETA 2 ETA 3, PARAM DESCRIPTIFS DE PSI
            !                        // TESTONS LES POSITIONS DES TROIS (OU MOINS)
            !                              PIEDS DE PSI PAR RAPPORT A LA MAILLE //
            !
            !                             ... ETA2  PAR TEST SUR PIED DE I ...
            if( MJ < KPIED(I) ) then
               !                                     PIED A DROITE DE LA MAILLE
               ETA2 = 1.d0
            elseif( MJ == KPIED(I) ) then
               !                                     PIED DANS LA MAILLE
               ETA2 = XPIED(I) / PASX
            else
               !                                     PIED A GAUCHE DE LA MAILLE
               ETA2 = 0.d0
            end if
            !
            !                             ... ETA1  PAR TEST SUR PIED DE I-1 ...
            !                                   ..SI I EGAL 1
            if( I /= 1 ) then
               !                                         SINON ..
               !                                         ...  SI ON EST DS KPIED(I-1)
               ETA1 = XPIED(I-1) / PASX
               !                                         ...  SINON ...
               if( MJ /= KPIED(I-1) ) then
                  ETA1 = 0.d0
               end if
            else
               !                                         ALORS ..
               ETA1 = ETA2
            end if
            !
            !                             ... ETA3  PAR TEST SUR PIED DE I+1 ...
            !                                   ..SI I EGAL IM
            if( I /= IM ) then
            !                                         SINON ..
               if( MJ < KPIED(I+1) ) then
                  !                                     PIED A DROITE DE LA MAILLE
                  ETA3 = 1.d0
               elseif( MJ == KPIED(I+1) ) then
                  !                                     PIED DANS LA MAILLE
                  ETA3 = XPIED(I+1) / PASX
               else
                  !                                     PIED A GAUCHE DE LA MAILLE
                  ETA3 = 0.d0
               end if
            else
               !                                         ALORS ..
               ETA3 = ETA2
            end if
            !              3.2-4  ...   INITIALISATION DES VALEURS LOCALES:
            !                           HGD,HDG,HDD AUXILIAIRES DE HAUTEUR DE PSI
            !
            HDG = HGG + ( ETA2 - ETA1 ) * PASX * PENTE
            HGD = HDG
            !                     // Y A-T-IL CHANGEMENT DE PENTE ? //
            !                           ... SI MJ EGAL KPIED(I) (I.E. ETA3 =/= ETA2)
            !if( ETA3 /= ETA2 ) then
            if( DABS(ETA3-ETA2).GT.EPS15 ) then
            !                               ALORS ...
               PENTE = - HPIED(I) / DDT(I+1)
            end if
            HDD = HGD + ( ETA3 - ETA2 ) * PASX * PENTE
            !
            !        3.3   INTEGRATION PAS A PAS . FORMULE DE SIMPSON
            !
            !              CALCUL DE L'INTEGRALE PSIN*FN (CLASSIQUE)
            !                     DE L'INTEGRALE PSIN*PRAD*G*DT/2. (PENTE DU CANAL)
            !                  ET DE L'INTEGRALE PSIN*FROT*G*DT/2. (FROTTEMENT)
            !
            !           POUR TOUTE FONCTION PSI CONVECTEE
            !
            !
            !              3.3-1  ...   INITIALISATION DES VALEURS LOCALES:
            !                           FG & FD , VALEURS DE PSI A GAUCHE & DROITE
            !
            !                              ... SI LA MAILLE EST INTERIEURE
            if( MJ == 0 .or. MJ == IM ) then
               !                                   ... SINON... EST-CE A GAUCHE ?
               if( MJ == IM ) then
                  !                                        NON , ALORS ..
                  FG   = FN(IM)
                  FD   = CD
                  PG   = 0.d0
                  PD   = 0.d0
                  FROG = 0.d0
                  FROD = 0.d0
               else
                  !                                        OUI , ALORS ..
                  FG   = CG
                  FD   = FN(1)
                  PG   = 0.d0
                  PD   = 0.d0
                  FROG = 0.d0
                  FROD = 0.d0
               end if
            else
               !                                       ALORS ...
               FG   = FN(MJ)
               FD   = FN(MJ+1)
               PG   = PRAD(MJ)
               PD   = PRAD(MJ+1)
               FROG = -FROT(MJ)
               FROD = -FROT(MJ+1)
            end if
            !
            !              3.3-2  INTEGRATION A GAUCHE
            !
            !              CALCUL DE L'INTEGRALE PSIN*FN
            !
            Z = FG * ( ( 1.d0 - ETA1 ) * HGG + ( 2.d0 - ETA1 - ETA2 ) * ( HGG + HDG ) + ( 1.d0 - ETA2 ) * HDG ) + &
                FD * ( ETA1 * HGG + ( ETA1 + ETA2 ) * ( HGG + HDG ) + ETA2 * HDG )
            !
            Z = Z * ( ETA2 - ETA1 ) * PASX
            !
            !                     DE L'INTEGRALE PSIN*PRAD*G*DT/2.
            !
            ZP = PG * ( ( 1.d0 - ETA1 ) * HGG + ( 2.d0 - ETA1 - ETA2 ) * ( HGG + HDG ) + ( 1.d0 - ETA2 ) * HDG ) + &
                 PD * ( ETA1 * HGG + ( ETA1 + ETA2 ) * ( HGG + HDG ) + ETA2 * HDG )
            !
            ZP = ZP * ( ETA2 - ETA1 ) * PASX * G * DT / 2.d0
            !
            !                  ET DE L'INTEGRALE PSIN*FROT*G*DT/2.
            !
            ZF = FROG * ( ( 1.d0 - ETA1 ) * HGG + ( 2.d0 - ETA1 - ETA2 ) * ( HGG + HDG ) + ( 1.d0 - ETA2 ) * HDG ) + &
                 FROD * ( ETA1 * HGG + ( ETA1 + ETA2 ) * ( HGG + HDG ) + ETA2 * HDG )
            !
            ZF = ZF * ( ETA2 - ETA1) * PASX * G * DT / 2.d0
            !
            !                 // SOMMATION DANS FNP1(I) DU TERME DE GAUCHE //
            !
            FNP1(I) = FNP1(I) + Z + ZP + ZF
            !
            !              3.3-2  INTEGRATION A DROITE
            !
            !              CALCUL DE L'INTEGRALE PSIN*FN
            !
            Z = FG * ( ( 1.d0 - ETA2 ) * HGD + ( 2.d0 - ETA2 - ETA3 ) * ( HGD + HDD ) + ( 1.d0 - ETA3 ) * HDD ) + &
                FD * ( ETA2 * HGD + ( ETA2 + ETA3 ) * ( HGD + HDD ) + ETA3 * HDD )
            !
            Z = Z * ( ETA3 - ETA2 ) * PASX
            !                     DE L'INTEGRALE PSIN*PRAD*G*DT/2.
            !
            ZP = PG * ( ( 1.d0 - ETA2 ) * HGD + ( 2.d0 - ETA2 - ETA3 ) * ( HGD + HDD ) + ( 1.d0 - ETA3 ) * HDD ) + &
                 PD * ( ETA2 * HGD + ( ETA2 + ETA3 ) * ( HGD + HDD ) + ETA3 * HDD )
            !
            ZP = ZP * ( ETA3 - ETA2 ) * PASX * G * DT / 2.d0
            !
            !
            !                  ET DE L'INTEGRALE PSIN*FROT*G*DT/2.
            !
            ZF = FROG * ( ( 1.d0 - ETA2 ) * HGD + ( 2.d0 - ETA2 - ETA3 ) * ( HGD + HDD ) + ( 1.d0 - ETA3 ) * HDD ) + &
                 FROD * ( ETA2 * HGD + ( ETA2 + ETA3 ) * ( HGD + HDD ) + ETA3 * HDD )
            !
            ZF = ZF * ( ETA3 - ETA2 ) * PASX * G * DT / 2.d0
            !
            !                 // SOMMATION DANS FNP1(I) DU TERME DE DROITE //
            !
            FNP1(I) = FNP1(I) + Z + ZP + ZF
            !
            !
            !       3.3-3   PASSAGE A LA MAILLE SUIVANTE
            !                                  OU ARRET INTEGRATION SUR PSI
            !
            !                 // ON PASSE A LA MAILLE SUIVANTE //
            MJ  = MJ + 1
            HGG = HDD
            !                 // L' INTEGRATION SUR PSI EST ELLE FINIE ? //
            !                    ... SI LA MAILLE A DROITE DE LA FIN DE PSI ATTEINTE
            !                              ALORS , ON PASSE A LA FONCTION SUIVANTE
            if( I == IM ) then
               KFIN = KPIED(I) + 1
            end if
            if( I /= IM ) then
               KFIN = KPIED(I+1) + 1
            end if
            !                           ... SINON  , ON CONTINUE L' INTEGRATION
            if( MJ == KFIN .or. MJ == IMP1 ) exit
         end do
         !
      end do
      !
      !      3.4 CALCUL DE L'INTEGRALE DE PSINP1*PRAD*G*DT/2.
      !              ET DE L'INTEGRALE DE PSINP1*FROT*G*DT/2.
      !
      !
      FNP1(1) = FNP1(1) + ( X(2) - X(1) ) * ( 2.d0 * PRAD(1) + PRAD(2) ) * G * DT / 2.d0 - &
                ( X(2) - X(1) ) * ( 2.d0 * FROT(1) + FROT(2) ) * G * DT / 2.d0
      !
      do I = 2 , IM-1
         FNP1(I) = FNP1(I) + ( X(I) - X(I-1) ) * ( 2.d0 * PRAD(I) + PRAD(I-1) ) * G * DT / 2.d0 + &
                   ( X(I+1) - X(I) ) * ( 2.d0 * PRAD(I) + PRAD(I+1) ) * G * DT / 2.d0 - &
                   ( X(I) - X(I-1) ) * ( 2.d0 * FROT(I) + FROT(I-1) ) * G * DT / 2.d0 - &
                   ( X(I+1) - X(I) ) * ( 2.d0 * FROT(I) + FROT(I+1) ) * G * DT / 2.d0
      end do
      !
      FNP1(IM) = FNP1(IM) + ( X(IM) - X(IM-1) ) * ( 2.d0 * PRAD(IM) + PRAD(IM-1) ) * G * DT / 2.d0 - &
                 ( X(IM) - X(IM-1) ) * ( 2.d0 * FROT(IM) + FROT(IM-1) ) * G * DT / 2.d0
      !
      !
      !       3.5  CALCUL DES POINTS DE DIRICHLET S'IL Y'A LIEU
      !
      !
      !            POINT DIRICHLET A L'AMONT
      if( DABS(C(1)) <= 1.d-10) then
         FNP1(1) = CG
      end if
      !            POINT DIRICHLET A L'AVAL
      if( DABS(A(IM)) <= 1.d-10) then
         FNP1(IM) = CD
      end if
      !
      !
      !       3.6  RESOLUTION
      !
      !
      NF = 1

      call BISSND_tracer( FNP1 , A , B , C , IM , NF , ERREUR )

   end if

   return


end subroutine HYP1FA
