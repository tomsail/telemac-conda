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

subroutine CALCUL( &
                    ! Resultats
                    Z , & ! Cote
                    Q , & ! Debit
              Matrice , & ! Matrice du probleme a resoudre
                    ! Donnees non modifiees
               P1, P2 , & ! Perimetres mouillees
           B1, B2, BS , & ! Largeurs au miroir
               S1, S2 , & ! Sections mouillees
               R1, R2 , & ! Rayons hydrauliaues
         DPDZ1, DPDZ2 , & ! Gradients du perimetre/Z
                    X , & ! Maillage
             CF1, CF2 , & ! Coefficients de frottement
               QINJEC , & ! Debits d'apport
              Connect , & ! Table de connectivite
    RDLIM,SDLIM,TDLIM , & ! Conditions aux limites
               PCSing , & ! Pertes de charges singulieres
            ModeleLit , & ! Modele du lit (Debord/Crugos)
                   DT , & ! Pas de temps
            NumeroPas , & ! numero du pas
           Impression , & ! Flag d'impression
         UniteListing , & ! Unite logique listing
        LoiFrottement , & ! Loi de frottement
          ASING,BSING , & ! Coefficients de l'equation discretisee d'une singularites
          CSING,DSING , &
          OptionCasier, & ! Presence de casiers
          Aliai,Bliai , & ! Coefficients de l'equation discretisee d'une liaison
          Cliai,Dliai , &
          Qliai       , &
          ApportPluie , &
          Liaison     , &
          Casier      , &
         NoConvection , & ! Attenuation de la convection
                 CQMV , & ! qmv des debits d'apports
               Erreur   & ! Erreur
                      )

! *********************************************************************
! PROGICIEL : MASCARET        F. ZAOUI
!                             S. DELMAS      C. COULET
!
! VERSION : V8P4R0               EDF-CEREMA-ARTELIA
! *********************************************************************
! - FONCTION :
!   - CALCUL DU COUPLE SOLUTION (DZi,DQi) POUR CHAQUE SECTION i
!   - MISE A JOUR DE LA LIGNE D'EAU EN CONSEQUENCE
!
!   PROGRAMME APPELANT : Rezo
!
!   COMMENTAIRES :
!      Appel au solveur direct Y12M pour le calcul de la solution de variation de cote et de debit
!         auteur Y12M : Z. ZLATEV       http://www.netlib.org/y12m
!           - Z. Zlatev: "Computational methods for general sparse matrices". KLUWER Academic Publishers, Dordrecht-Boston-London, 1991
!           - Z. Zlatev, J. Wasniewski and K. Schaumburg: "Y12M - solution of large and sparse linear algbraic equations". Springer-Verlag, Berlin-Heidelberg, New York, 1981
!           - O. Østerby and Z. Zlatev: "Direct methods for sparse matrices". Springer-Verlag, Berlin-Heidelberg-New York, 1983
!
   use M_PRECISION
   use M_CONSTANTES_CALCUL_C  ! Constantes parametres de calcul (TETA)
   use M_PARAMETRE_C          ! Parametres de calcul
   use M_MESSAGE_C            ! Liste des messages d'erreur
   use M_ERREUR_T             ! Definition du type ERREUR_T
   use M_CONNECT_T            ! Definition du type CONNECT_T
   use M_TRAITER_ERREUR_I     ! Traitement des erreurs
   use M_CONNECT_T            ! Type CONNECT_T
   use M_DEBITANCE_S          ! Calcul des coefficients de Striclkler
   use M_TRAITER_ERREUR_I     ! Traitement des erreurs
   use M_REZOMAT_T            ! Definition du type REZOMAT_T
   use M_APPORT_PLUIE_T
   use M_LIAISON_T            ! Definition du type LIAISON_T
   use M_CASIER_T             ! Definition du type CASIER_T
   use M_TRAITER_ERREUR_I     ! Traitement des erreurs
   use M_SURVOL_I             ! Interafce du sous-programme SURVOL
   use M_BILVOL_I             ! Interafce du sous-programme BILVOL
   use M_CONSTANTES_CASIER_C  ! Constantes casiers

   implicit none

   !
   ! Arguments
   !
   real(DOUBLE) , dimension(:) , intent(out)   :: Z, Q
   real(DOUBLE) , dimension(:) , intent(in)    :: P1, P2
   real(DOUBLE) , dimension(:) , intent(in)    :: B1, B2, BS
   real(DOUBLE) , dimension(:) , intent(in)    :: R1, R2
   real(DOUBLE) , dimension(:) , intent(in)    :: S1, S2
   real(DOUBLE) , dimension(:) , intent(in)    :: DPDZ1, DPDZ2
   real(DOUBLE) , dimension(:) , intent(in)    :: QINJEC
   real(DOUBLE) , dimension(:) , intent(in)    :: X                   ! Maillage
   real(DOUBLE) , dimension(:) , intent(in)    :: CF1, CF2
   real(DOUBLE) , dimension(:) , intent(in)    :: PCSing              ! Pertes de charge singulieres
   real(DOUBLE) , dimension(:) , intent(in)    :: RDLIM, SDLIM, TDLIM ! Conditions aux limites
   real(DOUBLE)                , intent(in)    :: DT
   real(DOUBLE),  dimension(:),  intent(in)    :: ASING, BSING, CSING, DSING
   real(DOUBLE),  dimension(:),  intent(in)    :: Aliai, Bliai, Cliai, Dliai, Qliai
   logical                     , intent(in)    :: Impression
   logical                     , intent(in   ) :: OptionCasier
   logical                     , intent(in)    :: NoConvection
   integer                     , intent(in)    :: UniteListing
   integer                     , intent(in)    :: LoiFrottement
   integer                     , intent(in)    :: NumeroPas
   integer                     , intent(in)    :: ModeleLit
   integer                     , intent(in)    :: CQMV
   type(ERREUR_T)              , intent(inout) :: Erreur
   type(CONNECT_T)             , intent(in)    :: Connect             ! Table de connectivite du reseau
   type(REZOMAT_T)             , intent(inout) :: Matrice             ! Matrice du reseau
   type(LIAISON_T), dimension(:), pointer, intent(inout) :: Liaison
   type(CASIER_T) , dimension(:), pointer, intent(inout) :: Casier
   type(APPORT_PLUIE_T), dimension(:), pointer, intent(inout) :: ApportPluie
   !
   ! Variables locales ..
   !
   integer i,j,k,jj,f,iliaison                    ! Indices de boucle
   integer kl,ku                                  ! Demi-largeurs de bande
   integer nval,nrow                              ! Indices de remplissage
   integer ori,ext                                ! Origine et extremite des biefs
   integer nbBief                                 ! Nombre de biefs
   integer nbExtLib                               ! Nombre d'extremites libres
   integer nbConflu                               ! Nombre de confluents
   integer numseuil                               ! Le numero de la singularite
   real(DOUBLE) deb_temp                          ! Valeur de debitance temporaire
   real(DOUBLE) un                                ! -1 ou +1 suivant le cas
   real(DOUBLE), dimension(size(X)) :: st1, st2   ! Coefficients de Strickler min et maj
   real(DOUBLE) :: ALPH1I, ALPH1J, ALPH2I, ALPH2J
   real(DOUBLE) :: B1I, B1J, B2I, B2J
   real(DOUBLE) :: BBI, BBJ
   real(DOUBLE) :: FP1I, FP1J, FP2I, FP2J
   real(DOUBLE) :: JS
   real(DOUBLE) :: P1I, P1J, P2I, P2J, PX, R1I, R1J, R2I, R2J, S1I, S1J, S2I, S2J
   real(DOUBLE) :: ST1I, ST1J, ST2I, ST2J
   real(DOUBLE) :: VI, VJ, WAI, WAJ, WAOI, WAOJ
   real(DOUBLE) :: WB1, WB2, WB3
   real(DOUBLE) :: WBETAI, WBETAJ
   real(DOUBLE) :: WBI, WBJ, WC1, WC4, WC5, WD1, WD4, WD5, WDI, WDJ
   real(DOUBLE) :: WE2, WE3
   real(DOUBLE) :: WETAI, WETAJ
   real(DOUBLE) :: WF1, WF2, WF3, WF4, WF5
   real(DOUBLE) :: WH1, WH4, WH5
   real(DOUBLE) :: WQ, WQ1, WQ2, WQ3, WQ4, WQ5
   real(DOUBLE) :: WRI, WRJ, WSI, WSJ
   real(DOUBLE) :: WST1I, WST1J, WST2I, WST2J
   real(DOUBLE) :: AG,AH,AI,AJ,AK,AL,AM,AN,AO,AP
   real(DOUBLE) :: CQMVI                 ! Constante pour la prise en compte du terme Quantite de Mouvement pour les apports
   real(DOUBLE) :: HI,HJ,FRI,FRJ,FR,CFR  ! Pour l'attenuation de la convection
   real(DOUBLE) , parameter :: theta = 1.D0  ! Coefficient d'implicitation des l'equation de continuite des casiers. 1.0 => Full Implicite

   !
   ! Initialisations
   !
   Erreur%Numero = 0
   nval          = 0
   nrow          = 0
   nbBief        = size( Connect%ORIGINEBIEF )
   nbExtLib      = size( Connect%NUMSECTIONEXTLIBRE )
   nbConflu      = size( Connect%NBBIEFCONFLUENCE )

   !
   ! Calcul des coefficients de Strickler
   !
   PASS0 : do i = 1 , size(X)
      call DEBITANCE_S( deb_temp , st1(i) , R1(i) , S1(i) , LoiFrottement , CF1(i) , Erreur )
      if( Erreur%Numero /= 0 ) return
      call DEBITANCE_S( deb_temp , st2(i) , R2(i) , S2(i) , LoiFrottement , CF2(i) , Erreur )
      if( Erreur%Numero /= 0 ) return
   end do PASS0

   !
   ! prise en compte de l'apport de debit dans la qte de mvt
   !
   if( CQMV.EQ.0 ) then
      CQMVI = 0.D0
   else
      CQMVI = 1.D0
   endif

   !
   ! Resolution d'un pas de temps : boucle sur les biefs du reseau
   !
   PASS1 : do f = 1 , nbBief

      ori = Connect%ORIGINEBIEF(f)
      ext = Connect%FINBIEF(f)

      PASS2 : do j = ori , ext - 1

         i    = j + 1
         nrow = nrow + 1

         !
         ! Singularite ou Casier present : Saint-Venant fluvial n'est plus valable
         !
         if( Matrice%SecSin(j).gt.0 ) then
            numseuil = Matrice%SecSin(j)
            ! Equation de continuite --> egalite des debits amont et aval de la singularite
            AG = 1._DOUBLE
            AH = 0._DOUBLE
            AI = 1._DOUBLE
            AJ = 0._DOUBLE
            AK = Q(j) - Q(i)
            ! Equation dynamique --> f(Q,Zam,Zav) = 0 specifique a chaque singularite
            ! cas 1 : B <> 0 et c <> 0  (seuil noye)
            if( dabs(BSING(numseuil)).gt.EPS6.and.dabs(CSING(numseuil)).gt.EPS6 ) then
               AL = 0._DOUBLE
               AM = 1._DOUBLE
               if( Q(j).gt.0._DOUBLE ) then ! debit positif
                  AN = -ASING(numseuil) / CSING(numseuil)
                  AO = -BSING(numseuil) / CSING(numseuil)
                  AP = DSING(numseuil) / CSING(numseuil)
               elseif( Q(j).lt.0._DOUBLE) then ! debit negatif
                  AN = -ASING(numseuil) / BSING(numseuil)
                  AO = -CSING(numseuil) / BSING(numseuil)
                  AP = DSING(numseuil) / BSING(numseuil)
               endif
            ! cas 2 : B <> 0 et C = 0 (seuil denoye)
            elseif( dabs(BSING(numseuil)).gt.EPS6 ) then
               if( Q(j).gt.0._DOUBLE ) then ! debit positif
                  AO = -1._DOUBLE
                  AN = 0._DOUBLE
                  AL = ASING(numseuil) / BSING(numseuil)
                  AM = 0._DOUBLE
                  AP = DSING(numseuil) / BSING(numseuil)
               elseif( Q(j).lt.0._DOUBLE) then ! debit negatif
                  AL = 0._DOUBLE
                  AM = 1._DOUBLE
                  AN = -ASING(numseuil) / BSING(numseuil)
                  AO = 0._DOUBLE
                  AP = DSING(numseuil) / BSING(numseuil)
               endif
            ! cas 3 : B = 0 et C <> 0 (type F(Q,Zaval)
            elseif( dabs(CSING(numseuil)).gt.EPS6 ) then
               if( Q(j).gt.0._DOUBLE ) then ! debit positif
                  AL = 0._DOUBLE
                  AM = 1._DOUBLE
                  AN = -ASING(numseuil) / CSING(numseuil)
                  AO = 0._DOUBLE
                  AP = DSING(numseuil) / CSING(numseuil)
               elseif( Q(j).lt.0._DOUBLE) then ! debit negatif
                  AO = -1._DOUBLE
                  AN = 0._DOUBLE
                  AL = ASING(numseuil) / CSING(numseuil)
                  AM = 0._DOUBLE
                  AP = DSING(numseuil) / CSING(numseuil)
               endif
            ! cas 4 : B = 0 et C = 0
            else
               if( Q(j).gt.0._DOUBLE ) then ! debit positif
                  AL = 0._DOUBLE
                  AM = 1._DOUBLE
                  AN = 0._DOUBLE
                  AO = 0._DOUBLE
                  AP = 0._DOUBLE
               elseif( Q(j).lt.0._DOUBLE) then ! debit negatif
                  AO = -1._DOUBLE
                  AN = 0._DOUBLE
                  AL = 0._DOUBLE
                  AM = 0._DOUBLE
                  AP = 0._DOUBLE
               endif
            endif

         elseif ( Matrice%SecLiai(j).gt.0 ) then
            AK = Q(i) - Q(j)
            AP = Z(i) - Z(j)
            do iliaison = 1, size(Matrice%LiaiSec)
                if ( Matrice%LiaiSec(iliaison) == j ) then
                    AK = AK + Qliai(iliaison)
                endif
            enddo

         else

             !
             ! Determination des macro-coefficients de l'equation de continuite : AG,AH,AI,AJ,AK
             !
             PX =  1.D0 / (X(I) - X(J))
             B1I  = B1(I)
             B1J  = B1(J)
             B2I  = B2(I)
             B2J  = B2(J)
             S1I  = S1(I)
             S1J  = S1(J)
             S2I  = S2(I)
             S2J  = S2(J)
             R1I  = R1(I)
             R1J  = R1(J)
             R2I  = R2(I)
             R2J  = R2(J)
             VI   = Q(I) / (S1(I)+S2(I))
             VJ   = Q(J) / (S1(J)+S2(J))
             ST1I = ST1(I)
             ST1J = ST1(J)
             ST2I = ST2(I)
             ST2J = ST2(J)

             if( ModeleLit == MODELE_LIT_FOND_BERGE ) then

                FP1I = P1(I) / (ST1I**W32)
                FP1J = P1(J) / (ST1J**W32)
                FP2I = P2(I) / (ST2I**W32)
                FP2J = P2(J) / (ST2J**W32)
                B1I  = B1(I) + B2(I)
                S1I  = S1(I) + S2(I)
                P1I  = P1(I) + P2(I)
                R1I  = S1I / P1I
                ST1I = ((P1(I)+P2(I))/(FP1I+FP2I))**W23
                B1J  = B1(J) + B2(J)
                S1J  = S1(J) + S2(J)
                P1J  = P1(J) + P2(J)
                R1J  = S1J / P1J
                ST1J = ((P1(J)+P2(J))/(FP1J+FP2J))**W23
                B2I  = 0._DOUBLE
                S2I  = 0._DOUBLE
                P2I  = 0._DOUBLE
                ST2I = ST1I
                B2J  = 0._DOUBLE
                S2J  = 0._DOUBLE
                P2J  = 0._DOUBLE
                ST2J = ST1J

             end if

             BBI = B1I + B2I + BS(I)
             BBJ = B1J + B2J + BS(J)

             AG = TETA
             AH = (( BBI + BBJ ) / (4._DOUBLE * DT * PX))
             AI = AG
             AJ = -AH
             AK = Q(J) - Q(I) + QINJEC(I)

             !
             ! Determination des macro-coefficients de l'equation de quantite de mouvement : AL,AM,AN,AO,AP
             !

             WB1   = ( SIGN( Q(I)*Q(I) , Q(I)) + SIGN(Q(J)*Q(J) , Q(J) ) ) * 0.5D0
             WB2   = TETA * DABS(Q(I) )
             WB3   = TETA * DABS(Q(J) )

             if( ModeleLit == MODELE_LIT_DEBORD ) then

                WAOI = 0.9_DOUBLE * ( ST2I / ST1I )**W16
                WAOJ = 0.9_DOUBLE * ( ST2J / ST1J )**W16
                WRI = R2I / R1I
                WRJ = R2J / R1J
                WAI = ( 1._DOUBLE - WAOI ) * 0.5D0 * DCOS( PI * WRI / 0.3_DOUBLE ) + ( 1._DOUBLE + WAOI ) * 0.5D0
                if( WRI > 0.3_DOUBLE ) then
                   WAI = WAOI
                end if
                WAJ = ( 1._DOUBLE - WAOJ ) * 0.5D0 * DCOS( PI * WRJ / 0.3_DOUBLE ) + ( 1._DOUBLE + WAOJ ) * 0.5D0
                if( WRJ > 0.3_DOUBLE ) then
                   WAJ = WAOJ
                end if
                WST1I = ST1I * WAI
                WST1J = ST1J * WAJ
                WST2I = ST2I
                WST2J = ST2J
                if( S2I > 0._DOUBLE ) then
                   WST2I = ST2I * DSQRT( 1._DOUBLE + S1I / S2I * ( 1._DOUBLE - WAI*WAI ) )
                end if
                if( S2J > 0._DOUBLE ) then
                   WST2J = ST2J * DSQRT( 1._DOUBLE + S1J / S2J * ( 1._DOUBLE - WAJ*WAJ ) )
                end if
             else

                WST1I = ST1I
                WST1J = ST1J
                WST2I = ST2I
                WST2J = ST2J
                if( S2I.LT.EPS6 ) then
                   WST2I = ST1I
                end if
                if( S2J.LT.EPS6 ) then
                   WST2J = ST1J
                end if
             end if

             WDI    = WST1I * S1I * R1I**W23 + WST2I * S2I * R2I**W23
             WDJ    = WST1J * S1J * R1J**W23 + WST2J * S2J * R2J**W23
             ALPH1I = W13 * WST1I * R1I**W23 * ( 5._DOUBLE * B1I - 2._DOUBLE * R1I * DPDZ1(I) )
             ALPH1J = W13 * WST1J * R1J**W23 * ( 5._DOUBLE * B1J - 2._DOUBLE * R1J * DPDZ1(J) )
             ALPH2I = W13 * WST2I * R2I**W23 * ( 5._DOUBLE * B2I - 2._DOUBLE * R2I * DPDZ2(I) )
             ALPH2J = W13 * WST2J * R2J**W23 * ( 5._DOUBLE * B2J - 2._DOUBLE * R2J * DPDZ2(J) )
             WC1    = ( WDI*WDI + WDJ*WDJ ) * 0.5D0
             WC4    = TETA * WDI * ( ALPH1I + ALPH2I )
             WC5    = TETA * WDJ * ( ALPH1J + ALPH2J )
             WE2    = 0.5_DOUBLE / DT
             WE3    = WE2
             WBI    = B1I + B2I
             WBJ    = B1J + B2J
             WSI    = S1I + S2I
             WSJ    = S1J + S2J
             WBETAI = 1._DOUBLE
             if( S2I.GT.EPS6 ) then
                WETAI  = WST1I / WST2I * S1I / S2I * ( R1I / R2I )**W23
                WBETAI = ( WETAI*WETAI / S1I + 1._DOUBLE / S2I ) * WSI / (( 1._DOUBLE + WETAI )*( 1._DOUBLE + WETAI ))
             end if
             WBETAJ = 1._DOUBLE
             if( S2J.GT.EPS6 ) then
                WETAJ  = WST1J / WST2J * S1J / S2J * ( R1J / R2J )**W23
                WBETAJ = ( WETAJ*WETAJ / S1J + 1._DOUBLE / S2J ) * WSJ / (( 1._DOUBLE + WETAJ )*( 1._DOUBLE + WETAJ ))
             end if
             WF1 = ( WBETAI * VI * Q(I) - WBETAJ * VJ * Q(J) ) * PX
             WF2 = 2._DOUBLE * TETA * PX * WBETAI * VI
             WF3 = -2._DOUBLE * TETA * PX * WBETAJ * VJ
             WF4 = -TETA * PX * WBETAI * WBI * VI*VI
             WF5 = TETA * PX * WBETAJ * WBJ * VJ*VJ

             if( CQMVI > 0._DOUBLE ) then ! prise en compte de la quantite de mouvement pour les apports
                WQ = -CQMVI * ( QINJEC(I) * PX ) * 0.5D0
                WQ1 = WQ * ( VI + VJ )
                WQ2 = WQ / WSI
                WQ3 = WQ / WSJ
                WQ4 = -WQ * ( Q(I) * WBI ) / ( WSI*WSI )
                WQ5 = -WQ * ( Q(J) * WBJ ) / ( WSJ*WSJ )
                WF1 = WF1 + WQ1
                WF2 = WF2 + WQ2
                WF3 = WF3 + WQ3
                WF4 = WF4 + WQ4
                WF5 = WF5 + WQ5
             end if

             WH1 = ( 1._DOUBLE / WSI + 1._DOUBLE / WSJ ) / (2._DOUBLE * GPES)
             WH4 = -TETA * WBI / (2._DOUBLE * GPES) / (WSI*WSI)
             WH5 = -TETA * WBJ / (2._DOUBLE * GPES) / (WSJ*WSJ)
             JS  = 0._DOUBLE
             if( WBETAJ * VJ > WBETAI * VI .and. abs(PCSing(I)).LT.EPS6 ) then
                JS = 0.3_DOUBLE * ( WBETAJ * VJ - WBETAI * VI )*( WBETAJ * VJ - WBETAI * VI ) / (2._DOUBLE * GPES)
             end if
             if( VJ > 0._DOUBLE ) then
                JS = JS + PCSing(I) * WBETAJ * VJ*VJ / (2._DOUBLE * GPES)
             else
                JS = JS + PCSing(I) * WBETAI * VI*VI / (2._DOUBLE * GPES)
             end if
             JS = JS * PX
             if( VI < 0._DOUBLE ) then
                JS = -JS
             end if
             WD1 = ( Z(I)-Z(J) ) * PX + JS
             WD4 = TETA * PX
             WD5 = -WD4

             ! Debut attenuation de la convection
             if(NoConvection) then
                ! Calcul d'un froude moyen sur 2 sections
                if( B1(j).GT.EPS6 ) then
                   HJ = ( S1(j)+S2(j) ) / ( B1(j)+B2(j) )
                else
                   HJ = EPS3
                endif
                FRJ = ( WBETAJ*VJ ) / DSQRT( (WBETAJ-W1) * WBETAJ * VJ*VJ + GPES*HJ )

                if(B1(i).GT.EPS6) then
                   HI = ( S1(i)+S2(i) ) / ( B1(i)+B2(i) )
                else
                   HI = EPS3
                endif
                FRI = ( WBETAI*VI ) / DSQRT( (WBETAI-W1) * WBETAI * VI*VI + GPES*HI )
                FR  = (FRI+FRJ) * 0.5D0

                ! Coefficient de relaxation : formule heuristique (RME)
                CFR = DMAX1(0._DOUBLE,1._DOUBLE-FR*FR)

                ! Relaxation des termes
                WF1 = WF1 * CFR
                WF2 = WF2 * CFR
                WF3 = WF3 * CFR
                WF4 = WF4 * CFR
                WF5 = WF5 * CFR
             endif

             AL = WB2 + WC1 * WH1 * ( WE2 + WF2 )
             AM = WC1 * ( WD4 + WH1 * WF4 + WH4 * WF1 ) + WC4 * ( WD1 + WH1 * WF1 )
             AN = -WB3 - WC1 * WH1 * ( WE3 + WF3 )
             AO = -WC1 * ( WD5 + WH1 * WF5 + WH5 * WF1 ) - WC5 * ( WD1 + WH1 * WF1 )
             AP = -WB1 - WC1 * ( WD1 + WH1 * WF1 )

         endif
         !
         ! Conditions Limites !
         !
         ! K et P !
         Matrice%b(nrow)   = AK
         Matrice%b(nrow+1) = AP
         if( Matrice%typSec(j).lt.0.and.Matrice%typSec(j).ne.-10 ) then ! amont
            do k = 1,nbExtLib
               if( Connect%NumSectionExtLibre(k).eq.j ) exit
            enddo
            if( Matrice%typSec(j).eq.-1) then ! debit impose
               Matrice%b(nrow)   = Matrice%b(nrow) + AI * TDLIM(k)
               Matrice%b(nrow+1) = Matrice%b(nrow+1) + AN * TDLIM(k)
               Q(j)              = Q(j) + TDLIM(k)
            elseif ( Matrice%typSec(j).eq.-2.or.Matrice%typSec(j).eq.-5 ) then ! cote imposee (et loi normale)
               Matrice%b(nrow)   = Matrice%b(nrow) + AJ * TDLIM(k)
               Matrice%b(nrow+1) = Matrice%b(nrow+1) + AO * TDLIM(k)
               Z(j)              = Z(j) + TDLIM(k)
            else ! autres lois type courbe de tarage
               AI                = AI - AJ * RDLIM(k) / SDLIM(k)
               AN                = AN - AO * RDLIM(k) / SDLIM(k)
            endif
         endif
         if( Matrice%typSec(i).lt.0.and.Matrice%typSec(i).ne.-10 ) then ! aval
            do k = 1,nbExtLib
               if( Connect%NumSectionExtLibre(k).eq.i ) exit
            enddo
            if( Matrice%typSec(i).eq.-1) then ! debit impose
               Matrice%b(nrow)   = Matrice%b(nrow) - AG * TDLIM(k)
               Matrice%b(nrow+1) = Matrice%b(nrow+1) - AL * TDLIM(k)
               Q(i)              = Q(i) + TDLIM(k)
            elseif ( Matrice%typSec(i).eq.-2.or.Matrice%typSec(i).eq.-5) then ! cote imposee (et loi normale)
               Matrice%b(nrow)   = Matrice%b(nrow) - AH * TDLIM(k)
               Matrice%b(nrow+1) = Matrice%b(nrow+1) - AM * TDLIM(k)
               Z(i)              = Z(i) + TDLIM(k)
            else
               AG                = AG - AH * RDLIM(k) / SDLIM(k)
               AL                = AL - AM * RDLIM(k) / SDLIM(k)
            endif
         endif

         !
         ! Remplissage de la matrice !
         !
          if ( Matrice%SecLiai(j) == 0 ) then
             ! I et N !
             if( Matrice%typSec(j).ne.-1 ) then
                nval = nval + 1
                Matrice%valA(nval) = -AI
                nval = nval + 1
                Matrice%valA(nval) = -AN
             endif
             ! J et O !
             if( Matrice%typSec(j).gt.-2 ) then
                nval = nval + 1
                Matrice%valA(nval) = -AJ
                nval = nval + 1
                Matrice%valA(nval) = -AO
             endif
             ! G et L !
             if( Matrice%typSec(i).ne.-1 ) then
                nval = nval + 1
                Matrice%valA(nval) = AG
                nval = nval + 1
                Matrice%valA(nval) = AL
             endif
             ! H et M !
             if( Matrice%typSec(i).gt.-2.or.Matrice%typSec(i).eq.-10 ) then
                nval = nval + 1
                Matrice%valA(nval) = AH
                nval = nval + 1
                Matrice%valA(nval) = AM
             endif

          elseif ( Matrice%SecLiai(j).gt.0 ) then ! La section est reliee a une liaison

            ! Conservation du debit
            nval = nval + 1
            Matrice%valA(nval) = + 1._DOUBLE
            nval = nval + 1
            Matrice%valA(nval) = - 1._DOUBLE
            do iliaison = 1, size(Matrice%LiaiSec)
                if ( Matrice%LiaiSec(iliaison) == j ) then
                    nval               = nval + 1
                    Matrice%ValA(nval) = - 1._DOUBLE
                endif
            enddo

            ! Egalite des cotes
            nval = nval + 1
            Matrice%valA(nval) = + 1._DOUBLE
            nval = nval + 1
            Matrice%valA(nval) = - 1._DOUBLE

         endif

         nrow = nrow + 1

      end do PASS2 ! fin de boucle sur les sections

   end do PASS1 ! fin de boucle sur les biefs

   !
   ! Remplissage des dernieres lignes de la matrice si presence de confluents
   !
   if( nbConflu > 0 ) then

      PASS3 : do k = 1,nbConflu

         ! Une equation pour la repartition conservative des debits
         nrow            = nrow + 1
         jj              = Matrice%headConflu(k)
         Matrice%b(nrow) = 0._DOUBLE

         do while( jj.ne.0)
            un                             = dble(jj / iabs(jj))
            nval                           = nval + 1
            Matrice%valA(nval)             = un
            Matrice%b(nrow)                = Matrice%b(nrow) - un * Q(iabs(jj))
            jj                             = Matrice%nextSecConflu(iabs(jj))
         end do

         ! (nbBief(k)-1) equations pour l'egalite des cotes
         j = iabs(Matrice%headConflu(k))
         i = iabs(Matrice%nextSecConflu(j))
         do while( i.ne.0 )
            nrow                          = nrow + 1
            nval                          = nval + 1
            Matrice%valA(nval)            = 1._DOUBLE
            nval                          = nval + 1
            Matrice%valA(nval)            = -1._DOUBLE
            Matrice%b(nrow)               = -Z(j) + Z(i)
            j                             = i
            i                             = iabs(Matrice%nextSecConflu(i))
         end do

      end do PASS3

   endif

   if( OptionCasier ) then
        !
        ! Liaisons
        !
        do i = 1, size(Matrice%LiaiSec)
            nrow            = nrow + 1
            nval                = nval + 1
            Matrice%valA(nval)  = Aliai(i)
            nval                = nval + 1
            Matrice%valA(nval)  = Bliai(i)
            nval                = nval + 1
            Matrice%valA(nval)  = Cliai(i)
            Matrice%b(nrow)     = Dliai(i)
        enddo

        !
        ! Casiers
        !
        do i = 1, size(Casier)
            nrow                = nrow + 1
            nval                = nval + 1
            Matrice%valA(nval)  = Casier(i)%Surface / Dt
            Matrice%b(nrow)     = 0

            if (size(ApportPluie) /= 0) then        ! Prise en compte des debits d'apports dans les casiers
                do j = 1, size(ApportPluie)
                    if (ApportPluie(j)%Numero == i) then
                        Matrice%b(nrow) = ApportPluie(j)%Debit
                    endif
                enddo
            endif

            do j = 1, size(Casier(i)%LiaisonRC(:,1))
                nval                = nval + 1
                Matrice%valA(nval)  = - theta
                Matrice%b(nrow)     = Matrice%b(nrow) + Qliai( Casier(i)%LiaisonRC(j,1) )
            enddo

            do j = 1, size(Casier(i)%LiaisonCC(:,1))    ! Le signe change en fonction de Camont/Caval
                if ( Liaison( Casier(i)%LiaisonCC(j,1) )%CaracCC%CasierOrigine == i ) then
                    nval                = nval + 1
                    Matrice%valA(nval)  = + theta
                    Matrice%b(nrow)     = Matrice%b(nrow) - Qliai( Casier(i)%LiaisonCC(j,1) )
                else
                    nval                = nval + 1
                    Matrice%valA(nval)  = - theta
                    Matrice%b(nrow)     = Matrice%b(nrow) + Qliai( Casier(i)%LiaisonCC(j,1) )
                endif
            enddo
        enddo
   endif

   !
   ! Resolution du systeme lineaire
   !
   if(Matrice%SOLV.eq.2) then ! => Y12M

      Matrice%snr(1:Matrice%NNZ) = Matrice%colA(1:Matrice%NNZ)
      Matrice%rnr(1:Matrice%NNZ) = Matrice%rowA(1:Matrice%NNZ)

      call Y12MAF( Matrice%N , Matrice%NNZ , Matrice%valA , Matrice%snr , Matrice%NN , &
                   Matrice%rnr , Matrice%NN1 , Matrice%pivot , Matrice%ha , Matrice%IHA , &
                   Matrice%AFLAG , Matrice%IFLAG, Matrice%b , Matrice%IFAIL )
      if( Matrice%IFAIL.ne.0 ) then
         Erreur%Numero = Matrice%IFAIL
         Erreur%Message = "Error with the linear solver Y12M"
         return
      endif

      if( OptionCasier.eqv..false. ) then
        Matrice%iflag(4) = 2
      endif

   else ! => LAPACK

     KL = Matrice%KL
     KU = Matrice%KU
     Matrice%AB(:,:) = 0._DOUBLE

     do i = 1,Matrice%NNZ
        Matrice%AB(KL+KU+1+Matrice%rowA(i)-Matrice%colA(i),Matrice%colA(i)) = Matrice%valA(i)
     end do

     call DGBSV(Matrice%N,KL,KU,1,Matrice%AB,Matrice%LDAB,Matrice%ipiv,Matrice%b,Matrice%N,Matrice%IFAIL)
     if( Matrice%IFAIL.ne.0 ) then
         Erreur%Numero = Matrice%IFAIL
         Erreur%Message = "Error with the linear solver DGBSV"
         return
     endif

   endif

   !
   ! Mise a jour de la solution sur la riviere
   !
   do I = 1 , size(X)
      if( Matrice%noVarDZ(I).gt.0 ) Z(I) = Z(I) + Matrice%b(Matrice%noVarDZ(I))
      if( Matrice%noVarDQ(I).gt.0 ) Q(I) = Q(I) + Matrice%b(Matrice%noVarDQ(I))
      if( Matrice%typSec(i).eq.-3.or.Matrice%typSec(i).eq.-4 ) then
         do k = 1 , nbExtLib
            if( Connect%NumSectionExtLibre(k).eq.i ) exit
         enddo
         Z(I) = Z(I) - RDLIM(k) * Matrice%b(Matrice%noVarDQ(I)) / SDLIM(k)
      endif
   end do

   !
   ! Mise a jour de la solution sur les liaisons et casiers
   !
   if( OptionCasier ) then
       do I = 1, size(liaison)
            Liaison(I)%DebitPrecedent = Liaison(I)%DebitEchange
            Liaison(I)%DebitEchange   = Qliai(I) +  Matrice%b( Matrice%noVarDQl(I) )

            if( abs(Liaison(I)%DebitEchange) .GT.EPS6 ) then
                Liaison(I)%VitesseEchange = Liaison(I)%DebitEchange / ( Liaison(I)%Largeur * &
                                          ( Liaison(I)%CoteMoyenne - Liaison(I)%Cote ) )
            end if
       enddo

       do I = 1, size(casier)
            ! calcul de la cote :
            Casier(I)%Cote = Casier(I)%Cote + Matrice%b( Matrice%noVarDZC(I) )
            ! Surface et volume :
            if( Casier(I)%Cote - Casier(I)%CoteFond < EPS6 ) then
                Casier(I)%Cote = Casier(I)%CoteFond
                Casier(I)%Surface = Casier(I)%Loi_Z_S(1,2)
                Casier(I)%Volume  = 0
            else
                call SURVOL( Casier(I) , I , Erreur )
                if( Erreur%Numero /= 0 ) then
                   return
                end if
            end if

            call BILVOL( Casier(I) , Liaison , dt , Erreur )
            if( Erreur%Numero /= 0 ) then
                return
            end if
       enddo
   endif

   return

end subroutine CALCUL
