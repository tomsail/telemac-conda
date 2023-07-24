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

subroutine BILAN ( &
                   H2OIB       , &
                   H2OTB       , &
                   H2OEB       , &
                   H2OSB       , &
                   H2OIBS      , &
                   H2OTBS      , &
                   CONSB       , &
                   H2OIC       , &
                   H2OTC       , &
                   H2OEC       , &
                   H2OSC       , &
                   CONSC       , &
                   H2OIG       , &
                   H2OTG       , &
                   H2OEG       , &
                   H2OSG       , &
                   H2OIGS      , &
                   H2OTGS      , &
                   CONSG       , &
                   SNODE, VOLS , &
                   QNODE       , &
                   QIN , X     , &
                   Connect     , &
                   NbExtremite , &
                   W , AIRS    , &
                   Confluent   , &
                   IDEB , IFIN , &
                  INDIC,INDICO , &
                   XBAR, ZBAR  , &
                   DT          , &
         ITEMP, Phase_post_imp , &
       NPMAX , T , NP , TMAX   , &
                   NBBIEF      , &
                   NBNOEU      , &
    Impression  , UniteListing , &    ! Unite logique fichier listing
                   CALCOS      , &
                   STOCKAGE    , &
                   Erreur      )      ! Erreur

!***********************************************************************
! PROGICIEL : MASCARET        F. MAUREL
!
! VERSION : V8P4R0              EDF-CEREMA
!***********************************************************************
!
!  FONCTION :
!  --------
!
!       CALCUL DES BILANS DE MASSE DE LIQUIDE,
!       ET DONC DE LA CONSERVATIVITE DANS CHAQUE BIEF ET GLOBALEMENT
!
!-----------------------------------------------------------------------
!
!                             ARGUMENTS
! .___________.____.____.______________________________________________.
! !    NOM    !TYPE!MODE!                   ROLE                       !
! !___________!____!____!______________________________________________!
! !H2OI(B,C,G)!T R !  R ! MASSE D'EAU INITIALE (BIEF,CONFLUENT,GLOBAL) !
! !H2OT(B,S,G)!T R !  R ! MASSE D'EAU TOTALE   (BIEF,CONFLUENT,GLOBAL) !
! !H2OE(B,C,G)!T R !  R ! MASSE D'EAU ENTREE   (BIEF,CONFLUENT,GLOBAL) !
! !H2OS(B,S,G)!T R !  R ! MASSE D'EAU SORTIE   (BIEF,CONFLUENT,GLOBAL) !
! !CONS(B,C,G)!T R !  R ! CONSERVATIVITE (BIEF,CONFLUENT,GLOBAL)       !
! !H2OI(B,G)S !T R !  R !                                              !
! !H2OT(B,G)S !T R !  R !                                              !
! !  SNODE    ! TR !  D ! SURFACE MOUILLEE                             !
! !  VOLS     ! TR !  D !                                              !
! !  QNODE    ! TR !  D ! DEBIT                                        !
! !  QIN      ! TR !  D ! DEBIT D'APPORT                               !
! !  X        ! TR !  D ! ABSCISSES DES POINTS DU MAILLAGE             !
! !  W        ! TR !  D ! VARAIBLE D'EATAT DANS LE CONFLUENT           !
! !  AIRS     ! TR !  D ! SURFACE DES CELLULES DU MAILLAGE CONFLUENT   !
! !  FINBIE   ! TI !  D ! INDICATEUR DE DEBUT ET FIN DE BIEF           !
! !  ISEC     ! TI !  D ! NUMERO DE SECTION LIMITE A UN CONFLUENT      !
! ! IDEB,IFIN ! TI !  D ! LIMITES DE LA ZONE DE CALCUL PAR BIEF        !
! !  INDIC    ! TI !  D ! INDICATEUR DE CALCUL PAR BIEF                !
! !  INDICO   ! TI !  D ! INDICATEUR DE CALCUL PAR CONFLUENT           !
! !  XBAR     !  R !  D ! POSITION DU BARRAGE                          !
! !  ZBAR     !  R !  D ! COTE DE LA SURFACE LIBRE AU BARRAGE          !
! !  DT       !  R !  D ! PAS DE TEMPS                                 !
! !  ITEMP    !  I !  D ! NUMERO DU PAS DE TEMPS                       !
! !  NPMAX    !  I !  D ! NOMBRE DE PAS DE TEMPS MAXIMAL               !
! !  T        !  R !  D ! TEMPS                                        !
! !  TMAX     !  R !  D ! TEMPS MAXIMAL                                !
! !  NBBIEF   !  I !  D ! NOMBRE DE BIEFS                              !
! !  NBNOEU   !  I !  D ! NOMBRE DE CONFLUENCES                        !
! !Impression !  L !  D ! LOGIQUE D'IMPRESSION DU BILAN DE MASSE !
! !  CALCOS   !  L !  D ! LOGIQUE DE CALCUL ONDE DE SUBMERSION         !
! !  STOCKAGE !  L !  D !                                              !
! !___________!____!____!______________________________________________!
!
!                             VARIABLES LOCALES
! .___________.____.____.______________________________________________.
! !  H2OB     !  R !  A ! VOLUME DU BARRAGE QUI CASSE                  !
! !  H2OTD    !  R !  A ! VOLUME D'EAU PRESENT LE DOMAINE DE CALCUL    !
! !  H2OXXM   !  R !  A ! VALEUR DE H2OXX A L'INSTANT PRECEDENT        !
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
   use M_CONNECT_T           ! Type CONNECT
   use M_ERREUR_T            ! ERREUR
   use M_CONFLUENT_T         ! Type Confluent
   use M_CONSTANTES_CALCUL_C ! phase du calcul

   !.. Declarations explicites ..
   !-----------------------------
   implicit none

   !.. Arguments ..
   !---------------
   real(DOUBLE) , dimension(:)    ,   intent(  out) :: H2OIB,H2OTB,H2OEB,H2OSB
   real(DOUBLE) , dimension(:)    ,   intent(  out) :: H2OIBS,H2OTBS
   real(DOUBLE) , dimension(:)    ,   intent(  out) :: CONSB
   real(DOUBLE) , dimension(:)    ,   intent(  out) :: H2OIC,H2OTC,H2OEC,H2OSC
   real(DOUBLE) , dimension(:)    ,   intent(  out) :: CONSC
   real(DOUBLE) ,                     intent(  out) :: H2OIG,H2OTG,H2OEG,H2OSG
   real(DOUBLE) ,                     intent(  out) :: H2OIGS,H2OTGS
   real(DOUBLE) ,                     intent(  out) :: CONSG
   ! 1ere dimension IM
   real(DOUBLE) , dimension(:)      , intent(in)    :: SNODE,VOLS,QNODE,QIN,X
   type(CONNECT_T) ,                  intent(in)    :: Connect
   integer         ,                  intent(in)    :: NbExtremite
   type (CONFLUENT_T) , dimension(:) ,intent(in)    :: Confluent
   ! 2nde dimension 12
   ! 1ere dimension  3 (pour W)
   real(DOUBLE) , dimension(:,:,:) , intent(in)    :: W
   real(DOUBLE) , dimension(:,:)   , intent(in)    :: AIRS
   integer      , dimension(:)     , intent(in)    :: IDEB,IFIN,INDIC,INDICO
   real(DOUBLE) ,                    intent(in)    :: XBAR,ZBAR
   real(DOUBLE) ,                   intent(in)     :: DT
   integer      ,                   intent(in)     :: ITEMP,NPMAX
   integer      ,                   intent(in)     :: Phase_post_imp
   integer      ,                   intent(in)     :: NP
   real(DOUBLE) ,                   intent(in)     :: T,TMAX
   integer      ,                   intent(in)     :: NBBIEF,NBNOEU
   logical      ,                   intent(in)     :: Impression
   integer      ,                   intent(in)     :: UniteListing
   logical      ,                   intent(in)     :: CALCOS,STOCKAGE
   Type (ERREUR_T)                , intent(inout)  :: Erreur

   !.. Variables locales ..
   !-----------------------
   integer       :: IPOIN , IBIEF , INOEU , IAFFLU , IEL , IBIEFO , IEXT
   integer       :: ISEC , FINBIE , ISECT
   real(DOUBLE)  :: H2OTBM , H2OEBM , H2OSBM , H2OTCM , H2OECM , H2OSCM
   real(DOUBLE)  :: H2OTGM , H2OEGM
   real(DOUBLE)  :: H2OB , H2OTD , H2OERR , H2OSGM , H2TGSM , H2TBSM
   !Character(132):: !arbredappel_old

   !============================= Instructions ===========================

   ! INITIALISATION
   !===============
   Erreur%Numero = 0
   !   arbredappel_old    = trim(!Erreur%arbredappel)
   !   Erreur%arbredappel = trim(!Erreur%arbredappel)//'=>BILAN'

   if( ITEMP == PHASE_INITIALISATION ) then
      ! INITIALISATIONS AVANT LE PREMIER PAS DE TEMPS
      ! =============================================
      H2OTG  = 0._DOUBLE
      H2OIG  = 0._DOUBLE
      H2OTGS = 0._DOUBLE
      H2OIGS = 0._DOUBLE
      H2OEG  = 0._DOUBLE
      H2OSG  = 0._DOUBLE

      ! DANS LES BIEFS
      ! --------------
      do IBIEF = 1 , NBBIEF

         H2OIB(IBIEF)  = 0._DOUBLE
         H2OIBS(IBIEF) = 0._DOUBLE
         H2OEB(IBIEF)  = 0._DOUBLE
         H2OSB(IBIEF)  = 0._DOUBLE

         do IPOIN = Connect%OrigineBief(IBIEF) + 1 , Connect%FinBief(IBIEF)
            H2OIB(IBIEF) = H2OIB(IBIEF) + ( SNODE(IPOIN - 1) + SNODE(IPOIN) ) * ( X(IPOIN) - X(IPOIN - 1) ) / 2._DOUBLE
         end do

         H2OTB (IBIEF) = H2OIB(IBIEF)
         H2OTBS(IBIEF) = H2OIBS(IBIEF)
         H2OIG         = H2OIG  + H2OIB(IBIEF)
         H2OIGS        = H2OIGS + H2OIBS(IBIEF)
         H2OTG         = H2OIG
         H2OTGS        = H2OIGS

      enddo

      ! DANS LES CONFLUENTS
      ! -------------------
      do INOEU = 1 , NBNOEU

         H2OIC(INOEU) = 0._DOUBLE
         H2OEC(INOEU) = 0._DOUBLE
         H2OSC(INOEU) = 0._DOUBLE

         do IEL = 1 , 3
           H2OIC(INOEU) = H2OIC(INOEU) + 0.5_DOUBLE * AIRS(IEL,INOEU) * W(1,IEL,INOEU)
         end do

         do IEL = 7 , 12
            H2OIC(INOEU) = H2OIC(INOEU) + AIRS(IEL,INOEU) * W(1,IEL,INOEU)
         end do

         H2OTC(INOEU) = H2OIC(INOEU)
         H2OIG        = H2OIG + H2OIC(INOEU)
         H2OTG        = H2OIG

      end do

      ! CALCUL DU VOLUME DE LA RETENUE
      ! ------------------------------
      if( CALCOS ) then

         if( ZBAR > 0._DOUBLE ) then

            IBIEFO = 1
            ! RUPTURE INSTANTANNEE
            H2OB = 0._DOUBLE

            do IBIEF = 1 , NBBIEF

               if( ( XBAR > X(Connect%OrigineBief(IBIEF)) ) .and. ( XBAR < X(Connect%FinBief(IBIEF)) ) ) then
                  IBIEFO = IBIEF
               endif

            end do

            do IPOIN = Connect%OrigineBief(IBIEFO) + 1 , Connect%FinBief(IBIEFO)

               if( X(IPOIN) <= XBAR ) then
                  H2OB = H2OB + ( SNODE(IPOIN - 1) + SNODE(IPOIN) ) * ( X(IPOIN) - X(IPOIN - 1) ) / 2._DOUBLE
               endif

            end do

            if( NBNOEU >= 1 ) THEN

               label_inoeu : do inoeu = 1, NBNOEU

                  do ibief = 1, Connect%NbBiefConfluence(inoeu)
                     if( Connect%NumBiefConfluence(inoeu,ibief) == IBIEFO ) exit label_inoeu
                  end do

               end do label_inoeu

               INOEU = Connect%NumBiefConfluence(inoeu,ibief)

               if( INOEU <= NBNOEU ) then
               ! IL Y A UN CONFLUENT A L'AMONT DU BARRAGE

                  do IAFFLU = 1 , 3
                     ibief = Connect%NumBiefConfluence(INOEU,IAFFLU)
                  end do

               endif

            endif

         endif

      endif

   else
      ! CALCUL A CHAQUE PAS DE TEMPS
      ! ============================
      H2OEGM = H2OEG
      H2OSGM = H2OSG
      H2OTGM = H2OTG
      H2OTG  = 0._DOUBLE
      H2TGSM = H2OTGS
      H2OTGS = 0._DOUBLE

      do IBIEF = 1 , NBBIEF
         ! BILAN DE MASSE PAR BIEF
         ! -----------------------
         H2OEBM        = H2OEB(IBIEF)
         H2OSBM        = H2OSB(IBIEF)
         H2OTBM        = H2OTB(IBIEF)
         H2OTB(IBIEF)  = 0._DOUBLE
         H2TBSM        = H2OTBS(IBIEF)
         H2OTBS(IBIEF) = 0._DOUBLE

         do IPOIN = Connect%OrigineBief(IBIEF) + 1 , Connect%FinBief(IBIEF)

            H2OTB(IBIEF) = H2OTB(IBIEF) + ( SNODE(IPOIN - 1) + SNODE(IPOIN) ) * ( X(IPOIN) - X(IPOIN - 1) ) / 2._DOUBLE
            ! APPORTS DE DEBITS
            if( QIN(IPOIN) > 0._DOUBLE ) then

               H2OEB(IBIEF) = H2OEB(IBIEF) + QIN(IPOIN) * ( X(IPOIN) - X(IPOIN - 1) ) * DT
               H2OEG        = H2OEG        + QIN(IPOIN) * ( X(IPOIN) - X(IPOIN - 1) ) * DT

            else

               H2OSG        = H2OSG        - QIN(IPOIN) * ( X(IPOIN) - X(IPOIN - 1) ) * DT

            endif

         end do

         ! FLUX AUX LIMITES
         if( QNODE(Connect%OrigineBief(IBIEF)) > 0._DOUBLE ) then
            H2OEB(IBIEF) = H2OEB(IBIEF) + QNODE(Connect%OrigineBief(IBIEF)) * DT
         else
            H2OSB(IBIEF) = H2OSB(IBIEF) - QNODE(Connect%OrigineBief(IBIEF)) * DT
         endif

         if( QNODE(Connect%FinBief(IBIEF)) > 0._DOUBLE) then
           H2OSB(IBIEF) = H2OSB(IBIEF) + QNODE(Connect%FinBief(IBIEF)) * DT
         else
           H2OEB(IBIEF) = H2OEB(IBIEF) - QNODE(Connect%FinBief(IBIEF)) * DT
         endif

         ! CALCUL DE L'ERREUR SUR LA MASSE PAR BIEF
         ! ----------------------------------------
         CONSB(IBIEF) = ( ( H2OTB(IBIEF) - H2OTBM ) - &
                          ( H2OEB(IBIEF) - H2OEBM )   + &
                          ( H2OSB(IBIEF) - H2OSBM )   + &
                          ( H2OTBS(IBIEF) - H2TBSM ) ) / H2OTB(IBIEF)

      end do

      ! BILAN DE MASSE SUR LES CONFLUENTS
      ! ---------------------------------
      do INOEU = 1 , NBNOEU

         H2OTCM         = H2OTC(INOEU)
         H2OECM         = H2OEC(INOEU)
         H2OSCM         = H2OSC(INOEU)
         H2OTC(INOEU) = 0._DOUBLE

         do IEL = 1 , 3
            H2OTC(INOEU) = H2OTC(INOEU) + 0.5_DOUBLE * AIRS(IEL,INOEU) * W(1,IEL,INOEU)
         end do

         do IEL = 7 , 12
            H2OTC(INOEU) = H2OTC(INOEU) + AIRS(IEL,INOEU) * W(1,IEL,INOEU)
         end do

         ! CALCUL DES FLUX DE MASSE
         do IAFFLU = 1 , 3

            FINBIE = Confluent(INOEU)%FINBIE(IAFFLU)
            ISEC   = Confluent(INOEU)%ISEC(IAFFLU)

            if( FINBIE == 1 ) then

               if( QNODE(ISEC) > 0._DOUBLE ) then
                  H2OEC(INOEU) = H2OEC(INOEU) + QNODE(ISEC) * DT
               else
                  H2OSC(INOEU) = H2OSC(INOEU) - QNODE(ISEC) * DT
               endif

            else

               if( QNODE(ISEC) > 0._DOUBLE ) then
                  H2OSC(INOEU) = H2OSC(INOEU) + QNODE(ISEC) * DT
               else
                  H2OEC(INOEU) = H2OEC(INOEU) - QNODE(ISEC) * DT
               endif

            endif

         end do

         CONSC(INOEU) = ( ( H2OTC(INOEU) - H2OTCM ) - &
                          ( H2OEC(INOEU) - H2OECM )   + &
                          ( H2OSC(INOEU) - H2OSCM ) ) / &
                            H2OTC(INOEU)

      end do

      ! BILAN DE MASSE GLOBALE
      do IBIEF = 1 , NBBIEF
         H2OTG  = H2OTG  + H2OTB(IBIEF)
         H2OTGS = H2OTGS + H2OTBS(IBIEF)
      end do

      do INOEU = 1 , NBNOEU
         H2OTG = H2OTG + H2OTC(INOEU)
      end do

      do iext = 1, NbExtremite

         ibief = Connect%NumBiefExtLibre(iext)
         isect = Connect%NumSectionExtlibre(iext)

         If( Isect == Connect%OrigineBief(Ibief) ) then

            if( QNODE(Connect%OrigineBief(IBIEF)) > 0._DOUBLE ) then
               H2OEG = H2OEG + QNODE(Connect%OrigineBief(IBIEF)) * DT
            else
               H2OSG = H2OSG - QNODE(Connect%OrigineBief(IBIEF)) * DT
            endif

         else

            if( QNODE(Connect%FinBief(IBIEF)) > 0._DOUBLE ) then
               H2OSG = H2OSG + QNODE(Connect%FinBief(IBIEF)) * DT
            else
               H2OEG = H2OEG - QNODE(Connect%FinBief(IBIEF)) * DT
            endif

         endif

      end do

      CONSG = ( ( H2OTG - H2OTGM ) - ( H2OEG - H2OEGM ) + ( H2OSG - H2OSGM ) + ( H2OTGS - H2TGSM ) ) / H2OTG

      ! VOLUME D'EAU PRESENT DANS LE DOMAINE DE CACUL
      if( CALCOS ) then

         H2OTD = 0._DOUBLE

         do IBIEF = 1 , NBBIEF

            if( INDIC(IBIEF) == 1 ) then

               do IPOIN = IDEB(IBIEF) + 1 , IFIN(IBIEF)
                  H2OTD = H2OTD + ( SNODE(IPOIN - 1) + SNODE(IPOIN) ) * ( X(IPOIN) - X(IPOIN - 1) ) / 2._DOUBLE
               end do

            endif

         end do

         do INOEU = 1 , NBNOEU
            if( INDICO(INOEU) == 1 ) then
               H2OTD = H2OTD + H2OTC(INOEU)
            endif
         end do

      endif

   endif

   ! IMPRESSION DES RESULTATS
   ! ========================
   if (UniteListing > 0) then
      if( ( Phase_post_imp == PHASE_CALCUL ) .OR. ( Phase_post_imp == PHASE_INITIALISATION ) ) then
         if( ITEMP == PHASE_INITIALISATION ) then

            ! IMPRESSION INITIALE
            write( UniteListing , 2000 )

            if( NBBIEF > 1 ) then

               do IBIEF = 1 , NBBIEF

                  write( UniteListing , 1000 ) IBIEF , H2OIB(IBIEF)

                  if( STOCKAGE ) then
                     write( UniteListing , 1001 ) IBIEF , H2OIBS(IBIEF)
                  endif

               end do

            endif

            if( NBNOEU > 0 ) then

               do INOEU = 1 , NBNOEU
                  write( UniteListing , 1005 ) INOEU , H2OIC(INOEU)
               end do

            endif

            write( UniteListing , 1010 ) H2OIG

            if( STOCKAGE ) then
               write( UniteListing , 1011 ) H2OIGS
            endif

            if( CALCOS ) then

               if( ZBAR > 0 ) then
                  write( UniteListing , 1015 ) H2OB
               else
                  write(UniteListing,1016)
               endif

            endif

            write( UniteListing , 2000 )

         endif

      endif
   endif ! fin if (UniteListing > 0)

   if( ( NP == NPMAX - 1 ) .or. ( T + DT >= TMAX ) ) then

      ! IMPRESSION FINALE
      if(UniteListing > 0) then
         write( UniteListing , 2000 )
      end if

      if( NBBIEF > 1 ) then

         do IBIEF = 1 , NBBIEF
            if(UniteListing > 0) then
               write( UniteListing ,1040 ) IBIEF
               write( UniteListing ,1050 ) H2OIB(IBIEF)
               write( UniteListing ,1060 ) H2OEB(IBIEF)
               write( UniteListing ,1061 ) H2OSB(IBIEF)
               write( UniteListing ,1069 ) H2OTB(IBIEF)

               if( STOCKAGE ) then
                  write( UniteListing , 1071 ) H2OIBS(IBIEF)
                  write( UniteListing , 1072 ) H2OTBS(IBIEF)
               endif
            endif

            H2OERR = H2OTB(IBIEF) - H2OIB(IBIEF) - &
                     H2OEB(IBIEF) + H2OSB(IBIEF) + &
                     H2OTBS(IBIEF) - H2OIBS(IBIEF)

            if(UniteListing > 0) then
               write( UniteListing , 1080 ) H2OERR
            endif

         end do

      endif

      if( NBNOEU > 0 ) then

         do INOEU = 1 , NBNOEU
           if(UniteListing > 0) then
              write( UniteListing , 1045 ) INOEU
              write( UniteListing ,1051 ) H2OIC(INOEU)
              write( UniteListing ,1060 ) H2OEC(INOEU)
              write( UniteListing ,1061 ) H2OSC(INOEU)
              write( UniteListing ,1070 ) H2OTC(INOEU)
           endif

           H2OERR = H2OTC(INOEU) - H2OIC(INOEU) - H2OEC(INOEU) + H2OSC(INOEU)

           if(UniteListing > 0) then
              write( UniteListing ,1080 ) H2OERR
           endif

         end do

      endif

      if(UniteListing > 0) then
         write( UniteListing ,1100 )
         write( UniteListing ,1050 ) H2OIG
         write( UniteListing ,1060 ) H2OEG
         write( UniteListing ,1061 ) H2OSG
         write( UniteListing ,1069 ) H2OTG

         if( STOCKAGE ) then
            write( UniteListing , 1071 ) H2OIGS
            write( UniteListing , 1072 ) H2OTGS
         endif
      endif

      H2OERR = H2OTG - H2OIG - H2OEG + H2OSG + H2OTGS - H2OIGS

      if(UniteListing > 0) then
         write( UniteListing , 1080 ) H2OERR
      endif

      CONSG = H2OERR / max( max( H2OIG , H2OTG ) , max( H2OEG , H2OSG ) )

      if(UniteListing > 0) then
         write( UniteListing , 1090 ) CONSG
         write( UniteListing , 2000 )
      endif

   endif

   !------------------
   ! Fin du traitement
   !------------------

   !Erreur%arbredappel = !arbredappel_old

   return

  !*****************************************************************************
  ! FORMATS D'IMPRESSION
  !*****************************************************************************

1000 format(2X,'MASSE D''EAU INITIALE DANS LE BIEF (HORS STOCKAGE)',I2, &
       '  : ',G16.7,' M3')
1001 format(2X,'MASSE D''EAU INITIALE STOCKEE DANS LE BIEF        ',I2, &
       '  : ',G16.7,' M3',/)
1005 format(2X,'MASSE D''EAU INITIALE DANS LE CONFLUENT',I2, '  : ',G16.7,' M3')
1010 format(/,2X,'MASSE D''EAU INITIALE DANS LE DOMAINE', &
       ' (HORS STOCKAGE) : ', G16.7,' M3')
1011 format(2X,'MASSE D''EAU INITIALE STOCKEE DANS LE DOMAINE : ', G16.7,' M3')
1015 format(/,2X,'MASSE D''EAU INITIALE DE LA RETENUE : ', G16.7,' M3')
1016 format(/,2X,'RUPTURE PROGRESSIVE DU BARRAGE PRINCIPAL', &
       2X,'VOLUME DE LA RETENUE NON DISPONIBLE')
1040 format(/,2X,'BILAN DE MASSE FINAL DANS LE BIEF :',I2,/, &
       2X,'-----------------------------------')
1045 format(/,2X,'BILAN DE MASSE FINAL DANS LE CONFLUENT :',I2,/, &
       2X,'--------------------------------------')
1050 format(2X,'MASSE D''EAU INITIALE (HORS STOCKAGE) : ',G16.7,' M3')
1051 format(2X,'MASSE D''EAU INITIALE                 : ',G16.7,' M3')
1060 format(2X,'MASSE D''EAU ENTREE AUX FRONTIERES    : ',G16.7,' M3')
1061 format(2X,'MASSE D''EAU SORTIE AUX FRONTIERES    : ',G16.7,' M3')
1070 format(2X,'MASSE D''EAU FINALE                   : ',G16.7,' M3')
1069 format(2X,'MASSE D''EAU FINALE (HORS STOCKAGE)   : ',G16.7,' M3')
1071 format(2X,'MASSE D''EAU STOCKEE INITIALE         : ',G16.7,' M3')
1072 format(2X,'MASSE D''EAU STOCKEE FINALE           : ',G16.7,' M3')
1080 format(2X,'ERREUR SUR LA MASSE D''EAU            : ',G16.7,' M3')
1090 format(2X,'ERREUR RELATIVE                      : ',G16.7)
1100 format(/,2X,'BILAN DE MASSE GLOBAL',/, 2X,'---------------------')
2000 format(/,'======================================================', &
       '=================================',/)

end subroutine BILAN
