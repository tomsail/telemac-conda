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

subroutine MASCARET_INTERFACE( &
                     ! Entree/Sorties
                     !---------------
                     ZNODE ,                     & ! Cote
                     QMIN , QMAJ ,               & ! debits  mineur, majeur
                     SMIN , SMAJ ,               & ! section mineur, majeur
                     W , AIRS ,                  & ! Etats du 2D pour les confluents
                     YNODE , UNODE ,CNODE ,      & ! Etats pour Mascaret
                     FLUX ,                      & ! Flux pour le schema de Roe
                     DebitFlux ,                 & ! Flux de debit
                     JGNODE , JDNODE , IFIGE ,   & ! indice de planimetrage
                     BETA ,                      & ! Coefficient de repartition mineur/majeur
                     FROUD ,                     & ! Nombre de Froude
                     XFRON ,                     & ! Abscisse du front d'onde
                     DT , Temps , num_pas ,      & ! Pas de temps optimal,Temps
                     Npmax ,                     & ! Nombre de pas de temps max
                     TempsMaximun ,              & ! Temps maximun
                     ! Conditions aux limites
                     Extremite , Apport ,        & ! Extremites libres,Debits d'apports
                     Qin ,                       & ! Debit injecte
                     Qdeverse ,                  & ! Debit au deversoir
                     ! Variables temporels
                     PhaseSimulation ,           &
                     Phase_post_imp ,            &
                     DZ , DZD ,                  &
                     XD ,                        &
                     NbPas ,                     &
                     ! Planimetrage
                     SectionPlan ,               &
                     ! Modele
                     !-------
                     ! Maillage, Strickler,Cote de Fond
                     X , STMIN , STMAJ , COTR ,  & ! Cote du fond
                     nb_sect ,                   & ! nombre de sections
                     ! Zones seches
                     ZoneSeche ,                 & ! Zone seche
                     ! Reseau
                     Connect ,                   & ! Table de connectivite
                     Singularite ,               & ! Singularites
                     Barrage ,                   & ! XBARP,ZBARP
                     PCSing ,                    & ! Pertes de charges singulieres
                     Deversoir ,                 & ! Deversoirs
                     Confluent ,                 & ! Confluents 2D
                     SVRAI , QVRAI , ZVRAI ,     & ! VALIDATION
                     ZINIT ,                     & ! COTE INITIALE
                     ! Parametres de calcul
                     HEPS , SEPS , GPES ,        & ! Hauteur et section minimale, acceleration pesant
                     CALCOS , CALCVA , IVALID ,  & ! Indicateur et numero de validation
                     REP ,                       &    ! Indicateur de reprise de calcul
                     FROLIM ,                    & ! Indicateur de condition aux limites
                     FRTIMP ,                    & ! Indicateur pour l'impliciation du frottement
                     Impli_Trans , Opt ,         & ! Indicateur pour l'implicitation du solveur
                     PerteElargissementTrans ,   & !Perte de Charge elargissement
                     Boussinesq ,                & ! Ajout des termes non hydrostatiques
                     CQMV ,                      & ! Apport dans la quantite de mvt
                     STOCKAGE ,                  & ! Indicateur de zones de stockage
                     PastempsVariable ,          & ! Indicateur de pas de temps variable
                     NombreCourant ,             & ! Nombre de Courant limite
                     ! Parametres
                     Impression , UniteListing , & ! Flag d'impression
                     ! Etats
                     VOLS ,                      &
                     Sauve ,                     &
                     NBARAD ,                    &
                     IDEB, IFIN, ITEM0 ,         &
                     Erreur ) ! apport/ provenant des zones de stockage

!***********************************************************************
! PROGICIEL : MASCARET        N. GOUTAL
!
! VERSION : V8P4R0              EDF-CEREMA
!***********************************************************************
!
!  FONCTION :
!  --------
!
!       SOUS-PROGRAMME DE CALCUL DU CODE MASCARET
!
!-----------------------------------------------------------------------
!
!                        VARIABLES EN ARGUMENTS
! .____________________________________________________________________
! !   X      ! I  !<-- !   POINTEURS DU TABLEAU   X
! !   XP     ! I  !<-- !   POINTEURS DU TABLEAU   XP
! !   DB     ! I  !<-- !   POINTEURS DU TABLEAU   DB
! !   PZREF  ! I  !<-- !  POINTEURS DU TABLEAU   ZREFP
! !   STP    ! I  !<-- !   POINTEURS DU TABLEAU   STP
! !   XDD    ! I  !<-- !   POINTEURS DU TABLEAU   XD
! !   FM1    ! I  !<-- !   POINTEURS DU TABLEAU   FM1
! !   FP1    ! I  !<-- !   POINTEURS DU TABLEAU   FP1
! !   UNP1   ! I  !<-- !   POINTEURS DU TABLEAU   UNP1
! !   YNP1   ! I  !<-- !   POINTEURS DU TABLEAU   YNP1
! !   SNP1   ! I  !<-- !   POINTEURS DU TABLEAU   SNP1
! !   CNP1   ! I  !<-- !   POINTEURS DU TABLEAU   CNP1
! !   FRNO   ! I  !<-- !   POINTEURS DU TABLEAU   FRNODE
! !   ST     ! I  !<-- !   POINTEURS DU TABLEAU   ST
! !   PRAD   ! I  !<-- !   POINTEURS DU TABLEAU   PRAD
! !   COTR   ! I  !<-- !   POINTEURS DU TABLEAU   COTR
! !   COTRD  ! I  !<-- !   POINTEURS DU TABLEAU   COTRD
! !   YDNP   ! I  !<-- !   POINTEURS DU TABLEAU   YDNP
! !   SDNP   ! I  !<-- !   POINTEURS DU TABLEAU   SDNP
! !   CDNP   ! I  !<-- !   POINTEURS DU TABLEAU   CDNP
! !   QDNP   ! I  !<-- !   POINTEURS DU TABLEAU   QDNP
! !   UDNP   ! I  !<-- !   POINTEURS DU TABLEAU   UDNP
! !   PRADD  ! I  !<-- !   POINTEURS DU TABLEAU   PRADD
! !   YNODE  ! I  !<-- !   POINTEURS DU TABLEAU   YNODE
! !   SNODE  ! I  !<-- !   POINTEURS DU TABLEAU   SNODE
! !   CNODE  ! I  !<-- !   POINTEURS DU TABLEAU   CNODE
! !   KNODE  ! I  !<-- !   POINTEURS DU TABLEAU   AKNODE
! !   QNODE  ! I  !<-- !   POINTEURS DU TABLEAU   QNODE
!-----------------------------------------------------------------------

   !============================= Declarations ===========================
   !.. Modules importes ..
   !----------------------
   use M_PRECISION
   use M_CONSTANTES_CALCUL_C !
   use M_MESSAGE_C
   use M_APPORT_T
   use M_BARRAGE_T
   use M_CONFLUENT_T
   use M_CONNECT_T
   use M_DEVERSOIR_T     ! Definition du type DEVERSOIR_T
   use M_ERREUR_T        ! Definition du type ERREUR_T
   use M_EXTREMITE_T
   use M_SECTION_PLAN_T
   use M_SINGULARITE_T
   use M_ZONE_SECHE_T    ! Type Zone_Seche_T
   use M_SAUVE_T         ! Variables sauvegardees
   use M_INTERPOLATION_S
   use M_LIMITE_I      ! Interface du sous-programme LIMITE
   use M_BILAN_I       ! Interface du sous-programme BILAN
   use M_BORNE_I       ! Interface du sous-programme BORNE
   use M_CALCFL_I      ! Interface du sous-programme CALCFL
   use M_CALVAR_I      ! Interface du sous-programme CALVAR
   use M_CONFLU_I      ! Interface du sous-programme CONFLU
   use M_CSUR_I        ! Interface de la fonction    CSUR
   use M_CSURM1_I      ! Interface de la fonction    CSURM1
   use M_CQINJ_I
   use M_DECBAR_I      ! Interface du sous-programme DECBAR
   use M_FROTTD_I      ! Interface du sous-programme FROTTD
   use M_PRECAL_I      ! Interface du sous-programme PRECAL
   use M_PRES_I        ! Interface de la fonction    PRES
   use M_PRESD_I       ! Interface de la fonction    PRESD
   use M_RESOL_I       ! Interface du sous-programme RESOL
   use M_VALIDA_I      ! Interface du sous-programme VALIDA
   use M_TRAITER_ERREUR_I ! Interface du programme de traitement des erreurs

   !.. Declarations explicites ..
   !-----------------------------
   implicit none

   ! Entrees/Sorties
   real(DOUBLE), dimension(:), intent(inout) :: ZNODE
   real(DOUBLE), dimension(:), intent(inout) :: SMIN
   real(DOUBLE), dimension(:), intent(inout) :: SMAJ
   real(DOUBLE), dimension(:), intent(inout) :: QMIN
   real(DOUBLE), dimension(:), intent(inout) :: QMAJ
   real(DOUBLE), dimension(:), intent(inout) :: BETA
   real(DOUBLE), dimension(:), intent(inout) :: FROUD
   real(DOUBLE), dimension(:), intent(inout) :: YNODE
   real(DOUBLE), dimension(:), intent(inout) :: UNODE
   real(DOUBLE), dimension(:), intent(inout) :: CNODE
   real(DOUBLE), dimension(:,:),intent(inout) :: FLUX
   real(DOUBLE), dimension(:), intent(inout)  :: DebitFlux
   integer     , dimension(:),intent(inout)  :: JGNODE,JDNODE,IFIGE
   ! 1ere dimension nb_bief
   real(DOUBLE), dimension(:), intent(  out) :: XFRON
   real(DOUBLE)              , intent(inout)    :: Temps
   real(DOUBLE)              , intent(in   )    :: TempsMaximun
   integer                   , intent(in   ) :: PhaseSimulation
   type(APPORT_T)         , dimension(:), intent(in   ) :: Apport
   real(DOUBLE)           , dimension(:), intent(inout) :: QDeverse
   real(DOUBLE)           , dimension(:), intent(inout) :: Qin
   type(EXTREMITE_T)      , dimension(:), intent(in   ) :: Extremite
   type(ZONE_SECHE_T)     , dimension(:), pointer       :: ZoneSeche
   ! Maillage
   real(DOUBLE), dimension(:), intent(in   ) ::  X
   real(DOUBLE), dimension(:), intent(in   ) ::  XD
   real(DOUBLE), dimension(:), intent(in   ) ::  STMIN
   real(DOUBLE), dimension(:), intent(in   ) ::  STMAJ
   real(DOUBLE), dimension(:), intent(in   ) ::  COTR
   ! Reseau
   type(CONNECT_T), intent(in   ) :: CONNECT
   ! Planimetrage
   real(DOUBLE), dimension(:), intent(in   ) ::  DZ
   real(DOUBLE), dimension(:), intent(in   ) ::  DZD
   integer                   , intent(in   ) ::  NbPas
   integer                   , intent(in   ) ::  Nb_sect
   ! 1ere dimension IM
   ! 1ere dimension IM1
   type(SECTION_PLAN_T),              intent(in   ) :: SectionPlan
   ! Donnees temporelles
   real(DOUBLE)             , intent(inout   ) :: DT
   ! Confluents
   type(CONFLUENT_T)     , dimension(:), intent(in   ) :: Confluent
   ! Pertes de charge singulieres
   real(DOUBLE)        , dimension(:), intent(in   ) :: PCSing
   ! Deversoirs
   type (DEVERSOIR_T)  , dimension(:), intent(in   ) :: Deversoir
   ! Singularite
   type(SINGULARITE_T) , dimension(:), intent(inout) :: Singularite
   ! Barrage Principal
   type(BARRAGE_T)                   , intent(in   ) :: Barrage
   ! Variables sauvegardees pour le bilan de volume
   type(SAUVE_T)                     , intent(inout) :: Sauve
   ! 2nde dimension 12, 3eme dimension nb_noeud
   real(DOUBLE) ,dimension (:,:,:) , intent (inout) :: W
   ! 1ere dimension 12, 2nde dimension nb_noeud
   real(DOUBLE) ,dimension (:,:) , intent (inout) :: AIRS
   ! Resultats analytiques
   real(DOUBLE), dimension(:) :: SVRAI
   real(DOUBLE), dimension(:) :: QVRAI
   real(DOUBLE), dimension(:) :: ZVRAI
   ! ligne d'eau initiale
   real(DOUBLE), dimension(:) :: ZINIT
   ! Parametres de calcul
   real(DOUBLE), intent(in   ) :: FROLIM
   real(DOUBLE), intent(in   ) :: HEPS
   real(DOUBLE), intent(in   ) :: SEPS
   real(DOUBLE), intent(in   ) :: GPES
   real(DOUBLE), intent(in   ) :: NombreCourant
   integer     , intent(in   ) :: IVALID
   logical     , intent(in   ) :: STOCKAGE
   logical     , intent(in   ) :: FRTIMP
   logical     , intent(in   ) :: Impli_Trans, Opt
   logical     , intent(in   ) :: PerteElargissementTrans
   logical     , intent(in   ) :: Boussinesq
   integer     , intent(in   ) :: CQMV
   logical     , intent(in   ) :: CALCOS
   logical     , intent(in   ) :: CALCVA
   logical     , intent(in   ) :: REP
   logical     , intent(in   ) :: Impression
   integer     , intent(in   ) :: UniteListing
   integer     , intent(in   ) :: Phase_post_imp
   logical     , intent(in   ) :: PasTempsVariable
   integer                     :: num_pas
   integer                     :: Npmax
   ! Etat
   ! Apport et soutirage zones de stockage
   real(DOUBLE), dimension(:), pointer :: VOLS
   ! Casiers
   integer                           , intent(inout) :: NBARAD    ! NBARAD
   integer, dimension(:), pointer :: IDEB      ! LIMITE DE DEBUT DE LA ZONE DE CALCUL PAR BIEF
   integer, dimension(:), pointer :: IFIN      ! LIMITE DE FIN DE LA ZONE DE CALCUL PAR BIEF
   integer, dimension(:), pointer :: ITEM0
   ! Erreur
   type(ERREUR_T)                    , intent(inout) :: Erreur
   ! Variables locales
   !==================
   ! Resultats analytiques
   real(DOUBLE) UVRAI(nb_sect)
   real(DOUBLE) YVRAI(nb_sect)
   integer                        :: nb_noeud
   integer                        :: nb_bief
   integer                        :: nb_sing
   integer                        :: nb_ext
   integer                        :: nb_pas
   real(DOUBLE) QSTO(nb_sect)
   real(DOUBLE) FRNODE(nb_sect)
   real(DOUBLE) Y1(nb_sect),Q1(nb_sect),S1(nb_sect)
   real(DOUBLE), dimension(size(Connect%OrigineBief))      :: CONSB
   real(DOUBLE), dimension(size(Connect%NbBiefConfluence)) :: CONSC
   real(DOUBLE)                                            :: CONSG
   real(DOUBLE)                                            :: T,DTCFL
   real(DOUBLE), dimension(nb_sect)                        :: SNODE
   real(DOUBLE), dimension(nb_sect)                        :: DTI
   real(DOUBLE), dimension(nb_sect)                        :: QNODE
   real(DOUBLE), dimension(nb_sect)                        :: AKNODE
   real(DOUBLE), dimension(nb_sect)                        :: KVOLS
   integer IP,ICL,IS,NB,IP1,IP2,IBIEF
   integer KHYLIM
   integer                                 :: INOEUD,icompt
   integer I,INDIC(20)
   integer INDCO(20)
   integer IFI,IDE,I_ZONE,I_SECT
   integer retour
   real(DOUBLE) FROD,ZFIX,YFIX,DTPREC
   real(DOUBLE) SLD,QLD,SLF,QLF
   real(DOUBLE) PRIS,PRISM1,CHARIS,CHARM1,PRIP,PRIPM1
   real(DOUBLE) SMOY,QMOY
   real(DOUBLE) ZIS,ZISM1,YZIS,YZISM1,YIS,YISM1,SZIS,SZISM1
   real(DOUBLE) SOURC,FROT,QFIX
   !character(132) :: !arbredappel_old
   logical AMONT,AVAL,SUITE
   ! save ITEM0,IDEB,IFIN,NBARAD

   !============================= Instructions ===========================

   !     INITIALISATIONS
   !     ===============
!   write(*,*) 'Mascaret_interface - DEBUT'

   Erreur%Numero = 0
   retour = 0
   !  !arbredappel_old    = trim(!Erreur%arbredappel)
   !  !Erreur%arbredappel = trim(!Erreur%arbredappel)//'=>MASCARET'

   nb_bief  = size( Connect%OrigineBief )
   nb_noeud = size( Connect%NbBiefConfluence )
   nb_sing  = size( Singularite )
   nb_ext   = size( Connect%NumBiefExtLibre )
   nb_pas   = nbpas

 !
   do i = 1 , nb_sect
      QNODE(i) = QMIN(i) + QMAJ(i)
      SNODE(i) = SMIN(i) + SMAJ(i)
      QIN(i)   = 0._Double
   enddo

   INDCO(:) = 1
!   write(*,*) 'Mascaret_interface - avant if( PhaseSimulation == PHASE_INITIALISATION )', &
!              PhaseSimulation, PHASE_INITIALISATION
   if( PhaseSimulation == PHASE_INITIALISATION ) then
      ITEM0(:) = 0

      NBARAD   = size( Singularite )
      icompt   = 0
      Ifige(:) = 0

      ! Allocations
      !------------
      if(.not.associated(VOLS)) then
          allocate( VOLS(size( X(:) )) , stat = retour )
          if( retour /= 0 ) then
             Erreur%Numero = 5
             Erreur%ft     = err_5
             Erreur%ft_c   = err_5c
             call TRAITER_ERREUR( Erreur , 'VOLS' )
             return
          end if
      endif

      VOLS (:) = 0._DOUBLE
!
! La Structure SAUVE contient les variables seravnt au bilan de masse
!
      if(.not.associated(Sauve%H2OIB)) then
          allocate( Sauve%H2OIB(nb_bief) , stat = retour )
          if( retour /= 0 ) then
             Erreur%Numero = 5
             Erreur%ft     = err_5
             Erreur%ft_c   = err_5c
             call TRAITER_ERREUR( Erreur , 'Sauve%H2OIB' )
             return
          end if
      endif
      if(.not.associated(Sauve%H2OTB)) then
          allocate( Sauve%H2OTB(nb_bief) , stat = retour )
          if( retour /= 0 ) then
             Erreur%Numero = 5
             Erreur%ft     = err_5
             Erreur%ft_c   = err_5c
             call TRAITER_ERREUR( Erreur , 'Sauve%H2OTB' )
             return
          end if
      endif
      if(.not.associated(Sauve%H2OEB)) then
          allocate( Sauve%H2OEB(nb_bief) , stat = retour )
          if( retour /= 0 ) then
             Erreur%Numero = 5
             Erreur%ft     = err_5
             Erreur%ft_c   = err_5c
             call TRAITER_ERREUR( Erreur , 'Sauve%H2OEB' )
             return
          end if
      endif
      if(.not.associated(Sauve%H2OSB)) then
          allocate( Sauve%H2OSB(nb_bief) , stat = retour )
          if( retour /= 0 ) then
             Erreur%Numero = 5
             Erreur%ft     = err_5
             Erreur%ft_c   = err_5c
             call TRAITER_ERREUR( Erreur , 'Sauve%H2OSB' )
             return
          end if
      endif
      if(.not.associated(Sauve%H2OIC)) then
          allocate( Sauve%H2OIC(nb_bief) , stat = retour )
          if( retour /= 0 ) then
             Erreur%Numero = 5
             Erreur%ft     = err_5
             Erreur%ft_c   = err_5c
             call TRAITER_ERREUR( Erreur , 'Sauve%H2OIC' )
             return
          end if
      endif
      if(.not.associated(Sauve%H2OTC)) then
          allocate( Sauve%H2OTC(nb_bief) , stat = retour )
          if( retour /= 0 ) then
             Erreur%Numero = 5
             Erreur%ft     = err_5
             Erreur%ft_c   = err_5c
             call TRAITER_ERREUR( Erreur , 'Sauve%H2OTC' )
             return
          end if
      endif
      if(.not.associated(Sauve%H2OEC)) then
          allocate( Sauve%H2OEC(nb_bief) , stat = retour )
          if( retour /= 0 ) then
             Erreur%Numero = 5
             Erreur%ft     = err_5
             Erreur%ft_c   = err_5c
             call TRAITER_ERREUR( Erreur , 'Sauve%H2OEC' )
             return
          end if
      endif
      if(.not.associated(Sauve%H2OSC)) then
          allocate( Sauve%H2OSC(nb_bief) , stat = retour )
          if( retour /= 0 ) then
             Erreur%Numero = 5
             Erreur%ft     = err_5
             Erreur%ft_c   = err_5c
             call TRAITER_ERREUR( Erreur , 'Sauve%H2OSC' )
             return
          end if
      endif
      if(.not.associated(Sauve%H2OTBS)) then
          allocate( Sauve%H2OTBS(nb_bief) , stat = retour )
          if( retour /= 0 ) then
             Erreur%Numero = 5
             Erreur%ft     = err_5
             Erreur%ft_c   = err_5c
             call TRAITER_ERREUR( Erreur , 'Sauve%H2OTBS' )
             return
          end if
      endif
      if(.not.associated(Sauve%H2OIBS)) then
          allocate( Sauve%H2OIBS(nb_bief) , stat = retour )
          if( retour /= 0 ) then
             Erreur%Numero = 5
             Erreur%ft     = err_5
             Erreur%ft_c   = err_5c
             call TRAITER_ERREUR( Erreur , 'Sauve%H2OIBS' )
             return
          end if
      endif
      if(.not.associated(Sauve%SPREC)) then
          allocate( Sauve%SPREC(size(X(:))) , stat = retour )
          if( retour /= 0 ) then
             Erreur%Numero = 5
             Erreur%ft     = err_5
             Erreur%ft_c   = err_5c
             call TRAITER_ERREUR( Erreur , 'Sauve%SPREC' )
             return
          end if
      endif
      if(.not.associated(Sauve%QPREC)) then
          allocate( Sauve%QPREC(size(X(:))) , stat = retour )
          if( retour /= 0 ) then
             Erreur%Numero = 5
             Erreur%ft     = err_5
             Erreur%ft_c   = err_5c
             call TRAITER_ERREUR( Erreur , 'Sauve%QPREC' )
             return
          end if
      endif

      !
      !  Prise en compte des zones seches
      !
      if( size( ZoneSeche ) > 0 ) then
         label_boucle_ZS : do i_zone = 1 , size( ZoneSeche )
            ide = ZoneSeche(i_zone)%SectionDeb
            ifi = ZoneSeche(i_zone)%SectionFin
            label_boucle_X : do i_sect = ide , ifi
               Znode(i_sect) = cotr(i_sect) + Heps / 10.D0
               Qnode(i_sect) = 0._DOUBLE
            enddo label_boucle_X
         enddo label_boucle_ZS
      endif

      ZINIT(:) = ZNODE(:)
      DTCFL    = DT
      DTI(:)   = DT
!
!         INITIALISATION DES VARIABLES
!
!      write(*,*) 'call PRECAL'
      call PRECAL                ( &
          SNODE                  , &
          SMIN                   , &
          SMAJ                   , &
          YNODE                  , &
          FROUD                  , &
          KVOLS                  , &
          UNODE                  , &
          CNODE                  , &
          ZNODE                  , &
          QNODE                  , &
  JGNODE,JDNODE                  , &
          QMIN                   , &
          QMAJ                   , &
          BETA                   , &
          ZINIT                  , &
          XFRON                  , &
          Barrage%Section        , &
          Confluent              , &
          W                      , &
          AIRS                   , &
          QVRAI ,SVRAI,UVRAI     , &
          YVRAI,ZVRAI            , &
          X                      , &
          Connect%OrigineBief(:) , &
          Connect%FinBief(:)     , &
          nb_bief                , &
          nb_noeud               , &
          nb_sect                , &
          nb_pas                 , &
          DZ                     , &
          COTR                   , &
          SectionPlan            , &
          REP,SUITE              , &
          SEPS                   , &
          CALCOS                 , &
          CALCVA                 , &
          STOCKAGE               , &
          AVAL                   , &
          Erreur                   &
          )
!      write(*,*) 'apres call PRECAL'
      if( Erreur%Numero /= 0 ) then
!         Print *,'ERREUR LORS DE L INITIALISATION DU MODELE : VERIFIEZ LE PLANIMETRAGE '
         return
      endif

      ! INITIALISATION DU BILAN DE MASSE
      ! --------------------------------
      call BILAN                  ( &
        sauve%H2OIB, sauve%H2OTB  , &
        Sauve%H2OEB, Sauve%H2OSB  , &
        Sauve%H2OIBS,Sauve%H2OTBS , &
        CONSB                     , &
        Sauve%H2OIC, Sauve%H2OTC  , &
        Sauve%H2OEC, Sauve%H2OSC  , &
        CONSC                     , &
        Sauve%H2OIG, Sauve%H2OTG  , &
        Sauve%H2OEG, Sauve%H2OSG  , &
        Sauve%H2OIGS,Sauve%H2OTGS , &
        CONSG                     , &
        SNODE                     , &
        VOLS                      , &
        QNODE                     , &
        QIN                       , &
        X                         , &
        Connect                   , &
        nb_ext                    , &
        W                         , &
        AIRS                      , &
        Confluent                 , &
        IDEB, IFIN                , &
        INDIC                     , &
        INDCO                     , &
        Barrage%AbscisseRel       , &
        Barrage%CoteCrete         , &
        DT                        , &
        PhaseSimulation           , &
        Phase_post_imp            , &
        Npmax,Temps ,num_pas      , &
        TempsMaximun              , &
        nb_bief, nb_noeud         , &
        Impression                , &
        UniteListing              , &
        CALCOS                    , &
        STOCKAGE                  , &
        Erreur                      &
       )
      if( Erreur%Numero /= 0 ) then
         return
      endif
   endif

   !      ========================================
   ! 1-1  BOUCLE SUR LES CONDITIONS LIMITES LIBRES
   !      ========================================
   label_boucle_CL : do ICL = 1 , nb_ext
      !    IS NUMERO DE SECTION EXTREME  DE LA CL no I
      !    IP NUMERO DE BIEF
      IP = Connect%NumBiefExtLibre(ICL)
      IS = Connect%NumSectionExtLibre(ICL)
      if( IS == Connect%OrigineBief(IP) ) then
         AMONT = .true.
         AVAL  = .false.
         IS = Connect%OrigineBief(IP)
      else
         AVAL  = .true.
         AMONT = .false.
      endif

      !    NOMBRE DE SECTIONS DU BIEF IP
      NB     = Connect%FinBief(IP) - Connect%OrigineBief(IP) + 1
      QFIX   = EXTREMITE(ICL)%PTQ(1)
      ZFIX   = EXTREMITE(ICL)%PTZ(1)
      YFIX   = ZFIX - COTR(IS)
      KHYLIM = EXTREMITE(ICL)%TYPE
      FROD   = dabs( FROUD(IS) )
      !
      !     TEST SUR LA HAUTEUR D'EAU EN REGIME TORRENTIEL
      !
      IF( ( IS ==1 ).and.( Frod >= 0.99D0 ).and.(UniteListing > 0) )  then
         If( YFIX <= 0.D0 ) then
            Write (UniteListing,*) 'HAUTEUR NEGATIVE EN ENTREE TORRENTIELLE'
            return
         endif
      endif
      !
      !    COTE ET DEBIT IMPOSES DE TYPE 8
      !
      if( KHYLIM == CONDITION_TYPE_COTE_DEBIT_IMPOSES ) then
         Q1(IS) = QFIX
         S1(IS) = CSUR( IS , YFIX , dz , SectionPlan%S , Nb_pas , Erreur )
      endif

      if( KHYLIM == CONDITION_TYPE_DEBIT_COTE ) then
         !     LOI HAUTEUR-DEBIT
         S1(IS) = SNODE(IS) + DT * ( QNODE(IS-1) - QNODE(IS) ) / ( X(IS) - X(IS-1) )
         Y1(IS) = CSURM1( S1(IS) , dz(IS) , SectionPlan%S(IS,:) , Erreur )
         if( Erreur%Numero /= 0 ) then
            return
         endif

         ZIS = Y1(IS) + COTR(IS)

         call INTERPOLATION_S( &
                          Q1(IS) , & ! Resultats
                            ZIS  , & ! Abscisse pour laquelle on veut YT
                              1  , & ! Ordre d'interpolation
             EXTREMITE(ICL)%PTZ  , & ! Tableau des abscisses
             EXTREMITE(ICL)%PTQ  , & ! Tableau des abscisses
      size( EXTREMITE(ICL)%PTZ ) , & ! dimension des tableaux X et Y
                          Erreur   & ! Erreur
                                   )
         if( Erreur%Numero /= 0 ) then
            return
         endif
      Endif

      if( ( ( AMONT .eqv. .True. ).and.( KHYLIM == CONDITION_TYPE_COTE_IMPOSE )).or.   &
          ( ( FROD >= FROLIM ).or.( KHYLIM == CONDITION_TYPE_SORTIE_LIBRE ) ) ) then

         !   CALCUL DE LA  CONDITION  LIMITE A L'AIDE DES INVARIANTS DE RIEMANN
         call LIMITE           ( &
              Y1(IS)           , &
              S1(IS)           , &
              Q1(IS)           , &
              nb_sect          , &
              KHYLIM           , &
              QFIX             , &
              YFIX             , &
              AMONT            , &
              DT               , &
              IS               , &
              NB               , &
              X                , &
              SNODE            , &
              CNODE            , &
              AKNODE           , &
              QNODE            , &
              UNODE            , &
              COTR             , &
              FRNODE           , &
              dz               , &
              SectionPlan%S    , &
              SectionPlan%B    , &
              SectionPlan%DEB  , &
              SectionPlan%INV  , &
              SectionPlan%INTE , &
              SectionPlan%DYDX , &
              Nb_pas           , &
              Impression       , &
              UniteListing     , &
              Erreur             &
                               )
         if( Erreur%Numero /= 0 ) then
            return
         endif
      else
         !   CALCUL DES CONDITIONS AUX LIMITES A L'AIDE DE LA CONTINUITE
         !     AMONT
         if( AMONT ) then
            if( FROD <= 0.99_DOUBLE ) then
               if( KHYLIM == 1 ) then
                  Q1(IS) = QFIX
                  if( PhaseSimulation == PHASE_INITIALISATION ) then
                     S1(IS) = SNODE(IS) + DT * ( QNODE(IS+1) - QFIX) / ( X(IS) - X(IS+1) )
                  else
                     S1(IS) = SNODE(IS) + DT * ( QNODE(IS+1) - QNODE(IS) ) / ( X(IS) - X(IS+1) )
                  endif
               endif
            else
               Q1(IS) = QFIX
               S1(IS) = CSUR( IS , YFIX , dz , SectionPlan%S , Nb_pas , Erreur )
               if( Erreur%Numero /= 0 ) then
                  return
               endif
            endif
         else
         !     AVAL
         !         DEBIT IMPOSE
            if( KHYLIM == CONDITION_TYPE_DEBIT_IMPOSE ) then
               Q1(IS) = QFIX
               S1(IS) = SNODE(IS) + DT * ( QNODE(IS-1) - QNODE(IS) ) / ( X(IS) - X(IS-1) )
            endif
            !
            ! COTE IMPOSEE
            !
            if( KHYLIM == CONDITION_TYPE_COTE_IMPOSE ) then
               S1(IS) = CSUR( IS , YFIX , dz , SectionPlan%S , Nb_pas , Erreur )
               if( Erreur%Numero /= 0 ) then
                  return
               endif

               YIS = CSURM1( SNODE(IS) , dz(IS) , SectionPlan%S(IS,:) , Erreur )
               if( Erreur%Numero /= 0 ) then
                  return
               endif

               YISM1 = CSURM1( SNODE(IS-1) , dz(IS-1) , SectionPlan%S(IS-1,:) , Erreur )
               if( Erreur%Numero /= 0 ) then
                  return
               endif

               ZIS   = YIS   + COTR(IS)
               ZISM1 = YISM1 + COTR(IS-1)
               YZIS  = ZIS - COTR(IS-1)
               YZISM1= ZISM1 - COTR(IS)

               SZIS = CSUR( IS - 1 , YZIS , dz , SectionPlan%S , Nb_pas , Erreur )
               if( Erreur%Numero /= 0 ) then
                  return
               endif

               SZISM1=CSUR(IS  ,YZISM1,dz,SectionPlan%S,Nb_pas,Erreur)
               if( Erreur%Numero /= 0 ) then
                  return
               endif

               PRIS = PRES( IS , SNODE(IS) , dz , SectionPlan%PRESS , SectionPlan%S , Nb_pas , Erreur )
               if( Erreur%Numero /= 0 ) then
                  return
               endif

               PRISM1 = PRES( IS - 1 , SNODE(IS-1) , dz , SectionPlan%PRESS , SectionPlan%S , Nb_pas , Erreur )
               if( Erreur%Numero /= 0 ) then
                  return
               endif

               PRIP = PRES( IS , SZISM1 , dz , SectionPlan%PRESS , SectionPlan%S , Nb_pas , Erreur )
               if( Erreur%Numero /= 0 ) then
                  return
               endif

               PRIPM1=PRES( IS-1 , SZIS , dz , SectionPlan%PRESS , SectionPlan%S , Nb_pas , Erreur )
               if( Erreur%Numero /= 0 ) then
                  return
               endif

               CHARIS = ( BETA(IS) * QNODE(IS) * QNODE(IS) / SNODE(IS) + PRIS )
               CHARM1 = ( BETA(IS-1) * QNODE(IS-1) * QNODE(IS-1) / SNODE(IS-1) + PRISM1 )
               SMOY = ( SNODE(IS) + SNODE(IS-1) ) / 2._DOUBLE
               QMOY = ( QNODE(IS) + QNODE(IS-1) ) / 2._DOUBLE

               call FROTTD( &
                         FROT , &
                       IS - 1 , &
                         SMOY , &
                         QMOY , &
             SECTIONPLAN%DEBD , &
               SECTIONPLAN%SD , &
                       Nb_pas , &
                       Nb_pas , &
                       Erreur )
               if( Erreur%Numero /= 0 ) then
                  return
               endif

               SOURC = - ( ( PRIS + PRIP ) - ( PRISM1 + PRIPM1 ) ) / ( 2._DOUBLE * ( X(IS) - X(IS-1) ) ) &
                       + GPES * ( SMOY * FROT )
               Q1(IS) = QNODE(IS) - DT * ( CHARIS - CHARM1 ) / ( X(IS) - X(IS-1) ) - DT * SOURC
            endif
         endif
      endif
   end do label_boucle_CL

   !       DECALAGE DES SINGULARITES
   !
   call DECBAR                     ( &
                Singularite(:)%Section , &
                   Singularite(:)%Type , &
              Singularite(:)%CoteCrete , &
                  Singularite(:)%Pente , &
                                    DT , &
                                NBARAD , &
            Singularite(:)%CoteRupture , &
                               nb_sing , &
                                 ZNODE , &
                                  COTR , &
                                Erreur )

   !      ==============================================================================================
   ! 1-2  BOUCLE SUR LES BIEFS POUR LE CALCUL DES BORNES LORSQUE l'OPTION ONDE DE SUBMERSION EST ACTIVEE
   !      ===============================================================================================
   label_boucle_bief : do IBIEF = 1 , nb_bief
      !     INITIALISATION DES BORNES
      IDEB(IBIEF) = Connect%OrigineBief(IBIEF)
      IFIN(IBIEF) = Connect%FinBief(IBIEF)

      if( CALCOS ) then
         !       CALCUL DES SECTIONS DE DEBUT ET DE FIN DE BIEF
         SLD = S1(Connect%OrigineBief(IBIEF))
         QLD = Q1(Connect%OrigineBief(IBIEF))
         SLF = S1(Connect%FinBief(IBIEF))
         QLF = Q1(Connect%FinBief(IBIEF))

         call BORNE        ( &
              SLF          , &
              QLF          , &
              INDIC        , &
              IBIEF        , &
              IFIN(IBIEF)  , &
              IDEB(IBIEF)  , &
              XFRON(IBIEF) , &
              X            , &
              SNODE        , &
              QNODE        , &
              Erreur         &
                           )

         S1(IFIN(IBIEF)) = SLF
         Q1(IFIN(IBIEF)) = QLF
         !
         ! IMPRESSION DE LA POSITION DU FRONT D'ONDE
         ! -------------------------------
         if( ( Phase_post_imp == PHASE_CALCUL ).OR.( Phase_post_imp == PHASE_INITIALISATION ) ) then
            if (UniteListing >0) then
               write(UniteListing,*) "BIEF NUMERO" , IBIEF , "PoSITION DU FRONT" , XFRON(IBIEF)
            endif
         endif
      endif
   end do label_boucle_bief

   !      =====================
   ! 1-2  BOUCLE SUR LES NOEUDS (CONFLUENTS)
   !      =====================
   if( nb_noeud >= 1 ) then
      label_boucle_noeud : do INOEUD = 1 , nb_noeud
         !     DETERMINATION DU BIEF PRINCIPAL AMONT
         IP  = Connect%NumBiefConfluence(INOEUD,1)
         IP1 = Connect%NumBiefConfluence(INOEUD,2)
         IP2 = Connect%NumBiefConfluence(INOEUD,3)
         !     IS SECTION AVAL DU BIEF
         IS = Connect%NumSectionConfluence(INOEUD,1)
         !     CE CONFLUENT EST-IL DANS LE DOMAINE DE CALCUL
         if( CALCOS ) then
            if( ( X(IS) > X(IFIN(IP) ) ).or.( X(IS) < X(IDEB(IP) ) ) ) then
            !
            !   Modification pour le reseau maille en OS (non active)
            !
               INDIC(IP1)    = -1
               INDIC(IP2)    = -1
               INDCO(INOEUD) = -1
            else
               INDIC(IP1)    =  1
               INDCO(INOEUD) =  1
               INDIC(IP2)    =  1

               if( ITEM0(INOEUD) < 3 ) then
                  IFIN(IP1)     = Connect%OrigineBief(IP1) + 10
                  IFIN(IP2)     = Connect%OrigineBief(IP2) + 10
                  S1(IFIN(IP2)) = SNODE(IFIN(IP2))
                  Q1(IFIN(IP2)) = QNODE(IFIN(IP2))
                  S1(IFIN(IP1)) = SNODE(IFIN(IP1))
                  Q1(IFIN(IP1)) = QNODE(IFIN(IP1))
               endif
            endif

            if( INDIC(IP1) == -1 ) cycle
            ITEM0(INOEUD) = ITEM0(INOEUD) +1
         endif

         call CONFLU         ( &
             S1              , &
             Q1              , &
             W(1:,1:,INOEUD) , &
             AIRS(1:,INOEUD) , &
             Confluent       , &
             X               , &
             QNODE           , &
             SNODE           , &
             ZNODE           , &
             COTR            , &
             STMIN           , &
             dz              , &
             SectionPlan%S   , &
             INOEUD          , &
             PhaseSimulation , &
             DT              , &
             HEPS            , &
             Nb_pas          , &
             Erreur            &
                            )

         if( Erreur%Numero /= 0 ) then
            return
         endif
      end do label_boucle_noeud
   endif

   !      ===============================
   ! 1-3  BOUCLE SUR LES APPORTS DE DEBIT
   !      ===============================
   call CQINJ (         &
         QIN          , & ! Vecteur Debit d'apport
         X            , & ! Abscisse des sections de calcul
         ZNODE        , & ! Cotes
         Apport       , & ! Apports
         Deversoir    , & ! Deversoirs
         Qdeverse     , &
         Erreur         & ! Erreur
                     )
   if (Erreur%Numero /= 0) then
      return
   endif

   do IS = 2 , size(QIN)
      QIN(IS) = QIN(IS) / ( X(IS) - X(IS-1) )
   end do

   !
   !       PRISE EN COMPTE DES DEBITS D'APPORTS
   do INOEUD = 1 , nb_sect
      QSTO(INOEUD) = QIN(INOEUD)
   end do

   !      ====================
   ! 1-3  BOUCLE SUR LES BIEFS
   !      ====================
   label_boucle_bief2: do IBIEF = 1,nb_bief

      if( INDIC(IBIEF) == -1 ) cycle
      IDE = IDEB(IBIEF)
      IFI = IFIN(IBIEF)
      call RESOL                   (  &
          SNODE                    , &
          QNODE                    , &
          UNODE                    , &
          ZNODE                    , &
          YNODE                    , &
          FROUD                    , &
          CNODE                    , &
          FLUX                     , &
          DebitFlux                , &
          JGNODE, JDNODE ,IFIGE    , &
          BETA                     , &
          S1(IFI)                  , &
          Q1(IFI)                  , &
          S1(IDE)                  , &
          Q1(IDE)                  , &
          QSTO                     , &
          X                        , &
          SectionPlan%S            , &
          SectionPlan%B            , &
          SectionPlan%SD           , &
          SectionPlan%PRESSD       , &
          SectionPlan%DEB          , &
          SectionPlan%DEBD         , &
          SectionPlan%S1GEO        , &
          COTR                     , &
          DZD                      , &
          dz                       , &
          DT , DTI                 , &
          Icompt                   , &
          HEPS , SEPS              , &
          IDE                      , &
          IFI                      , &
          NBARAD                   , &
          nb_sing                  , &
          Singularite              , &
          PCsing                   , &
          Sauve%Sprec              , &
          Sauve%Qprec              , &
          Nb_pas                   , &
          nb_sect                  , &
          FRTIMP                   , &
          Impli_Trans              , &
          PerteElargissementTrans  , &
          Boussinesq               , &
          CQMV                     , &
          Erreur                     &
                                    )

      if (Erreur%Numero /= 0) then
         print *,'erreur resol'
         return
      endif
   end do label_boucle_bief2

   if( CALCVA ) then
     ! (ATTENTION : NE FONCTIONNE QUE SUR BIEF UNIQUE)
      call VALIDA              ( &
           SVRAI               , &
           QVRAI               , &
           UVRAI               , &
           ZVRAI               , &
           YVRAI               , &
           Q1(1)               , &
           S1(1)               , &
           S1(nb_sect)         , &
           X                   , &
           COTR                , &
           Temps               , &
           PhaseSimulation     , &
           nb_sect             , &
           IVALID              , &
           Impression          , &
           UniteListing        , &
           Erreur                &
                               )
      if( Erreur%Numero /= 0 ) then
         return
      endif
   endif

   ! CALCUL DE L'ENSEMBLE DES VARIABLES
   ! ----------------------------------
   call       CALVAR (    &
        QMIN  , QMAJ    , &
        SMIN  , SMAJ    , &
        BETA            , &
        XFRON           , &
        YNODE           , &
        FROUD           , &
        UNODE           , &
        CNODE           , &
        ZNODE           , &
        QNODE           , &
        SNODE           , &
        JGNODE , JDNODE , &
        IFIGE           , &
        ZINIT           , &
        X               , &
        IDEB , IFIN     , &
        COTR            , &
        SectionPlan     , &
        DZ              , &
        T               , &
        nb_bief         , &
        Nb_Pas          , &
        CALCOS          , &
        AVAL            , &
        Erreur            &
                        )
   if( Erreur%Numero /= 0 ) then
      return
   endif
   !    CALCUL DU NOMBRE DE COURANT, DU PAS DE Temps,
   DTPREC = DT
   if( PhaseSimulation == PHASE_INITIALISATION ) then
      Temps   = Temps + DT
      Num_pas = Num_pas + 1
   endif

   call CALCFL( DT , DTI , IFIGE , ICOMPT , Opt , UNODE , CNODE , X , IDEB , IFIN , NombreCourant , nb_bief , &
                   Phase_Post_Imp , UniteListing , PasTempsVariable , Erreur )

   if (Erreur%Numero /= 0) then
      return
   endif

   !    CALCUL DU BILAN
   call BILAN                   ( &
       Sauve%H2OIB, Sauve%H2OTB , &
       Sauve%H2OEB, Sauve%H2OSB , &
      Sauve%H2OIBS,Sauve%H2OTBS , &
                          CONSB , &
       Sauve%H2OIC, Sauve%H2OTC , &
       Sauve%H2OEC, Sauve%H2OSC , &
                          CONSC , &
       Sauve%H2OIG, Sauve%H2OTG , &
       Sauve%H2OEG, Sauve%H2OSG , &
      Sauve%H2OIGS,Sauve%H2OTGS , &
                          CONSG , &
                          SNODE , &
                           VOLS , &
                          QNODE , &
                            QIN , &
                              X , &
                        Connect , &
                         nb_ext , &
                              W , &
                           AIRS , &
                      Confluent , &
                    IDEB , IFIN , &
                          INDIC , &
                          INDCO , &
            Barrage%abscisseRel , &
              Barrage%CoteCrete , &
                             DT , &
                PhaseSimulation , &
                 Phase_post_imp , &
        NPMAX , Temps , num_pas , &
                   TempsMaximun , &
             nb_bief , nb_noeud , &
                     Impression , &
                   UniteListing , &
                         CALCOS , &
                       STOCKAGE , &
                         Erreur   &
                                )
   if( Erreur%Numero /= 0 ) then
      return
   endif

   ! Fin des traitements
   ! -------------------

   !Erreur%arbredappel = !arbredappel_old

   return

end subroutine MASCARET_INTERFACE
